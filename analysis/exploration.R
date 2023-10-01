

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(slider)

# Data --------------------------------------------------------------------

source(here::here("injury", "data", "fn_data.R"))
df <- dh_getQuery(postgre_con, "injury_query.sql")
anl_cols <- colnames(df)[16:length(colnames(df))]
df <- mutate(df, plot_injury_date = if_else(!is.na(injury_date), game_date, NA))


# Plot --------------------------------------------------------------------

df_plt <- filter(df, year_season >= 2023) |> 
  pivot_longer(cols = all_of(anl_cols), names_to = "stat") |> 
  nest_by(stat, .keep = TRUE) |> 
  mutate(stat = ordered(stat, anl_cols)) |> 
  arrange(stat)
df_plt$data <- map(df_plt$data, print) 

df_plt <- mutate(df_plt, plot = list(
  ggplot(data, aes(x = game_date, y = value)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = df$plot_injury_date, colour = "red") +
  labs(title = stat)
))

pull(df_plt, plot)



# Top 5 categories --------------------------------------------------------

top_cats_df <- dh_getQuery(postgre_con, "top_cats.sql") |> 
  slice_max(order_by = min, prop = 0.75) |> 
  mutate(across(all_of(anl_cols), \(x) scales::rescale(x))) |> 
  pivot_longer(all_of(anl_cols), names_to = "stat") |> 
  group_by(player_id) |> 
  slice_max(order_by = value, n = 5, with_ties = FALSE) |> 
  arrange(player_id, desc(value)) |> 
  mutate(stat_order = paste0("stat_", 1:5)) |> 
  select(player_id, player_name, stat, stat_order)

injury_df <- dh_getQuery(postgre_con, "injury_query.sql") |> 
  group_by(player_id, year_season) |> 
  mutate(across(all_of(anl_cols), ~ replace_na(.x, 0))) |> 
  mutate(across(all_of(anl_cols), ~ scales::rescale(.x))) |> 
  pivot_longer(all_of(anl_cols), names_to = "stat") |> 
  inner_join(top_cats_df, join_by(player_id, player_name, stat)) |> 
  select(-stat) |> 
  arrange(cat_spec) |> 
  pivot_wider(names_from = cat_spec)


# Feature Engineering -----------------------------------------------------

df_model <- group_by(injury_df, player_id, year_season) |> 
  arrange(game_date) |> 
  mutate(across(c(min, tov, plus_minus, pf), scales::rescale)) |> 
  mutate(across(all_of(colnames(injury_df)[12:20]), ~ slide_mean(.x, before = 5, after=-1), .names = "slide5_mean_{.col}")) |>
  mutate(across(all_of(colnames(injury_df)[12:20]), ~ slide_dbl(.x, sd, .before = 5, .after=-1), .names = "slide5_sd_{.col}")) |>
  mutate(across(all_of(colnames(injury_df)[12:20]), ~ slide_dbl(.x, possibly(\(obs) line(obs)$coefficients[2], NA_real_), .before = 5, .after=-1), .names = "slide5_grad_{.col}"))

# How to get this working using across
for(x in colnames(injury_df)[12:20]) df_model[paste0("obs-slide_", x)] = df_model[x] - df_model[paste0("slide5_mean_", x)]

for(x in colnames(injury_df)[12:20]){
  df_model[paste0("sd-diff_", x)] = case_when(
    df_model[x] < df_model[paste0("slide5_mean_", x)] - df_model[paste0("slide5_sd_", x)] ~ "less than one sd",
    df_model[x] > df_model[paste0("slide5_mean_", x)] + df_model[paste0("slide5_sd_", x)] ~ "greater than one sd",
    is.na(df_model[paste0("slide5_sd_", x)]) ~ NA,
    .default = "within one sd"
  )
}


# Opponent
df_model <- df_model |> 
  mutate(opponent = str_remove(matchup, "\\w{3}( vs. | @ )")) |> 
  mutate(prev_opponent = lag(opponent, order_by = game_date))

# Result in injury
df_model <- mutate(df_model, injury_result = !is.na(notes)) |> 
  ungroup()



# model -------------------------------------------------------------------

glm(
  data = na.omit(select(df_model, -c(1:10), -starts_with("cat_"), -c(min, tov, plus_minus, pf), -contains("_sd_"), -ends_with("opponent"))),
  formula = "injury_result ~ .",
  family = binomial()
) |> summary()



