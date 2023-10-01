
library(tidymodels)
library(tidyverse)
library(slider)
library(baguette)
library(discrim)
library(themis)
library(doParallel)
source(here::here("injury", "data", "fn_data.R"))


# Top Categories ----------------------------------------------------------

anl_cols <- c("fg2_a", "fg2_pct", "fg3_a", "fg3_pct", "fta", "ft_pct", "oreb", "dreb", "ast", "stl", "blk")
top_cats_df <- dh_getQuery(postgre_con, "top_cats.sql") |> 
  slice_max(order_by = min, prop = 0.75) |> 
  mutate(across(all_of(anl_cols), \(x) scales::rescale(x))) |> 
  pivot_longer(all_of(anl_cols), names_to = "stat") |> 
  group_by(player_id) |> 
  slice_max(order_by = value, n = 5, with_ties = FALSE) |> 
  arrange(player_id, desc(value)) |> 
  mutate(stat_order = paste0("stat_", 1:5)) |> 
  select(player_id, player_name, stat, stat_order)


# Pre processing ------------------------------------------------------------------

df_orig <- dh_getQuery(postgre_con, "modelling_injury_query.sql") |> 
  mutate(injury_flag = as.factor(injury_flag))

rescale_df <- inner_join(df_orig, distinct(top_cats_df, player_id)) |>  
  pivot_longer(all_of(anl_cols), names_to = "stat") |> 
  group_by(player_id, year_season, stat) |> 
  summarise(maximum = max(value), minimum = min(value)) |> 
  (\(df){
    max_season <- max(df$year_season)
    t_df <- filter(df, year_season == max_season) |> 
      mutate(year_season = max_season + 1)
    
    bind_rows(df, t_df)
  })() |> 
  inner_join(select(top_cats_df, -player_name), by = join_by(player_id, stat))

# Before recipe processing
df <- pivot_longer(df_orig, all_of(anl_cols), names_to = "stat") |> 
  inner_join(rescale_df, by = join_by(player_id, year_season, stat)) |> 
  mutate(value = coalesce((value - minimum) / (maximum - minimum), 0)) |> 
  select(-c(maximum, minimum, stat)) |> 
  arrange(stat_order) |> 
  pivot_wider(names_from = stat_order) |> 
  mutate(height = case_when(
    str_length(height) < 4 ~ as.numeric(str_replace(height, "-", ".0")),
    .default = as.numeric(str_replace(height, "-", "."))
  ))

# Split
df_split <- initial_time_split(group_by(df, player_id, year_season), lag = 5)
df_train <- training(df_split)
df_test <- testing(df_split)

# Recipe
original_rec <- recipe(injury_flag ~ ., data = df_train) |> 
  update_role(c(player_id, player_name), new_role = "id") |> 
  step_range(age, height, weight, years_experience, min, pf, tov, plus_minus) |> 
  step_arrange(game_date) |> 
  step_mutate(across(starts_with("stat_"), ~ slide_mean(.x, before = 5, after=-1), .names = "mean5_{.col}")) |>
  step_mutate(across(starts_with("stat_"), ~ slide_dbl(.x, sd, .before = 5, .after=-1), .names = "sd5_{.col}")) |>
  step_mutate(across(starts_with("stat_"), ~ slide_dbl(.x, possibly(\(obs) line(obs)$coefficients[2], NA_real_), .before = 5, .after=-1), .names = "grad5_{.col}")) |> 
  # step_mutate()   .default = "within one sd". HOW TO DO THIS?
  step_rm(slug_season, game_date, game_id, injury_date, notes, matchup, starts_with("sd5_"), starts_with("stat_")) |> # maybe rm year_season
  step_naomit(everything()) |> 
  step_dummy(position, season_type, wl) # Have to make this step last other NA error

smote_rec <- step_smotenc(original_rec, injury_flag)
upsample_rec <- step_upsample(original_rec, injury_flag)
downsample_rec <- step_downsample(original_rec, injury_flag)
  

# bake(rec, new_data = df_train) |> view(">")
recs <- mget(str_subset(objects(), "_rec"))



# Models ------------------------------------------------------------------

# baguette
bag_tree_rpart_spec <-
  bag_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

boost_tree_xgboost_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

decision_tree_rpart_spec <-
  decision_tree(tree_depth = tune(), min_n = tune(), cost_complexity = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

logistic_reg_glmnet_spec <-
  logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

# discrim
naive_Bayes_naivebayes_spec <-
  naive_Bayes(smoothness = tune(), Laplace = tune()) %>%
  set_engine("naivebayes")

nearest_neighbor_kknn_spec <-
  nearest_neighbor(neighbors = tune(), weight_func = tune(), dist_power = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

rand_forest_ranger_spec <-
  rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")

svm_poly_kernlab_spec <-
  svm_poly(cost = tune(), degree = tune(), scale_factor = tune(), margin = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

logistic_reg_glm_spec <-
  logistic_reg() %>%
  set_engine("glm")


mods <- mget(str_subset(objects(), "_spec"))


# Workflow & Tuning -------------------------------------------------------


wflows <- workflow_set(recs, mods)

# Parallel processing
# cl <- makePSOCKcluster(parallel::detectCores(logical = FALSE) / 2)
# registerDoParallel(cl)

# Tuning
tuned_mods <- workflow_map(
  wflows, 
  "tune_grid", 
  resamples = vfold_cv(df_train, strata = injury_flag, v = 5), 
  grid = 10,
  metrics = metric_set(accuracy, f_meas, sens, specificity), 
  verbose = TRUE
)

# Turn off parallel processing
# stopCluster(cl)


# Plot model performance --------------------------------------------------

autoplot(tuned_mods)



# Best workflow -----------------------------------------------------------

fin_mod <- extract_workflow(wflows, "rec_decision_tree_rpart_spec")
fit <- fit(fin_mod, df_test)

predict(fit, df_test) |>
  bind_cols(df_test["injury_flag"]) |> 
  conf_mat(injury_flag, .pred_class)

