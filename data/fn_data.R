
# Database Connections & Functions ----------------------------------------

db_info <- ini::read.ini(here::here("database.ini"))

if(stringr::str_detect(osVersion, "macOS")){
  postgre_con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    user = db_info$postgre$user,
    host = db_info$postgre$host,
    port = db_info$postgre$port,
    password = db_info$postgre$password,
    dbname = db_info$postgre$database,
    options="-c search_path=nba"
  )
}


# cockroach_con <- DBI::dbConnect(
#   drv = RPostgres::Postgres(),
#   user = db_info$cockroach$user,
#   host = paste0(db_info$cockroach$cluster, ".", db_info$cockroach$host),
#   port = db_info$cockroach$port,
#   password = db_info$cockroach$password,
#   dbname = db_info$cockroach$database,
#   options = paste0("--cluster=", db_info$cockroach$cluster)
# )


dh_getQuery <- function(connection, query, glue_params=NULL){
  
  query <- if(!stringr::str_detect(query, ".sql$")) query
    else glue::glue(readr::read_file(here::here("injury", "data", query)))
  
  connection |> 
    DBI::dbGetQuery(query) |> 
    tibble::as_tibble() |> 
    dplyr::mutate(dplyr::across(where(~ class(.x) == "integer64"), ~ as.integer(.x)))
}