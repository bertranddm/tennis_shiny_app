#' Updates the table of all matches
#'
#' Allows us to update all along our dataset according to the updates to source data
#'
#' This goes online and downloads the latest datasets and updates our base function dataset
#' @param central_matches current database of matches
#' @import dplyr
#' @import purrr
#' @importFrom rlang .data


tennis_update <- function(central_matches){

  df <- central_matches %>% select(- .data$X)

  update_date <- df %>% select(.data$tourney_date) %>% distinct(.data$tourney_date) %>%
    top_n(1) %>%
    as.character() %>%
    as.Date(format = '%Y%m%d')

  tyear <- as.numeric(substr(as.character(Sys.Date()),1,4))


  last_matches <- read.csv(file=paste("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_",tyear,".csv",sep = ""))

  last_matches <- last_matches %>%
    select(.data$tourney_date) %>%
    distinct(.data$tourney_date) %>%
    top_n(1) %>%
    as.character() %>%
    as.Date(format = '%Y%m%d')

  update_year <- as.numeric(substr(as.character(update_date),1,4))


  if(update_date < last_matches){

    difference <- tyear-update_year
    years_missing <- c((tyear - (difference)):tyear)
    df <- union(df,map_df(years_missing,import_central_matches)) %>% distinct()

    df %>% write.csv(file="atp_matches_combined.csv")
  }
  else{NULL}



  return(as.data.frame(df) %>% distinct())
}

#' Fills the blanks in the dataset
#'
#' Fills the Nones in the surface collumn for the Davis Cup 2017
#'
#' @param central_matches dataset of matches
#'
#' @import utils
#' @import dplyr
#' @importFrom rlang .data


data_fill <- function(central_matches) {
  central_matches <- central_matches %>% mutate(surface = as.character(.data$surface))
  central_matches <- central_matches %>% mutate(surface = case_when(.data$surface == "None" ~ "Hard",
                                                                    TRUE ~ .data$surface
  ))
  central_matches <- central_matches %>% mutate( tourney_name = case_when(grepl("a del Mar",.data$tourney_name) ~ "Vina del Mar", TRUE ~ .data$tourney_name))
  return(central_matches)
}
