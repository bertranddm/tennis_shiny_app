#' Import yearly matches
#'
#' Adds matches from one year to the dataset
#'
#' This goes online and downloads and concatenates the tables of yearly matches from one year to the current table
#'
#' @param i year to import
#' @param central_matches database preceeding the call to the function
#' @import utils
#' @import dplyr



import_central_matches <- function(i,central_matches = as.data.frame(c())){
  years_matches <- read.csv(file=paste("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_",i,".csv",sep = ""),encoding = "ASCII")
  central_matches <- central_matches %>% bind_rows(years_matches)
  return(central_matches)
}

#' Import all matches
#'
#' Adds all matches according to dataset
#'
#' This goes online and downloads all dataset for the relevant years
#'
#' @import utils
#' @import dplyr
#' @import purrr


import <- function(){
  timeline <- c(1968:as.numeric(substr(as.character(Sys.Date()),1,4)))
  central_matches <- map_df(timeline,import_central_matches) %>% distinct()
  central_matches %>% write.csv(file="atp_matches_combined.csv") %>% iconv("latin1", "ASCII", sub="n")
  return(central_matches)
}

#' Import all players
#'
#' Adds all players from a table of players
#'
#' This goes online and downloads all players from a table
#'


importplayer <- function(){
  players <- read.csv(file = ("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_players.csv"))
  return(players)
}


