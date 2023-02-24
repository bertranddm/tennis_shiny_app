#' Player match history
#'
#' Match history of a player from his ID with all collumns
#'
#' Based on central matches computes the players match history
#'
#' @param ID player id
#' @param data match central database
#' @import utils
#' @import dplyr
#' @importFrom rlang .data
#' @export

player_history_table_raw <- function(ID, data) {
  central_matches_temp <- data %>%
    filter(.data$winner_id == ID | .data$loser_id == ID) %>%
    mutate(status = case_when(.data$winner_id == ID ~ "Win",T ~ "Loss"),
           opponent = case_when(.data$winner_id == ID ~ .data$loser_name,
                                T ~ .data$winner_name),
           year = as.integer(substr(.data$tourney_date, 1, 4)))
  return(central_matches_temp)
}

#' Streamlined match history
#'
#' Match history of a player from his ID with only relevant collumns
#'
#' @param ID player id
#' @param data criterias through selected input data
#' @seealso player_history_raw
#' @import utils
#' @import dplyr
#' @importFrom rlang .data
#' @export

player_history_table <- function(ID, data) {
  tablehist <- player_history_table_raw(ID, data) %>%
    select(.data$status,.data$tourney_date,.data$opponent,.data$score,.data$surface)
  return(tablehist)
}

#' Multiple players history
#'
#' Matches history of several players from vector of IDs with all columns (histories of each player are binded by row)
#'
#' @param IDs player ids
#' @param data criterias through selected input data
#' @seealso player_history_table_raw
#' @import utils
#' @import dplyr
#' @importFrom rlang .data
#' @export

player_history_table_raw_multiple <- function(IDs, data) {
  n <- length(IDs)
  players_history <- map_dfr(IDs, player_history_table_raw, data = data, .id = "id")
  names <- map(as.character(IDs), ID_to_name)
  for (i in 1:length(players_history$id)) {
    players_history$id[i] <- as.character(names[[as.integer(players_history$id[i])]])
  }
  players_history <- players_history %>%
    mutate(id = as.factor(id),
           round = as.character(round))

  return(players_history)
}

#' Player elo history
#'
#' Yearly elo for a given player
#'
#' Elo history of a player from ID : returns the ELO of the player at the end of each year from 1968 to 2018
#'
#' @param ID player id
#' @param data source
#' @seealso player_history_table_raw
#' @import utils
#' @import dplyr
#' @importFrom rlang .data
#' @export

elo_history <- function(ID, data) {
  elo_history <- player_history_table_raw(ID, data) %>%
    group_by(.data$year) %>%
    slice(n()) %>%
    summarise(elo = case_when(
      .data$winner_id == ID ~ elo1,
      .data$loser_id == ID ~ elo2))
  time_range <- data.frame(year = 1968:2018)

  return(full_join(time_range, elo_history %>% mutate(year = as.numeric(.data$year)), by = "year"))
}



#' Multiple elo history
#'
#' Yearly elo for several players
#'
#' ELOs history of several players from a vector of IDs with all columns : returns the ELO of each player at the end of each year from 1968 to 2018 (histories are binded by row)
#'
#' @param IDs list of player IDs
#' @param data criterias
#' @import utils
#' @import dplyr
#' @importFrom rlang .data
#' @export

elo_history_multiple <- function(IDs, data) {
  players_elo_history <- map_dfr(IDs, elo_history, data = data, .id = "id")

  names <- map(as.character(IDs), ID_to_name)

  for (i in 1:length(players_elo_history$id)) {
    players_elo_history$id[i] <- as.character(names[[as.integer(players_elo_history$id[i])]])
  }
  players_elo_history <- players_elo_history %>% mutate(id = as.factor(id))
  return(players_elo_history)
}
