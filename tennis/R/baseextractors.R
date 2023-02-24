#' Creating talbe linking player to ID
#'
#' Create a table associating ID to player based on central_matches (Name Surname format)
#'
#' @param central_matches dataset of matches
#' @import utils
#' @import dplyr
#' @importFrom rlang .data


player_ID_table <- function(central_matches) {
  IDtblw <- central_matches %>%
    select(.data$winner_id,.data$winner_name) %>%
    group_by(.data$winner_id) %>%
    distinct() %>%
    ungroup() %>%
    rename("IDw" = "winner_id" ) %>%
    rename("name" = "winner_name")

  IDtbll <- central_matches %>%
    select(.data$loser_id,.data$loser_name) %>%
    group_by(.data$loser_id) %>%
    distinct() %>%
    ungroup() %>%
    rename("IDl" = "loser_id" ) %>%
    rename("name" = "loser_name")

  IDtbl <- full_join(.data$IDtbll, .data$IDtblw, by = "name") %>% mutate(
    ID = case_when(is.na(.data$IDw) == F ~ .data$IDw,T ~ .data$IDl)) %>%
    select(-.data$IDl,-.data$IDw)
}

#' Finding an ID from name
#'
#' Finding ID of player with his full name from the Id table
#'
#' @param Name full name of player
#' @param data id table
#' @import utils
#' @import dplyr
#' @importFrom rlang .data


findID <- function(Name, data = .data$Idtbl) {
  x <- .data$IDtbl %>% filter(.data$name == Name)
  x <- as.numeric(x$ID)
  return(x)
  remove(x)
}


#' Finding an name from ID
#'
#' Finding name of player using ID from the Id table
#'
#' @param id id of player
#' @param data id table
#' @import utils
#' @import dplyr
#' @importFrom rlang .data



ID_to_name <- function(id, data = .data$IDtbl) {
  ID <- .data$IDtbl %>% filter(ID == id) %>%
    select(.data$name)
  return(ID)
}
