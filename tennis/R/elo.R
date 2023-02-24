#' Computes the elo
#'
#' Basic function to compute the elo from the dataset of matches
#'
#' @param central_matches dataset of matches
#'
#' @import utils
#' @import dplyr
#' @import elo
#' @importFrom rlang .data
#' @export

elo_calculation <- function(central_matches) {

  central_matches_temp <- central_matches

  central_matches_temp <- central_matches_temp %>%
    mutate(tourney_id = as.character(.data$tourney_id)) %>%
    mutate(
      score1 = 1, # Winners always score 1
      score0 = 0, # Loosers always score 0
      winner_id = as.character(.data$winner_id),
      loser_id = as.character(.data$loser_id)
    )

  elo_run <- elo.run(formula =  score(score1 , score0) ~ winner_id + loser_id, data = central_matches_temp, k = 50)

  elo_scores <- elo_run[["elos"]]
  elo_players <- elo_run[["teams"]]

  elo_scores <- as.data.frame(elo_scores)
  colnames(elo_scores)[1:7] <- c("id1","id2","proba","winner","diffelo","elo1","elo2")
  elo_scores$id1 <- lapply(elo_scores$id1,FUN = function(x){elo_players[[x]]}) #ID update from elo_players
  elo_scores$id2 <- lapply(elo_scores$id2,FUN = function(x){elo_players[[x]]})
  elo_scores <- elo_scores %>% select(-.data$winner)

  return(elo_scores)
}

#' Format the results of elo computation
#'
#' Puts the results of elo computations in the dataset by adding elo, prob of winning and change fom before columns
#'
#' @param central_matches dataset of matches
#' @seealso elo_calculation
#' @import utils
#' @import dplyr
#' @import tidyr
#' @import elo
#' @importFrom rlang .data
#' @export

elo_central_matches <- function(central_matches) {
  central_matches_temp <- central_matches %>% drop_na(.data$tourney_date,.data$winner_id)
  elos <- elo_calculation(central_matches)
  central_matches_temp <- cbind.data.frame(central_matches_temp,elos)
  central_matches_temp <- central_matches_temp %>% select(-c(.data$id1,.data$id2)) # We can get rid of id1 and id2 since it is redundant with winner_id and loser_id
  return(central_matches_temp)
}


#' Computes elo for each surface
#'
#' Separates the elo calculations for each surface
#'
#' @param surface list of pertinent surfaces
#' @seealso elo_central_matches
#' @import utils
#' @import dplyr
#' @importFrom rlang .data
#' @export

elo_surface <- function(surface = c(levels(as.factor(.data$central_matches$surface)))){
  df <- .data$central_matches %>% filter(.data$surface %in% surface)
  df <- elo_central_matches(df)
  return(df)

}

#' Elo of a player
#'
#' Function to return last elo for a player and given a dataframe with elo computed with wanted criterias
#'
#' @param ID id of considered player
#' @param data critera
#' @seealso elo_central_matches
#' @import utils
#' @import dplyr
#' @importFrom rlang .data
#' @export

elo_player <- function(ID, data) {
  data %>%
    filter(.data$winner_id == ID | .data$loser_id == ID) %>%
    mutate(elo = case_when(
      .data$winner_id == ID ~ elo1,
      .data$loser_id == ID ~ elo2)) %>%
    slice(n()) %>%
    select(.data$elo)
}

#' Probable outcomes
#'
#' Calculate the probable outcome of a match using the elo
#'
#' @param ID1 id of 1st player
#' @param ID2 id of 2nd player
#' @param data criterias through selected input data
#' @seealso elo_player
#' @import utils
#' @import dplyr
#' @importFrom rlang .data
#' @export

prob_win <- function(ID1,ID2, data) {
  prob <- elo.prob(as.numeric(elo_player(ID1, data)),as.numeric(elo_player(ID2, data)))
  return(prob)
}


#' Bet
#'
#' Allows the user to bet on a game based on the elo prediction
#'
#' @param odd1 odds of first player
#' @param odd2 odds of second player
#' @param ID1 id of 1st player
#' @param ID2 id of 2nd player
#' @param risk risk factor
#' @param data input of filtered data
#' @seealso prob_win
#' @import utils
#' @import dplyr
#' @importFrom glue glue
#' @importFrom rlang .data
#' @export

odds_bet<- function(odd1,odd2,ID1,ID2, risk = 5, data){
  myprobwin1 <- prob_win(ID1,ID2, data)
  pwinbetter1 <- 1/odd1
  pwinbetter2 <- 1/odd2

  seuil <- 1/(risk*5)

  delta1 <- myprobwin1 - pwinbetter1
  delta2 <- 1-myprobwin1 - pwinbetter2

  if (pwinbetter1 + pwinbetter2 < 1) {
    recommandation <- glue::glue("that you check the betting odds again! If they are correct you can bet on both players in such ways so you will win money whatever happens : Bet at least {round(pwinbetter1,2)} of the amount on player 1,  and at least {round(pwinbetter2,2)} of the amount on player 2.")
  }

  else {

    if (delta1 >= seuil & delta2 <= seuil){
      recommandation <- glue::glue("betting on {ID_to_name(ID1)}")
    }
    else if (delta2 >= seuil & delta1 <= seuil){
      recommandation <- glue::glue("betting on {ID_to_name(ID2)}")
    }
    else if (delta1 <= seuil & delta2 <= seuil){
      recommandation <- "not betting"
    }

  }

  recommandation <- glue::glue("We would recommand {recommandation} !")

  return(list(myprobwin1, 1-myprobwin1, recommandation))
}
