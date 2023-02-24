
library(tidyverse)
library(purrr)
library(shiny)
library(DT)

#' User ui
#'
#' Shiny app to render an user UI
#'
#' Based on central matches computes the players match history
#'
#' @import utils
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import purr
#' @import shiny
#' @import ggplot2
#' @import DT
#' @importFrom rlang .data


##Ajouter les 2 datasets central matchs et IDtbl + stat table qui seront stockés dans le package

ui <- fluidPage(

  tabsetPanel(

    tabPanel(title ="Tennis players performance evolution", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput("players_names", label = "Select players:", multiple = TRUE, choices = IDtbl$name, selected = c("Roger Federer", "Rafael Nadal")),
                 sliderInput(inputId = "time_range",label = "choose a time range",value = c(1968,2018),min=1968,max=2018),
                 checkboxGroupInput(inputId = "surface",label = "Surface selection", choices = c("Clay" = "Clay", "Grass" = "Grass", "Carpet" = "Carpet", "Hard" = "Hard"),selected = c("Clay", "Carpet", "Hard", "Grass")),
                 checkboxInput(inputId = "final_stages", label = "Display only final stages (QF, SF & F) ?"),
                 checkboxInput(inputId = "grand_slam", label = "Take into account only Grand Slam tourneys ?"),
                 selectInput(inputId = "stat", label = "Select the statistic you want to plot:", multiple = FALSE, choices = c("mean number of aces per match",
                                                                                                                               "mean number of double fault",
                                                                                                                               "mean percentage of first serve in per match",
                                                                                                                               "mean percentage of first serve won per match",
                                                                                                                               "mean percentage of second serve won per match",
                                                                                                                               "mean percentage of break points saved"))
                 ),
               mainPanel(
                 plotOutput("plot_elo"),
                 plotOutput("plot_wins"),
                 plotOutput("plot_stat")
                 )
             )
    ),

    tabPanel(title ="Bet prediction", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput("player1", label = "Select first player", choices = IDtbl$name, selected = "Roger Federer"),
                 numericInput("odd_player1", label = "Enter first player odd", value = 1),
                 selectInput("player2", label = "Select second player", choices = IDtbl$name, selected = "Richard Gasquet"),
                 numericInput("odd_player2", label = "Enter second player odd", value = 1),
                 selectInput("surface2", label = "Select the Surface", choices = c("Clay" = "Clay", "Grass" = "Grass", "Carpet" = "Carpet", "Hard" = "Hard"), selected = "Grass"),
                 checkboxInput(inputId = "final_stages2", label = "Display only final stages (QF, SF & F) ?"),
                 checkboxInput(inputId = "grand_slam2", label = "Take into account only Grand Slam tourneys ?"),
                 sliderInput(inputId = "risk",label = "Choose your level of risk", value = 5, min = 1, max = 20, step = 1),
                 actionButton(inputId = "go",label = "go",icon = icon("refresh"))
                 ),
               mainPanel(
                 tabsetPanel(
                   tabPanel(title = "Recommandation",
                            textOutput("reco2"),
                            textOutput("reco3"),
                            textOutput("reco4"),
                            plotOutput("proba_plot")),
                   tabPanel(title = "History first player",
                            DT::DTOutput("last_player1")),
                   tabPanel(title = "History second player",
                            DT::DTOutput("last_player2"))
                 )
               )
             )
    )
  )
)



server <- function(input, output) {

  ### Tab n°1 ###
  #_______________________________________________________________________________________________

  #Getting ID with the names
  players_ids <- reactive({
    req(input$players_names)
    as.vector(t(IDtbl %>% filter(name %in% input$players_names) %>% select(ID)))
  })

  #getting right surface
  data_surface <- reactive({
    central_matches %>%
      filter(surface %in% input$surface)
  })

  #getting right tourneys
  data_tourneys <- reactive({
    req(data_surface())
    if (input$grand_slam == TRUE) {
      data_surface() %>% filter(tourney_name %in% c("Roland Garros", "US Open", "Wimbledon","Australian Open"))
    }
    else {
      data_surface()
    }
  })

  #Getting right rounds
  data_rounds <- reactive({
    req(data_tourneys())
    if (input$final_stages == TRUE) {
      data_tourneys() %>% filter(round %in% c("QF", "SF", "F"))
    }
    else {
      data_tourneys()
    }
  })

  #getting selected players history
  data_players <- reactive({
    req(players_ids(), data_rounds())
    player_history_table_raw_multiple(players_ids(), data_rounds())
  })

  #Function for plotting % of wins of selected players
  wins_plot <- function(data) {
    data %>%
      group_by(year, id) %>%
      summarise(total_wins = sum(status == "Win")/n()*100) %>%
      ggplot(aes(x = year, y = total_wins, colour = id)) +
      geom_line() +
      labs(title = "Annual proportion of wins per year", x = "Years", y = "wins", colour = "Players")
  }

  stat_table <- tibble(choices = c("mean number of aces per match",
                                   "mean number of double fault",
                                   "mean percentage of first serve in per match",
                                   "mean percentage of first serve won per match",
                                   "mean percentage of second serve won per match",
                                   "mean percentage of break points saved"),
                       short = c("ace",
                                 "df",
                                 "1stin_n",
                                 "1stWon_n",
                                 "2ndWon_n",
                                 "bp_n")
  )

  #Statistic to plot
  statp <- reactive({
    req(input$stat)
    as.character(
    stat_table %>%
      filter(choices == input$stat) %>%
      select(short)
    )
  })

  #computing elo on matches corresponding to parameters
  elo_dataframe <- reactive({
    req(data_rounds())
    data_rounds() %>%
      elo_central_matches()
  })

  #Getting elo history for the given IDs and criterias
  elo_hist <- reactive({
    req(players_ids(), elo_dataframe())
    elo_history_multiple(players_ids(), elo_dataframe())
  })

  #plotting elo function
  elo_plot <- function(data) {
    data %>% na.omit() %>%
      ggplot(aes(x = year, y = elo, colour = id)) +
      geom_line() +
      labs(title = "ELO Evolution over time", x = "Years", y = "ELO", colour = "Players")
  }


  #Outputs for Tab 1 :

  output$plot_elo <- renderPlot({
  req(elo_hist())
  elo_plot(elo_hist()) +
    coord_cartesian(xlim = input$time_range)
  })

  output$plot_wins <- renderPlot({
    req(data_players())
    wins_plot(data_players()) +
      coord_cartesian(xlim = input$time_range)
  })

  output$plot_stat <- renderPlot({
    req(data_players(), statp())
    stat_plot(data_players(), stat = statp()) +
      labs(title = glue::glue("Evolution of {input$stat} over time"), x = "years", y = input$stat) +
      coord_cartesian(xlim = input$time_range)
  })


  ### Tab n°2 ###
  #__________________________________________________________________________________________________

  #Getting ID with the names of the two players
  opponent_id1 <- eventReactive(input$go, {
    req(input$player1)
    as.vector(t(IDtbl %>%
                  filter(name == input$player1) %>%
                  select(ID)
    )
    )
  })

  opponent_id2 <- eventReactive(input$go, {
    req(input$player2)
    as.vector(t(IDtbl %>%
                  filter(name == input$player2) %>%
                  select(ID)
    )
    )
  })

  name1 <- eventReactive(input$go, {
    input$player1
  })

  name2 <- eventReactive(input$go, {
    input$player2
  })

  ### Getting data on which to run elo computation and take history

  #getting right surface
  data_surface2 <- eventReactive(input$go, {
    central_matches %>%
      filter(surface %in% input$surface2)
  })

  #getting right tourneys
  data_tourneys2 <- eventReactive(input$go, {
    req(data_surface2())
    if (input$grand_slam2 == TRUE) {
      data_surface2() %>% filter(tourney_name %in% c("Roland Garros", "US Open", "Wimbledon","Australian Open"))
    }
    else {
      data_surface2()
    }
  })

  #Getting right rounds
  data_rounds2 <- eventReactive(input$go, {
    req(data_tourneys2())
    if (input$final_stages2 == TRUE) {
      data_tourneys2() %>% filter(round %in% c("QF", "SF", "F"))
    }
    else {
      data_tourneys2()
    }
  })

  #computing elo on matches corresponding to parameters
  elo_dataframe2 <- eventReactive(input$go, {
    req(data_rounds2())
    data_rounds2() %>%
      elo_central_matches()
  })

  #Getting elo of both players given parameters
  elo_player1 <- eventReactive(input$go, {
    req(opponent_id1(), elo_dataframe2())
    round(elo_player(ID = as.numeric(opponent_id1()), data = elo_dataframe2()), digits = 3)
  })
  elo_player2 <- reactive({
    req(opponent_id2())
    round(elo_player(ID = as.numeric(opponent_id2()), data = elo_dataframe2()),  digits = 3)
  })

  #Getting betting recommandation
  recommandation <- eventReactive(input$go, {
    req(opponent_id1(), opponent_id2(), elo_dataframe2(), input$odd_player1, input$odd_player2, input$risk)
    recommandation <- odds_bet(input$odd_player1, input$odd_player2, as.numeric(opponent_id1()), as.numeric(opponent_id2()), input$risk, elo_dataframe2())
  })

  #Getting last matches of both players given parameters
  last_matches_player1 <- eventReactive(input$go, {
    req(opponent_id1(), data_rounds2())
    player_history_table(as.numeric(opponent_id1()), data_rounds2()) %>%
    arrange(-row_number())
  })

  last_matches_player2 <- eventReactive(input$go, {
    req(opponent_id2(), data_rounds2())
    player_history_table(as.numeric(opponent_id2()), data_rounds2()) %>%
    arrange(-row_number())
  })

  #Outputs for tabs 2

  output$reco2 <- renderText({
    glue::glue("ELO of {name1()} for selected criterias : {elo_player1()}")
  })
  output$reco3 <- renderText({
    glue::glue("ELO of {name2()} for selected criterias : {elo_player2()}")
  })
  output$reco4 <- renderText({
    glue::glue("{recommandation()[[3]]}")
  })

  output$proba_plot <- renderPlot({
    data.frame(
      players = c(name1(), name2()),
      probas = c(round(recommandation()[[1]]*100, 2), round(recommandation()[[2]]*100, 2))) %>%
      ggplot(aes(x= "", y = probas, fill = players)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      geom_text(aes(y = probas/2 + c(0, cumsum(probas)[-2]), label = paste0(probas, "%")), size=5) +
      labs(title = "Probabilities of victory of each player", x ="", y ="") +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
  }, height = 400, width = 400)

  output$last_player1 <- DT::renderDT({
    req(last_matches_player1())
    last_matches_player1()
  })

  output$last_player2 <- DT::renderDT({
    req(last_matches_player2())
    last_matches_player2()
  })

}

shinyApp(ui = ui, server = server)
