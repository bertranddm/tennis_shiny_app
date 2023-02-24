#' Plot
#'
#' Function to plot stats in the shiny app
#'
#' @param data data input for plotting function
#' @param stat stat input
#' @import utils
#' @import dplyr
#' @import ggplot2
#' @importFrom stats na.omit
#' @importFrom rlang .data



stat_plot <- function(data, stat) {
  data <- na.omit(data)

  total <- data %>%
    group_by(.data$year, .data$id) %>%
    summarise(total = n())

  a1 <- data %>%
    filter(.data$status == "Loss") %>%
    group_by(.data$year, id) %>%
    summarise_at(vars(matches(paste0("l_", stat))), sum)

  a2 <- data %>% filter(.data$status == "Win") %>%
    group_by(.data$year, id) %>%
    summarise_at(vars(matches(paste0("w_", stat))), sum)

  table <- inner_join(total, a1, by = c("year", "id")) %>%
    inner_join(a2, by = c("year", "id"))
  names(table)[4:5] <- c("x1","x2")
  table %>%
    group_by(.data$year, id) %>%
    mutate(mean_stats = (.data$x1+.data$x2)/total) %>%
    ggplot(aes(x = .data$year, y = .data$mean_stats, colour = id)) +
    geom_line()


}
