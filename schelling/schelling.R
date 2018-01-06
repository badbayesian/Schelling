library(tidyverse)
library(utils)

# Schelling process is defined by a DataFrame which is used in a tidy-styled
# system
# init_board
#   - schelling
#     - neighbors
#     - satisfaction_check
#   - plot_board
#   - plot_satisfaction_board

#' Create column of bools of whether each individual is satisfied according to
#' `tolerance`.
#'
#' @param board DataFrame
#' @param height int
#' @param width int
#' @param tolerance float
#' @param penalties list (float)
#' @return satisfied (DataFrame column of bools)
satisfaction_check <- function(board, neighborhood, height, tolerance,
                               penalties){
  
  neighbor_counts <- lapply(1:nrow(board), function(i)
    data.frame(race = board$race[(neighborhood[[i]]$width - 1)*height +
                                   neighborhood[[i]]$height],
               wealth = board$wealth[(neighborhood[[i]]$width - 1)*height +
                                       neighborhood[[i]]$height]))
  
  satisfied <- sapply(1:nrow(board), function(i)
    tolerance <= (
      penalties[1] * (tabulate(neighbor_counts[[i]]$race)[board$race[i]] - 1) /
        (nrow(neighbor_counts[[i]]) - 1)) +
      penalties[2] * ((tabulate(neighbor_counts[[i]]$wealth)[board$wealth[i]] - 1) /
         (nrow(neighbor_counts[[i]]) - 1)) -
      board$distance[i])

  return(satisfied)
}

#' Find coordinates of neighbors for each agent
#' TODO: allow for arbitrary neighborhood function (non-morse neighborhoods)
#' 
#' @param board DataFrame
#' @param height int
#' @param width int
#' @param neighborhood_size int
#' @return neighborhood (list of neighbors at index i)
neighbors <- function(board, height, width, neighborhood_size = 1){
  height_neighbors <- lapply(board$height, function(i)
    seq(i - neighborhood_size, i + neighborhood_size))
  trim_excess_height <- lapply(1:nrow(board), function(i)
    (height_neighbors[[i]] > 0) & (height_neighbors[[i]] <= height))
  height_neighbors <- lapply(1:nrow(board), function(i)
    height_neighbors[[i]][trim_excess_height[[i]]])
  
  width_neighbors <- lapply(board$width, function(i)
    seq(i - neighborhood_size, i + neighborhood_size))
  trim_excess_width <- lapply(1:nrow(board), function(i)
    (width_neighbors[[i]] > 0) & (width_neighbors[[i]] <= width))
  width_neighbors <- lapply(1:nrow(board), function(i)
    width_neighbors[[i]][trim_excess_width[[i]]])
  
  neighborhood <- lapply(1:nrow(board), function(i)
    expand.grid(height = height_neighbors[[i]],
                width = width_neighbors[[i]]))
  
  return(neighborhood)
}

#' Process Schelling Algorithm on board until `max_iterations`or
#' `satisfied_agents` are met.
#'
#' @param board DataFrame
#' @param neighborhood_size int
#' @param tolerance float
#' @param max_iterations int
#' @param satisfied_agents flaot
#' @param business_center list (int)
#' @param max_race_penalty float
#' @param max_wealth_penalty float
#' @param max_distance_penalty float
#' @return board (DataFrame)
schelling <- function(board, neighborhood_size = 1, tolerance = 0.33,
                      max_iterations = 100, satisfied_agents = 0.95,
                      business_center = NA, max_race_penalty = 1,
                      max_wealth_penalty = 0, max_distance_penalty = 0) {
  height <- max(board$height)
  width <- max(board$width)
  number_of_agents <- sum(tabulate(board$race))
  neighborhood <- neighbors(board = board, height = height, width = width,
                            neighborhood_size = neighborhood_size)
  penalties <- c(max_race_penalty, max_wealth_penalty)
  
  if (!is.na(business_center[1])) {
    distance <- sqrt((board$width - business_center[1])^2 +
                       (board$height - business_center[2])^2)
    board$distance <- distance/max(distance)*max_distance_penalty
  }
  
  for (i in 1:max_iterations) {
    board$empty <- is.na(board$race)
    board$satisfied <- satisfaction_check(board = board,
                                          neighborhood = neighborhood,
                                          height = height,
                                          tolerance = tolerance,
                                          penalties = penalties)
    
    if (sum(na.omit(board$satisfied))/(number_of_agents) >= satisfied_agents) {
      break
    }
    
    unsatisfied_agents <- board %>%
      filter(satisfied == FALSE) %>%
      select(race, wealth)
    board$empty <- !board$satisfied | is.na(board$race)
    
    empty_spots <- rep(NA, sum(board$empty) - nrow(unsatisfied_agents))
    empty_spots <- data.frame(race = empty_spots, wealth = empty_spots)
    
    available_agents <- bind_rows(unsatisfied_agents, empty_spots)
    
    board[board$empty == TRUE,  c('race', 'wealth')] <- available_agents %>%
      sample_n(size = nrow(.))
  }
  
  return(board)
}

#' Initialize schelling board
#'
#' @param height int
#' @param width int
#' @param filled float
#' @param race_distribution list (float)
#' @param weath_distribution list (float)
#' @return board (DataFrame)
init_board <- function(height = 50, width = 100, filled = 0.95,
                       race_distribution = c(0.5, 0.5),
                       wealth_distribution = c(0.10, 0.70, 0.20)) {
  
  if (sum(race_distribution) > 1) {
    stop("Race distribution needs to be equal to or less than 1.")
  }
  
  if (sum(wealth_distribution) > 1) {
    stop("Wealth distribution needs to be equal to or less than 1.")
  }
  
  races <- length(race_distribution)
  number_per_race <- round(filled*height*width*race_distribution)
  
  wealth_levels <- length(wealth_distribution)
  number_per_wealth <- round(filled*height*width*wealth_distribution)
  
  number_of_empty <- height*width - sum(number_per_race)
  agent_race <- c(rep(NA, number_of_empty),
                  rep(1:races, number_per_race[1:races]))
  agent_wealth <- rep(1:wealth_levels, number_per_wealth[1:wealth_levels])
  
  
  board <- data.frame(expand.grid(height = 1:height, width = 1:width),
                      empty = FALSE,
                      satisfied = FALSE,
                      distance = 0,
                      race = sample(agent_race))
  
  board$wealth[!is.na(board$race)] <- sample(agent_wealth)
  
  return(board)
}

#' Plot board with wealth as shape and race as color
#'
#' @param board DataFrame
#' @param size int
#' @param show_wealth bool
#' @return ggplot object
plot_board <- function(board, size = 2, show_wealth = FALSE){
  if (show_wealth) {
    plot <- ggplot(na.omit(board)) +
      aes(x = width, y = height,
          color = as.factor(race), shape = as.factor(wealth)) +
      geom_point(size = size) +
      labs(title = "Schelling", x = "", y = "",
           color = "Race", shape = "Wealth") +
      theme_bw() +
      coord_cartesian(xlim = c(0, max(board$width) + 1),
                      ylim = c(0, max(board$height) + 1),
                      expand = FALSE)
  }
  else {
    plot <- ggplot(na.omit(board)) +
      aes(x = width, y = height, color = as.factor(race)) +
      geom_point(size = size) +
      labs(title = "Schelling", x = "", y = "", color = "Race") +
      theme_bw() +
      coord_cartesian(xlim = c(0, max(board$width) + 1),
                      ylim = c(0, max(board$height) + 1),
                      expand = FALSE)
  }
  
  return(plot)
}

#' Plot satisfaction board
#'
#' @param board DataFrame
#' @param size int
#' @return ggplot object
plot_satisfaction_board <- function(board, size = 2){
  plot <- ggplot(na.omit(board)) +
    aes(x = width, y = height, color = satisfied) +
    geom_point(size = size) +
    labs(title = "Schelling", x = "", y = "", color = "Satisfied") +
    theme_bw() +
    coord_cartesian(xlim = c(0, max(board$width) + 1),
                    ylim = c(0, max(board$height) + 1),
                    expand = FALSE)
  
  return(plot)
}

#'
#'
segregation_distribution <- function(board){
  height <- max(board$height)
  width <- max(board$width)
  
  neighborhood <- neighbors(board, height, width)
  
  race_counts <- lapply(1:(height*width), function(i)
    board$race[(neighborhood[[i]]$width - 1)*height +
                 neighborhood[[i]]$height])
  
  matching <- sapply(1:(height*width), function(i)
    tabulate(race_counts[[i]])[board$race[i]] - 1)
  
  moments <- data.frame(race = board$race, matching) %>%
    group_by(race) %>%
    summarise(distribution = list(tabulate(matching)))
  
  return(moments)
}
