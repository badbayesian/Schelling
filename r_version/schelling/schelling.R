library(tidyverse)
library(utils)

# Schelling process is defined by a DataFrame which is used in a tidy-styled
# system
# init_board
#   - schelling
#     - neighbors
#     - race_check
#   - plot_board
#   - plot_satisfaction_board

#' Create column of bools of whether each individual is satisfied according to
#' `tolerance`.
#'
#' @param board DataFrame
#' @param height int
#' @param width int
#' @param tolerance float
#' @return satisfied (DataFrame column of bools)
race_check <- function(board, neighborhood, height, tolerance){
  
  race_counts <- lapply(1:nrow(board), function(i)
    board$race[(neighborhood[[i]]$width - 1)*height +
                 neighborhood[[i]]$height])

  satisfied <- sapply(1:nrow(board), function(i)
    tolerance <= ((tabulate(race_counts[[i]])[board$race[i]] - 1) /
      (length(race_counts[[i]]) - 1) - board$distance[i]))

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
#' @return board (DataFrame)
schelling <- function(board, neighborhood_size = 1, tolerance = 0.33,
                      max_iterations = 100, satisfied_agents = 0.95,
                      business_center = NA, max_distance_penalty = .5) {
  height <- max(board$height)
  width <- max(board$width)
  number_of_agents <- sum(tabulate(board$race))
  neighborhood <- neighbors(board, height, width, neighborhood_size)
  
  if (!is.na(business_center[1])) {
    distance <- sqrt((board$width - business_center[1])^2 +
                       (board$height - business_center[2])^2)
    board$distance <- distance/max(distance)*max_distance_penalty
  }
  
  for (i in 1:max_iterations) {
    board$empty <- is.na(board$race)
    board$satisfied <- race_check(board, neighborhood, height, tolerance)
    
    if (sum(na.omit(board$satisfied))/(number_of_agents) >= satisfied_agents) {
      break
    }
    
    unsatisfied_agents <- board %>%
      filter(satisfied == FALSE) %>%
      select(race)
    board$empty <- !board$satisfied | is.na(board$race)
    empty_spots <- sum(board$empty)
    fill_spots <- c(unsatisfied_agents$race,
                    rep(NA, empty_spots - length(unsatisfied_agents$race)))
    board$race[board$empty == TRUE] <- sample(fill_spots)
  }

  return(board)
}

#' Initialize schelling board
#'
#' @param height int
#' @param width int
#' @param race_distribution list (float)
#' @param filled float
#' @return board (DataFrame)
init_board <- function(height = 50, width = 100,
                       race_distribution = c(0.5, 0.5), filled = 0.95) {
  
  if (sum(race_distribution) > 1) {
    stop("Race distribution needs to be equal to or less than 1.")
  }
  
  different_agents <- length(race_distribution)
  number_of_agents <- filled*height*width*race_distribution
  number_of_agents <- sapply(1:different_agents, function(i)
    round(number_of_agents[i]))
  number_of_empty <- height*width - sum(number_of_agents)
  agents <- c(rep(NA, number_of_empty),
             rep(1:different_agents, number_of_agents[1:different_agents]))
  
  
  board <- data.frame(expand.grid(height = 1:height, width = 1:width),
                     empty = FALSE,
                     satisfied = FALSE,
                     distance = 0,
                     race = sample(agents))
  
  return(board)
}

#' Plot board
#'
#' @param board
#' @return ggplot object
plot_board <- function(board){
  plot <- ggplot(na.omit(board)) +
    aes(x = width, y = height, color = as.factor(race)) +
    geom_point(size = 2.5) +
    labs(title = "Schelling", x = "", y = "", color = "Race") +
    theme_bw() +
    coord_cartesian(xlim = c(0, max(board$width) + 1),
                    ylim = c(0, max(board$height) + 1),
                    expand = FALSE)

  return(plot)
}

#' Plot satisfaction board
#'
#' @param board
#' @return ggplot object
plot_satisfaction_board <- function(board){
  plot <- ggplot(na.omit(board)) +
    aes(x = width, y = height, color = satisfied) +
    geom_point(size = 2.5) +
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
