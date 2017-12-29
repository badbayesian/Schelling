library(tidyverse)
library(utils)

#' neighbors finds the neighborhood for each index
#' TODO: allow for arbitrary neighborhood function
#' 
#' @param board DataFrame
#' @param height int
#' @param width int
#' @return neighborhood list of neighbors at index i
neighbors <- function(board, height, width){
  height_neighbors <- sapply(board$height, function(i) seq(i - 1, i + 1))
  excess_height <- which((height_neighbors == 0) |
                           (height_neighbors == height + 1), arr.ind = T)
  height_neighbors[excess_height] <- NA
  
  width_neighbors <- sapply(board$width, function(i) seq(i - 1, i + 1))
  excess_width <- which((width_neighbors == 0) |
                          (width_neighbors == width + 1), arr.ind = T)
  width_neighbors[excess_width] <- NA
  
  neighborhood <- lapply(1:(height*width), function(i)
    expand.grid(height = na.omit(height_neighbors[, i]),
                width = na.omit(width_neighbors[, i])))
  
  return(neighborhood)
}

#' Create column of bools of whether each individual is satisfied according to
#' `tolerance`.
#'
#' @param board DataFrame
#' @param height int
#' @param width int
#' @param tolerance float
#' @return satisfied list of bools
race_check <- function(board, height, width, tolerance){
  neighborhood <- neighbors(board, height, width)
  
  # TODO optimize with sparse matrix multiplication
  race_counts <- lapply(1:(height*width), function(i)
    board$height %in% neighborhood[[i]]$height *
      board$width %in% neighborhood[[i]]$width *
      board$race)
  race_counts <- lapply(1:(height*width), function(i)
    table(race_counts[[i]][race_counts[[i]] != 0]))

  satisfied <- sapply(1:(height*width), function(i)
    tolerance <= ((race_counts[[i]][toString(board$race[i])] - 1) /
                    sum(race_counts[[i]])))

  return(satisfied)
}

#' Process Schelling Algorithm on board
#'
#' @param board
#' @param tolerance
#' @param max_iterations
#' @param satisfied_agents
#' @return board
schelling <- function(board, tolerance = 0.33, max_iterations = 100,
                      satisfied_agents = 0.95) {
  height <- max(board$height)
  width <- max(board$width)
  number_of_agents <- height*width
  
  for (i in 1:max_iterations) {
    board$satisfied <- race_check(board, height, width, tolerance)
    
    if (sum(board$satisfied)/(number_of_agents) >= satisfied_agents) {
      break
    }
    
    unsatisfied_agents <- board %>%
      filter(satisfied == FALSE) %>%
      select(race)
    board <- board %>%
      mutate(empty = !satisfied,
             race = (!empty) * race)
    board$race[board$empty == TRUE] <- sample(unsatisfied_agents$race)
    board$empty <- (board$race == 0)
  }

  return(board)
}

#' Initialize schelling board
#'
#' @param height int
#' @param width int
#' @return board
board <- function(height = 50, width = 100) {
  number_of_agents <- height*width
  
  board <- data.frame(expand.grid(height = 1:height, width = 1:width),
                     empty = FALSE,
                     satisfied = FALSE,
                     race = sample(3, number_of_agents, replace = TRUE))
  
  return(board)
}

#' Plot board
#'
#' @param board
#' @return print(plot)
plot_board <- function(board){
  plot <- ggplot(board) +
    aes(x = width, y = height, color = as.factor(race)) +
    geom_point(size = 2.5) +
    labs(title = "Schelling", x = "", y = "", color = "Race") +
    theme_bw() +
    coord_cartesian(xlim = c(0, max(board$width) + 1),
                    ylim = c(0, max(board$height) + 1),
                    expand = FALSE)
  print(plot)
}


