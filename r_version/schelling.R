library(tidyverse)
library(utils)

#' neighbors finds the neighborhood for each index
#' TODO: allow for arbitrary neighborhood function
#' 
#' @param grid DataFrame
#' @param max_height int
#' @param max_width int
#' @return neighborhood list of neighbors at index i
neighbors <- function(grid, max_height, max_width){
  height_neighbors <- sapply(grid$height, function(i) seq(i - 1, i + 1))
  excess_height <- which((height_neighbors == 0) |
                           (height_neighbors == max_height + 1), arr.ind = T)
  height_neighbors[excess_height] <- NA
  
  width_neighbors <- sapply(grid$width, function(i) seq(i - 1, i + 1))
  excess_width <- which((width_neighbors == 0) |
                          (width_neighbors == max_width + 1), arr.ind = T)
  width_neighbors[excess_width] <- NA
  
  neighborhood <- lapply(1:(max_height*max_width), function(i)
    expand.grid(height = na.omit(height_neighbors[, i]),
                width = na.omit(width_neighbors[, i])))
  
  return(neighborhood)
}

#' Create column of bools of whether each individual is satisfied according to
#' `tolerance`.
#'
#' @param grid DataFrame
#' @param max_height int
#' @param max_width int
#' @param tolerance float
#' @return satisfied list of bools
race_check <- function(grid, max_height, max_width, tolerance){
  neighborhood <- neighbors(grid, max_height, max_width)
  
  # TODO optimize with sparse matrix multiplication
  race_counts <- lapply(1:(max_height*max_width), function(i)
    grid$height %in% neighborhood[[i]]$height *
      grid$width %in% neighborhood[[i]]$width *
      grid$race)
  race_counts <- lapply(1:(max_height*max_width), function(i)
    table(race_counts[[i]][race_counts[[i]] != 0]))

  satisfied <- sapply(1:(max_height*max_width), function(i)
    tolerance <= ((race_counts[[i]][toString(grid$race[i])] - 1) /
                    sum(race_counts[[i]])))

  return(satisfied)
}

#' Initialize and run Schelling process
#'
#' @param max_height int
#' @param max_width int
#' @param tolerence float
#' @param max_iterations int
#' @param satisfied_agents float
#' @return grid
schelling <- function(max_height = 50, max_width = 100, tolerence = 0.33,
                      max_iterations = 100, satisfied_agents = 0.95){
  number_of_agents <- max_height*max_width
  
  grid <- data.frame(expand.grid(height = 1:max_height, width = 1:max_width),
                     empty = FALSE,
                     satisfied = FALSE,
                     race = sample(3, number_of_agents, replace = TRUE))
  for (i in 1:max_iterations) {
    grid$satisfied <- race_check(grid, max_height, max_width, tolerence)
    
    if (sum(grid$satisfied)/(number_of_agents) >= satisfied_agents) {
      break
    }
    
    unsatisfied_agents <- grid %>%
      filter(satisfied == FALSE) %>%
      select(race)
    grid <- grid %>%
      mutate(empty = !satisfied,
             race = (!empty) * race)
    grid$race[grid$empty == TRUE] <- sample(unsatisfied_agents$race)
    grid$empty <- (grid$race == 0)
  }

  return(grid)
}


