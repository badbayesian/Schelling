# Schelling Simulation

This repository holds my Undergraduate Economic Thesis for the University of Chicago in 2016. I am currently updating the code so that it is more readable and allowing full access to the model in the shiny app. Regardless, the old code can be found [here](https://github.com/badbayesian/schelling_extension/blob/master/r_version/schelling_old.R)

[Shiny App](https://badbayesian.shinyapps.io/schelling/)

Abstract:

This paper models decentralized segregation through a hybrid of a simple housing model and Schelling sorting in Chicago. Using Census tract data and Racial Isolation and Racial Dissimilarity indices, I estimate a racial tolerance level for Chicago in 2000 and 2010. To achieve similar levels of segregation in Chicago for both years on average, the tolerance level required is at least 50%, which is in line with actual surveyed tolerance levels.  Consequently, there is some evidence that racial attitudes in Chicago have not changed significantly between 2000 and 2010. This is evidence that racial segregation is perpetuated by a decentralized racism, where White people pay a premium to live in a predominately White neighborhood, rather than by legal barriers, as suggested in Cutler, Glaeser, and Vigdor (1999).

Schelling in R has tidy-styled API
```r
board <- init_board(height = 50, width = 100, race_distribution = c(0.5, 0.5), filled = 0.95) %>%
          schelling(tolerance = 0.33)

plot_board(board)
```
