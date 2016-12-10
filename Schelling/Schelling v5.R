#======================================================================================#
# Daniel Silva-Inclan
# Schelling v5

# Clear all
dev.off()
rm(list=ls())
set.seed(573028263)

# Required Directories
library(base)
library(dgof)
library(dplyr)
library(ggmap)
library(ggplot2)
library(lubridate)
library(readr)
library(rmarkdown)
library(scales)
library(stargazer)


# Set work directory
setwd("/Users/Daniel/Dropbox/Thesis/Schelling")

#======================================================================================#
# PARAMETERS:
# There are trials which vary tolerance level and simulations which repeat the program
# with the same parameters. The whole grid is called the city.

# t.agents is number types of agents, p.agents is the percentage per each type of agent
# n is the number of total agents, height and width defines the size of the board,
# un.agents stops the process once its below this percentage of unsatisfied agents,
# s.image defines how many turns until it prints a plot, tol is tolerance, dt how much 
# the tolerance increases per trial, end.tol is the final inclusive tolerance level in
# the program, loc is the location of emploment.
n.rep = 1 
height = 100
width = 100
total = height*width
n = floor(.92*total)
steps = 200
un.agents = 0.10
s.image = 2
size = 1

# Defines distribution of races
t.agents = 3
p.agents = c(.33,.33,.33)
n.agent = cumsum(p.agents)

# Initial, change and final tolerance levels (make sure that tol + i*dt = end.tol)!
tol = .4
dt = .1
end.tol = .4

# Attraction zone (center of the city)
loc = c(height/2, width/2)

# Morgage rate (CHECK NUMBER)
h = 0.07
w = 0.04

rate <- data.frame(h,w)

# Struct of schelling process
unsat.agent <- c()
unsat.l <- 0
new.empty <- c()
new.l <- 0
old.empty <- c()
old.l <- 0

g.list <- list(unsat.agent, unsat.l, new.empty, new.l, old.empty, old.l)

# Wealth conditional on race parameters
w.agents = 3

w.wealth = sample(1:3,total,replace=T)
b.wealth = sample(1:3,total,replace=T)
h.wealth = sample(1:3,total,replace=T)

wealth <- data.frame(cbind(w.wealth, b.wealth, h.wealth))

# Weighting on each preference
r.err = rep(.5,total)
w.err = rep(.5,total)
d.err = rep(0,total)

#for(i in 1:total){
#  r.err[i] = sample(1:100,total,replace=T)
#  w.err[i] = sample(1:(100-r.err[i]),total,replace=T)
#  d.err[i] = 100-r.err[i]-w.err[i]
#}

errors <- data.frame(cbind(r.err, w.err, d.err))

# Random variable parameter and function
rv = c(0,0.01)
rv.dist = rnorm

#======================================================================================#
# FUNCTIONS:
# Creates data frame sorted by each potential HOME per row and each characteristic per 
# column. Therefore, charateristics of the home (X,Y,etc.) do not move with the agent.
# Eg: Race, Wealth, Dist from center, X coor, Y coor, Error on each preference.
# Note that the X,Y coor are organized by first X then Y. (1,1), (2,1), (3,1), ... (1,2)
# create.grid: num num num coor num coor list -> dataframe 
create.grid <- function(n, height, width, n.agent, t.agents, loc, errors, wealth){
  total <- height*width
  empty <- (total - n)/total
  grid <- data.frame(race = rep(0,total), wealth = rep(0,total), dist = rep(0,total),
                     X = rep(1:width,height), Y = rep(1:height,each=width), 
                     h.price = rep(0,total), r.err = errors$r.err, 
                     w.err = errors$w.err, d.err = errors$d.err)
  r = runif(total)
  for(i in 2:t.agents){
    grid$race[n.agent[i-1] < r & r <= n.agent[i]] = i
  }
  grid$race[grid$race < n.agent[1]] = 1
  r <- runif(total)
  grid$race[which(r<empty)] = 0
  grid$dist <- round(
    sqrt((loc[2] - grid$X)^2 + (loc[1] - grid$Y)^2)/(sqrt(height*width)/sqrt(2)), 2)
  for(i in 1:total){
    grid$wealth[i] <- ifelse(grid$race[i]!=0,wealth[i,grid$race[i]],0)
    grid$h.price[i] <- ifelse(grid$dist[i]>.33,ifelse(grid$dist[i]<.66,2,1),3)
  }
  return(grid)
}

# Checks for the 9 cases of comparison on the board (in order: center, sides, corner)
# and returns the positions of the neighbors using the organization of X,Y coor.
# case.check: num num num num num -> (list of num)
case.check <- function(X,Y,hgt,wid,i){
  if(X>1&X<wid&Y>1&Y<hgt){homes=c(i-wid-1,i-wid,i-wid+1,i-1,i+1,i+wid-1,i+wid,i+wid+1)}
  if(X>1&X<wid&Y==1){homes=c(i-1,i+1,i+wid-1,i+wid,i+wid+1)}
  if(X==1&Y>1&Y<hgt){homes=c(i-wid,i-wid+1,i+1,i+wid,i+wid+1)}
  if(X==wid&Y>1&Y<hgt){homes=c(i-wid-1,i-wid,i-1,i+wid-1,i+wid)}
  if(X>1&X<wid&Y==hgt){homes=c(i-wid-1,i-wid,i-wid+1,i-1,i+1)}
  if(X==wid&Y==1){homes=c(i-1,i+wid-1,i+wid)} 
  if(X==wid&Y==hgt){homes=c(i-wid-1,i-wid,i-1)}
  if(X==1&Y==1){homes=c(i+1, i+wid, i+wid+1)}
  if(X==1&Y==hgt){homes=c(i-wid,i-wid+1,i+1)}
  return(homes)
}

# Checks the satisfaction of race in local area. If agents are satisfied = 1.
# sat.check: dataframe (list of num) num num -> boolean
sat.check <- function(grid, area, i, tol, rv, rv.dist){
  zero = 0
  race.check = 0
  wealth.check = 0
  len <- length(area)
  for(j in 1:len){
    if(grid$race[area[j]]==0){zero = zero + 1}
    if(grid$race[area[j]]==grid$race[i]){race.check = race.check + 1}
    if(grid$wealth[area[j]]==grid$wealth[i]){wealth.check = wealth.check + 1}
  }
  if(zero == len){return(1)}
  sat.c <- (grid$r.err[i]*race.check/(len-zero) + grid$w.err[i]*wealth.check/(len-zero) +
              grid$d.err[i]*grid$dist + rv.dist(1,rv[1],rv[2]))
  if(sat.c <= tol){return(0)}
  return(1)
}

# Checks satisfaction of each point to find coordinates of unsatisfied agents and empty
# locations.
# sat: num num dataframe nom (big list) -> (big list)
sat <- function(height, width, grid, tol, g.list, rv, rv.dist){
  total = height*width
  tmp = rep(0,total)
  tmp1 <- data.frame(tmp,tmp,tmp,tmp,tmp)
  colnames(tmp1) <- c("race","wealth","r.err","w.err","d.err")
  
  tmp2 <- data.frame(tmp,tmp)
  colnames(tmp2) <- c("Y","X")
  
  unsat.agent <- tmp1
  unsat.l <- 0
  new.empty <- tmp2
  new.l <- 0
  old.empty <- tmp2
  old.l <- 0
  
  for(i in 1:total){
    if(grid$race[i]==0){
      old.empty[old.l+1,] <- c(grid$Y[i],grid$X[i])
      old.l <- old.l + 1
      next
    }
    area <- case.check(grid$X[i],grid$Y[i],height,width, i)
    t <- sat.check(grid, area, i, tol, rv, rv.dist)
    if(t == 0){
      unsat.agent[unsat.l+1,] = 
        c(grid$race[i],grid$wealth[i],grid$r.err[i],grid$w.err[i],grid$d.err[i])
      unsat.l = unsat.l + 1
      new.empty[new.l+1,] = c(grid$Y[i],grid$X[i])
      new.l = new.l + 1
    }
  }
  g.list = list(unsat.agent,unsat.l,new.empty,new.l,old.empty,old.l)
  return(g.list)
}

# Replaces all unsatisfied agents with empty houses in the dataframe. In other words, it
# sets all but X, Y, and h.price to zero **SHOULD CHANGE h.price AND THE WEALTH**
## update.0: (big list) dataframe -> dataframe
update.0 <- function(g.list,grid,width){
  unsat.agent <- g.list[[1]]
  unsat.l <- g.list[[2]]
  new.empty <- g.list[[3]]
  new.l <- g.list[[4]]
  old.empty <- g.list[[5]]
  old.l <- g.list[[6]]
  
  for(i in 1:new.l){
    y = new.empty[i,1]
    x = new.empty[i,2]
    z = (y-1)*width + x
    grid[z,c(1,2,7,8,9)] <- 0
    grid[z,2] <- grid[z,2]
  }
  return(grid)
}

# Reorders agents and empty houses (both new and old) coordinates randomly
# combine.mix: (big list) -> (big list) 
combine.mix <- function(g.list){
  unsat.agent <- g.list[[1]]
  unsat.l <- g.list[[2]]
  new.empty <- g.list[[3]]
  new.l <- g.list[[4]]
  old.empty <- g.list[[5]]
  old.l <- g.list[[6]]
  
  unsat.agent[1:unsat.l,] <- unsat.agent[sample(1:unsat.l),]
  new.empty[new.l+1:old.l,] <- old.empty[1:old.l,]
  new.l <- old.l + new.l
  rand <- runif(new.l)
  new.empty[1:new.l,] <- new.empty[order(rand),]
  
  g.list <- list(unsat.agent,unsat.l,new.empty,new.l,old.empty,old.l)
  return(g.list)
}

# Places unsatisfied agents into new empty houses in dataframe **SHOULD CHANGE h.price
# AND THE WEALTH OF THE AGENTS**
## update.spot: (big list) dataframe -> dataframe
update.spot <- function(g.list,grid,width){
  unsat.agent <- g.list[[1]]
  unsat.l <- g.list[[2]]
  new.empty <- g.list[[3]]
  new.l <- g.list[[4]]
  old.empty <- g.list[[5]]
  old.l <- g.list[[6]]
  
  for(i in 1:unsat.l){
    y = new.empty[i,1]
    x = new.empty[i,2]
    z = (y-1)*width + x
    grid[z,c(1,2,7,8,9)] <- unsat.agent[i,c(1:5)]
    grid[z,2] <- grid[z,2]
  }
  return(grid)
}

# Plots graphs using ggplot
# plot.grid: num num dataframe num num num num -> plot
plot.grid <- function(height, width, grid, tol, sim, n.rep, iter, steps, size){
  ggplot(grid) + geom_point(aes(x=X, y=Y, color=factor(race), alpha=factor(wealth)), 
                            size=size, shape=16) + theme_bw() + 
    theme(panel.grid.major = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    scale_color_manual(name=c("Race"), 
                       values=c('0'="white",'1'='red','2'='blue','3'='forestgreen')) +
    guides(color = guide_legend(override.aes = list(size=5)),
           alpha = guide_legend(override.aes = list(size=5))) +
    labs(title=paste("Schelling Model: Tol:", tol,"||", "Sim:", 
                     paste(sim,"/",n.rep,sep=""),"||", "Step:",
                     paste(iter,"/", steps,sep="")) , x="", y="") +
    coord_cartesian(xlim=c(0,width+1), ylim=c(0,height+1),FALSE) +
    scale_x_continuous(breaks = round(seq(0, width, by = width/10),0)) +
    scale_y_continuous(breaks = round(seq(0, height, by = height/10),0)) +
    scale_alpha_discrete(range = c(0,1))
}

# Ploting house prices
plot_p.house <- function(start.mygrid){
  ggplot(start.mygrid) + geom_point(aes(x=X, y=Y, color=factor(h.price))) +
    labs(title="House prices") + theme_bw() +
    coord_cartesian(xlim=c(0,width+1), ylim=c(0,height+1),FALSE) +
    scale_x_continuous(breaks = round(seq(0, width, by = width/10),0)) +
    scale_y_continuous(breaks = round(seq(0, height, by = height/10),0)) +
    guides(color = guide_legend(override.aes = list(size=5)))
}

# Changes decimals to percents correctly
# percent: num -> num
percent <- function(num){
  per <- 100*round(num,4)
  per <- paste(per,"%",sep="")
  return(per)
}

# Counts the number of matching agents in the area
# seg.check: dataframe (list of num) num -> (list of num)
seg.check <- function(grid, area, i){
  race = grid$race[i]
  wealth = grid$wealth[i]
  r.check = 0
  w.check = 0
  len = length(area)
  for(j in 1:len){
    r.check <- ifelse(race==grid$race[area[j]], r.check+1, r.check)
    w.check <- ifelse(wealth==grid$wealth[area[j]], w.check+1, w.check)
  }
  check <- data.frame(r.check,w.check)
  return(check)
}

# Creates a matrix of the frequency of each level of segregation in city
# segregation: num num dataframe num -> (list of matrix)
segregation <- function(height,width,grid,t.agents,w.agents){
  r.seg.matrix <- matrix(0, nrow = t.agents, ncol = 9)
  w.seg.matrix <- matrix(0, nrow = w.agents, ncol = 9)
  total <- height*width
  for(i in 1:total){
    if(grid$race[i]!=0){
      area <- case.check(grid$X[i],grid$Y[i],height,width, i)
      if(length(area)==8){
        seg <- seg.check(grid, area, i)
        race <- grid$race[i]
        wealth <- grid$wealth[i]
        r.seg.matrix[race,seg$r.check+1] <- r.seg.matrix[race,seg$r.check+1] + 1
        w.seg.matrix[wealth,seg$w.check+1] <- w.seg.matrix[wealth,seg$w.check+1] + 1
      }
    }
  }
  seg.matrix <- list(r.seg.matrix,w.seg.matrix)
  return(seg.matrix)
}

# Runs n.rep number of simulations and to find the average segregation freq level
# ave.agent: num num num num num num num num num num (big list) num -> (list of matrix)
ave.agent <- function(n, n.rep, height, width, steps, un.agents, s.image, t.agents, 
                      w.agents, n.agent, tol, g.list, total, errors, wealth, size, rv,
                      rv.dist){
  r.info <- matrix(0, nrow=t.agents,ncol=9)
  w.info <- matrix(0, nrow=w.agents,ncol=9)
  for(j in 1:n.rep){
    cat("Simulation:",j, "of", n.rep, "\n")
    mygrid <- create.grid(n, height, width, n.agent, t.agents, loc, errors, wealth)
    start.mygrid <<- mygrid 
    print(plot.grid(height, width, mygrid, tol, j, n.rep, 0, "--", size))
    for(i in 1:(steps+1)){
      g.list <- sat(height, width, mygrid, tol, g.list, rv, rv.dist)
      cat("Running step:", paste(i, ":", steps,sep=""),"||",
          percent(g.list[[2]]/n), "unsatisfied agents.", "\n")
      if(un.agents >= g.list[[2]]/n || i==steps){ste = i; break}
      mygrid <- update.0(g.list,mygrid,width)
      g.list <- combine.mix(g.list)
      mygrid <- update.spot(g.list,mygrid,width)
      if(i%%s.image==0){print(plot.grid(height,width,mygrid,tol,j,n.rep, i,"--",size))}
    }
    print(plot.grid(height, width, mygrid, tol, j, n.rep, ste, ste,size))
    seg <- segregation(height,width,mygrid,t.agents,w.agents)
    r.info <- r.info + (seg[[1]]/n.rep)
    w.info <- w.info + (seg[[2]]/n.rep)
  }
  end.mygrid <<- mygrid
  info <- list(r.info,w.info)
  return(info)
}

# Runs ave.agent for each tolerance specified
# prog: 13x num (big list) num -> (list of matrix)
prog <- function(n, n.rep, height, width, steps, un.agents, s.image, t.agents, w.agents,
                 n.agent, tol, dt, end.tol, g.list, total, errors, wealth, size, rv,
                 rv.dist){
  t <- (end.tol - tol)/dt + 1
  info.list <- list()
  for(j in 1:t){
    tol.new <- tol + (j-1)*dt
    cat("\n")
    cat("TOLERANCE:", tol.new, "\n")
    info.list[[j]]<-ave.agent(n, n.rep, height, width, steps, un.agents, s.image, 
                              t.agents, w.agents, n.agent, tol, g.list, total, errors, 
                              wealth,size, rv, rv.dist)
  }
  g.info.list <<- info.list
  return(info.list)
}

# Plots segregation frequency levels
# seg.plot: data.frame num num num num -> plots
seg.plot <- function(seg.mat, tol, end.tol, dt, t.agents, w.agents){
  cat("Creating Segregation Plots...", "\n")
  nt <- ifelse((end.tol-tol)!=0,ifelse(tol==0,(end.tol-tol)/dt,(end.tol-tol)/dt+1),1)
  for(i in 1:nt){
    tol.dt = ifelse(nt!=1,.1*(1-nt),tol)
    Red = seg.mat[[i]][[1]][1,]
    Blue = seg.mat[[i]][[1]][2,]
    Green = seg.mat[[i]][[1]][3,]
    Low = seg.mat[[i]][[2]][1,]
    Medium = seg.mat[[i]][[2]][2,]
    High = seg.mat[[i]][[2]][3,]
    tmp = data.frame(x = 0:8,Red,Blue,Green,Low,Medium,High)
    for(j in 1:t.agents){
      color <- ifelse(j==1,"Red",ifelse(j==2,"Blue","Green"))
      print(ggplot(data = tmp) + geom_point(aes(x = x, y=eval(parse(text=color)))) +
              theme_bw() + labs(title=paste("Segregation Levels for", color, 
                                            "Agents with tol:", tol.dt),
                                x=paste("Number of Matching", color,
                                        "Agents in Neighborhood"),
                                y="Frequency of Neighborhood Type") +
              scale_x_continuous(breaks = round(seq(0, max(tmp$x), by = 1),0)) +
              scale_y_continuous(breaks=round(seq(0,max(tmp[,color])+200,by=width),0)))
    }
    for(j in 1:w.agents){
      color <- ifelse(j==1,"Low",ifelse(j==2,"Medium","High"))
      print(ggplot(data = tmp) + geom_point(aes(x=x, y=eval(parse(text=color)))) +
              theme_bw() + labs(title=paste("Segregation Levels for", color,
                                            "Wealth Agents with tol:", tol.dt),
                                x=paste("Number of Matching",color,
                                        "Wealth Agents in Neighborhood"),
                                y="Frequency of Neighborhood Type") +
              scale_x_continuous(breaks = round(seq(0, max(tmp$x), by = 1),0)) +
              scale_y_continuous(breaks=round(seq(0,max(tmp[,color])+200,by=width),0)))
    }
  }
  cat("Finished Segregation Plots...", "\n")
}

#======================================================================================#
# Runs program
# Creates the city, runs until (=< un.agents) of agents are satisfied while plotting the
# city every (s.image) times.
cat("Starting Program.\n")
cat("Retriving Map of Chicago...\n")
map <- get_map(location="chicago",zoom=12,source="osm")
cat("Map Received.\n")

cat("Starting Simulation:\n")
start.time <- proc.time()
seg.mat <- prog(n, n.rep, height, width, steps, un.agents, s.image, t.agents, w.agents,
                n.agent, tol, dt, end.tol, g.list, total, errors, wealth, size, rv,
                rv.dist)
plot_p.house(start.mygrid)
seg.plot(seg.mat, tol, end.tol, dt, t.agents, w.agents)
end.time <- proc.time()
total.time <- end.time - start.time
total.time
cat("Finished Simulation.\n")
cat("Finished Program.\n")
#======================================================================================#