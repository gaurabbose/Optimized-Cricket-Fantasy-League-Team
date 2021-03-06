
## Selecting An Optimal IPL Cricket Fantasy Team

Author: Gaurab Bose

The objective of this predictive model is to predict the optimal IPL (Indian Premier League) Fantasy League Cricket Team. This is a simulated web game that allows individuals to form leagues with their family and friends, wherein each individual selects a team of players from the entire league and is scored based on their each of their players' performance. The winning team with the highest cumulative score by the end of the season gains fame, glory, and hopefully a predesignated pot of cash.

The individual faces the constraint that they must pick players with a total budget of Rs. 10M and each player has a varying cost. The situation therefore presents a lofty challenge which is to build an optimization model that weights the merits of a player against his monetary cost / opportunity cost. 

This program will attempt to achieve its goal through a number of major steps:

1. Import historical IPL cricket data for 615 matches (for every ball bowled from 2008 - 2015) and transform it into a relevant format i.e. create a database of IPL player statistics

2. Group the players into batsman and bowlers, since we aim to pick 5 of each type. The 1 remaining wicket keeper will not be within the scope of this model.

3. Within each of the two cohorts, rank the players based on a scoring system, that takes into account their historical performance, but weighted towards more recent performance.

4. Model out and solve an optimization problem that rewards a higher score but is constrained by the budget to create a team.

I thought I'd walk you through my major steps through the instructions and comments

The data has been downloaded from cricsheet (https://cricsheet.org/) but is in .yaml format for each game. I first convert these to data frames.

```{r, eval = FALSE}

library(rmarkdown)
library(yaml)
library(yorkr)x`


setwd("C:/Users/fm114rt/Documents/R Projects/IPL/data/yaml")

file_list <- list.files()

for(file in file_list){
  tryCatch({
  convertYaml2RDataframe(file,"C:/Users/fm114rt/Documents/R Projects/IPL/data/yaml", "C:/Users/fm114rt/Documents/R Projects/IPL/data/matches")
  }, error = function(e){})
}

```


I then consolidate the seperate data frames into a master file by row binding.

```{r, eval= FALSE}

setwd("C:/Users/fm114rt/Documents/R Projects/IPL/data/matches")
file_list <- list.files()
for (file in file_list){
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <-get(load(file))
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-get(load(file))
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}

write.csv(dataset, "C:/Users/fm114rt/Documents/R Projects/IPL/data/dataset.csv")
head(dataset)
```

This creates a master data file that I will use from now on.


```{r}

#Defining helper functions for scoring algorithm
fix_name <- function(name){
  return(paste(substring(name,1,1),sub(".* ", "", name)))
}

encode <- function(year){
  if(year == 2008){
    return(0.2)
  }
  if(year == 2009){
    return(0.4)
  }
  if(year == 2010){
    return(0.6)
  }
  
  if(year == 2011){
    return(0.8)
  }
  
  if(year == 2012){
    return(0.9)
  }
  
  if(year == 2013){
    return(0.95)
  }
  
  else
    return(1)
  
}
economy_calc <- function(rate){
  if(rate <= 5){
    return(15)
  }
  if(rate <= 8){
    return(10)
  }
  if(rate <= 10){
    return(5)
  }
  
  if(rate <= 12){
    return(-10)
  }
  else
    return(-15)
}

strike_calc <- function(rate){
  if(rate < 75){
    return(-15)
  }
  if(rate < 100){
    return(-10)
  }
  if(rate < 150){
    return(5)
  }
  
  if(rate < 200){
    return(10)
  }
  else
    return(15)
}

#import data

dataset <- read.csv("C:/Users/fm114rt/Documents/R Projects/IPL/data/dataset.csv", header = TRUE)
dataset <- dataset[-1]
#Print first 10 rows of master data
head(dataset)

# Aggregate stats for bowlers

# Extract relevant part of dataframe
bowling_data <- data.frame(cbind(dataset$bowler,dataset[,6:14], dataset$wicketKind), dataset$date, stringsAsFactors = FALSE)

# Extract year from date
bowling_data$dataset.year <- substring(bowling_data$dataset.date,1,4)


# Check if 'not-out' or 'out'
bowling_data$dataset.wicketKind <- ifelse(bowling_data$dataset.wicketKind == "not-out", 0, 1)

# Aggregating by bowler and year
bowler_stats <- cbind(aggregate(bowling_data[,2:11],bowling_data[c("dataset.bowler","dataset.year")], FUN = sum),
                      aggregate(bowling_data[,12],bowling_data[c("dataset.bowler","dataset.year")], FUN = function(x) length(unique(x))))
bowler_stats <- bowler_stats[complete.cases(bowler_stats$totalRuns), ]

# Expected scoring system
list <- data.frame(read.csv("C:/Users/fm114rt/Documents/R Projects/IPL/data/player_cost.csv"))
list$score <- 0
list$playerType <- 0 

#Scoring algorithm
count_i = 0
for(i in list$Player){
  count_i = count_i + 1
  count_j = 0
  for(j in bowler_stats$dataset.bowler){
    count_j = count_j + 1
    if(identical(fix_name(j),fix_name(i))){
      list$score[count_i] <- list$score[count_i] +
                                (encode(bowler_stats$dataset.year[count_j])*
                                ((20*bowler_stats$dataset.wicketKind[count_j]/bowler_stats$x[count_j]) +
                                economy_calc(bowler_stats$totalRuns[count_j]/bowler_stats$x[count_j])))
    list$playerType[count_i] <- 1
      log=TRUE
    }
  }
}


# Aggregate stats for batsman
# Extract relevant part of dataframe
batting_data <- data.frame(cbind(dataset$batsman,dataset[,6:14], dataset$wicketKind), dataset$date, dataset$ball, stringsAsFactors = FALSE)

# Extract year from date
batting_data$dataset.year <- substring(batting_data$dataset.date,1,4)


# Aggregating by bowler and year
batsman_stats <- cbind(aggregate(batting_data[,2:10],batting_data[c("dataset.batsman","dataset.year")], FUN = sum),
                      aggregate(batting_data[,12],batting_data[c("dataset.batsman","dataset.year")], FUN = function(x) length(unique(x))),
                      aggregate(batting_data$dataset.ball,batting_data[c("dataset.batsman","dataset.year")], FUN = function(balls_faced) length(unique(balls_faced))))
batsman_stats <- batsman_stats[complete.cases(batsman_stats$totalRuns), ]
  
#Scoring algorithm
count_i = 0
for(i in list$Player){
  count_i = count_i + 1
  count_j = 0
  for(j in batsman_stats$dataset.batsman){
    count_j = count_j + 1
    if(identical(fix_name(j),fix_name(i))){
      list$score[count_i] <- list$score[count_i] +
        (encode(batsman_stats$dataset.year[count_j])*
           ((batsman_stats$totalRuns[count_j]/batsman_stats$x[count_j]) +
              strike_calc(batsman_stats$totalRuns[count_j]/(batsman_stats[,17][count_j]*batsman_stats$x[count_j]))))
    }
  }
}


# Now I have the scores, so I must optimze the picks with respect to a budget constraint of 10m


library(lpSolve)
obj_fun <- list$score
constraints <- matrix(list$Cost, nrow = 1)
constraints2 <- matrix(list$playerType, nrow = 1) 
cons <- rbind(constraints, constraints2)
c_dir <- "<="
c_rhs <- c(10000000,5)

lp <- lp("max",
         obj_fun,
         cons,
         c_dir,
         c_rhs, 
         all.bin = TRUE)


#This is the optimzed team with 5 bowlers and 6 batsman

pred_score <- sum(list[lp$solution == 1, 4])
pred_cost <- sum(list[lp$solution == 1, 2])

print(list[lp$solution == 1,])

```

To explain the above, I aggregated all the bowlers and batsman by year and then by all relevant metrics for scoring purposes. I then scored each player by an equation that closely mimics the scoring system of the fantasy league, but implemented on their historical data. The encode function basically encodes each played year as a weight which therefore weights more recent perfrmance more heavily. Then the scores are tabulated with the player list and their costs.

The last step is essentially solving the 'grand' linear optimization problem that I set up which maximizes the score with the budget constraints of total cost = 10m and the player type sum = 5 (i.e 5 bowlers). 
  
