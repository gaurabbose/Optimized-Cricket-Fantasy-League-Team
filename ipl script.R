
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


# Now we have our scores, so we must optimze the picks with respect to a budget constraint of 10m


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

