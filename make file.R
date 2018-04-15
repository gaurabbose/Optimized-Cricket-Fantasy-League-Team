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
