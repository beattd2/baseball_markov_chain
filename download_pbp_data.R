#TODO: implement function to correct non-existent baserunners in extra innings

library(baseballr)
library(dplyr)
library(readr)

download_pbp_data <- function(year) {
  file_name <- paste0('data/download.folder/unzipped/all',year,'.csv')
  if (file.exists(file_name)) {
    read_csv(file_name)
  } else {
    get_retrosheet_data(path_to_directory = paste0(getwd(),'/data'), years_to_acquire = year)
    #read_csv("data/download.folder/unzipped/all2024.csv") %>% glimpse()	
    read_csv(paste0("data/download.folder/unzipped/all", year, ".csv"))
  }
}