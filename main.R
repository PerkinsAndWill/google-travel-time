# Installing packages-----------------------------------------------------------
library(tidyverse)
library(readxl)
library(googleway)
library(janitor)
library(tigris)
library(scales)

# source config script
# source('config.R')
setwd(getwd())

trips <- read_excel("input.xlsx",sheet = "input") 

ifelse(!dir.exists(file.path(getwd(), "output")), 
       dir.create(file.path(get(),"output")), FALSE)

config <- read_excel("input.xlsx",sheet = "config",n_max = 7)
rownames(config) <- config$Category

gKey = config[which(rownames(config) == "gKey"),]$Input
set_key(gKey)

mode_sel = config[which(rownames(config) == "mode"),]$Input
departure_datetime = config[which(rownames(config) == "departure_datetime"),]$Input
departure_datetime = gsub('\"',"",departure_datetime)

arrival_datetime = config[which(rownames(config) == "arrival_datetime"),]$Input
arrival_datetime = gsub('\"',"",arrival_datetime)

timezone = config[which(rownames(config) == "timezone"),]$Input
traffic_model_sel = config[which(rownames(config) == "traffic_model"),]$Input
traffic_model_sel = gsub("\\s*\\([^\\)]+\\)","",as.character(traffic_model_sel))

transit_mode = config[which(rownames(config) == "transit_mode"),]$Input


# reading script-----------------------------------------------------------------
if(mode_sel == 'driving'){
  source("script\\driving.R", echo = TRUE)
} else if(mode_sel == 'transit'){
  source("script\\transit.R", echo = TRUE)
} else if(mode_sel == 'walking'){
  source("script\\walking.R", echo = TRUE)
} else if(mode_sel =='bicycling'){
  source("script\\bicycling.R", echo = TRUE)
}
