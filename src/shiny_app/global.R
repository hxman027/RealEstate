library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(shinyjs)
library(randomForest)

#PIC used for some testing
# CA-BC-200-001019632060000


# for bookmarking button
enableBookmarking("url")

# read data
dat <- readr::read_delim("test_train_data.txt", 
                         delim = ",", col_types = "ciccdddddf")

# creates dataset with only the top 52 municipalities
counts <- dat %>% 
  count(municipality, sort = TRUE)

datshort <- dat %>% 
  filter(municipality %in% counts$municipality[1:52])


# load dataset used for rf.mill
rfdat <- readRDS("rf_data.rds")

# create dataset used for rf.as
asdat <- readRDS("as_data.rds")
  
