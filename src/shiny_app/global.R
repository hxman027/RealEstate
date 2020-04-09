library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyjs)
library(tidyr)
library(shinyjs)

#PIC using for testing
#CA-BC-022-04490000


# for bookmarking button
enableBookmarking("url")

# read data
dat <- readr::read_delim("./../../data/test_train_data.txt", 
                         delim = ",", col_types = "ciccdddddf")


