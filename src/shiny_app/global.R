library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(shinyjs)

#PIC used for some testing
#CA-BC-022-04490000


# for bookmarking button
enableBookmarking("url")

# read data
# dat <- readr::read_delim("./../../data/test_train_data.txt", 
#                          delim = ",", col_types = "ciccdddddf")

dat <- readr::read_delim("test_train_data.txt", 
                         delim = ",", col_types = "ciccdddddf")

