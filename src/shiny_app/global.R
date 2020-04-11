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
dat <- readr::read_delim("test_train_data.txt", 
                         delim = ",", col_types = "ciccdddddf")

# creates dataset with only the top 52 municipalities
counts <- dat %>% 
  count(municipality, sort = TRUE)

datshort <- dat %>% 
  filter(municipality %in% counts$municipality[1:52])
  
