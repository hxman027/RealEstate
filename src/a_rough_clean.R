# Mallory ###
# some cleaning.. with some simplifications we probably don't want
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(viridis)
library(here)
library(rnn)
rm(list=ls())

### DATA CLEANING 
# import assessment and sales data for all years
assess_all <- read.csv(here("data/Assessment/2016 - 2020 Raw.csv"),
                       header = F)
col_names <- read_excel(here("data/Assessment", "2020 Only.xlsx"))
colnames(assess_all) <- colnames(col_names)

# select relevant features 
assess <- assess_all %>%
  select(PIC, Year, AssessorAreaCode:AddressAssessorMunicipalityDesc, 
         TaxClassCode:TaxClassTaxRate)  %>% 
  filter(TaxClassCode %in% c("01","05","06"))

# change types
assess <- assess %>% 
  mutate_at(c(6, 10, 11), as.character) %>% 
  mutate_at(c(6, 10, 11), as.numeric)

# check for NA's (thanks to STAT450 for this code)
for (feature in colnames(assess)){
  variable = eval(parse(text = paste0("assess$",feature)))
  null.count = sum(is.na(variable))
  print(paste(feature, null.count, sep = ":"))
} 

# check for constant mill rate across municipalities
test <- assess %>% 
  group_by(Year, AddressAssessorMunicipalityDesc, TaxClassCode) %>% 
  filter(!is.na(TaxClassTaxRate)) %>% 
  summarise(test = var(TaxClassTaxRate)) %>%
  filter(test !=0)

### for now just took mean of mill rates..
assess.rate <- assess %>%
  group_by(Year, AddressAssessorMunicipalityDesc, TaxClassCode) %>% 
  filter(!is.na(TaxClassTaxRate)) %>% 
  mutate(TaxClassTaxRate = mean(TaxClassTaxRate))

# check that this reduced variance in mill rates to 0 for all municipalities
test1 <- assess.rate %>% 
  group_by(Year, AddressAssessorMunicipalityDesc, TaxClassCode) %>%
  summarise(test = var(TaxClassTaxRate)) %>%
  filter(test !=0)

# create dataset listing mill rates per municipality, year, and tax class
assess.agg <- assess.rate%>%
  group_by(Year, AddressAssessorMunicipalityDesc,TaxClassCode)%>%
  filter(!is.na(TaxClassTaxRate))%>%
  summarise(rate = unique(TaxClassTaxRate))

# merge these rates into the whole dataset
assess.merge <- merge(assess.rate, assess.agg,
                      by = c("AddressAssessorMunicipalityDesc", 
                             "Year", "TaxClassCode"), all = TRUE) %>%
  mutate(TaxClassTaxRate = rate) %>%
  select(-rate)

# add additional column for mean property assessment value in each municipality
assess_final <- assess.merge %>%
  group_by(Year, AddressAssessorMunicipalityDesc,TaxClassCode) %>%
  summarise(meanAssessTotal = sum(AssessedValueAmt)/length(unique(PIC)))

assessments <- merge(assess.merge, assess_final,
                     by = c("AddressAssessorMunicipalityDesc", 
                            "Year", "TaxClassCode"), all = TRUE)

# export to csv file
write.csv(assessments, "data/Assessment/allyears_cleaned.csv")


#### For prediting next years assessment values ####
# Add response column which is next years assessment values per property
temp.assess <- assessments %>% 
  filter(!is.na(AssessedValueAmt)) %>% 
  select(c(PIC, Year, AddressAssessorMunicipalityDesc, AssetTypeDesc,
           AssessedValueAmt, TaxClassCode))

# decrease year by 1 and rename column
temp.assess$Year <- temp.assess$Year - 1
names(temp.assess)[names(temp.assess)=="AssessedValueAmt"] <- "NextYearAssess"

# add this new column to dataset, joined by mutual columns as in temp.assess
# this causes duplicates and increases # of rows - does anyone know how
# this is happening?!
assess.data <- left_join(assessments, temp.assess, all.x = TRUE) %>% 
  select(AddressAssessorMunicipalityDesc, Year, TaxClassCode, PIC,
         AssetTypeDesc, AssessedValueAmt, TaxClassTaxRate, meanAssessTotal,
         NextYearAssess)