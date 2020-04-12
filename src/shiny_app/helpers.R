# This file will have helpers for the model and the loading of data for models

# function to modify data for random forest (if needed)

# used to create mill rate data for random forest
# no longer needed for shiny app; included for future use if needed
rfData <- function(data) {
  
  dat <- data
  dat$municipality <- as.factor(dat$municipality)
  dat$tax.class <- as.factor(dat$tax.class)
  
  factors_tbl = dat %>% 
    group_by(municipality) %>% 
    count(name="mun_count", sort = TRUE) %>% 
    ungroup() %>% 
    mutate(perc = mun_count/sum(mun_count),
           cum_perc = cumsum(perc)) %>% 
    arrange(desc(mun_count)) %>% 
    mutate(rank = row_number(),
           municipality = fct_reorder(municipality, rank)) %>% 
    mutate(col_municipality = fct_collapse(municipality, other = levels(municipality)[-c(1:52)])) %>% 
    select(municipality, col_municipality)
  
  
  avg_assessment_by_municipality = dat %>% 
    left_join(factors_tbl) %>% 
    select(-c(municipality)) %>% 
    rename(municipality = col_municipality) %>% 
    filter(test.train == "train") %>% 
    group_by(municipality, tax.class, year) %>% 
    mutate(avg_assessment = mean(total.assessment, na.rm=TRUE)) %>% 
    select(municipality, year, tax.class, avg_assessment) %>% 
    distinct(municipality, .keep_all = T)
  
  dat_mill <- dat %>% 
    left_join(factors_tbl) %>% 
    select(-c(municipality)) %>% 
    rename(municipality = col_municipality) %>% 
    left_join(avg_assessment_by_municipality) %>% 
    group_by(PIC) %>% 
    mutate(next.assess = lead(avg_assessment, order_by = year),
           past.mill = lag(mill.rate, order_by = year)) %>%
    arrange(PIC) %>% 
    group_by(municipality, year) %>% 
    mutate(n.prop = n()) %>% 
    arrange(desc(n.prop)) %>% 
    distinct(municipality, .keep_all = T) %>% 
    select(-c(tax, improvement.assessment, land.assessment, total.assessment))
  
  dat_mill
}

# used to create assessment value data for random forest
# no longer needed for shiny app; included for future use if needed
asData <- function(data) {
  
  dat <- data
  dat$municipality <- as.factor(dat$municipality)
  dat$tax.class <- as.factor(dat$tax.class)
  
  factors_tbl = dat %>% 
    group_by(municipality) %>% 
    count(name="mun_count", sort = TRUE) %>% 
    ungroup() %>% 
    mutate(perc = mun_count/sum(mun_count),
           cum_perc = cumsum(perc)) %>% 
    arrange(desc(mun_count)) %>% 
    mutate(rank = row_number(),
           municipality = fct_reorder(municipality, rank)) %>% 
    mutate(col_municipality = fct_collapse(municipality, other = levels(municipality)[-c(1:52)])) %>% 
    select(municipality, col_municipality)
  
  
  avg_assessment_by_municipality = dat %>% 
    left_join(factors_tbl) %>% 
    select(-c(municipality)) %>% 
    rename(municipality = col_municipality) %>% 
    filter(test.train == "train") %>% 
    group_by(municipality, tax.class, year) %>% 
    mutate(avg_assessment = mean(total.assessment, na.rm=TRUE)) %>% 
    select(municipality, year, tax.class, avg_assessment) %>% 
    distinct(municipality, .keep_all = T)
  
  dat_as <- dat %>% 
    left_join(factors_tbl) %>% 
    select(-c(municipality)) %>% 
    rename(municipality = col_municipality) %>% 
    group_by(PIC) %>% 
    arrange(year) %>% 
    mutate(next.assess = lead(total.assessment, order_by = year),
           past.mill = lag(mill.rate, order_by = year)) %>% 
    group_by(municipality) %>% 
    top_n(25, wt = total.assessment) %>% 
    arrange(municipality)
  
  dat_as
}


