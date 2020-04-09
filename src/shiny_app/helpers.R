# This file will have helpers for the model and the loading of data for models

# function to modify data for random forest (if needed)
rfData <- function(data) {
  
  rfdat <- data
  
  rfdat$municipality <- as.factor(rfdat$municipality)
  
  factors_tbl = rfdat %>% 
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
  
  
  
  rfdat <- rfdat %>% 
    left_join(factors_tbl) %>% 
    select(-c(municipality)) %>% 
    rename(municipality = col_municipality) %>% 
    group_by(PIC) %>% 
    mutate(next.assess = lead(total.assessment, order_by = year),
           past.mill = lag(mill.rate, order_by = year)) %>%
    arrange(PIC) %>% 
    group_by(municipality, year) %>% 
    mutate(n.prop = n()) %>% 
    arrange(desc(n.prop)) %>% 
    distinct(municipality, .keep_all = T) 
  
  rfdat
}


