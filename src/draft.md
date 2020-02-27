EDA
================
Xuechun Lu, Yuting Wen, Peter Han
25/02/2020

R Markdown
----------

libraries

``` r
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)
library(readxl)
```

``` r
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
 if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
```

1.  data processing

``` r
# all municipalities
municipality.list = c(
  "Burnaby", 
  "Coquitlam", 
  "Delta", 
  "Langley - City", 
  "Langley - Township",
  "Maple Ridge",
  "Maple Ridge Rural", 
  "North Vancouver - City",
  "North Vancouver - Dist",
  "Pitt Meadows", 
  "Port Coquitlam", 
  "Port Moody", 
  "Richmond", 
  "Surrey", 
  "Vancouver", 
  "White Rock", 
  "West Vancouver", 
  "Bowen Island", 
  "Anmore", 
  "Belcarra",
  "Lions Bay")

# budget and assessment data import and process
tax_pct<-read_csv(here("data", "tax_pct.csv"))
assessment_pct<-read_csv(here("data", "assessment_pct.csv"))
# omit 2016
assessment_pct<-na.omit(assessment_pct)
assessment_pct<-assessment_pct[,-c(1)]
names(assessment_pct)[2]<-paste("Municipalities")
tax_pct<-tax_pct[,-c(1)]
#head(tax_pct)
```

Note: tax\_pct doesn't have Maple Ridge Rural. We decide to treat Maple Ridge Rural and Maple Ridge with the same tax.

2019 tax pct is missing, we imputed 2019 tax by the average of previous tax pct

``` r
tax_pct[,4]<-(tax_pct$pct_2017+tax_pct$pct_2018)/2
#tax_pct
names(tax_pct)[4]<-paste("pct_2019")
#head(tax_pct)
```

aggregate tax and assessment

``` r
assessment2017<-assessment_pct  %>%  filter(Year=="2017")
tax2017<-tax_pct[,1:2]
assessment2017<-assessment2017 %>% left_join(tax2017, by = c("Municipalities"))
names(assessment2017)[9]<-paste("tax")

assessment2018<-assessment_pct  %>%  filter(Year=="2018")
tax2018<-tax_pct[,c(1,3)]
assessment2018<-assessment2018 %>% left_join(tax2018, by = c("Municipalities"))
names(assessment2018)[9]<-paste("tax")

assessment2019<-assessment_pct  %>%  filter(Year=="2019")
tax2019<-tax_pct[,c(1,4)]
assessment2019<-assessment2019 %>% left_join(tax2019, by = c("Municipalities"))
names(assessment2019)[9]<-paste("tax")

pct_final<-assessment2017 %>% full_join(assessment2018) %>% full_join(assessment2019)
```

    ## Joining, by = c("Year", "Municipalities", "TaxClassCode",
    ## "assessTotal_pct", "landTotal_pct", "improvementTotal_pct",
    ## "propertyCount_pct", "rate_pct", "tax")Joining, by = c("Year",
    ## "Municipalities", "TaxClassCode", "assessTotal_pct", "landTotal_pct",
    ## "improvementTotal_pct", "propertyCount_pct", "rate_pct", "tax")

``` r
write.csv(pct_final, here("data","data_final.csv"))

# all pct data aggregated
#head(pct_final)
```

2019 tax is missing, we imputed 2019 tax using previous values

``` r
tax_final <- read.csv(here("data","tax_final.csv"))
#head(tax_final)

# tax_final is the budget
tax_modified <- tax_final  %>% mutate(X2019=tax_final$X2018*(1+(((tax_final$X2017-tax_final$X2016)/tax_final$X2016 + (tax_final$X2018-tax_final$X2017)/tax_final$X2017)/2)))

# tax_modified contains all the budget values from 2016 to 2019
#head(tax_modified)
```

global variables

``` r
# all pct aggregated
pct_final
```

    ## # A tibble: 165 x 9
    ##     Year Municipalities TaxClassCode assessTotal_pct landTotal_pct
    ##    <dbl> <chr>          <chr>                  <dbl>         <dbl>
    ##  1  2017 Anmore         01                   -13.9          -0.380
    ##  2  2017 Anmore         06                    10.3          45.4  
    ##  3  2017 Belcarra       01                    12.2          14.7  
    ##  4  2017 Bowen Island   01                     7.59         12.9  
    ##  5  2017 Bowen Island   05                    -0.490         0    
    ##  6  2017 Bowen Island   06                    14.7          37.5  
    ##  7  2017 Burnaby        01                    12.0          25.4  
    ##  8  2017 Burnaby        05                    17.5          31.0  
    ##  9  2017 Burnaby        06                    15.1          23.9  
    ## 10  2017 Coquitlam      01                     9.80         11.9  
    ## # ... with 155 more rows, and 4 more variables:
    ## #   improvementTotal_pct <dbl>, propertyCount_pct <dbl>, rate_pct <dbl>,
    ## #   tax <dbl>

``` r
# all budget values
tax_modified
```

    ##     X         Municipalities      X2016      X2017      X2018      X2019
    ## 1   1                 Anmore    4265156    4627081    4881334    5222552
    ## 2   2               Belcarra    1855564    1931623    2067747    2182984
    ## 3   3           Bowen Island    7219146    7497379    8121276    8615684
    ## 4   4                Burnaby  432220186  450355493  474814512  497669462
    ## 5   5              Coquitlam  216182822  227096791  240417810  253537746
    ## 6   6                  Delta  192862492  208509386  210564865  220144278
    ## 7   7         Langley - City   37056327  190218801  201523378  623983226
    ## 8   8     Langley - Township  180361779   38227355   40259997   25466877
    ## 9   9              Lions Bay    2152138    2337089    2459587    2629733
    ## 10 10            Maple Ridge   97232843  103257133  109137582  115626189
    ## 11 11 North Vancouver - City   89284753   94275663   98708171  103787459
    ## 12 12 North Vancouver - Dist  145586665  154117460  163829833  173791939
    ## 13 13           Pitt Meadows   24871507   26483421   27853805   29477050
    ## 14 14         Port Coquitlam   84721363   89609311   92674259   96932542
    ## 15 15             Port Moody   50587243   54750183   57661906   61567758
    ## 16 16               Richmond  358738675  382830769  398658518  420286095
    ## 17 17                 Surrey  555370000  597000000  636460000  681348311
    ## 18 18              Vancouver 1357194000 1495095000 1536009000 1635060782
    ## 19 19         West Vancouver  122852194  130516395  131670339  136359575
    ## 20 20             White Rock   30725883   32348682   32939664   34110414

1.  val\_plots

``` r
# mill rate vesus year for each municipality and tax class
assessment_aggregate <- read.csv(here("data","assessment_aggregate.csv"))
dat_rate <- assessment_aggregate %>% select(Year, AddressAssessorMunicipalityDesc, TaxClassCode, rate)
#head(dat_rate)

dat_rate_1 <- dat_rate %>% filter(AddressAssessorMunicipalityDesc %in% municipality.list) %>% filter(TaxClassCode=="1") 
#head(dat_rate_1)
rate_class1 <- dat_rate_1 %>% ggplot(aes(x=Year,y=rate,group=AddressAssessorMunicipalityDesc,color=AddressAssessorMunicipalityDesc)) + geom_line() + ggtitle("mill rate for taxClass 1") + theme(legend.position = "none")
#rate_class1

dat_rate_5 <- dat_rate %>% filter(AddressAssessorMunicipalityDesc %in% municipality.list) %>% filter(TaxClassCode=="5") 
#head(dat_rate_5)
rate_class5 <- dat_rate_5 %>% ggplot(aes(x=Year,y=rate,group=AddressAssessorMunicipalityDesc,color=AddressAssessorMunicipalityDesc)) + geom_line() + ggtitle("mill rate for taxClass 5") + theme(legend.position = "none")
#rate_class5

dat_rate_6 <- dat_rate %>% filter(AddressAssessorMunicipalityDesc %in% municipality.list) %>% filter(TaxClassCode=="6") 
#head(dat_rate_6)
rate_class6 <- dat_rate_6 %>% ggplot(aes(x=Year,y=rate,group=AddressAssessorMunicipalityDesc,color=AddressAssessorMunicipalityDesc)) + geom_line() + ggtitle("mill rate for taxClass 6") + theme(legend.position = "none")
#rate_class6

#multiplot(rate_class1, rate_class5, rate_class6)

# plot assessed value vesus year for each municipality and tax class
dat_assess <- assessment_aggregate %>% select(Year, AddressAssessorMunicipalityDesc, TaxClassCode, assessTotal)
#head(dat_assess)

dat_assess_1 <- dat_assess %>% filter(AddressAssessorMunicipalityDesc %in% municipality.list) %>% filter(TaxClassCode=="1") 
#head(dat_assess_1)
assess_class1 <- dat_assess_1 %>% ggplot(aes(x=Year,y=assessTotal,group=AddressAssessorMunicipalityDesc,color=AddressAssessorMunicipalityDesc)) + geom_line() + ggtitle("assessTotal for taxClass 1") + theme(legend.position = "none")
#assess_class1

dat_assess_5 <- dat_assess %>% filter(AddressAssessorMunicipalityDesc %in% municipality.list) %>% filter(TaxClassCode=="5") 
#head(dat_assess_5)
assess_class5 <- dat_assess_5 %>% ggplot(aes(x=Year,y=assessTotal,group=AddressAssessorMunicipalityDesc,color=AddressAssessorMunicipalityDesc)) + geom_line() + ggtitle("assessTotal for taxClass 5") + theme(legend.position = "none")
#assess_class5

dat_assess_6 <- dat_assess %>% filter(AddressAssessorMunicipalityDesc %in% municipality.list) %>% filter(TaxClassCode=="6") 
#head(dat_assess_6)
assess_class6 <- dat_assess_1 %>% ggplot(aes(x=Year,y=assessTotal,group=AddressAssessorMunicipalityDesc,color=AddressAssessorMunicipalityDesc)) + geom_line() + ggtitle("assessTotal for taxClass 6") + theme(legend.position = "none")
#assess_class6

#multiplot(assess_class1, assess_class5, assess_class6)

# plot budgets vesus year
dat_tax_long <- gather(tax_modified, Year, TaxAmount, X2016:X2019, factor_key=TRUE)
#dat_tax_long
budget_plot <- dat_tax_long %>% ggplot(aes(x=Year,y=TaxAmount,group=Municipalities,color=Municipalities)) + geom_line() +ggtitle("Tax v.s Year")
#budget_plot

multiplot(rate_class1,assess_class1,budget_plot)
```

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

``` r
multiplot(rate_class5,assess_class5,budget_plot)
```

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-2.png)

``` r
multiplot(rate_class6,assess_class6,budget_plot)
```

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-3.png)

1.  pct\_plots pct change of mill rates for each tax class

``` r
pct_rate1 <- pct_final %>% 
  filter(Municipalities %in% municipality.list) %>%
  filter(TaxClassCode == "01") %>%
  ggplot(aes(x=Year, y=rate_pct, group=Municipalities, color=Municipalities)) +
    geom_line() + ggtitle("taxClass1 pct rate v.s year") + theme(legend.position = "none")

pct_rate5 <- pct_final %>% 
  filter(Municipalities %in% municipality.list) %>%
  filter(TaxClassCode == "05") %>%
  ggplot(aes(x=Year, y=rate_pct, group=Municipalities, color=Municipalities)) +
    geom_line() + ggtitle("taxClass5 pct rate v.s year") + theme(legend.position = "none")

pct_rate6 <- pct_final %>% 
  filter(Municipalities %in% municipality.list) %>%
  filter(TaxClassCode == "06") %>%
  ggplot(aes(x=Year, y=rate_pct, group=Municipalities, color=Municipalities)) +
    geom_line() + ggtitle("taxClass6 pct rate v.s year") + theme(legend.position = "none")

pct_assess1 <- pct_final %>% 
  filter(Municipalities %in% municipality.list) %>%
  filter(TaxClassCode == "01") %>%
  ggplot(aes(x=Year, y=assessTotal_pct, group=Municipalities, color=Municipalities)) +
    geom_line() + ggtitle("taxClass1 pct assessment v.s year") + theme(legend.position = "none")

pct_assess5 <- pct_final %>% 
  filter(Municipalities %in% municipality.list) %>%
  filter(TaxClassCode == "05") %>%
  ggplot(aes(x=Year, y=assessTotal_pct, group=Municipalities, color=Municipalities)) +
    geom_line() + ggtitle("taxClass5 pct assessment v.s year") + theme(legend.position = "none")

pct_assess6 <- pct_final %>% 
  filter(Municipalities %in% municipality.list) %>%
  filter(TaxClassCode == "06") %>%
  ggplot(aes(x=Year, y=assessTotal_pct, group=Municipalities, color=Municipalities)) +
    geom_line() + ggtitle("taxClass6 pct assessment v.s year") + theme(legend.position = "none")



multiplot(pct_rate1,pct_assess1)
```

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-1.png)

``` r
multiplot(pct_rate5,pct_assess5)
```

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-2.png)

``` r
multiplot(pct_rate6,pct_assess6)
```

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-3.png) pct change of mill rate for each municipality

``` r
# for each municipality
tax.list <- c("01","05","06")
for (i in 1:21){
  plot<-pct_final %>% filter(Municipalities == municipality.list[i]) %>% 
  filter(TaxClassCode %in% tax.list) %>% 
  ggplot(aes(x=Year, y=rate_pct, group=TaxClassCode, color=TaxClassCode)) +
    geom_line() + ggtitle(municipality.list[i])
  print(plot)
}
```

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-2.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-3.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-4.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-5.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-6.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-7.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-8.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-9.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-10.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-11.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-12.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-13.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-14.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-15.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-16.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-17.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-18.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-19.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-20.png)

    ## geom_path: Each group consists of only one observation. Do you need to
    ## adjust the group aesthetic?

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-21.png)

plot pct change of assessment

``` r
# total trend of assessment 
for (i in 1:21){
  plot<-pct_final %>% filter(Municipalities == municipality.list[i]) %>% 
  filter(TaxClassCode %in% tax.list) %>% 
  ggplot(aes(x=Year, y=assessTotal_pct, group=TaxClassCode, color=TaxClassCode)) +
    geom_line() + ggtitle(municipality.list[i])
  print(plot)
}
```

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-1.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-2.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-3.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-4.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-5.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-6.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-7.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-8.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-9.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-10.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-11.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-12.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-13.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-14.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-15.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-16.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-17.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-18.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-19.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-20.png)

    ## geom_path: Each group consists of only one observation. Do you need to
    ## adjust the group aesthetic?

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-21.png)

plot pct change of tax(budget)

``` r
# total trend for budget 
for (i in 1:21){
  plot<-pct_final %>% filter(Municipalities == municipality.list[i]) %>% 
  filter(TaxClassCode %in% tax.list) %>% 
  ggplot(aes(x=Year, y=tax, group=TaxClassCode, color=TaxClassCode)) +
    geom_line() + ggtitle(municipality.list[i])
  print(plot)
}
```

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-1.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-2.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-3.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-4.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-5.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-6.png)

    ## Warning: Removed 6 rows containing missing values (geom_path).

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-7.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-8.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-9.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-10.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-11.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-12.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-13.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-14.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-15.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-16.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-17.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-18.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-19.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-20.png)

    ## geom_path: Each group consists of only one observation. Do you need to
    ## adjust the group aesthetic?

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-21.png)

``` r
# plot rate, assessment, and budget for each municipality. 

for (i in 1:21){
  test1<-pct_final %>% filter(Municipalities == municipality.list[i]) %>% 
  filter(TaxClassCode %in% tax.list) %>% 
  ggplot(aes(x=Year, y=rate_pct, group=TaxClassCode, color=TaxClassCode)) +
   geom_line() 
  
  test2<-pct_final %>% filter(Municipalities == municipality.list[i]) %>% 
  filter(TaxClassCode %in% tax.list) %>% 
  ggplot(aes(x=Year, y=assessTotal_pct, group=TaxClassCode, color=TaxClassCode)) +
   geom_line() 

  test3<-pct_final %>% filter(Municipalities == municipality.list[i]) %>% 
  filter(TaxClassCode %in% tax.list) %>% 
  ggplot(aes(x=Year, y=tax)) +
   geom_line() + theme(legend.position = "none")
  
  multiplot(test1,test2,test3)
  
}
```

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-1.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-2.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-3.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-4.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-5.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-6.png)

    ## Warning: Removed 6 rows containing missing values (geom_path).

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-7.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-8.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-9.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-10.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-11.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-12.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-13.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-14.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-15.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-16.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-17.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-18.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-19.png)![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-20.png)

    ## geom_path: Each group consists of only one observation. Do you need to
    ## adjust the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to
    ## adjust the group aesthetic?

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-21.png)

1.  box plots of mill rates, grouped by region

``` r
rate_boxes <- ggplot(dat_rate, aes(x=AddressAssessorMunicipalityDesc,y=rate,fill=TaxClassCode)) + geom_boxplot() + facet_wrap(~TaxClassCode) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")
rate_boxes 
```

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-14-1.png)

1.  Test correlations between past mill rates and assessed values and government budget.

``` r
#tax_final
#assessment_aggregate

#colnames(tax_modified) <- c("X","Municipalities","2016","2017","2018","2019")
#head(tax_modified)
#dat_tax_long <- gather(tax_modified, Year, TaxAmount, "2016":"2019", factor_key=TRUE)

#tax.all <- full_join(assessment_aggregate, tax_modified, by = 'Municipalities') 
#tax.all
#(tax.all <- tax.all[,-5])
# correlation by Municipalities?
#mill.budget.cor <- tax.all  %>% summarize(cor_2017 = cor(pct_2017, X2017), cor_2018 = cor(pct_2018, X2018), cor_2019 = cor(pct_2019, X2019))
#mill.budget.cor



years <- c(2017, 2018, 2019)
city.cor <- data.frame(Municipality = municipality.list)
corr.list <- vector(length = 21)
corr.list
```

    ##  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [12] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

``` r
for(city in municipality.list){
  corr <- assessment_aggregate %>% filter(AddressAssessorMunicipalityDesc == city) %>% summarize(cor = cor(assessTotal, rate))
  #print(corr)
  corr.list <- cbind(corr.list, corr)
}
corr.list
```

    ##    corr.list         cor       cor       cor       cor        cor
    ## 1      FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 2      FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 3      FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 4      FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 5      FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 6      FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 7      FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 8      FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 9      FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 10     FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 11     FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 12     FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 13     FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 14     FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 15     FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 16     FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 17     FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 18     FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 19     FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 20     FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ## 21     FALSE -0.09468404 -0.290223 0.5151795 0.2311235 -0.1800361
    ##          cor        cor        cor        cor         cor      cor
    ## 1  0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 2  0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 3  0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 4  0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 5  0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 6  0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 7  0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 8  0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 9  0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 10 0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 11 0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 12 0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 13 0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 14 0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 15 0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 16 0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 17 0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 18 0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 19 0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 20 0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ## 21 0.2480501 -0.8203257 -0.2697796 -0.8533351 -0.06697237 0.205535
    ##           cor        cor        cor        cor        cor        cor
    ## 1  -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 2  -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 3  -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 4  -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 5  -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 6  -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 7  -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 8  -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 9  -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 10 -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 11 -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 12 -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 13 -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 14 -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 15 -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 16 -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 17 -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 18 -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 19 -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 20 -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ## 21 -0.5248267 0.08754046 0.01192877 -0.8150188 -0.9944171 -0.7755168
    ##           cor        cor        cor        cor
    ## 1  -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 2  -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 3  -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 4  -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 5  -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 6  -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 7  -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 8  -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 9  -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 10 -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 11 -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 12 -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 13 -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 14 -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 15 -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 16 -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 17 -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 18 -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 19 -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 20 -0.8769237 -0.8955714 -0.9757262 -0.9944927
    ## 21 -0.8769237 -0.8955714 -0.9757262 -0.9944927

``` r
city.cor <- cbind(city.cor,corr.list)
#city.cor
```

1.  Compute autocorrelations of past mill rates to test the independence of mill rates.

``` r
# all white noise
for (i in 1:21) {
  print(municipality.list[i])
  dat_tmp <- dat_rate %>% filter(AddressAssessorMunicipalityDesc %in% municipality.list[i] & TaxClassCode %in% "1") %>% select(rate)
  if (municipality.list[i] != "Maple Ridge Rural") {
    as.ts(dat_tmp)
    acf(dat_tmp)
  }
}
```

    ## [1] "Burnaby"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-1.png)

    ## [1] "Coquitlam"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-2.png)

    ## [1] "Delta"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-3.png)

    ## [1] "Langley - City"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-4.png)

    ## [1] "Langley - Township"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-5.png)

    ## [1] "Maple Ridge"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-6.png)

    ## [1] "Maple Ridge Rural"
    ## [1] "North Vancouver - City"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-7.png)

    ## [1] "North Vancouver - Dist"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-8.png)

    ## [1] "Pitt Meadows"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-9.png)

    ## [1] "Port Coquitlam"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-10.png)

    ## [1] "Port Moody"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-11.png)

    ## [1] "Richmond"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-12.png)

    ## [1] "Surrey"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-13.png)

    ## [1] "Vancouver"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-14.png)

    ## [1] "White Rock"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-15.png)

    ## [1] "West Vancouver"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-16.png)

    ## [1] "Bowen Island"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-17.png)

    ## [1] "Anmore"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-18.png)

    ## [1] "Belcarra"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-19.png)

    ## [1] "Lions Bay"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-20.png)

``` r
for (i in 1:21) {
  print(municipality.list[i])
  dat_tmp <- dat_rate %>% filter(AddressAssessorMunicipalityDesc %in% municipality.list[i] & TaxClassCode %in% "5") %>% select(rate)
  if (municipality.list[i] != "Maple Ridge Rural" & municipality.list[i] != "White Rock" & municipality.list[i] != "Anmore" & municipality.list[i] != "Belcarra" & municipality.list[i] != "Lions Bay") {
    as.ts(dat_tmp)
    acf(dat_tmp)
  }
}
```

    ## [1] "Burnaby"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-21.png)

    ## [1] "Coquitlam"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-22.png)

    ## [1] "Delta"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-23.png)

    ## [1] "Langley - City"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-24.png)

    ## [1] "Langley - Township"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-25.png)

    ## [1] "Maple Ridge"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-26.png)

    ## [1] "Maple Ridge Rural"
    ## [1] "North Vancouver - City"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-27.png)

    ## [1] "North Vancouver - Dist"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-28.png)

    ## [1] "Pitt Meadows"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-29.png)

    ## [1] "Port Coquitlam"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-30.png)

    ## [1] "Port Moody"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-31.png)

    ## [1] "Richmond"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-32.png)

    ## [1] "Surrey"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-33.png)

    ## [1] "Vancouver"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-34.png)

    ## [1] "White Rock"
    ## [1] "West Vancouver"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-35.png)

    ## [1] "Bowen Island"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-36.png)

    ## [1] "Anmore"
    ## [1] "Belcarra"
    ## [1] "Lions Bay"

``` r
for (i in 1:21) {
  print(municipality.list[i])
  dat_tmp <- dat_rate %>% filter(AddressAssessorMunicipalityDesc %in% municipality.list[i] & TaxClassCode %in% "6") %>% select(rate)
  if (municipality.list[i] != "Maple Ridge Rural") {
    as.ts(dat_tmp)
    acf(dat_tmp)
  }
}
```

    ## [1] "Burnaby"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-37.png)

    ## [1] "Coquitlam"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-38.png)

    ## [1] "Delta"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-39.png)

    ## [1] "Langley - City"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-40.png)

    ## [1] "Langley - Township"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-41.png)

    ## [1] "Maple Ridge"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-42.png)

    ## [1] "Maple Ridge Rural"
    ## [1] "North Vancouver - City"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-43.png)

    ## [1] "North Vancouver - Dist"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-44.png)

    ## [1] "Pitt Meadows"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-45.png)

    ## [1] "Port Coquitlam"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-46.png)

    ## [1] "Port Moody"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-47.png)

    ## [1] "Richmond"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-48.png)

    ## [1] "Surrey"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-49.png)

    ## [1] "Vancouver"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-50.png)

    ## [1] "White Rock"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-51.png)

    ## [1] "West Vancouver"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-52.png)

    ## [1] "Bowen Island"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-53.png)

    ## [1] "Anmore"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-54.png)

    ## [1] "Belcarra"

    ## [1] "Lions Bay"

![](draft_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-55.png)

model fitting

``` r
linear1<-lm(rate_pct~factor(Year)+factor(TaxClassCode)+factor(Municipalities)+assessTotal_pct+tax,data=pct_final)
summary(linear1)
```

    ## 
    ## Call:
    ## lm(formula = rate_pct ~ factor(Year) + factor(TaxClassCode) + 
    ##     factor(Municipalities) + assessTotal_pct + tax, data = pct_final)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -30.8819  -4.0828  -0.0492   4.4060  29.4439 
    ## 
    ## Coefficients:
    ##                                                Estimate Std. Error t value
    ## (Intercept)                                  -1.189e+01  3.304e+00  -3.599
    ## factor(Year)2018                              5.329e+00  1.491e+00   3.574
    ## factor(Year)2019                              7.008e+00  1.455e+00   4.816
    ## factor(TaxClassCode)05                       -2.029e+00  1.521e+00  -1.334
    ## factor(TaxClassCode)06                       -2.627e+00  1.401e+00  -1.875
    ## factor(Municipalities)Belcarra               -1.929e+00  4.814e+00  -0.401
    ## factor(Municipalities)Bowen Island            1.247e+00  3.940e+00   0.316
    ## factor(Municipalities)Burnaby                -2.048e+00  3.933e+00  -0.521
    ## factor(Municipalities)Coquitlam              -2.896e+00  3.935e+00  -0.736
    ## factor(Municipalities)Delta                   9.072e-01  3.929e+00   0.231
    ## factor(Municipalities)Langley - City         -2.488e+00  4.946e+00  -0.503
    ## factor(Municipalities)Langley - Township     -1.356e+00  4.006e+00  -0.339
    ## factor(Municipalities)Lions Bay               4.867e+00  6.160e+00   0.790
    ## factor(Municipalities)Maple Ridge            -1.351e+00  3.929e+00  -0.344
    ## factor(Municipalities)North Vancouver - City -3.497e+00  3.932e+00  -0.890
    ## factor(Municipalities)North Vancouver - Dist -2.728e+00  3.929e+00  -0.694
    ## factor(Municipalities)Pitt Meadows           -1.856e-01  3.938e+00  -0.047
    ## factor(Municipalities)Port Coquitlam         -6.389e-01  3.929e+00  -0.163
    ## factor(Municipalities)Port Moody             -8.472e-01  3.929e+00  -0.216
    ## factor(Municipalities)Richmond               -1.860e+00  3.929e+00  -0.473
    ## factor(Municipalities)Surrey                 -6.265e-01  3.929e+00  -0.159
    ## factor(Municipalities)Vancouver               1.497e-01  3.930e+00   0.038
    ## factor(Municipalities)West Vancouver          1.080e+00  4.294e+00   0.251
    ## factor(Municipalities)White Rock             -1.928e-01  4.286e+00  -0.045
    ## assessTotal_pct                              -1.100e-02  4.256e-02  -0.259
    ## tax                                           1.621e-04  1.485e-02   0.011
    ##                                              Pr(>|t|)    
    ## (Intercept)                                  0.000449 ***
    ## factor(Year)2018                             0.000491 ***
    ## factor(Year)2019                             3.92e-06 ***
    ## factor(TaxClassCode)05                       0.184540    
    ## factor(TaxClassCode)06                       0.062969 .  
    ## factor(Municipalities)Belcarra               0.689300    
    ## factor(Municipalities)Bowen Island           0.752138    
    ## factor(Municipalities)Burnaby                0.603432    
    ## factor(Municipalities)Coquitlam              0.463138    
    ## factor(Municipalities)Delta                  0.817742    
    ## factor(Municipalities)Langley - City         0.615741    
    ## factor(Municipalities)Langley - Township     0.735491    
    ## factor(Municipalities)Lions Bay              0.430814    
    ## factor(Municipalities)Maple Ridge            0.731553    
    ## factor(Municipalities)North Vancouver - City 0.375315    
    ## factor(Municipalities)North Vancouver - Dist 0.488753    
    ## factor(Municipalities)Pitt Meadows           0.962481    
    ## factor(Municipalities)Port Coquitlam         0.871084    
    ## factor(Municipalities)Port Moody             0.829598    
    ## factor(Municipalities)Richmond               0.636760    
    ## factor(Municipalities)Surrey                 0.873538    
    ## factor(Municipalities)Vancouver              0.969663    
    ## factor(Municipalities)West Vancouver         0.801824    
    ## factor(Municipalities)White Rock             0.964194    
    ## assessTotal_pct                              0.796415    
    ## tax                                          0.991308    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.405 on 133 degrees of freedom
    ##   (6 observations deleted due to missingness)
    ## Multiple R-squared:  0.2217, Adjusted R-squared:  0.07545 
    ## F-statistic: 1.516 on 25 and 133 DF,  p-value: 0.0698

``` r
linear2<-lm(rate_pct~factor(TaxClassCode)+factor(Municipalities)+assessTotal_pct+tax,data=pct_final)
summary(linear2)
```

    ## 
    ## Call:
    ## lm(formula = rate_pct ~ factor(TaxClassCode) + factor(Municipalities) + 
    ##     assessTotal_pct + tax, data = pct_final)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -28.0028  -2.0628   0.5496   3.8465  30.6607 
    ## 
    ## Coefficients:
    ##                                               Estimate Std. Error t value
    ## (Intercept)                                  -7.822056   3.460392  -2.260
    ## factor(TaxClassCode)05                       -1.990812   1.645029  -1.210
    ## factor(TaxClassCode)06                       -2.535446   1.515212  -1.673
    ## factor(Municipalities)Belcarra               -1.159500   5.203147  -0.223
    ## factor(Municipalities)Bowen Island            1.267075   4.261658   0.297
    ## factor(Municipalities)Burnaby                -2.090676   4.254545  -0.491
    ## factor(Municipalities)Coquitlam              -2.935114   4.256666  -0.690
    ## factor(Municipalities)Delta                   0.882764   4.249604   0.208
    ## factor(Municipalities)Langley - City         -0.315764   5.310126  -0.059
    ## factor(Municipalities)Langley - Township     -1.868413   4.328997  -0.432
    ## factor(Municipalities)Lions Bay               7.851588   6.610575   1.188
    ## factor(Municipalities)Maple Ridge            -1.361110   4.249425  -0.320
    ## factor(Municipalities)North Vancouver - City -3.532639   4.252733  -0.831
    ## factor(Municipalities)North Vancouver - Dist -2.742585   4.249790  -0.645
    ## factor(Municipalities)Pitt Meadows           -0.224231   4.258863  -0.053
    ## factor(Municipalities)Port Coquitlam         -0.659039   4.249977  -0.155
    ## factor(Municipalities)Port Moody             -0.850653   4.249406  -0.200
    ## factor(Municipalities)Richmond               -1.873446   4.249575  -0.441
    ## factor(Municipalities)Surrey                 -0.625917   4.249386  -0.147
    ## factor(Municipalities)Vancouver               0.134208   4.250695   0.032
    ## factor(Municipalities)West Vancouver          1.082938   4.644611   0.233
    ## factor(Municipalities)White Rock             -0.199724   4.635911  -0.043
    ## assessTotal_pct                              -0.006789   0.045622  -0.149
    ## tax                                          -0.010622   0.015720  -0.676
    ##                                              Pr(>|t|)  
    ## (Intercept)                                    0.0254 *
    ## factor(TaxClassCode)05                         0.2283  
    ## factor(TaxClassCode)06                         0.0966 .
    ## factor(Municipalities)Belcarra                 0.8240  
    ## factor(Municipalities)Bowen Island             0.7667  
    ## factor(Municipalities)Burnaby                  0.6239  
    ## factor(Municipalities)Coquitlam                0.4917  
    ## factor(Municipalities)Delta                    0.8358  
    ## factor(Municipalities)Langley - City           0.9527  
    ## factor(Municipalities)Langley - Township       0.6667  
    ## factor(Municipalities)Lions Bay                0.2370  
    ## factor(Municipalities)Maple Ridge              0.7492  
    ## factor(Municipalities)North Vancouver - City   0.4076  
    ## factor(Municipalities)North Vancouver - Dist   0.5198  
    ## factor(Municipalities)Pitt Meadows             0.9581  
    ## factor(Municipalities)Port Coquitlam           0.8770  
    ## factor(Municipalities)Port Moody               0.8416  
    ## factor(Municipalities)Richmond                 0.6600  
    ## factor(Municipalities)Surrey                   0.8831  
    ## factor(Municipalities)Vancouver                0.9749  
    ## factor(Municipalities)West Vancouver           0.8160  
    ## factor(Municipalities)White Rock               0.9657  
    ## assessTotal_pct                                0.8819  
    ## tax                                            0.5004  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.009 on 135 degrees of freedom
    ##   (6 observations deleted due to missingness)
    ## Multiple R-squared:  0.07575,    Adjusted R-squared:  -0.08172 
    ## F-statistic: 0.481 on 23 and 135 DF,  p-value: 0.9782
