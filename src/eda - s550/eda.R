# Gian Carlo EDA

# preamble ####
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
ggplot2::theme_set(theme_classic())
library(viridis)

# data wrangling ####

# data import
columnnames <- readxl::read_xlsx("colnames.xlsx") %>% 
  colnames()

real.estate_full <- readr::read_csv("2016 - 2020 Raw.csv",
                                    na = c("", "NA", "NULL", "NULL_1"),
                                    col_names = columnnames,
                                    col_types = "ciccccccccccccccdddddiicccddddc")
# data wrangling

# assessments by PIC and year
assessments <- real.estate_full %>% 
  dplyr::select(PIC, Year, # relevant variables
                AssessedValueAmt, AssetTypeDesc) %>% 
  dplyr::rename(year = Year,
                assessment = AssessedValueAmt,
                assessment.type = AssetTypeDesc) %>% 
  dplyr::group_by(PIC, year, assessment.type) %>% 
  dplyr::summarise(assessment = sum(assessment)) %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(assessment.type, assessment) %>% 
  dplyr::rename(improvement.assessment = Improvement,
                land.assessment = Land) %>% 
  dplyr::select(PIC, year, improvement.assessment, land.assessment) %>% 
  dplyr::mutate(total.assessment = improvement.assessment + land.assessment)


re <- real.estate_full %>% 
  dplyr::select(PIC, Year, AddressAssessorMunicipalityDesc, # relevant variables
                TaxClassCode, TaxOwingAmountTotalCalculated, TaxClassTaxRate) %>% 
  dplyr::rename(year = Year,
                municipality = AddressAssessorMunicipalityDesc, # human-readable names
                tax.class = TaxClassCode,
                tax = TaxOwingAmountTotalCalculated,
                mill.rate = TaxClassTaxRate) %>% 
  dplyr::filter(tax.class %in% c("01", "05", "06")) %>%  # relevant values for tax class
  dplyr::distinct() %>% 
  dplyr::left_join(assessments, by = c("PIC" = "PIC", "year" = "year")) # add assessment

# properties by municipality 2020
re %>% 
  dplyr::select(municipality, PIC) %>% 
  #dplyr::filter(year == 2020) %>% 
  dplyr::group_by(municipality) %>% 
  dplyr::summarise(n.properties = n()) %>% 
  dplyr::arrange(desc(n.properties)) %>% 
  dplyr::mutate(perc.properties = n.properties / sum(n.properties),
                acum.properties = cumsum(perc.properties)) %>% 
  View()
  
  

# data viz ####
# tax classes dictionary
tax.classes <- as_labeller(c(
  `01` = "01 - Residential",
  `05` = "05 - Industrial",
  `06` = "06 - Commercial"
))


# facet scatter plots with year
re %>% 
  dplyr::select(-PIC) %>% 
  dplyr::filter(!is.na(total.assessment), !is.na(mill.rate)) %>% 
  dplyr::group_by(year, municipality, tax.class) %>% 
  dplyr::summarise(total.assessment = mean(total.assessment), mill.rate = mean(mill.rate)) %>% 
  ggplot(aes(x = log(total.assessment), y = log(mill.rate), color = factor(year))) +
  geom_point() +
  facet_wrap(.~tax.class, labeller = tax.classes) +
  labs(x = "mean log assessment value",
       y = "mean log mill rate",
       color = "year") +
  scale_color_viridis_d() +
  theme(text = element_text(size = 18))
ggsave("RealEstate/src/eda - s550/plots/1. scatter with year.pdf")
ggsave("RealEstate/src/eda - s550/plots/1. scatter with year.png")


# facet scatter plots for 2020 by municipality
re %>% 
  dplyr::filter(year == 2020) %>% 
  dplyr::select(-PIC, -year) %>% 
  dplyr::filter(!is.na(total.assessment), !is.na(mill.rate)) %>% 
  dplyr::group_by(municipality, tax.class) %>% 
  dplyr::summarise(total.assessment = mean(total.assessment), mill.rate = mean(mill.rate)) %>% 
  ggplot(aes(x = log(total.assessment), y = log(mill.rate))) +
  geom_point(color = viridis(20)[3]) +
  facet_wrap(.~tax.class, labeller = tax.classes) +
  labs(x = "mean log assessment value",
       y = "mean log mill rate") +
  theme(text = element_text(size = 18))
ggsave("RealEstate/src/eda - s550/plots/2. scatter 2020 by municipality.pdf")
ggsave("RealEstate/src/eda - s550/plots/2. scatter 2020 by municipality.png")

# facet scatter plots for 2020 by property
re %>% 
  dplyr::filter(year == 2020) %>% 
  dplyr::filter(!is.na(total.assessment), !is.na(mill.rate)) %>% 
  dplyr::group_by(PIC, tax.class) %>% 
  dplyr::summarise(total.assessment = mean(total.assessment), mill.rate = mean(mill.rate)) %>% 
  ggplot(aes(x = log(total.assessment), y = log(mill.rate))) +
  geom_point(color = viridis(20)[3]) +
  facet_wrap(.~tax.class, labeller = tax.classes) +
  labs(x = "log assessment",
       y = "log mill rate",
       title = "Assessment vs mill rates by property  for 2020")
ggsave("RealEstate/src/eda - s550/plots/3. scatter 2020 by property.pdf")
ggsave("RealEstate/src/eda - s550/plots/3. scatter 2020 by property.png")

# repeat for each year
for(i in 2016:2019){
  p <- re %>% 
    dplyr::filter(year == i) %>% 
    dplyr::select(-PIC, -year) %>% 
    dplyr::filter(!is.na(total.assessment), !is.na(mill.rate)) %>% 
    dplyr::group_by(municipality, tax.class) %>% 
    dplyr::summarise(total.assessment = mean(total.assessment), mill.rate = mean(mill.rate)) %>% 
    ggplot(aes(x = log(total.assessment), y = log(mill.rate))) +
    geom_point(color = viridis(20)[3]) +
    facet_wrap(.~tax.class, labeller = tax.classes) +
    labs(x = "log assessment",
         y = "log mill rate",
         title = paste0("Assessment vs mill rates by municipality  for ", as.character(i)))
  no <- i - 2015
  title <- paste0("RealEstate/src/eda - s550/plots/2.", as.character(no), " scatter ", as.character(i), " by municipality")
  ggsave(filename = paste0(title, ".pdf"), plot = p)
  ggsave(filename = paste0(title, ".png"), plot = p)
}

# facet scatter plots average over years
re %>% 
  dplyr::select(-PIC) %>% 
  dplyr::filter(!is.na(total.assessment), !is.na(mill.rate)) %>% 
  dplyr::group_by(municipality, tax.class) %>% 
  dplyr::summarise(total.assessment = mean(total.assessment), mill.rate = mean(mill.rate)) %>% 
  ggplot(aes(x = log(total.assessment), y = log(mill.rate))) +
  geom_point(color = viridis(20)[3]) +
  facet_wrap(.~tax.class, labeller = tax.classes) +
  labs(x = "log assessment",
       y = "log mill rate",
       title = "Assessment vs mill rates yearly average by municipality, 2016-2020")
ggsave("RealEstate/src/eda - s550/plots/4. scatter average year.pdf")
ggsave("RealEstate/src/eda - s550/plots/4. scatter average year.png")


# facet line trends full
  re %>% 
    dplyr::select(-PIC) %>% 
    dplyr::filter(!is.na(total.assessment), !is.na(mill.rate)) %>% 
    dplyr::group_by(year, municipality, tax.class) %>% 
    dplyr::summarise(total.assessment = mean(total.assessment), mill.rate = mean(mill.rate)) %>% 
    ggplot(aes(x = year, y = log(mill.rate), group = municipality, color = municipality)) +
    geom_line() +
    facet_wrap(.~tax.class, labeller = tax.classes) +
    labs(x = "year",
         y = "log mill rate",
         title = "Mill rates over time for all municipalities") +
    scale_color_viridis_d() +
    theme(legend.position = "none")
  ggsave("RealEstate/src/eda - s550/plots/5. mill rate evolution full.pdf")
  ggsave("RealEstate/src/eda - s550/plots/5. mill rate evolution full.png")
  
# facet line trends sample of 10
  re %>% 
    dplyr::select(-PIC) %>% 
    dplyr::filter(!is.na(total.assessment), !is.na(mill.rate)) %>% 
    dplyr::group_by(year, tax.class) %>% 
    dplyr::sample_n(size = 10) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(year, tax.class, municipality) %>% 
    dplyr::summarise(total.assessment = mean(total.assessment), mill.rate = mean(mill.rate)) %>% 
    ggplot(aes(x = year, y = log(mill.rate), group = municipality)) +
    geom_line(color = viridis(20)[3]) +
    facet_wrap(.~tax.class, labeller = tax.classes) + 
    #scale_color_viridis_d() +
    #theme(legend.position = "none") +
    labs(x = "year",
         y = "log mill rate") +
    theme(text = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("RealEstate/src/eda - s550/plots/6. mill rate evolution sample.pdf")
  ggsave("RealEstate/src/eda - s550/plots/6. mill rate evolution sample.png")
  
  
  # facet line trends sample of 10 assessment
  re %>% 
    dplyr::select(-PIC) %>% 
    dplyr::filter(!is.na(total.assessment), !is.na(mill.rate)) %>% 
    dplyr::group_by(year, tax.class) %>% 
    dplyr::sample_n(size = 10) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(year, tax.class, municipality) %>% 
    dplyr::summarise(total.assessment = mean(total.assessment), mill.rate = mean(mill.rate)) %>% 
    ggplot(aes(x = year, y = log(total.assessment), group = municipality)) +
    geom_line(color = viridis(20)[3]) +
    facet_wrap(.~tax.class, labeller = tax.classes) + 
    #scale_color_viridis_d() +
    #theme(legend.position = "none") +
    labs(x = "year",
         y = "log assessment value") +
    theme(text = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("RealEstate/src/eda - s550/plots/6.1 assessment evolution sample.pdf")
  ggsave("RealEstate/src/eda - s550/plots/6.1 assessment evolution sample.png")
  
  
  # violin plots of mill rates accross tax classes, for 2020
  re %>%  
    dplyr::filter(!is.na(total.assessment), !is.na(mill.rate), year == 2020) %>% 
    dplyr::group_by(PIC, tax.class) %>% 
    dplyr::summarise(total.assessment = mean(total.assessment), mill.rate = mean(mill.rate)) %>% 
    ggplot(aes(x = tax.class, y = log(mill.rate), fill = tax.class)) +
    geom_violin(alpha = 0.5, width = 1) +
    geom_boxplot(alpha = 0.75, width = 0.1) +
    labs(x = "tax class",
         y = "log mill rate") +
    #scale_fill_viridis_d(begin=0, end=1) +
    scale_fill_manual(values = c("#3E4A89FF", "#26828EFF","#B4DE2CFF" )) +
    theme(legend.position = "none") +
    scale_x_discrete(labels = c("01 - Residential", "05 - Industrial", "06 - Commercial")) +
    theme(text = element_text(size = 18))
  ggsave("RealEstate/src/eda - s550/plots/7. violin mill rates.pdf")
  ggsave("RealEstate/src/eda - s550/plots/7. violin mill rates.png")
  
  
  # violin plots of assessment values accross tax classes, for 2020
  re %>%  
    dplyr::filter(!is.na(total.assessment), !is.na(mill.rate), year == 2020) %>% 
    dplyr::group_by(PIC, tax.class) %>% 
    dplyr::summarise(total.assessment = mean(total.assessment), mill.rate = mean(mill.rate)) %>% 
    ggplot(aes(x = tax.class, y = log(total.assessment), fill = tax.class)) +
    geom_violin(alpha = 0.5, width = 1) +
    geom_boxplot(alpha = 0.75, width = 0.1) +
    labs(x = "tax class",
         y = "log assessment value") +
    #scale_fill_viridis_d(begin=0, end=1) +
    scale_fill_manual(values = c("#3E4A89FF", "#26828EFF","#B4DE2CFF" )) +
    theme(legend.position = "none") +
    scale_x_discrete(labels = c("01 - Residential", "05 - Industrial", "06 - Commercial")) +
    theme(text = element_text(size = 18))
  ggsave("RealEstate/src/eda - s550/plots/7.1 violin assessment values.pdf")
  ggsave("RealEstate/src/eda - s550/plots/7.1 violin assessment values.png")
  
  
  # boxplots of mill rates accross tax classes, for 2020
  re %>%  
    dplyr::filter(!is.na(total.assessment), !is.na(mill.rate), year == 2020) %>% 
    dplyr::group_by(PIC, tax.class) %>% 
    dplyr::summarise(total.assessment = mean(total.assessment), mill.rate = mean(mill.rate)) %>% 
    ggplot(aes(x = tax.class, y = log(mill.rate))) +
    geom_boxplot() +
    labs(x = "tax class",
         y = "log mill rate",
         title = "Mill rates accross tax classes")
  ggsave("RealEstate/src/eda - s550/plots/8. boxplot mill rates.pdf")
  ggsave("RealEstate/src/eda - s550/plots/8. boxplot mill rates.png")
  
  

# correlation plot
  re %>% 
    dplyr::select(-PIC) %>% 
    dplyr::filter(!is.na(total.assessment), !is.na(mill.rate), year == 2020) %>% 
    dplyr::select(municipality, total.assessment) %>% 
    dplyr::group_by(municipality) %>% 
    dplyr::summarise(total.assessment = mean(total.assessment)) %>% 
    tidyr::spread(key = municipality, value = total.assessment) %>% 
    cor() %>% 
    corrplot::corrplot(method = "color",
                       type = "upper",
                       tl.col = "black",
                       tl.pos="n")
  
  
  re %>% 
    dplyr::filter(!is.na(total.assessment), !is.na(mill.rate), year == 2020, tax.class == "01") %>% 
    dplyr::select(PIC, total.assessment) %>% 
    dplyr::group_by(PIC) %>% 
    dplyr::summarise(total.assessment = mean(total.assessment)) %>% 
    tidyr::spread(key = PIC, value = total.assessment) %>% 
    cor() %>% 
    corrplot::corrplot(method = "color",
                       type = "upper",
                       tl.col = "black",
                       tl.pos="n")
  
  
  re %>% 
    dplyr::filter(!is.na(total.assessment), !is.na(mill.rate), year == 2020, tax.class == "01") %>% 
    dplyr::select(municipality, total.assessment) %>% 
    GGally::ggpairs(cardinality_threshold = 162)
  
  re %>% 
    dplyr::filter(!is.na(total.assessment), !is.na(mill.rate), year == 2020, tax.class == "01") %>% 
    dplyr::select(total.assessment) %>% 
    acf(plot = FALSE) %>% 
    with(data.frame(lag, acf)) %>% 
    ggplot(aes(lag, acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(aes(yintercept = 1.96/sqrt(nrow(re))),
               linetype = 2, color = 'darkblue') + 
    geom_hline(aes(yintercept = -1.96/sqrt(nrow(re))),
               linetype = 2, color = 'darkblue') +
    labs(x = "Lag",
         y = "Autocorrelation function") +
    theme(text = element_text(size = 18))
  
  re %>% 
    dplyr::filter(!is.na(total.assessment), !is.na(mill.rate), year == 2020, tax.class == "01") %>% 
    dplyr::select(municipality, total.assessment) %>% 
    dplyr::group_by(municipality) %>% 
    dplyr::summarise(total.assessment = mean(total.assessment)) %>%  
    dplyr::select(total.assessment) %>% 
    acf(plot = FALSE) %>% 
    with(data.frame(lag, acf)) %>% 
    ggplot(aes(lag, acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(aes(yintercept = 1.96/sqrt(nrow(re))),
               linetype = 2, color = 'darkblue') + 
    geom_hline(aes(yintercept = -1.96/sqrt(nrow(re))),
               linetype = 2, color = 'darkblue') +
    labs(x = "Lag",
         y = "Autocorrelation function") +
    theme(text = element_text(size = 18))
  
  
  
  
corr <- data.frame(assessment = corr.values$total.assessment)
rownames(corr) <- corr.values$municipality
cor(corr)
  ggsave("RealEstate/src/eda - s550/plots/9. correlation matrix.pdf")
  ggsave("RealEstate/src/eda - s550/plots/9. correlation matrix.png")

# export data set ####
set.seed(2303)
train <- re %>% 
  dplyr::select(PIC) %>% 
  dplyr::distinct() %>% 
  dplyr::sample_frac(0.75) %>% 
  dplyr::pull()

re.out <- re %>% 
  dplyr::mutate(test.train = ifelse(PIC %in% train, "train", "test"))

readr::write_delim(re.out, "test_train_data.txt", delim = ",")
# read for test
dataset <- readr::read_delim("test_train_data.txt", delim = ",",
                             col_types = "ciccdddddc")

## S450 data ####
re.450 <- read_csv("RealEstate/data/assessment_aggregate.csv")

re.450 %>% 
  ggplot(aes(x = log(assessTotal), y = rate, color = factor(Year))) +
  geom_point() +
  facet_wrap(.~TaxClassCode) +
  labs(x = "log assessment",
       y = "mill rate",
       color = "year") +
  scale_color_viridis_d()
