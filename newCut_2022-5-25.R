#**********************************
# Project on tax dashboard
# Patrick Schumacher
# Rockefeller Institute of Government
# Created on 2022.4.2
#**********************************

#*********************************************
# Set working directory----
#*********************************************

rm(list = ls()) #clear environment

setwd("/Users/patricksaccount/Desktop/RIG Work/Stat Yearbook Tax Data")
# setwd("/Users/PatrickSchumacher/Desktop/RIG Work/Stat Yearbook Tax Data")


#*********************************************
# Set environment----
#*********************************************
pacman::p_load(tidyverse,
               tidycensus,
               tmap,
               sf,
               readxl,
               janitor,
               lubridate,
               readr,
               scales,
               RColorBrewer,
               RSocrata,
               scales)

# Inverse of %in% operator
`%ni%` <- Negate(`%in%`)

# Function for ordering factors 
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

# Remove scientific notation
options(scipen=999)


#*********************************************
# Import tax datasets----
#*********************************************
url = "https://data.ny.gov/resource/5bb2-yb85.json"
dat1 <- read.socrata(url) 


url = "https://data.ny.gov/resource/73iw-kuxv.json"
alpha1 <- read.socrata(url) 

url = "https://data.ny.gov/resource/ny73-2j3u.json"
sales <- read.socrata(url) 

tax99to19 <- readRDS("./data/tax99to19.rds")
tax_states <- readRDS("./data/working/tax_states.rds")


#*********************************************
# tax99-14 (1999-2014)----
#*********************************************

tax99to14 <- dat1 %>%
  filter(state== "New York", income_class== "Total",
         county %ni% c("Grand Total, Full-Year Resident", "Residence Unknown ++",
                       "NYS Unclassified +", "All Places")) %>%
  mutate_at(vars(number_of_all_returns:tax_liability_of_all_returns_in_thousands), as.numeric) %>%
  mutate(county= str_trim(county, "left"),
         county= str_replace(county, "New York City - ", ""),
         county= ifelse(county== "Manhattan", "New York", county),
         county= ifelse(county== "Hamilton" &  ny_agi_of_all_returns_in_thousands== 802099, "Greene", county),
         agi_of_all_returns=  ny_agi_of_all_returns_in_thousands*1000,
         tax_liability_of_all_returns= tax_liability_of_all_returns_in_thousands* 1000) %>%
  select(tax_year, county, 
         agi_of_all_returns,
         tax_liability_of_all_returns,
         number_of_all_returns) 

tax15to19 <- alpha1 %>%
  filter(state== "New York", tax_liability_status== "All Returns",
         county %ni% c("All New York City", "Other Places- NYS Resident",
                       "All Counties Outside of New York City", "All Places")) %>%
  mutate_at(vars(number_of_returns:number_filing_married_separate_or_widow_widower), as.numeric) %>%
  mutate(county= ifelse(county== "Manhattan", "New York", county),
         county= str_trim(county, "left")) %>%
  select(tax_year, county, 
         agi_of_all_returns= new_york_state_amount_of_ny_adjusted_gross_income,
         tax_liability_of_all_returns= tax_liability,
         number_of_all_returns= number_of_returns)

tax99to19 <- rbind(tax99to14, tax15to19)


#*********************************************
# tax_states (1999-2019)----
#*********************************************

state1 <- dat1 %>%
  filter(income_class == "Total",
         county == "Grand Total, Full-Year Resident" | county == "All", 
         state %in% c(state.name)) %>%
  mutate_at(vars(number_of_all_returns:tax_liability_of_all_returns_in_thousands), as.numeric) %>%
  mutate(agi_of_all_returns=  ny_agi_of_all_returns_in_thousands*1000,
         tax_liability_of_all_returns= tax_liability_of_all_returns_in_thousands* 1000) %>%
  select(tax_year, 
         state, 
         agi_of_all_returns,
         tax_liability_of_all_returns,
         number_of_all_returns) 

state2 <- alpha1 %>%
  filter(tax_liability_status== "All Returns", 
         place_of_residence %in% c(state.name, "All Places"), 
         county %in% c("All", "All Places"),
         state %in% state.name) %>%
  mutate_at(vars(number_of_returns:number_filing_married_separate_or_widow_widower), as.numeric) %>%
  select(tax_year, 
         state, 
         agi_of_all_returns= federal_amount_of_ny_adjusted_gross_income,
         tax_liability_of_all_returns= tax_liability,
         number_of_all_returns= number_of_returns)


tax_states <- rbind(state1, state2)

#*********************************************
# Figure 1 (downstate v. upstate)----
#*********************************************

downstate <- c("New York","Bronx","Kings","Queens","Richmond","Rockland","Nassau","Suffolk","Orange","Putnam","Dutchess","Westchester")

downVup <- tax99to19 %>%
  mutate(region= ifelse(county %in% downstate, "Downstate", "Upstate")) %>%
  group_by(tax_year, region) %>%
  summarise_at(.vars = vars(agi_of_all_returns:number_of_all_returns),
               .funs = c(sum="sum")) %>%
  mutate(`AGI per returnee`= agi_of_all_returns_sum/number_of_all_returns_sum,
         `Tax liability per returnee`= tax_liability_of_all_returns_sum/number_of_all_returns_sum) %>%
  select(1,2,6,7) %>%
  pivot_longer(cols= `AGI per returnee`:`Tax liability per returnee`, values_to = "value", names_to= "variable")

#*********************************************
# Figure 2 (how progressive is NYS?)----
#*********************************************

plotRate <- dat1 %>%
  mutate_at(vars(tax_year, income_class_sort_order:tax_liability_of_all_returns_in_thousands), as.numeric) %>%
  filter(county == "Grand Total, Full-Year Resident" | county == "All", 
         state == "New York",
         # tax_year== 2014,
         tax_year %in% c(2007:2014),
         income_class != "Total") %>%
  mutate(income_group= fct_case_when(income_class %in% c("Under 5,000", "5,000 - 9,999", "10,000 - 19,999", "20,000 - 29,999") ~ "Less than $30,000",
                                     income_class %in% c("30,000 - 39,999", "40,000 - 49,999","50,000 - 59,999") ~ "$30,000- $59,999",
                                     income_class %in% c("60,000 - 74,999", "75,000 - 99,999") ~ "$60,000- $99,999",
                                     income_class %in% c("100,000 - 199,999") ~ "100,000- 199,999",
                                     income_class %in% c("200,000 - 249,999", "250,000 - 499,999")~ "$200,000- $499,999",
                                     income_class %in% c("500,000 and over") ~ "$500,000 and over"), .after= income_class) %>%
  mutate(agi_of_all_returns=  ny_agi_of_all_returns_in_thousands*1000,
         tax_liability_of_all_returns= tax_liability_of_all_returns_in_thousands* 1000) %>%
  group_by(tax_year, income_group) %>%
  summarize(sumAGI= sum(agi_of_all_returns, na.rm= T),
            sumLiability= sum(tax_liability_of_all_returns, na.rm = T)) %>%
  mutate(taxRate= sumLiability/sumAGI) %>%
  select(tax_year, income_group, taxRate) %>%
  pivot_longer(cols= taxRate, values_to = "value", names_to = "variable")

#*********************************************
# Figure 3 and 4 (state level analysis)----
#*********************************************

propStates <- tax_states %>%
  mutate(category= ifelse(state== "New York", "New York", "Other States")) %>%
  group_by(tax_year, category)  %>%
  summarize(sumliability= sum(tax_liability_of_all_returns, na.rm = T)) %>%
  transmute(category, percLiability= sumliability/sum(sumliability)) %>%
  pivot_longer(cols= percLiability, values_to = "value", names_to= "variable") 

propStates2 <- tax_states %>%
  group_by(tax_year) %>%
  transmute(state, percLiability= tax_liability_of_all_returns/sum(tax_liability_of_all_returns),
            percAGI= agi_of_all_returns/sum(agi_of_all_returns)) %>%
  pivot_longer(cols= percLiability:percAGI, values_to = "value", names_to= "variable") %>%
  filter(variable== "percLiability")

#*********************************************
# Figure 5 and 6 (state level analysis)----
#*********************************************

salesRev <- sales %>%
  mutate(jurisdiction= str_to_title(jurisdiction),
         jurisdiction= str_replace(jurisdiction, "St ", "St. "),
         sales_tax_year= str_replace(sales_tax_year," .*", ""),
         taxable_sales_and_purchases= as.numeric(taxable_sales_and_purchases)) %>%
  rename(county= jurisdiction) %>%
  group_by(sales_tax_year, county, naics_industry_group, description) %>%
  summarize(Revenue= sum(taxable_sales_and_purchases, na.rm= T)) %>%
  group_by(sales_tax_year, county) %>%
  mutate(`Percent of total revenue` = Revenue/sum(Revenue)*100) %>%
  filter(naics_industry_group %in% c("4541", "4522")) %>%
  pivot_longer(cols= Revenue:`Percent of total revenue`, values_to = "value", names_to= "variable") 


#*********************************************
# Save datasets into .rda object (1999-2019)----
#*********************************************

save(downVup,plotRate,propStates, propStates2, file= "./data/working/githubUpload.rda")

getwd()
