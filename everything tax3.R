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
setwd("/Users/PatrickSchumacher/Desktop/RIG Work/Stat Yearbook Tax Data")


#*********************************************
# Set environment----
#*********************************************
pacman::p_load(tidyverse,
               tidycensus,
               tigris,
               tmap,
               sf,
               readxl,
               janitor,
               lubridate,
               readr,
               scales,
               RColorBrewer,
               RSocrata,
               mapdeck,
               scales,
               gifski)

# Inverse of %in% operator
`%ni%` <- Negate(`%in%`)

# Function for ordering factors 
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

options(scipen=999)


#*********************************************
# Import tax datasets----
#*********************************************
url = "https://data.ny.gov/resource/5bb2-yb85.json"
dat1 <- read.socrata(url) 


url = "https://data.ny.gov/resource/73iw-kuxv.json"
alpha1 <- read.socrata(url) 

# saveRDS(dat1, "./data/dat1.rds")
# saveRDS(alpha1, "./data/alpha1.rds")
readRDS("./data/dat1.rds")
readRDS("./data/alpha1.rds")

#*********************************************
# Process datasets 1 (1999-2014)----
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


#*********************************************
# Process datasets 2 (2015-2019)----
#*********************************************

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

#*********************************************
# Combine datasets (1999-2019)----
#*********************************************

tax99to19 <- rbind(tax99to14, tax15to19)

# saveRDS(tax99to19, "./data/tax99to19.rds")
tax99to19 <- readRDS("./data/tax99to19.rds")

#count to make sure the same number of counties are in each year
test <- tax99to19 %>%
  group_by(tax_year) %>%
  summarise(freq=n()) 

# Pivot longer for RShiny app

pivTax99to19 <- tax99to19 %>%
  pivot_longer(cols= ny_agi_of_all_returns:number_of_all_returns, values_to = "value", names_to = "variable") %>%
  mutate(county= str_trim(county, "left"))

saveRDS(pivTax99to19, "./data/pivTax99to19.rds")


test <- pivTax99to19 %>%
  group_by(county, tax_year) %>%
  summarise(freq=n()) 

#*********************************************
# Downstate versus upstate counties (1999-2019)----
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

saveRDS(downVup, "./data/working/downVup.rds")

ggplot(data= filter(downVup, variable== "perReturneeLiability"), aes(x= tax_year, y= perc, color= region, group= region)) +
  geom_line(stat="identity") + theme_bw() + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(title= "Tax liability per returnee, 1999 to 2019",
       y= "Tax liability per returnee", x= "Tax year", color= "Region")+
  scale_y_continuous(labels = comma)+
  scale_color_brewer(palette = "Dark2") 

ggplot(data= filter(downVup, variable== "perReturneeAGI"), aes(x= tax_year, y= perc, color= region, group= region)) +
  geom_line(stat="identity") + theme_bw() + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(title= "Adjusted gross income per returnee, 1999 to 2019",
       y= "Adjusted gross income", x= "Tax year", color= "Region")+
  scale_y_continuous(label = scales::comma)+ scale_color_brewer(palette = "Dark2") 


#*********************************************
# What income group pays the largest share (1999-2019)----
#*********************************************

#tax liability as a share of adjusted gross income 

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
                                     income_class %in% c("200,000 - 249,999", "250,000 - 499,999")~ "200,000- 499,999",
                                     income_class %in% c("500,000 and over") ~ "500,000 and over"), .after= income_class) %>%
  mutate(agi_of_all_returns=  ny_agi_of_all_returns_in_thousands*1000,
         tax_liability_of_all_returns= tax_liability_of_all_returns_in_thousands* 1000) %>%
  group_by(tax_year, income_group) %>%
  summarize(sumAGI= sum(agi_of_all_returns, na.rm= T),
            sumLiability= sum(tax_liability_of_all_returns, na.rm = T)) %>%
  mutate(taxRate= sumLiability/sumAGI) %>%
  select(tax_year, income_group, taxRate) %>%
  pivot_longer(cols= taxRate, values_to = "value", names_to = "variable")

saveRDS(plotRate, "./data/working/plotRate.rds")

ggplot(data= plotRate, aes(x= income_group, y= value))+
  geom_bar(stat= "identity", fill= "lightblue")+ theme_bw()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(title= str_wrap("The New York Tax system is progressive", 60),
       subtitle= 'Tax liability as a share of NYS adjusted gross income, 2014',
       y= "Tax liability as a share of AGI", x= "Income group")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  geom_hline(yintercept = mean(plotRate$perc),linetype="dashed", 
             color = "red", size=0.3) +
  annotate("text", x = "Less than $30,000", y = 0.05,label = c("The average share \n for all groups: 4.0%"), size= 3)


p <- ggplot(mtcars, aes(x = disp, y= am, text = paste("Date:", disp, "\nnewlabel:", am), color = as.factor(cyl), 
                        gear=gear, hp=hp))+geom_point()
ggplotly(p,tooltip = "text")


#*********************************************
# Adjusted gross income (1999-2019)----
#*********************************************

#summary

test <- tax99to19 %>%
  filter(tax_year== 1999)
summary(test$ny_agi_of_all_returns)
sum(test$ny_agi_of_all_returns)

test <- tax99to19 %>%
  filter(tax_year== 2019)
summary(test$ny_agi_of_all_returns)
sum(test$ny_agi_of_all_returns)

#How has adjusted gross income for counties changed over time?

#largest counties
max99 <- tax99to19 %>%
  filter(tax_year== 2019) %>%
  slice_max(ny_agi_of_all_returns, n= 5)

# png("./figures/plot1.1.png", units="in", width=8, height=6, res=300)
ggplot(data= filter(tax99to19, county %in% max99$county), aes(x= tax_year, y= ny_agi_of_all_returns, color= county, group= county)) +
  geom_line(stat="identity") + theme_bw() + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(title= "Counties with highest adjusted gross income, 1999 to 2019",
       y= "Adjusted gross income (billions)", x= "Tax year", color= "County")+
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9))+
  scale_color_brewer(palette = "Dark2") +
  annotate("text", x = "2010", y = 150000000000,label = c("Great recession"), size= 3)
# dev.off()

#smallest counties 
min99 <- tax99to19 %>%
  filter(tax_year== 2019) %>%
  slice_min(ny_agi_of_all_returns, n= 5)

ggplot(data= filter(tax99to19, county %in% min99$county), aes(x= tax_year, y= ny_agi_of_all_returns, color= county, group= county)) +
  geom_line(stat="identity") + theme_bw() + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(title= "Counties with highest adjusted gross income, 1999 to 2019",
       y= "Adjusted gross income (millions)", x= "Tax year", color= "County")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6, accuracy = NULL))+
  scale_color_brewer(palette = "Set1")

#*********************************************
# Tax liability (1999-2019)----
#*********************************************
names(tax99to19)
#summary
summary(tax99to19$tax_liability_of_all_returns)

test <- tax99to19 %>%
  filter(tax_year== 1999)
summary(test$tax_liability_of_all_returns)
sum(test$tax_liability_of_all_returns)


#How has adjusted gross income for counties changed over time?

#largest counties
max99 <- tax99to19 %>%
  filter(tax_year== 2019) %>%
  slice_max(tax_liability_of_all_returns, n= 5)

ggplot(data= filter(tax99to19, county %in% max99$county), aes(x= tax_year, y= tax_liability_of_all_returns, color= county, group= county)) +
  geom_line(stat="identity") + theme_bw() + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(title= "Counties with highest tax liability, 1999 to 2019",
       y= "Tax liability (billions)", x= "Tax year", color= "County")+
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9))+
  scale_color_brewer(palette = "Dark2") 

#smallest counties 
min99 <- tax99to19 %>%
  filter(tax_year== 2019) %>%
  slice_min(tax_liability_of_all_returns, n= 5)

ggplot(data= filter(tax99to19, county %in% min99$county), aes(x= tax_year, y= tax_liability_of_all_returns, color= county, group= county)) +
  geom_line(stat="identity") + theme_bw() + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(title= "Counties with the lowest tax liability, 1999 to 2019",
       y= "Tax liability (millions)", x= "Tax year", color= "County")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6, accuracy = NULL))+
  scale_color_brewer(palette = "Set1")

#*********************************************
# Number of returns (1999-2019)----
#*********************************************
names(tax99to19)
#summary
summary(tax99to19$number_of_all_returns)

test <- tax99to19 %>%
  filter(tax_year== 1999)
summary(test$number_of_all_returns)
sum(test$number_of_all_returns)

#How has adjusted gross income for counties changed over time?

#largest counties
max99 <- tax99to19 %>%
  filter(tax_year== 2019) %>%
  slice_max(number_of_all_returns, n= 5)

ggplot(data= filter(tax99to19, county %in% max99$county), aes(x= tax_year, y= number_of_all_returns, color= county, group= county)) +
  geom_line(stat="identity") + theme_bw() + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(title= "Counties with the highest number of returns, 1999 to 2019",
       y= "Adjusted gross income (thousands)", x= "Tax year", color= "County")+
  scale_y_continuous(labels = unit_format(unit = "T", scale = 1e-3))+
  scale_color_brewer(palette = "Dark2") 

#smallest counties 
min99 <- tax99to19 %>%
  filter(tax_year== 2019) %>%
  slice_min(number_of_all_returns, n= 5)

ggplot(data= filter(tax99to19, county %in% min99$county), aes(x= tax_year, y= number_of_all_returns, color= county, group= county)) +
  geom_line(stat="identity") + theme_bw() + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(title= "Counties with the lowest number of returns, 1999 to 2019",
       y= "Number of returns (thousands)", x= "Tax year", color= "County")+
  scale_y_continuous(labels = unit_format(unit = "T", scale = 1e-3, accuracy = NULL))+
  scale_color_brewer(palette = "Set1")

#*********************************************
# Barplot of adjusted gross income (2019)----
#*********************************************

# , fig.height=10, fig.width=10

ggplot(data= filter(tax99to19, tax_year== 2019), aes(x= reorder(county, ny_agi_of_all_returns), y= ny_agi_of_all_returns)) + geom_bar(stat="identity", fill= "blue") + coord_flip()+ theme(text = element_text(size=17))+
  labs(title= str_wrap("Adjusted gross income by county in New York State, tax year 2019", 85), y= "Adjusted gross income (in millions)", fill = "Place of death") +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9, accuracy = NULL))

# dataset with boundary file for New York State
boundary <- counties() %>%
  select(GEOID, county= NAME, geometry) %>%
  filter(str_detect(GEOID, "^36"))

tax19 <- filter(tax99to19, tax_year== 2019)

heatmap <- left_join(boundary, tax19) 







#*********************************************
# Orginal State-level analysis----
#*********************************************

state1 <- dat1 %>%
  filter(income_class == "Total", state != "New York", state %in% state.name) %>%
  mutate_at(vars(number_of_all_returns:tax_liability_of_all_returns_in_thousands), as.numeric) %>%
  mutate(agi_of_all_returns=  ny_agi_of_all_returns_in_thousands*1000,
         tax_liability_of_all_returns= tax_liability_of_all_returns_in_thousands* 1000,
         new= ifelse(state %ni% c("Connecticut","New Jersey","Pennsylvania", "Florida", "Massachusetts"), "Other states", state), .before= state) %>%
  select(tax_year, new, state, 
         agi_of_all_returns,
         tax_liability_of_all_returns,
         number_of_all_returns) 

state2 <- alpha1 %>%
  filter(tax_liability_status== "All Returns", state %in% state.name, state != "New York") %>%
  mutate_at(vars(number_of_returns:number_filing_married_separate_or_widow_widower), as.numeric) %>%
  mutate(new= ifelse(state %ni% c("Connecticut","New Jersey","Pennsylvania", "Florida", "Massachusetts"), "Other states", state), .before= state) %>%
  select(tax_year, new, state, 
         agi_of_all_returns= federal_amount_of_ny_adjusted_gross_income,
         tax_liability_of_all_returns= tax_liability,
         number_of_all_returns= number_of_returns)


tax_states <- rbind(state1, state2)

saveRDS(tax_states, "./data/tax_states.rds")

log


# onestate is missing, but I don't know which
####
#Graph it (top ten states for AGI income). Consider looking at the nice website for
#graphing states. Or what about doing the one with the bins. 

max99 <- tax99to19_2 %>%
  filter(tax_year== 1999) %>%
  slice_max(ny_agi_of_all_returns_in_thousands, n= 10)

ggplot(data= max99, aes(x= state, y= ny_agi_of_all_returns_in_thousands)) +
  geom_bar(stat="identity", fill= "blue") +
  coord_flip()

#Top ten states for tax liabilty 

max99 <- tax99to19_2 %>%
  filter(tax_year== 1999) %>%
  slice_max(tax_liability_of_all_returns_in_thousands, n= 10)

ggplot(data= max99, aes(x= state, y= tax_liability_of_all_returns)) +
  geom_bar(stat="identity", fill= "blue") +
  coord_flip()


#Top ten states for number of returns 

max99 <- tax99to19_2 %>%
  filter(tax_year== 1999) %>%
  slice_max(number_of_all_returns, n= 10)

ggplot(data= max99, aes(x= state, y= number_of_all_returns)) +
  geom_bar(stat="identity", fill= "blue") +
  coord_flip()

##############
#Line graph

lineGraph <- tax_states %>%
  filter(state %in% c("Connecticut","New Jersey","New York", "Pennsylvania", "Florida", "Massachusetts", "Washington"))

lineGraph <- tax_states %>%
  filter(state %in% c("Washington"))

lineGraph <- tax_states %>%
  group_by(tax_year, new) %>%
  summarize(sum= sum(tax_liability_of_all_returns)) %>%
  rename(State= new)
saveRDS(lineGraph, "./data/working/lineGraph.rds")

ggplot(data= lineGraph, aes(x= tax_year, y= ny_agi_of_all_returns, group= state)) +
  geom_line(stat="identity")

test2 <- tax_states %>%
  filter(tax_year== 2019) %>%
  adorn_totals(where = "row")

# 106,959,178,923
# 124,819,253,000

7,672,902,374
test1 <- tax_states %>%
  filter(tax_year== 1999) %>%
  adorn_totals(where = "row")

summary(test$ny_agi_of_all_returns)
sum(test$ny_agi_of_all_returns)

test <- tax_states %>%
  filter(tax_year== 2019)
summary(test$tax_liability_of_all_returns)
sum(test$tax_liability_of_all_returns)



test <- tax_states %>%
  filter(tax_year== 2019)
summary(test$number_of_all_returns)
sum(test$number_of_all_returns)

#*********************************************
# New State-level analysis----
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

saveRDS(tax_states, "./data/working/tax_states.rds")

propStates <- tax_states %>%
  # filter(tax_year== "2019") %>%
  group_by(tax_year) %>%
  transmute(state, percLiability= tax_liability_of_all_returns/sum(tax_liability_of_all_returns),
            percAGI= agi_of_all_returns/sum(agi_of_all_returns)) %>%
  pivot_longer(cols= percLiability:percAGI, values_to = "perc", names_to= "variable") 

#Wait, it doesn't make sense to look at Adjusted gross income
#could you allow the user to select total outside states, there's even a variable for that, like all excluding new york 
#convert to factor, and have that at the top of the drop down menue 

ggplot(data= filter(propStates, state %in% c("New Jersey", "Connecticut", "California", "Pennsylvania", "Florida", "Texas"),
                    variable== "percLiability"), aes(x= tax_year, y= perc, color= state, group= state)) +
  geom_line(stat="identity") + theme_bw()+ geom_point(pch= 2)+
  theme(axis.title.x=element_blank(), axis.text.x=element_text(angle = -45, hjust = 0))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(title= str_wrap("New Jerseyeans contribute about 8 percent of the total income tax collected by New York State, 1999-2019", 60), 
       y= "Percentage of tax liability", color= "State")

propStates2 <- tax_states %>%
  mutate(category= ifelse(state== "New York", "New York", "Other States")) %>%
  group_by(tax_year, category)  %>%
  summarize(sumliability= sum(tax_liability_of_all_returns, na.rm = T)) %>%
  transmute(category, percLiability= sumliability/sum(sumliability)) %>%
  pivot_longer(cols= percLiability, values_to = "perc", names_to= "variable") 

# ggplot(data= propStates2, aes(x= tax_year, y= perc, color= category, group= category)) +
#   geom_line(stat="identity") + theme_bw()+
#   scale_color_brewer(palette = "Dark2") 

ggplot(data= propStates2, aes(x= tax_year, y= perc, fill= category)) +
  geom_bar(stat="identity") + theme_bw() + coord_flip()+
  scale_fill_brewer(palette = "Set2")  +
  labs(title= str_wrap("Residents of other states pay, on average, 14 percent of the total income tax collected by New York State, 1999-2019", 60),
       y= "Percentage of total tax liability", fill= "Place")+
  theme(axis.title.y=element_blank()) + scale_y_continuous(labels = scales::percent_format(accuracy = 1))

test <- propStates2 %>%
  filter(category== "Other States") 

mean(test$perc)


?unit_format
#Other states, pay on average, X percent of the tax liability in New York State


#*********************************************
# Heat map----
#*********************************************

pacman::p_load(tigris, tidyverse)

# dataset with boundary file for New York State
ny_boundary <- counties() %>%
  select(GEOID, county= NAME, geometry) %>%
  filter(str_detect(GEOID, "^36"))

# dataset with boundary file for United States
state_boundary <- states() 

# Save boundary datasets to file
saveRDS(ny_boundary, "./data/ny_boundary.rds")
saveRDS(state_boundary, "./data/state_boundary.rds")

tax99to19 <- readRDS("./data/tax99to19.rds")
ny_boundary <- readRDS("./data/ny_boundary.rds")
state_boundary <- readRDS("./data/state_boundary.rds")

# join new york dataset with boundary file
df <-  tax99to19 %>%
  filter(tax_year== 2019) %>%
  mutate(ny_agi_of_all_returns_in_millions= ny_agi_of_all_returns*.0000001)

frame <- left_join(ny_boundary, df) 

# tmap takes forever to load and does not have the option to rescale the axis
# tm_shape(frame)+tm_fill(col = "ny_agi_of_all_returns_in_millions")+ tm_borders()
names(frame)

frame2= frame["ny_agi_of_all_returns"]

n=5 # number of breaks
my.palette = heat.colors(n)

png("./figures/log_county_plot.png", units="in", width=8, height=6, res=300)
plot_log <- plot(frame2, key.pos=4, logz=T, nbreaks=n, breaks='jenks',
     pal = rev(my.palette), main= "NY AGI of all returns (log scale)")
dev.off()

png("./figures/county_plot.png", units="in", width=8, height=6, res=300)
plot_no_log <- plot(frame2, key.pos=4, nbreaks=n, breaks='jenks',
                 pal = rev(my.palette), main= "NY AGI of all returns")
dev.off()


names(frame)
names(frame)


n=5 # number of breaks
my.palette = heat.colors(n)
plot(frame2, key.pos=4, logz=T, nbreaks=n, breaks='jenks',
     pal = rev(my.palette))

plot(frame2, logz=T)



#*********************************************
# sales data----
#*********************************************

url = "https://data.ny.gov/resource/ny73-2j3u.json"
sales <- read.socrata(url) 

test <- sales %>%
  mutate(taxable_sales_and_purchases= as.numeric(taxable_sales_and_purchases)) %>%
  group_by(jurisdiction, sales_tax_year, naics_industry_group, description) %>%
  summarize(total_rev= sum(taxable_sales_and_purchases, na.rm= T)) %>%
  filter(jurisdiction== "ALBANY", sales_tax_year== "2020 - 2021") %>%
  filter(naics_industry_group== "4411")


saveRDS(sales, "./data/sales.rds")
sales <- readRDS("./data/sales.rds")
ncais <- read.csv("./data/working/ncais.csv")
ny_boundary <- readRDS("./data/ny_boundary.rds")

rm(list=setdiff(ls(), c("sales", "%ni%","ncais", "ny_boundary")))

descriptions <- as.data.frame(table(sales$description))
industry <- as.data.frame(table(sales$naics_industry_group))
juristictions <- as.data.frame(table(sales$jurisdiction))

alpha <- sales %>%
  mutate(sector= as.numeric(substr(naics_industry_group, 1, 2)),
         taxable_sales_and_purchases= as.numeric(taxable_sales_and_purchases)) %>%
  merge(ncais, by= "sector", all.x= T) %>%
  group_by(sales_tax_year, jurisdiction, sector_description) %>%
  summarize(total_rev= sum(taxable_sales_and_purchases, na.rm= T)) %>%
  filter(jurisdiction== "NY STATE", sales_tax_year== "2020 - 2021") 

beta <- alpha %>%
  # adorn_totals(where = "row", fill = NA) %>%
  adorn_percentages(denominator = "col", na.rm= T) %>%
  slice_max(total_rev,n= 10)

# Okay, so clean this up and it is one figure
ggplot(beta, aes(x=sector_description, y= total_rev))+
  geom_bar(stat= "identity")+ coord_flip()

# Maybe another figure with how top three variables have changed over time for New York State

# Map with top industry in each county (that sounds hard as fuck)

#sales heatmap----

d1 <- sales %>%
  mutate(sector= as.numeric(substr(naics_industry_group, 1, 2)),
         jurisdiction= str_to_title(jurisdiction),
         jurisdiction= str_replace(jurisdiction, "St ", "St. "),
         taxable_sales_and_purchases= as.numeric(taxable_sales_and_purchases)) %>%
  rename(county= jurisdiction) %>%
  group_by(sales_tax_year, county, naics_industry_group, description) %>%
  summarize(total_rev= sum(taxable_sales_and_purchases, na.rm= T))

d2 <- alpha %>% 
  group_by(sales_tax_year, county) %>% 
  top_n(1, total_rev) 

ny_boundary <- readRDS("./data/ny_boundary.rds") %>%
  filter(county %ni% c("Richmond", "Queens", "Kings", "New York", "Bronx"))
table(ny_boundary$county)
table(d2$county)


# I guess I could create do a mutate if else deal to get mCT to equal those new York counties

frame <- left_join(ny_boundary,d2) %>%
  rename(Industry= description)
saveRDS(frame, "./data/working/frame.rds")

sales_anim = tm_shape(frame) + tm_fill(col = "Industry") +
  tm_borders()+ tm_layout(legend.height= 15, legend.width = 11, title = "Top industry in each county")+
  tm_facets(along = "sales_tax_year", free.coords = FALSE)

tmap_animation(sales_anim, filename = "./figures/sales_anim.gif", delay = 120)

table(frame$county)

tmap_mode("plot")
tmap_mode("view")


test <- sales %>%
  filter(jurisdiction== "NY STATE", sales_tax_year== "2020 - 2021") %>%
  mutate(taxable_sales_and_purchases= as.numeric(taxable_sales_and_purchases)) %>%
  group_by(sales_tax_year, jurisdiction, naics_industry_group, description) %>%
  summarize(total_rev= sum(taxable_sales_and_purchases, na.rm= T)) %>%
  # filter(jurisdiction== "NY STATE", sales_tax_year== "2020 - 2021") %>%
  ungroup %>%
  slice_max(order_by = total_rev, n= 10)

#*********************************************
# summary data----
#*********************************************

tax99to19 %>%
  group_by(tax_year) %>%
  slice_max(number_of_all_returns, n= 5)



# Old state stuff----

# Inverse of %in% operator
`%ni%` <- Negate(`%in%`)

#clean data for graph
state1 <- dat1 %>%
  filter(income_class != "Total", state %in% state.name) %>%
  mutate(new= ifelse(state %ni% c("Connecticut","New Jersey","New York", "Pennsylvania"), "Other states", state), .before= state) %>%
  mutate_at(vars(place_of_residence_sort_order:tax_liability_of_all_returns_in_thousands), as.numeric) %>%
  group_by(tax_year, new)  %>% 
  summarise_at(.vars = vars(number_of_all_returns:tax_liability_of_all_returns_in_thousands),
               .funs = sum, na.rm= T) %>%
  select(tax_year, new, number_of_all_returns, ny_agi_of_all_returns_in_thousands,tax_liability_of_all_returns_in_thousands) 

state2 <- alpha1 %>%
  filter(tax_liability_status== "All Returns", state %in% state.name) %>%
  mutate(new= ifelse(state %ni% c("Connecticut","New Jersey","New York", "Pennsylvania"), "Other states", state), .before= state) %>%
  mutate_at(vars(number_of_returns:number_filing_married_separate_or_widow_widower), as.numeric) %>%
  group_by(tax_year, new)  %>% 
  mutate(ny_agi_of_all_returns_in_thousands= new_york_state_amount_of_ny_adjusted_gross_income/1000,
         tax_liability_of_all_returns= as.numeric(tax_liability) /1000) %>%
  dplyr::summarise_at(.vars = vars(number_of_returns:tax_liability_of_all_returns),
                      .funs = sum, na.rm= T) %>%
  select(tax_year, new, number_of_returns, new_york_state_amount_of_ny_adjusted_gross_income,tax_liability) %>%
  setNames(c("tax_year", "new", "number_of_all_returns", "ny_agi_of_all_returns_in_thousands","tax_liability_of_all_returns_in_thousands"))

names(dat2)
names(alpha2)

tax99to19_2 <- rbind(state1, state2)
names(tax99to19)

max99 <- tax99to19_2 %>%
  filter(tax_year== 1999, new != "New York") %>%
  slice_max(number_of_all_returns, n= 5)

ggplot(data= tax99to19_2, aes(x= tax_year, y= number_of_all_returns, fill= new)) +
  geom_bar(stat="identity")

ggplot(data= )


#what about a bargraph and fill by state. This doesn't work because new York is 
#obviously too large a share
ggplot(max99, aes(x= state, y= type, fill= name))+
  geom_bar(stat="identity", position=position_dodge())+ coord_flip()
#what about calculating the totoal. You could do adorn_total and adorn_percentages












# junk----

# Not hit as hard by the 2008 and 2010 recession 
# Obviously we see a data entry error (add this to the plot as an appendum)
# Create barplot for bottom five observations in 99
# mention data quality of Open Data New York. Possible option is to impute the value
ggplot(data= filter(tax99to19, county %in% min99$county), aes(x= tax_year, y= ny_agi_of_all_returns_in_thousands, color= county, group= county)) +
  geom_line(stat="identity")



tax99to19 %>%
  filter(county== "Hamilton", year == 2004)

# Try to figure out the Hamilton county mess
# got it it 
#Is it green that is missing a value in the 2005 dataset (I guess I will worry about it later)

test <- tax99to19 %>%
  distinct(tax_year, county) 

test3 <- test %>%
  group_by(tax_year) %>%
  summarise(freq=n()) 

test <- tax99to19 %>%
  filter(ny_agi_of_all_returns_in_thousands != 802099.00)

test1 <- test %>%
  filter(tax_year== 2005) %>%
  distinct(county, .keep_all= T)

test2 <- test %>%
  filter(tax_year== 2004) %>%
  distinct(county, .keep_all= T)

x <- intersect(test1$county, test2$county)

test2 %>%
  filter(county %ni% x)


?setdiff

test <- tax99to19 %>%
  distinct(tax_year, county) 
# %>%
#   filter(tax_year== "2005", county== "Hamilton")

max99 <- tax_states %>%
  pivot_longer(cols= ny_agi_of_all_returns:number_of_all_returns, values_to= "type")

#Notes----
# I would like to present this material for your review as a dashboard. This is possible to do using RShiny. Instead of presenting multiple line graphs for different variables, there could be a drop-down menu to select the variable and the plot would automatically change in response. The code is not that more technically difficult, it's just a matter of me learning it, which I'm doing on my own time

#Save datasets to file----
library(openxlsx)
list_of_datasets <- list("NY Counties"= tax99to19,
                         "States" = tax_states)
write.xlsx(list_of_datasets, file = "./data/working/taxData_clean_2022-5-18.xlsx")


library(curl)


url <- "https://github.com/pschuma1/reportsrig/raw/main/tax99to19.rds"
data <- readRDS(url(url, method="libcurl"))



frame %>%
  filter(is.na(sales_tax_year))
drop_na()

frame2 <- frame %>%
  filter(sales_tax_year== "2019 - 2020") %>%
  select(-GEOID) 

tm_shape(frame2) + tm_fill(col = "description") +
  tm_borders()+tm_layout(legend.height= 15, legend.width = 11)

tm_shape(frame) + tm_fill() +
  tm_borders()

?tm_layout


alpha <- sales %>%
  mutate(sector= as.numeric(substr(naics_industry_group, 1, 2)),
         jurisdiction= str_to_title(jurisdiction),
         jurisdiction= str_replace(jurisdiction, "St ", "St. "),
         taxable_sales_and_purchases= as.numeric(taxable_sales_and_purchases)) %>%
  merge(ncais, by= "sector", all.x= T) %>%
  filter(jurisdiction %ni% c("Mctd", "Ny State", "Ny City"),
         sales_tax_quarter== "4") %>%
  rename(county= jurisdiction) 
names(alpha)
setdiff(ny_boundary$county, sales2$jurisdiction)

beta <- alpha %>%
  group_by(sales_tax_year, naics_industry_group) %>%
  summarize(total= sum(taxable_sales_and_purchases, na.rm= T))


delta <- beta %>% 
  arrange(desc(taxable_sales_and_purchases)) %>% 
  group_by(sales_tax_year) %>% slice(1:5)

# load package
install.packages("concordance")
library(concordance)

# get product description
get_desc(sourcevar = c("0184"), origin = "NAICS2017")
?get_desc

frame <- left_join(ny_boundary, sales2) 

frame2 <- frame %>%
  filter(sales_tax_quarter== 4)
library(tmap)
# I think this works 
sales_anim = tm_shape(frame2) + tm_fill(col = "taxable_sales_and_purchases") +
  tm_borders()+
  tm_facets(along = "sales_tax_year", free.coords = FALSE)

tmap_animation(sales_anim, filename = "sales_anim.gif", delay = 25)


load(file='data.rda')



