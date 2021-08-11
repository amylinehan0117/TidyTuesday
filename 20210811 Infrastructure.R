# BEA Infrastructure Investment

# Goal:  Perform basic ggplot review of provided infrastructure data

# I. Set-Up ---------------------------------------------------------------
require("tidyverse")
require("data.table")
require("dtplyr")
require("ggplot2")

library(scales) # for ggplot2 labels 


rm(list = ls())
gc(reset=T)


# II. Data Loading --------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 33)

investment <- tuesdata$investment
chain_investment <-tuesdata$chain_investment
ipd <- tuesdata$ipd

summary(chain_investment)

# III. Data Cleaning ------------------------------------------------------


# A. Chain Investment 

chain_investment %>% filter(group_num == 1) %>% group_by(year) %>% summarise(n())
# group num 1 -> Total Basic Infrastructure , total social infrastructure and total digit infrastucture 

chain_investment %>% filter(group_num == 1 & year == 2000)

dta <- chain_investment %>% filter(group_num == 1)
# 213

# IV. Data Visualization 

# Bar Chart
g <- ggplot(data =  dta  , mapping = aes(x= year, y = gross_inv_chain))
g + geom_bar(stat="identity", aes(fill = category)) +
  scale_y_continuous(labels = comma) +
  theme_bw() + 
  theme(axis.text.x =  element_text(angle = 65, vjust = .6)) +
  labs(
    title ='Annual Gross Inventory by Infrastructure Category',
  ) +
  xlab("Year") + 
  ylab("Gross Inventory (Millions)")

# Area Chart 
g2 <- ggplot(data =  dta
             , mapping = aes(x= year, y = gross_inv_chain, fill= category))

g2 + geom_area() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Evolution of investment on US infrastructures",
    x="Year",
    y="Investment (millions of 2021 $)"
  )+
  theme_minimal() +
  scale_fill_manual(values=c("orange", "purple", "pink"))


# IV. Additional Review: Percentage Change ---------------------------


dta <- dta %>% 
  mutate(last_yr_inv =lag(gross_inv_chain)) %>% 
  mutate(last_yr_inv = ifelse(year == 1947, NA, last_yr_inv) )

# Calculate the percentage change 
# (this year - last year)/ last year 
dta <- dta %>% 
  mutate(pct_change = ifelse(is.na(last_yr_inv) == T, 0, 
                             (gross_inv_chain - last_yr_inv)/last_yr_inv))


g2 <- ggplot(data =  dta
             , mapping = aes(x= year, y = pct_change))

g2 + geom_point() +
  facet_grid(. ~ category) +
  stat_smooth(method='lm')+
  theme_bw() 

# exclude outliers 
g2 <- ggplot(data =  dta %>% filter(abs(pct_change)<.25)
             , mapping = aes(x= year, y = pct_change))

g2 + geom_point() +
  facet_grid(. ~ category) +
  stat_smooth(method='lm')+
  theme_bw() 

