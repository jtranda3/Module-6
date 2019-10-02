########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
########################################################################################
# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----
cotton <- read.csv("data/cotton-usda-nass.csv")
View(cotton)
str(cotton)
head(cotton)
tail(cotton)
dim(cotton)
summary(cotton)

# 3.1. Create a NC data subset ----
cotton %>%
  filter(state == "NORTH CAROLINA") %>%
  select(year, state, ag_district, county, data_item, value) -> NC
NC 

# 3.2. Divide the data_item column ----
NC_Updated <- NC %>%
  separate(data_item, into = c("cotton_type", "measurement"), sep = "-")
NC_Updated
# 3.3. Convert the value column to numeric type ----
NC_Updated %>%
  filter(value != "(D)")
as.numeric(NC_Updated$value)
NC_Updated$value <- as.numeric(NC_Updated$value)
str(NC_Updated)
head(NC_Updated)
summary(NC_Updated)

# 4. Visualizing trends ----
NC_Updated %>%
  ggplot(mapping = aes(x = year, y = value)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_grid(measurement ~ ag_district)

# 5. Summarize data from 2018 ----
NC_Updated
NC_Final <- NC_Updated %>%
  filter(year == "2018") %>%
  spread(NC_Updated, key = measurement, value = value)

NC_Final

NC_Final$` ACRES HARVESTED` <- as.numeric(NC_Final$` ACRES HARVESTED`)
NC_Final$` YIELD, MEASURED IN LB / ACRE` <- as.numeric(NC_Final$` YIELD, MEASURED IN LB / ACRE`)

NC_Total <- NC_Final %>%
  mutate(total_lbs = ` ACRES HARVESTED` * ` YIELD, MEASURED IN LB / ACRE`)

NC_Total

NC_Total %>%
  select(county, total_lbs) %>%
  mutate(rank = min_rank(desc(total_lbs))) %>%
  arrange(rank) %>%
  top_n(3, total_lbs)

