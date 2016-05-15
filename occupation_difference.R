library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
source("multiplot.R")

Australia <- read.csv(file="ABS_data/Australia_stats.csv",
                      stringsAsFactors=FALSE)
Launceston_NW <- read.csv(file="ABS_data/Launceston_NW.csv",
                      stringsAsFactors=FALSE)

# Create Region factor column 
Aust_tmp <- mutate(Australia, Region = "AU")
LNW_tmp  <- mutate(Launceston_NW, Region = "LNW")

# Append datasets
combined <- rbind(Aust_tmp, LNW_tmp)
# Remove temporary files
rm(list = ls(pattern = "_tmp*"))

# Convert region to factor
combined$Region <- as.factor(combined$Region)
# Re-order factors
combined$Region <- factor(combined$Region, levels = c("AU","TAS","LNW"))
# Move Region to first column
combined <- select(combined, Region, Time:Description)
# Rename MEASURE to Code
combined <- rename(combined, Code = MEASURE)
# Convert Code to factor
combined$Code <- as.factor(combined$Code)

# Extract Employment Industry %
industry <- filter(combined, grepl('^EMP_IND_', Code), Code != "EMP_IND_21") %>%
            select(Region, Value, Code, Description)
# Split Description column
industry <- separate(industry, Description, 
                     into = c("Type","Occupation"), 
                     sep = " - ")
industry <- select(industry, -Type)
# Strip ending '(%)'
industry$Occupation <- sub("\\(\\%\\)", "", industry$Occupation)

# Remove Type column
industry <- select(industry, Region, Code, Value, Occupation)
# Spread Region coulmn into 'AUS' and 'LNW'
industry_wide <- industry %>% spread(Region, Value)
# Create Difference column
industry_wide <- industry_wide %>% mutate(Difference = LNW - AU)
# Sort by Difference
industry_wide <- industry_wide %>% arrange(Difference)

# plot data: ggplot2
g <-
  ggplot(data = industry_wide, aes(x = Occupation, y = Difference)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Launceston and North West (2011 Cencus)", x = "", y =
         "Percentage Difference from rest of Australia")

print(g)
