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

# Extract Employment education %
education <- filter(combined, grepl('^SCHOOL_', Code), Code != "SCHOOL_2") %>%
            select(Region, Value, Code, Description)
# Split Description column
education <- separate(education, Description, 
                     into = c("Type","Level"), 
                     sep = " - ")
education <- select(education, -Type)
# Strip ending '(%)'
education$Level <- sub("\\(\\%\\)", "", education$Level)

# plot data: ggplot2
g <-
  ggplot(data = education, aes(x = Level, y = Value, fill = Region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  labs(title = "Post High School Qualifications (2011 Cencus)", 
       x = "", y = "Percentage") +
  theme_linedraw()

print(g)

