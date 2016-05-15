library(tidyr)
library(dplyr)
library(magrittr)

Australia <- read.csv(file="ABS_data/Australia_stats.csv",
                      stringsAsFactors=FALSE)
Tasmania <- read.csv(file="ABS_data/Tasmania.csv",
                     stringsAsFactors=FALSE)
Launceston_NW <- read.csv(file="ABS_data/Launceston_NW.csv",
                      stringsAsFactors=FALSE)

# Create Region factor column 
Aust_tmp <- mutate(Australia, Region = "AU")
Tas_tmp  <- mutate(Tasmania, Region = "TAS")
LNW_tmp  <- mutate(Launceston_NW, Region = "LNW")

# Append datasets
combined <- rbind(Aust_tmp, Tas_tmp, LNW_tmp)
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

# Extract Employment %
employment <- filter(combined, Code=="LF_4") %>%
            select(Region, Value, Code, Description)
# Split Description column
employment <- separate(employment, Description, into = c("Type","Description"), sep = " - ")

# plot data: ggplot2
ggplot(data = employment, aes(x = Region, y = Value)) +
  geom_bar(stat="identity")
