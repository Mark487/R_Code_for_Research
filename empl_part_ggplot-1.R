library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)


# Load individual data files
Australia <- read.csv(file="ABS_data/Australia_stats.csv",
                      stringsAsFactors=FALSE)
Tasmania <- read.csv(file="ABS_data/Tasmania.csv",
                     stringsAsFactors=FALSE)
Launceston_NW <- read.csv(file="ABS_data/Launceston_NW.csv",
                      stringsAsFactors=FALSE)

# Create AU classifier and and move to first column.
Aust_tmp <- mutate(Australia, Region = "AU")
Tas_tmp  <- mutate(Tasmania, Region = "TAS")
LNW_tmp  <- mutate(Launceston_NW, Region = "LNW")

# Append datasets
combined <- rbind(Aust_tmp, Tas_tmp, LNW_tmp)

# Convert region to factor
combined$Region <- as.factor(combined$Region)
# Re-order factors
combined$Region <- factor(combined$Region, levels = c("AU","TAS","LNW"))
# Move Region to first column
combined <- select(combined, Region, Time:Description)
# Rename MEASURE to Measure
combined <- rename(combined, Measure = MEASURE)
# Convert Measure to factor
combined$Measure <- as.factor(combined$Measure)

# Filter for employment % and participation %
empl_part <- filter(combined, Measure %in% c("LF_5", "LF_4")) %>%
             select(Region, Measure, Value)

ggplot(data = empl_part, aes(x = Region, y = Value, fill=Measure)) +
  geom_bar(stat="identity", position=position_dodge()) 



