library(tidyr)
library(dplyr)
library(magrittr)
source("multiplot.R")

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

# Extract Employment and Participation %
empl_part <- filter(combined, c(Code=="LF_4" | Code=="LF_5")) %>%
            select(Region, Value, Code, Description)
# Split Description column
empl_part <- separate(empl_part, Description, into = c("Type","Description"), sep = " - ")

# Split for graphing data
employment <- filter(empl_part, Code=="LF_4")
participation <- filter(empl_part, Code=="LF_5")

# plot data: ggplot2
emp_graph <- ggplot(data = employment, aes(Region, Value, fill=Region)) +
              geom_bar(stat="identity") +
              ggtitle(employment$Description) +
              labs(x="", y="") + guides(fill=FALSE) + theme_linedraw()
part_graph <- ggplot(data = participation, aes(Region, Value, fill=Region)) +
              geom_bar(stat="identity") +
              ggtitle(participation$Description) +
              labs(x="", y="") + guides(fill=FALSE) + theme_linedraw()
multiplot(emp_graph, part_graph, cols = 2)

