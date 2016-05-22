library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)

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
combined$Region <- factor(combined$Region, levels = c("AU","LNW"))
# Move Region to first column
combined <- select(combined, Region, Time:Description)
# Rename MEASURE to Code
combined <- rename(combined, Code = MEASURE)
# Convert Code to factor
combined$Code <- as.factor(combined$Code)

# Extract education %
education <- filter(combined, Code=="SCHOOL_2") %>%
             select(Region, Value)

print(education)

# plot data: ggplot2
g <- ggplot(data = education, aes(x = Region, y = Value)) +
  geom_bar(stat = "identity", fill="#FF9999", colour="black") +
  coord_fixed(ratio = .05) +
  labs(title = "Post High School Qualifications \n (2011 Cencus)",
       x = "Region", y = "Percentage") +
  scale_y_continuous(limits = c(0,60)) +
  geom_text(aes(label=Value),
            position=position_dodge(width=0.9),
            vjust=-0.40,
            size = 5
            ) +
  theme_linedraw()

print(g)
