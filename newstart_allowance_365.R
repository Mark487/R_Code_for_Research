# Newstart Allowance - on income support more than 365 days (%)
# 2010-2013
# Australia vs Launceston.
# Data source: Australian Bureau of Statistics
# http://govhack.abs.gov.au/Index.aspx

library(ggplot2)
library(dplyr)
source("multiplot.R")

region <- factor(c("Launceston","Launceston",
                   "Launceston","Launceston",
                   "Australia", "Australia",
                   "Australia", "Australia"))
year <- c(2010, 2011, 2012, 2013, 2010, 2011, 2012, 2013)
value <- c(68.2, 68.2, 70, 73.1, 59.9, 62.7, 61.9, 66)

newstart <- data.frame(year, region, value)

g <- ggplot(data = newstart,
            aes(x=year, y=value,
                group=region, shape=region, color=region)) +
    geom_line(size=1.5) +
    geom_point(size=3, fill="white") +
    scale_shape_manual(values=c(22,21)) +
    labs(title = "Newstart Allowance - on income support more than 365 days",
         x = "", y = "Percentage")

print(g)

