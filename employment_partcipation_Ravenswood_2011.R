# Employment and Participation Rate (%) 2011
# Australia vs Ravenswood, Launceston.
# Data source: Australian Bureau of Statistics
# http://govhack.abs.gov.au/Index.aspx

library(ggplot2)
library(dplyr)
source("multiplot.R")

region <- factor(c("Ravenswood","Ravenswood", "Australia", "Australia"))
statistic <-factor(c("Unemployment rate", "Participation rate", "Unemployment rate", "Participation rate"))
value <- c(16.4, 45.8, 5.6, 61.4)

combined <- data.frame(region, statistic, value)
print(combined)

unemployment <- filter(combined, statistic=="Unemployment rate")
participation <- filter(combined, statistic=="Participation rate")

emp_graph <- ggplot(data = unemployment, aes(region, value, fill=region)) +
    geom_bar(stat="identity") +
    ggtitle("Unemployment rate (2011)") +
    labs(x="", y="Percentage") + guides(fill=FALSE) +
    scale_y_continuous(limits = c(0,18)) +
    geom_text(aes(label=value),
              position=position_dodge(width=0.9),
              vjust=-0.40,
              size = 5
    ) +
    theme_linedraw()

part_graph <- ggplot(data = participation, aes(region, value, fill=region)) +
    geom_bar(stat="identity") +
    ggtitle("Participation rate (2011)") +
    labs(x="", y="Percentage") + guides(fill=FALSE) +
    scale_y_continuous(limits = c(0,70)) +
    geom_text(aes(label=value),
              position=position_dodge(width=0.9),
              vjust=-0.40,
              size = 5
    ) +
    theme_linedraw()

multiplot(emp_graph, part_graph, cols = 2)
