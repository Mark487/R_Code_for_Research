# Underutilised Persons (Aged 15 years and over)
# by State and Territory - February 2016
#
# Data source: ABS (2016). Labour Force, Australia, Mar 2016.
#              Canberra: Australian Bureau of Statistics
#

library(ggplot2)

underutilized <- read.csv(file="ABS_data/Underutilized_persons.csv",
                      stringsAsFactors=FALSE)
# Make colours for bars and bind
bar_colour = c("black","grey","grey","grey","grey","grey","red","grey","grey")
underutilized <- cbind(underutilized, bar_colour)

# plot data: ggplot2
g <- ggplot(data = underutilized, aes(Region, Value)) +
     geom_bar(stat = "identity", fill = bar_colour, colour="black") +
     coord_flip() +
     ggtitle("Underutilised Persons (Aged 15 years and over)
             by State and Territory - February 2016") +
     labs(x = "", y = "Percentage") +
     scale_y_continuous(limits = c(0,20)) +
     geom_text(aes(label=Value),
               position=position_dodge(width=0.9),
               hjust=-0.35,
               size = 4) +
     guides(fill = FALSE) +
     theme_linedraw()


print(g)
# ggsave("images/underutilized_persons.png", height = 5, width = 7)

