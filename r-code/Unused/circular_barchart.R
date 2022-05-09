#circular barchart for species. 


# Libraries
library(tidyverse)
library(ggplot2)
library(dplyr)

totals <- read.csv(file = here("data", "Totals_By_Species.csv")) 
summary(totals)

totals2 <- totals %>% 
  arrange(Species, Total) %>% 
  filter(Total > 80)


ggplot(totals2, aes(x = reorder(Species,Total), y = Total, fill = Species))+
  geom_bar(stat = "identity")+
  theme(legend.position = "none")+
  ylim(-50,1500)+
  coord_polar(start = 0)
  
  



# regular circular for total ----------------------------------------------

#from: https://www.r-graph-gallery.com/299-circular-stacked-barplot.html

data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value=sample( seq(10,100), 60, replace=T)
)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


# ----- ------------------------------------------- ---- #

p <- ggplot(totals, aes(x=Species, y=Total, fill=Species)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  scale_y_continuous("Totals") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p