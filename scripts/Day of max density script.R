#Calculate de day of maximum density
setwd("C:your path")
library(ggplot2)
library(ggpubr)
library(ggeasy)

#load data
data <- read.table("all_data50.txt", h = T, sep = "t")

#eliminate NA values if necessary
data <- na.omit(data)

#Subset by period and year
subset_period <- subset(data, data$period == "modern")
subset_year <- subset(subset_period, subset_period$YEAR == 2016)

#Visualize
ggplot(data = subset_year, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="",y="Density")


#Extract day of maximum density
#Define function to estimate local maxima
localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

#Estimate density for subset of period
density_data <- density(subset_year$day_of_year)

#Calculate local maxima 
loc.max <- density_data$x[localMaxima(density_data$y)]
#max <- which.max(density_data$y)
#day_max_density <- density_data$x[max]

#Visualize
ggplot(data = subset_year, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="",y="Density")+
  scale_x_continuous(breaks=seq(1,366,30.5))+
  geom_vline(xintercept=loc.max
  )

#Identify local maxima of interest
loc.max[2]

####Explore graphs and statistics
#Load data with local maxima
histvsmod_max <- read.delim("~your path/histvsmod_max.txt")

#Subset historical and modern as needed
years <- subset(histvsmod_max, Year!="all")
years$Year <- as.numeric(years$Year)
dataset <- subset(years, Dataset== "plus50")
dataset$Year <- as.numeric(dataset$Year)

#Graphs
ggplot(data = dataset, aes(x=Year, y=Day_maxdensity_spring, color = Dataset))+
  geom_point()+
  geom_abline()+
  geom_smooth(method =lm)

ggplot(data = dataset, aes(x=Year, y=Day_maxdensity_fall, color = Dataset))+
  geom_point()+
  geom_abline()+
  geom_smooth(method = lm)


ggarrange(spring, fall,
          ncol=1,
          nrow=2,
          labels = "AUTO")
  
model <- lm(dataset$Day_maxdensity_fall~dataset$Year)
summary(model)
