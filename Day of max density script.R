#CALCULAR EL DÍA EN QUE SE PRESENTA LA MÁXIMA DENSIDAD
setwd("C:/Users/USER/OneDrive/Documentos/Universidad/Evolvert/Proyecto migratorias/Fase 4")
library(ggplot2)
library(ggpubr)
library(ggeasy)

#Cargar datos
data <- read.table("all_data50.txt", h = T, sep = "t")

#Eliminar NAs (de ser necesario)
data <- na.omit(data)

#Hacer subset por periodo y año (de ser necesario)
subset_period <- subset(data, data$period == "modern")
subset_year <- subset(subset_period, subset_period$YEAR == 2016)

#Comprobar con la gráfica
ggplot(data = subset_year, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="",y="Density")


#Extraer el día en que se presenta la máxima densidad
#Definir función para calcular máximos locales
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

#Calcular densidad para el subset de periodo
density_data <- density(subset_year$day_of_year)

#Calcular máximos locales 
loc.max <- density_data$x[localMaxima(density_data$y)]
#max <- which.max(density_data$y)
#day_max_density <- density_data$x[max]

#Comprobar con la gráfica
ggplot(data = subset_year, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="",y="Density")+
  scale_x_continuous(breaks=seq(1,366,30.5))+
  geom_vline(xintercept=loc.max
  )

#Buscar máximos locales de interés
loc.max[2]

####EXPLORAR GRÁFICAS Y ESTADÍSTICAS
#Cargar base de datos de días de máxima densidad
histvsmod_max <- read.delim("~/Universidad/Evolvert/Proyecto migratorias/Fase 4/histvsmod_max.txt")

#Hacer subset de histórico y modernos (modificar según necesidad)
years <- subset(histvsmod_max, Year!="all")
years$Year <- as.numeric(years$Year)
dataset <- subset(years, Dataset== "plus50")
dataset$Year <- as.numeric(dataset$Year)

#Gráficas 
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
