#CALCULAR EL DÍA EN QUE SE PRESENTA LA MÁXIMA DENSIDAD

library(ggplot2)
library(ggpubr)
library(ggeasy)

#Cargar datos
data <- read.table("C:\\Users\\camil\\OneDrive\\Documents\\Camila\\Publicaciones\\Historical migration Phenology\\Datos Fenología DAG\\Datos Fenología DAG\\all_data.txt", h = T, sep = "t")

#Eliminar NAs (de ser necesario)
Data <- na.omit(data)
head(Data)

dat_50 <- Data[Data$SPECIES_NAME %in% c("Piranga rubra", "Setophaga petechia", "Setophaga fusca","Catharus ustulatus","Parkesia noveboracensis","Setophaga ruticilla", "Geothlypis philadelphia", "Setophaga castanea", "Mniotilta varia","Empidonax virescens", "Leiothlypis peregrina","Myiarchus crinitus" ),]

windows()
ggplot(data = dat_50, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.7) + scale_fill_manual(values = c("darkblue", "yellow"), name = NULL)+ theme_bw(20)+
  labs(x="Day of the year",y="Density records of migratory birds")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=18), axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")))+ geom_vline(xintercept= c(47,315), linetype = "dashed", color = "goldenrod3") + geom_vline(xintercept= c(51, 309),linetype = "dashed", color = "darkblue") +
theme(legend.position = "top")

#Hacer subset por periodo y año (de ser necesario)
subset_period <- subset(data, data$period == "modern")
subset_year <- subset(subset_period, subset_period$YEAR == 2016)

#Comprobar con la gráfica
ggplot(data = data, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + scale_fill_manual(values = c("darkblue", "yellow"))+
  labs(x="",y="Density")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=18), axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")))+ 
  theme(legend.position = "none")
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
loc.max[3]

####EXPLORAR GRÁFICAS Y ESTADÍSTICAS
#Cargar base de datos de días de máxima densidad
histvsmod_max <- read.delim("C:\\Users\\camil\\OneDrive\\Documents\\Camila\\Publicaciones\\Historical migration Phenology\\Datos Fenología DAG\\Datos Fenología DAG\\histvsmod_max.txt")

head(histvsmod_max)
#Hacer subset de histórico y modernos (modificar según necesidad)
years <- subset(histvsmod_max, Year!="all")
years$Year <- as.numeric(years$Year)
dataset <- subset(years, Dataset== "plus50")
dataset$Year <- as.numeric(dataset$Year)

#Gráficas 
windows()
ggplot(data = dataset, aes(x=Year, y=Day_maxdensity_spring))+
  geom_point(size = 4, color = "yellow" )+
  geom_abline()+
  geom_smooth(method =lm, color = "black")+ labs(x = "", y = "Day of peak spring records (range: Jan 29 - Mar 31)")+ theme_bw(20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=18), axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")))+ 
theme(legend.position = "none")

ggplot(data = dataset, aes(x=Year, y=Day_maxdensity_fall))+
  geom_point(size = 4, color = "yellow" )+
  geom_abline()+
  geom_smooth(method =lm, color = "black")+ labs(x = "", y = "Day of peak fall records (range: Nov 4 - Nov 23)")+ theme_bw(20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=18), axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")))+ 
  theme(legend.position = "none")


model_fall <- lm(dataset$Day_maxdensity_fall~dataset$Year)
summary(model_fall)

model_spring <- lm(dataset$Day_maxdensity_spring~dataset$Year)
summary(model_spring)


# Boxplots peak days
dataset1 <- subset(histvsmod_max, Year == "all")

p <- ggplot(dataset1, aes( x = Period, y = Day_maxdensity_spring, fill = Period)) +
  geom_boxplot()+scale_fill_viridis_d(option="C")+
  geom_jitter(width = 0.2, size = 4)+ labs(x = "", y = "Day of peak spring records")+ theme_bw(20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=18), axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")))+ 
  theme(legend.position = "none")
p

p <- ggplot(dataset1, aes( x = Period, y = Day_maxdensity_fall, fill = Period)) +
  geom_boxplot()+scale_fill_viridis_d(option="C")+
  geom_jitter(width = 0.2, size = 4)+ labs(x = "", y = "Day of peak fall records")+ theme_bw(20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=18), axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")))+ 
  theme(legend.position = "none")
p

#All years
windows()
p <- ggplot(histvsmod_max, aes( x = Period, y = Day_maxdensity_spring, fill = Period)) +
  geom_boxplot()+scale_fill_viridis_d(option="C")+
  geom_jitter(width = 0.2, size = 4)+  ylim(c(20,100))+ labs(x = "", y = "Day of peak spring records")+ theme_bw(20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=18), axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")))+ 
  theme(legend.position = "none")
p

windows()
p <- ggplot(histvsmod_max, aes( x = Period, y = Day_maxdensity_fall, fill = Period)) +
  geom_boxplot()+scale_fill_viridis_d(option="C")+
  geom_jitter(width = 0.2, size = 4)+ ylim(c(250,365)) + labs(x = "", y = "Day of peak fall records")+ theme_bw(20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=18), axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")))+ 
  theme(legend.position = "none")
p

## Differences
windows()
ggplot(data = dataset, aes(x=Year, y=Spring_difference))+
  geom_point(size = 4, color = "yellow" )+
  geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = 1)+
  labs(x = "", y = "Difference in days between peak records")+ theme_bw(20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=18), axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")))+ 
  theme(legend.position = "none")

windows()
ggplot(data = dataset, aes(x=Year, y=Fall_difference))+
  geom_point(size = 4, color = "yellow" )+
  geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = 1)+
  labs(x = "", y = "Difference in days between peak records")+ theme_bw(20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=18), axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")))+ 
  theme(legend.position = "none")

windows()
ggplot(data = dataset, aes(x=Spring_difference, y=Fall_difference))+
  geom_point(size = 4, color = "darkblue" )+
  geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = 1)+
  geom_vline(aes(xintercept = 0), linetype = "dashed", linewidth = 1)+
  labs(x = "Difference in days between spring peak records", y = "Difference in days between fall peak records")+ theme_bw(20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=18), axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")))+ 
  theme(legend.position = "none")

#Randomization of differences
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


iter    <- 999;          # Total iterations (+1 for observed data = 10k)
diff_spring    <- NULL;# To add difference between groups
diff_fall    <- NULL;# To add difference between groups

N_obs <- dim(dat_50)[1]; # Total number of observations


for(i in 1:iter){   
  obs_samp   <- sample(x = dat_50[,2], size = N_obs, replace = FALSE);
  samp_hist  <- which(obs_samp == "historical");
  samp_mod   <- which(obs_samp == "modern");
  density_hist <- density(dat_50[samp_hist,1]);
  density_mod <- density(dat_50[samp_mod,1]);
  max_samp_hi   <- density_hist$x[localMaxima(density_hist$y)]
  max_samp_mo   <- density_mod$x[localMaxima(density_mod$y)]
  diff_spring[i] <- max_samp_hi[1] - max_samp_mo[1];
  diff_fall[i] <- max_samp_hi[2] - max_samp_mo[3];
}

windows()
xlabel <- expression( paste("Random group mean difference (", 
                            max[historical] - max[modern], ")" )
); # Gets the Greek mu with subscripts
hist(diff_spring, xlab = xlabel, col = "grey", main = "", cex.lab = 1.5, 
     cex.axis = 1.5, breaks = 20);
arrows(x0 = -3.59257, x1 = -3.59257, y0 = 100, y1 = 5, lwd = 3, length = 0.1);


less_or_equal_obs <- sum(diff_spring <= -3.59257) + 1;
total_generated   <- length(diff_spring) + 1;
new_p_value       <- less_or_equal_obs / total_generated;

windows()
xlabel <- expression( paste("Random group mean difference (", 
                            max[historical] - max[modern], ")" )
); # Gets the Greek mu with subscripts
hist(diff_fall, xlab = xlabel, col = "grey", main = "", cex.lab = 1.5, 
     cex.axis = 1.5, breaks = 20);
arrows(x0 = 3.1662, x1 = 3.1662, y0 = 80, y1 = 5, lwd = 3, length = 0.1);

less_or_equal_obs <- sum(diff_fall <= 3.1662) + 1;
total_generated   <- length(diff_fall) + 1;
new_p_value       <- less_or_equal_obs / total_generated;


