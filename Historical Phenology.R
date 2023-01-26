library(ggplot2)
library(dplyr)
library(lubridate)
library(maps)

#Histograma y density plot con datos de registros historicos
hist_migs <- read.csv("C:\\Users\\camil\\OneDrive\\Documents\\Camila\\Publicaciones\\Historical migration Phenology\\Hist_Records_Migrants.csv", h = T, sep = ";")

head(hist_migs)
days <- hist_migs %>% 
  mutate(date_form = as.Date(ORIGINAL_DATE, "%m/%d/%Y")) %>% 
  mutate(day_of_year = yday(date_form)) %>%
  mutate(period = "historical") %>% 
  select(day_of_year, period, SPECIES_NAME, LATITUDE, LONGITUDE)

windows()
ggplot(data = days, aes(day_of_year)) +
  geom_histogram(fill="#375E97",alpha=0.5) + 
  labs(x="Date of record",y="Density")

ggplot(data = days, aes(day_of_year)) +
geom_density(col="#FFBB00",size=1.5) +
  labs(x="Date of record",y="Density")

#Datos por especie_historicos
df <- days %>% group_by(SPECIES_NAME) %>% 
  mutate(count_name_occurr = n())

windows()
g2<-ggplot(data=df, aes(x=reorder(SPECIES_NAME,-count_name_occurr))) +
  geom_bar(stat="count")+
  labs(x="",y="Number of records")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

#Species with more than 20 records

plus20 <- df %>% filter(count_name_occurr >= 20) 

windows()
ggplot(data = plus20, aes(day_of_year, fill = SPECIES_NAME)) +
  geom_density(col="#FFBB00",size=1.5, alpha = 0.5) +
  labs(x="Date of record",y="Density")

#Species with more than 50 records - probably the only group of species worth including?

plus50 <- df %>% filter(count_name_occurr >= 50) 

windows()
ggplot(data = plus50, aes(day_of_year, fill = SPECIES_NAME)) +
  geom_density(col="#FFBB00",size=1.5, alpha = 0.5) +
  labs(x="Date of record",y="Density")

#Species with more than 150 records - probably the only group of species worth including?

plus150 <- df %>% filter(count_name_occurr >= 150) 

windows()
ggplot(data = plus150, aes(day_of_year, fill = SPECIES_NAME)) +
  geom_density(col="#FFBB00",size=1.5, alpha = 0.5) +
  labs(x="Date of record",y="Density")

#Latitude and date

plot(df$day_of_year, df$LATITUDE)
plot(df$day_of_year, df$LONGITUDE)

#Crear un data frame con el listado de especies que tienen registros historicos para filtrar datos de eBird

sp <- days %>% select(SPECIES_NAME) 
sp <- data.frame(SPECIES.NAME = unique(sp$SPECIES_NAME))

sp20 <- plus20 %>% select(SPECIES_NAME)
sp20 <- data.frame(SPECIES.NAME = unique(sp20$SPECIES_NAME))

sp50 <- plus50 %>% select(SPECIES_NAME)
sp50 <- data.frame(SPECIES.NAME = unique(sp50$SPECIES_NAME))

sp150 <- plus150 %>% select(SPECIES_NAME)
sp150 <- data.frame(SPECIES.NAME = unique(sp150$SPECIES_NAME))
#Datos de eBird para todo Colombia hasta Agosto 2018.

my_data <- read.delim("C:\\Users\\camil\\OneDrive\\Documents\\Camila\\Publicaciones\\Historical migration Phenology\\ebd_filtered_COL.txt")
head(my_data)

species <- levels(factor(days$SPECIES_NAME))

#Filtrar campos de interes
modern <- my_data %>% 
  select(COMMON.NAME, SCIENTIFIC.NAME,OBSERVATION.COUNT,STATE,LOCALITY, LOCALITY.ID, LATITUDE, LONGITUDE,OBSERVATION.DATE, PROTOCOL.TYPE,ALL.SPECIES.REPORTED,APPROVED) 

#Observaciones del 2009 en adelante

modern2 <- modern[modern$SCIENTIFIC.NAME %in% sp$SPECIES.NAME,] %>% filter(OBSERVATION.DATE > 2010)
modern3 <- modern[modern$SCIENTIFIC.NAME %in% sp20$SPECIES.NAME,] %>% filter(OBSERVATION.DATE > 2010)
modern4 <- modern[modern$SCIENTIFIC.NAME %in% sp50$SPECIES.NAME,] %>% filter(OBSERVATION.DATE > 2010)
modern5 <- modern[modern$SCIENTIFIC.NAME %in% sp150$SPECIES.NAME,] %>% filter(OBSERVATION.DATE > 2010)

days_mod <- modern2 %>% 
  mutate(modern_date = as.Date(OBSERVATION.DATE, "%Y-%m-%d")) %>% 
  mutate(day_of_year = yday(modern_date)) %>% 
  mutate(period = "modern") %>% 
  select(day_of_year, period, SCIENTIFIC.NAME, LATITUDE, LONGITUDE) %>% 
  mutate(SPECIES_NAME = SCIENTIFIC.NAME) %>% 
  select(day_of_year, period, SPECIES_NAME, LATITUDE, LONGITUDE)

days_mod3 <- modern3 %>% 
  mutate(modern_date = as.Date(OBSERVATION.DATE, "%Y-%m-%d")) %>% 
  mutate(day_of_year = yday(modern_date)) %>% 
  mutate(period = "modern") %>% 
  select(day_of_year, period, SCIENTIFIC.NAME, LATITUDE, LONGITUDE) %>% 
  mutate(SPECIES_NAME = SCIENTIFIC.NAME) %>% 
  select(day_of_year, period, SPECIES_NAME, LATITUDE, LONGITUDE)

days_mod4 <- modern4 %>% 
  mutate(modern_date = as.Date(OBSERVATION.DATE, "%Y-%m-%d")) %>% 
  mutate(day_of_year = yday(modern_date)) %>% 
  mutate(period = "modern") %>% 
  select(day_of_year, period, SCIENTIFIC.NAME, LATITUDE, LONGITUDE) %>% 
  mutate(SPECIES_NAME = SCIENTIFIC.NAME) %>% 
  select(day_of_year, period, SPECIES_NAME, LATITUDE, LONGITUDE)

days_mod5 <- modern5 %>% 
  mutate(modern_date = as.Date(OBSERVATION.DATE, "%Y-%m-%d")) %>% 
  mutate(day_of_year = yday(modern_date)) %>% 
  mutate(period = "modern") %>% 
  select(day_of_year, period, SCIENTIFIC.NAME, LATITUDE, LONGITUDE) %>% 
  mutate(SPECIES_NAME = SCIENTIFIC.NAME) %>% 
  select(day_of_year, period, SPECIES_NAME, LATITUDE, LONGITUDE)
  

#Unir registros historicos y modernos
all_data <- rbind(days, days_mod)

all_data20 <- rbind(days, days_mod3)

all_data50 <- rbind(days, days_mod4)

all_data150 <- rbind(days, days_mod5)


windows()
ggplot(data = all_data, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

windows()
ggplot(data = all_data20, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

windows()
ggplot(data = all_data50, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

windows()
ggplot(data = all_data150, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

#Solo especies con Plus 50 records

swth <- all_data50 %>% filter(SPECIES_NAME == "Catharus ustulatus") 
 
windows()
ggplot(data = swth, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

evir <- all_data50 %>% filter(SPECIES_NAME == "Empidonax virescens") 

windows()
ggplot(data = evir, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

gphi <- all_data50 %>% filter(SPECIES_NAME == "Geothlypis philadelphia") 

windows()
ggplot(data = gphi, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

mvar <- all_data50 %>% filter(SPECIES_NAME == "Mniotilta varia") 

windows()
ggplot(data = mvar, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

mcri <- all_data50 %>% filter(SPECIES_NAME == "Myiarchus crinitus") 

windows()
ggplot(data = mcri, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")


pnov <- all_data50 %>% filter(SPECIES_NAME == "Parkesia noveboracensis") 

windows()
ggplot(data = pnov, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")


scas <- all_data50 %>% filter(SPECIES_NAME == "Setophaga castanea") 

windows()
ggplot(data = scas, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

sfus <- all_data50 %>% filter(SPECIES_NAME == "Setophaga fusca") 

windows()
ggplot(data = sfus, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")


spet <- all_data50 %>% filter(SPECIES_NAME == "Setophaga petechia") 

windows()
ggplot(data = spet, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

srut <- all_data50 %>% filter(SPECIES_NAME == "Setophaga ruticilla") 

windows()
ggplot(data = srut, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

tsav <- all_data50 %>% filter(SPECIES_NAME == "Tyrannus savana") 

windows()
ggplot(data = tsav, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

voli <- all_data50 %>% filter(SPECIES_NAME == "Vireo olivaceus") 

windows()
ggplot(data = voli, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

lpere <- modern %>% filter(SCIENTIFIC.NAME == "Oreothlypis peregrina") %>%
 mutate(SPECIES_NAME = "Leiothlypis peregrina")


lpere <- lpere[lpere$SPECIES_NAME %in% sp50$SPECIES.NAME,] %>% filter(OBSERVATION.DATE > 2009)

days_mod_Lpere <- lpere %>% 
  mutate(modern_date = as.Date(OBSERVATION.DATE, "%Y-%m-%d")) %>% 
  mutate(day_of_year = yday(modern_date)) %>% 
  mutate(period = "modern") %>% 
  select(day_of_year, period, SPECIES_NAME, LATITUDE, LONGITUDE)
  
LPERE2 <- rbind(days, days_mod_Lpere)

lpereg <- LPERE2 %>% filter(SPECIES_NAME == "Leiothlypis peregrina") 

windows()
ggplot(data = lpereg, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")
