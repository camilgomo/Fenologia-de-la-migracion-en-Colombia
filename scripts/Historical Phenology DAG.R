library(ggplot2)
library(dplyr)
library(lubridate)
library(maps)

#Histogram and density plot with historical data
setwd("your path")
hist_migs <- read.csv("Hist_Records_Migrants.csv", h = T, sep = ";")

head(hist_migs)
days <- hist_migs %>% 
  mutate(date_form = as.Date(ORIGINAL_DATE, "%m/%d/%Y")) %>% 
  mutate(day_of_year = yday(date_form)) %>%
  mutate(period = "historical") %>% 
  select(day_of_year, period, SPECIES_NAME, LATITUDE, LONGITUDE, YEAR)

ggplot(data = days, aes(day_of_year)) +
  geom_histogram(fill="#375E97",alpha=0.5) + 
  labs(x="Date of record",y="Density")

ggplot(data = days, aes(day_of_year)) +
geom_density(col="#FFBB00",size=1.5) +
  labs(x="Date of record",y="Density")

#Data for each species
df <- days %>% group_by(SPECIES_NAME) %>% 
  mutate(count_name_occurr = n())

g2<-ggplot(data=df, aes(x=reorder(SPECIES_NAME,-count_name_occurr))) +
  geom_bar(stat="count")+
  labs(x="",y="Number of records")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

#Species with more than 20 records

plus20 <- df %>% filter(count_name_occurr >= 20) 

ggplot(data = plus20, aes(day_of_year, fill = SPECIES_NAME)) +
  geom_density(col="#FFBB00",size=1.5, alpha = 0.5) +
  labs(x="Date of record",y="Density")

#Species with more than 50 records 

plus50 <- df %>% filter(count_name_occurr >= 50)

#Remove T. savana and V. olivaceus
plus50 <- subset(plus50, SPECIES_NAME!= "Tyrannus savana")
plus50 <- subset(plus50, SPECIES_NAME!= "Pygochelidon cyanoleuca")
plus50 <- subset(plus50, SPECIES_NAME!= "Vireo olivaceus")
plus50 <- subset(plus50, SPECIES_NAME!= "Myiodynastes maculatus")


ggplot(data = plus50, aes(day_of_year, fill = SPECIES_NAME)) +
  geom_density(col="#FFBB00",size=1.5, alpha = 0.5) +
  labs(x="Date of record",y="Density")



#Species with more than 150 records 

plus150 <- df %>% filter(count_name_occurr >= 150) 

ggplot(data = plus150, aes(day_of_year, fill = SPECIES_NAME)) +
  geom_density(col="#FFBB00",size=1.5, alpha = 0.5) +
  labs(x="Date of record",y="Density")
  #scale_x_continuous(breaks=pretty(plus150$day_of_year, n=13))


#Exploring spatial and temporal biases in data
#Latitude and date

plot(df$day_of_year, df$LATITUDE)
plot(df$day_of_year, df$LONGITUDE)

#Create a data frame of species with enough historical records to join with eBird data

sp <- days %>% select(SPECIES_NAME) 
sp <- data.frame(SPECIES.NAME = unique(sp$SPECIES_NAME))

sp20 <- plus20 %>% select(SPECIES_NAME)
sp20 <- data.frame(SPECIES.NAME = unique(sp20$SPECIES_NAME))

sp50 <- plus50 %>% select(SPECIES_NAME)
sp50 <- data.frame(SPECIES.NAME = unique(sp50$SPECIES_NAME))


sp150 <- plus150 %>% select(SPECIES_NAME)
sp150 <- data.frame(SPECIES.NAME = unique(sp150$SPECIES_NAME))


#Load eBird data
my_data <- read.delim(file = "your path/ebd_CO_2022.txt", 
                      header=T,
                      sep="\t",
                      quote="", # very important
                      stringsAsFactors = F # OPTIONAL, False = read characters as strings instead of factors
)

head(my_data)

species <- levels(factor(days$SPECIES_NAME))

#Filter variables of interest
modern <- my_data %>% 
  select(COMMON.NAME, SCIENTIFIC.NAME,OBSERVATION.COUNT,STATE,LOCALITY, LOCALITY.ID, LATITUDE, LONGITUDE,OBSERVATION.DATE, PROTOCOL.TYPE,ALL.SPECIES.REPORTED,APPROVED) 

#Format dates
modern %>% 
  mutate(modern_date = as.Date(OBSERVATION.DATE, "%Y-%m-%d"))

#Extract year and add to data frame
year <- as.numeric(format(as.Date(modern$OBSERVATION.DATE, format="%Y-%m-%d"),"%Y"))
modern$YEAR <- year

#Filter observations from 2009 onwards

modern2 <- modern[modern$SCIENTIFIC.NAME %in% sp$SPECIES.NAME,] %>% filter(YEAR > 2008)
modern3 <- modern[modern$SCIENTIFIC.NAME %in% sp20$SPECIES.NAME,] %>% filter(YEAR > 2008)
modern4 <- modern[modern$SCIENTIFIC.NAME %in% sp50$SPECIES.NAME,] %>% filter(YEAR > 2008)
modern5 <- modern[modern$SCIENTIFIC.NAME %in% sp150$SPECIES.NAME,] %>% filter(YEAR > 2008)



days_mod <- modern2 %>%
  mutate(modern_date = as.Date(OBSERVATION.DATE, "%Y-%m-%d")) %>% 
  mutate(day_of_year = yday(modern_date)) %>%
  mutate(period = "modern") %>% 
  select(day_of_year, period, SCIENTIFIC.NAME, LATITUDE, LONGITUDE, YEAR) %>% 
  mutate(SPECIES_NAME = SCIENTIFIC.NAME) %>% 
  select(day_of_year, period, SPECIES_NAME, LATITUDE, LONGITUDE, YEAR)

days_mod3 <- modern3 %>% 
  mutate(modern_date = as.Date(OBSERVATION.DATE, "%Y-%m-%d")) %>% 
  mutate(day_of_year = yday(modern_date)) %>% 
  mutate(period = "modern") %>% 
  select(day_of_year, period, SCIENTIFIC.NAME, LATITUDE, LONGITUDE, YEAR) %>% 
  mutate(SPECIES_NAME = SCIENTIFIC.NAME) %>% 
  select(day_of_year, period, SPECIES_NAME, LATITUDE, LONGITUDE, YEAR)

days_mod4 <- modern4 %>%
  mutate(modern_date = as.Date(OBSERVATION.DATE, "%Y-%m-%d")) %>% 
  mutate(day_of_year = yday(modern_date)) %>% 
  mutate(period = "modern") %>% 
  select(day_of_year, period, SCIENTIFIC.NAME, LATITUDE, LONGITUDE, YEAR) %>% 
  mutate(SPECIES_NAME = SCIENTIFIC.NAME) %>% 
  select(day_of_year, period, SPECIES_NAME, LATITUDE, LONGITUDE, YEAR)

days_mod5 <- modern5 %>% 
  mutate(modern_date = as.Date(OBSERVATION.DATE, "%Y-%m-%d")) %>% 
  mutate(day_of_year = yday(modern_date)) %>% 
  mutate(period = "modern") %>% 
  select(day_of_year, period, SCIENTIFIC.NAME, LATITUDE, LONGITUDE, YEAR) %>% 
  mutate(SPECIES_NAME = SCIENTIFIC.NAME) %>% 
  select(day_of_year, period, SPECIES_NAME, LATITUDE, LONGITUDE, YEAR)
  

#Join historical and modern records
all_data <- rbind(days, days_mod)

all_data20 <- rbind(plus20, days_mod3)

all_data50 <- rbind(plus50, days_mod4)

all_data150 <- rbind(plus150, days_mod5)

#Export data for further use
write.table(all_data, file="all_data.txt", col.names = T, sep= "t")
write.table(all_data150, file="all_data150.txt", col.names = T, sep= "t")
write.table(all_data20, file="all_data20.txt", col.names = T, sep= "t")
write.table(all_data50, file="all_data50.txt", col.names = T, sep= "t")

#Data visualizations

theme_set(theme_classic())

ggplot(data = all_data, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")+
  scale_x_continuous(breaks=seq(1,366,30.5))

ggplot(data = all_data20, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")+
  scale_x_continuous(breaks=seq(1,366,30.5))

ggplot(data = all_data50, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="",y="Density")+
  scale_x_continuous(breaks=seq(1,366,30.5))
  #ggtitle("Cambio histórico en la fenología de passeriformes migratorios")
  #scale_fill_manual(values = c("darkorange", "navy"))

ggplot(data = all_data150, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")+
  scale_x_continuous(breaks=seq(1,366,30.5))

#Species with Plus 50 records

swth <- all_data50 %>% filter(SPECIES_NAME == "Catharus ustulatus") 
 
ggplot(data = swth, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

evir <- all_data50 %>% filter(SPECIES_NAME == "Empidonax virescens") 

ggplot(data = evir, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

gphi <- all_data50 %>% filter(SPECIES_NAME == "Geothlypis philadelphia") 

ggplot(data = gphi, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

mvar <- all_data50 %>% filter(SPECIES_NAME == "Mniotilta varia") 

ggplot(data = mvar, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

mcri <- all_data50 %>% filter(SPECIES_NAME == "Myiarchus crinitus") 

ggplot(data = mcri, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

mmac <- all_data50 %>% filter(SPECIES_NAME == "Myiodynastes maculatus") 

ggplot(data = mmac, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

pnov <- all_data50 %>% filter(SPECIES_NAME == "Parkesia noveboracensis") 

ggplot(data = pnov, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")


scas <- all_data50 %>% filter(SPECIES_NAME == "Setophaga castanea") 

ggplot(data = scas, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

sfus <- all_data50 %>% filter(SPECIES_NAME == "Setophaga fusca") 

ggplot(data = sfus, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")


spet <- all_data50 %>% filter(SPECIES_NAME == "Setophaga petechia") 


ggplot(data = spet, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

srut <- all_data50 %>% filter(SPECIES_NAME == "Setophaga ruticilla") 

ggplot(data = srut, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

tsav <- all_data50 %>% filter(SPECIES_NAME == "Tyrannus savana") 

ggplot(data = tsav, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")

voli <- all_data50 %>% filter(SPECIES_NAME == "Vireo olivaceus") 

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

ggplot(data = lpereg, aes(day_of_year, fill = period)) +
  geom_density(alpha = 0.25) + 
  labs(x="Date of record",y="Density")



