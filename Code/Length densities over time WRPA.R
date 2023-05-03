# Length Freq over time by species by valley
setwd("~/Long term abundance")
# Compare brms models
library(tidyverse)
library(ggridges)

### NEED TO MATCH TO WRPA
#mydata <- read_csv("Bio data/combined bio data all electro.csv")
mydata2 <- read.csv("Boat_backpack_combined_with_fixed_WRPA.csv") %>%
  select(SWWRPANAME_NEW, SiteName, SampleLatitude, SampleLongitude) %>% distinct()


# Silver perch
key_species <- c("Murray cod", "Golden perch", "Silver perch",
                 "Macquarie perch", "Freshwater catfish", "Common carp")

##### Catfish ####
j = 5#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))

# 
# mydata <- read_csv("Bio data/combined bio data all electro.csv") %>% filter(CommonName == key_species[j])  %>%
#  filter(Length_mm != 0) %>% #filter(Method == "Boat Electrofishing") %>%
#   mutate(Date=lubridate::mdy(Date),
#          Month = lubridate::month(Date),
#          Year = lubridate::year(Date),
#          FinYear = case_when(Month <=6 ~ Year,
#                              Month > 6 ~ Year + 1),
#          fFinYear = as.character(FinYear)) %>% filter(FinYear <= 2022)

mydata <- read_csv("Length plotting data Jan 23.csv") %>% filter(CommonName == key_species[j]) %>%
  filter(Length_mm != 0) %>% 
  mutate(Date=as.Date(SampleDate),
         Month = lubridate::month(Date),
         Year = lubridate::year(Date),
         FinYear = case_when(Month <=6 ~ Year,
                             Month > 6 ~ Year + 1),
         fFinYear = as.factor(as.character(FinYear))) %>% filter(FinYear <= 2022) %>% filter(FinYear >1993) %>%
filter(FinYear <= 2022)


# mydata <- mydata %>% left_join(mydata2)

mydata <- mydata %>% mutate(SWWRPANAME_NEW = as.character(SWWRPANAME_NEW)) %>% filter(is.na(SWWRPANAME_NEW) == F)
table(mydata$SWWRPANAME_NEW)

sample_size = mydata %>% group_by(SWWRPANAME_NEW) %>% summarise(n=n()) %>%
  mutate(lab = paste0("n = ",n))
sample_size
sum(sample_size$n)

table(mydata$Method)
table(mydata$FinYear)

ggplot(mydata, aes(x = Length_mm, y = fFinYear, height = stat(density))) + 
  stat_density_ridges(quantile_lines = T, quantiles = 2, rel_min_height=0.01, scale=0.95, fill="cyan", from=0)+ # , scale=0.9
  coord_flip() + facet_wrap(~SWWRPANAME_NEW)+
  scale_y_discrete(breaks = c(2000,2010,2020))+
  geom_text(data=sample_size, aes(x = max(mydata$Length_mm)*1.3, y = 4, label=lab), inherit.aes = F)+
  theme_classic() + xlab("Length (mm)")+ ylab("Year ending June")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        strip.text = element_text(size=10, face="bold"))

ggsave("Catfish WRPA Length Distribtions.png", width=21, height = 18, units="cm")
ggsave("Catfish WRPA Length Distribtions.pdf", width=21, height = 18, units="cm")

#### Murray cod ####
j = 1#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))



mydata <- read_csv("Length plotting data Jan 23.csv") %>% filter(CommonName == key_species[j]) %>%
  filter(Length_mm != 0) %>% filter(Method == "BTE") %>%
  mutate(Date=as.Date(SampleDate),
         Month = lubridate::month(Date),
         Year = lubridate::year(Date),
         FinYear = case_when(Month <=6 ~ Year,
                             Month > 6 ~ Year + 1),
         fFinYear = as.factor(as.character(FinYear))) %>% filter(FinYear <= 2022) %>% filter(FinYear >1993) 

mydata <- mydata %>% mutate(SWWRPANAME_NEW = as.character(SWWRPANAME_NEW)) %>% filter(is.na(SWWRPANAME_NEW) == F)
table(mydata$SWWRPANAME_NEW)

sample_size = mydata %>% group_by(SWWRPANAME_NEW) %>% summarise(n=n()) %>%
  mutate(lab = paste0("n = ",n))
sample_size
sum(sample_size$n)

table(mydata$Method)
table(mydata$FinYear)

ggplot(mydata, aes(x = Length_mm, y = fFinYear, height = stat(density))) + 
  stat_density_ridges(quantile_lines = T, quantiles = 2, rel_min_height=0.01, scale=0.95, fill="cyan", from=0)+ # , scale=0.9
  coord_flip() + facet_wrap(~SWWRPANAME_NEW, ncol=3)+
  scale_y_discrete(breaks = c(2000,2010,2020))+
  geom_text(data=sample_size, aes(x = max(mydata$Length_mm)*1.1, y = 4, label=lab), inherit.aes = F)+
  theme_classic() + xlab("Length (mm)")+ ylab("Year ending June")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        strip.text = element_text(size=10, face="bold"))

ggsave("Murray cod WRPA Length Distribtions.png", width=21, height = 23, units="cm")
ggsave("Murray cod WRPA Length Distribtions.pdf", width=21, height = 23, units="cm")


#### Golden perch ####
j = 2#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))



mydata <- read_csv("Length plotting data Jan 23.csv") %>% filter(CommonName == key_species[j]) %>%
  filter(Length_mm != 0) %>% filter(Method == "BTE") %>%
  mutate(Date=as.Date(SampleDate),
         Month = lubridate::month(Date),
         Year = lubridate::year(Date),
         FinYear = case_when(Month <=6 ~ Year,
                             Month > 6 ~ Year + 1),
         fFinYear = as.factor(as.character(FinYear))) %>% filter(FinYear <= 2022) %>% filter(FinYear >1993) 


mydata <- mydata %>% mutate(SWWRPANAME_NEW = as.character(SWWRPANAME_NEW)) %>% filter(is.na(SWWRPANAME_NEW) == F)
table(mydata$SWWRPANAME_NEW)

sample_size = mydata %>% group_by(SWWRPANAME_NEW) %>% summarise(n=n()) %>%
  mutate(lab = paste0("n = ",n))
sample_size
sum(sample_size$n)

table(mydata$Method)
table(mydata$FinYear)

ggplot(mydata, aes(x = Length_mm, y = fFinYear, height = stat(density))) + 
  stat_density_ridges(quantile_lines = T, quantiles = 2, rel_min_height=0.01, scale=0.95, fill="cyan", from=0)+ # , scale=0.9
  coord_flip() + facet_wrap(~SWWRPANAME_NEW, ncol=3)+
  scale_y_discrete(breaks = c(2000,2010,2020))+
  geom_text(data=sample_size, aes(x = max(mydata$Length_mm)*1.1, y = 4, label=lab), inherit.aes = F)+
  theme_classic() + xlab("Length (mm)")+ ylab("Year ending June")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        strip.text = element_text(size=10, face="bold"))

ggsave("Golden perch WRPA Length Distribtions.png", width=21, height = 24, units="cm")
ggsave("Golden perch WRPA Length Distribtions.pdf", width=21, height = 24, units="cm")


#### Silver perch ####
j = 3#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))


mydata <- read_csv("Length plotting data Jan 23.csv") %>% filter(CommonName == key_species[j]) %>%
  filter(Length_mm != 0) %>% filter(Method == "BTE") %>%
  mutate(Date=as.Date(SampleDate),
         Month = lubridate::month(Date),
         Year = lubridate::year(Date),
         FinYear = case_when(Month <=6 ~ Year,
                             Month > 6 ~ Year + 1),
         fFinYear = as.factor(as.character(FinYear))) %>% filter(FinYear <= 2022) %>% filter(FinYear >1993) 



mydata <- mydata %>% mutate(SWWRPANAME_NEW = as.character(SWWRPANAME_NEW)) %>% filter(is.na(SWWRPANAME_NEW) == F)
table(mydata$SWWRPANAME_NEW)

sample_size = mydata %>% group_by(SWWRPANAME_NEW) %>% summarise(n=n()) %>%
  mutate(lab = paste0("n = ",n))
sample_size
sum(sample_size$n)

table(mydata$Method)
table(mydata$FinYear)

ggplot(mydata, aes(x = Length_mm, y = fFinYear, height = stat(density))) + 
  stat_density_ridges(quantile_lines = T, quantiles = 2, rel_min_height=0.01, scale=0.95, fill="cyan", from=0)+ # , scale=0.9
  coord_flip() + facet_wrap(~SWWRPANAME_NEW, ncol=3)+
  scale_y_discrete(breaks = c(2000,2010,2020))+
  geom_text(data=sample_size, aes(x = max(mydata$Length_mm)*1.1, y = 4, label=lab), inherit.aes = F)+
  theme_classic() + xlab("Length (mm)")+ ylab("Year ending June")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        strip.text = element_text(size=10, face="bold"))

ggsave("Silver perch WRPA Length Distribtions.png", width=21, height = 18, units="cm")
ggsave("Silver perch WRPA Length Distribtions.pdf", width=21, height = 18, units="cm")


#### MQ Perch ####
j = 4#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))


mydata <- read_csv("Length plotting data Jan 23.csv") %>% filter(CommonName == key_species[j]) %>%
  filter(Length_mm != 0) %>% #filter(Method == "BTE") %>%
  mutate(Date=as.Date(SampleDate),
         Month = lubridate::month(Date),
         Year = lubridate::year(Date),
         FinYear = case_when(Month <=6 ~ Year,
                             Month > 6 ~ Year + 1),
         fFinYear = as.factor(as.character(FinYear))) %>% filter(FinYear <= 2022) %>% filter(FinYear >1993) 



mydata <- mydata %>% mutate(SWWRPANAME_NEW = as.character(SWWRPANAME_NEW)) %>% filter(is.na(SWWRPANAME_NEW) == F)
table(mydata$SWWRPANAME_NEW)

sample_size = mydata %>% group_by(SWWRPANAME_NEW) %>% summarise(n=n()) %>%
  mutate(lab = paste0("n = ",n))
sample_size
sum(sample_size$n)

table(mydata$Method)
table(mydata$FinYear)

ggplot(mydata, aes(x = Length_mm, y = fFinYear, height = stat(density))) + 
  stat_density_ridges(quantile_lines = T, quantiles = 2, rel_min_height=0.01, scale=0.95, fill="cyan", from=0)+ # , scale=0.9
  coord_flip() + facet_wrap(~SWWRPANAME_NEW, ncol=3)+
  scale_y_discrete(breaks = c(2000,2010,2020))+
  geom_text(data=sample_size, aes(x = max(mydata$Length_mm)*1.1, y = 7, label=lab), inherit.aes = F)+
  theme_classic() + xlab("Length (mm)")+ ylab("Year ending June")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        strip.text = element_text(size=10, face="bold"))

ggsave("Macquarie perch WRPA Length Distribtions.png", width=21, height = 14, units="cm")
ggsave("Macquarie perch WRPA Length Distribtions.pdf", width=21, height = 14, units="cm")


#### Common carp ####
j = 6#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))

mydata <- read_csv("Length plotting data Jan 23.csv") %>% filter(CommonName == key_species[j]) %>%
  filter(Length_mm != 0) %>% #filter(Method == "BTE") %>%
  mutate(Date=as.Date(SampleDate),
         Month = lubridate::month(Date),
         Year = lubridate::year(Date),
         FinYear = case_when(Month <=6 ~ Year,
                             Month > 6 ~ Year + 1),
         fFinYear = as.factor(as.character(FinYear))) %>% filter(FinYear <= 2022) %>% filter(FinYear >1993) 



mydata <- mydata %>% mutate(SWWRPANAME_NEW = as.character(SWWRPANAME_NEW)) %>% filter(is.na(SWWRPANAME_NEW) == F)
table(mydata$SWWRPANAME_NEW)

sample_size = mydata %>% group_by(SWWRPANAME_NEW) %>% summarise(n=n()) %>%
  mutate(lab = paste0("n = ",n))
sample_size
sum(sample_size$n)

table(mydata$Method)
table(mydata$FinYear)

ggplot(mydata, aes(x = Length_mm, y = fFinYear, height = stat(density))) + 
  stat_density_ridges(quantile_lines = T, quantiles = 2, rel_min_height=0.01, scale=0.95, fill="cyan", from=0)+ # , scale=0.9
  coord_flip() + facet_wrap(~SWWRPANAME_NEW, ncol=3)+
  scale_y_discrete(breaks = c(2000,2010,2020))+
  geom_text(data=sample_size, aes(x = max(mydata$Length_mm)*0.9, y = 6, label=lab), inherit.aes = F)+
  theme_classic() + xlab("Length (mm)")+ ylab("Year ending June")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        strip.text = element_text(size=10, face="bold"))

ggsave("Common carp WRPA Length Distribtions.png", width=21, height = 23, units="cm")
ggsave("Common carp WRPA Length Distribtions.pdf", width=21, height = 23, units="cm")
