### Main Figure Plot (Figure 2)

# Overall abundance plots timeseries
setwd("C:/Users/schilh01/OneDrive - DPIE")

library(tidyverse)

effort_dat <- read_csv("Sampling Effort for plots.csv") %>% rename(Species = CommonName)
species_sum <- effort_dat %>% group_by(Species) %>% summarise(min_Date = min(Date),
                                                              max_Date = max(Date))

key_species <- c("Murray cod", "Golden perch", "Silver perch",
                 "Macquarie perch", "Freshwater catfish", "Common carp")
key_species2 <- c("Murray\ncod", "Golden\nperch", "Silver\nperch",
                  "Macquarie\nperch", "Freshwater\ncatfish", "Common\ncarp")

golden <- read_csv("Golden_perch_updated good prediction plot data overall_boat method.csv")
murray <- read_csv("Murray cod_updated good prediction plot data overall_boat method.csv")
catfish <- read_csv("Freshwater catfish updated good prediction plot data overall.csv")
silver <- read_csv("Silver_perch_updated good prediction plot data overall_boat method.csv")
mq <- read_csv("Macquarie perch updated good prediction plot data overall.csv")
carp <- read_csv("Common carp updated good prediction plot data overall.csv")

all_dat <- bind_rows(golden, murray, catfish, silver, mq, carp) %>% left_join(species_sum) %>%
  filter(Date >= min_Date) %>% filter(Date <= max_Date)

library(tidybayes)

ggplot(all_dat, aes(Date)) + facet_wrap(~Species, scales = "free_y") +
  stat_lineribbon(aes(y= .epred), alpha=0.25) + theme_classic() +
  geom_rug(data=effort_dat)+
  theme(axis.title = element_text(face="bold", size = 14),
        axis.text = element_text(size=12, colour="black"),
        axis.ticks = element_line(colour="black"),
        legend.position = "bottom") +
  scale_fill_viridis_d(name="Credible Interval")+
  ylab("Relative Abundance\n(per 90sec E-Fishing)")

#ggsave("overall model plot.pdf", dpi=600, units = "cm",
#       width=21, height = 14.8)
#ggsave("overall model plot.png", dpi=600, units = "cm",
#       width=21, height = 14.8)

plot_list_abund <- list()

for(i in 1:6){
  pdat <- all_dat %>% filter(Species == key_species[i])
  edat <- effort_dat %>% filter(Species == key_species[i])
  if(i %in% c(2,3,4,5)){
    plot_list_abund[[i]] <- ggplot(pdat, aes(Date))+
      stat_lineribbon(aes(y= .epred), alpha=0.25) + theme_classic() +
      geom_rug(data=edat)+ xlab(NULL)+
      scale_x_date(labels=NULL)+
      theme(axis.title = element_text(face="bold", size = 12),
            axis.text = element_text(size=10, colour="black"),
            axis.ticks = element_line(colour="black"),
            legend.position = "bottom") +
      scale_fill_viridis_d(name="Credible Interval")+
      ylab(key_species2[i]) # \n(per 90sec E-Fishing)
  }
  else if(i ==1){
    plot_list_abund[[i]] <- ggplot(pdat, aes(Date))+
      stat_lineribbon(aes(y= .epred), alpha=0.25) + theme_classic() +
      ggtitle("Relative\nAbundance")+
      geom_rug(data=edat)+ xlab(NULL)+
      scale_x_date(labels=NULL)+
      theme(axis.title = element_text(face="bold", size = 12),
            axis.text = element_text(size=10, colour="black"),
            axis.ticks = element_line(colour="black"),
            legend.position = "bottom",
            plot.title = element_text(face="bold", size=14, hjust=0.5)) +
      scale_fill_viridis_d(name="Credible Interval")+
      ylab(key_species2[i]) # \n(per 90sec E-Fishing)
  }
  else(
    plot_list_abund[[i]] <- ggplot(pdat, aes(Date))+
      stat_lineribbon(aes(y= .epred), alpha=0.25) + theme_classic() +
      geom_rug(data=edat)+
      theme(axis.title = element_text(face="bold", size = 12),
            axis.text = element_text(size=10, colour="black"),
            axis.ticks = element_line(colour="black"),
            legend.position = "bottom") +
      scale_fill_viridis_d(name="Credible Interval")+
      ylab(key_species2[i]) #\n(per 90sec E-Fishing)
  )
}

library(patchwork)
plot_list_abund[[1]] + plot_list_abund[[2]] + plot_list_abund[[3]]+
  plot_list_abund[[4]] + plot_list_abund[[5]] + plot_list_abund[[6]] +
  plot_layout(ncol=1, guides = 'collect') & theme(legend.position = "bottom")

#ggsave("Abundance combined stacked plot.png", width = 7, height = 28, units="cm", dpi=600)


#### Length Plots ####
# Length Freq over time
setwd("~/Long term abundance")
# Compare brms models
library(tidyverse)
library(ggridges)

# 
key_species <- c("Murray cod", "Golden perch", "Silver perch",
                 "Macquarie perch", "Freshwater catfish", "Common carp")

mydata2 <- read.csv("Boat_backpack_combined_with_fixed_WRPA.csv") %>%
  select(SWWRPANAME_NEW, SiteName, SampleLatitude, SampleLongitude) %>% distinct()


### in a loop

plots <- list()

for(j in 1:6){
  mydata <- read_csv("Bio data/combined bio data all electro.csv") %>%
    filter(Length_mm != 0) %>% 
    mutate(Date=lubridate::mdy(Date),
           Month = lubridate::month(Date),
           Year = lubridate::year(Date),
           FinYear = case_when(Month <=6 ~ Year,
                               Month > 6 ~ Year + 1),
           fFinYear = as.factor(as.character(FinYear))) %>% filter(FinYear <= 2022) %>% filter(FinYear >1993)
  if(j %in% c(1,2,3)){
    mydata <- mydata %>% filter(Method == "Boat Electrofishing")
  }
  
  mydata <- mydata %>% left_join(mydata2)
  
  mydata <- mydata %>% mutate(SWWRPANAME_NEW = as.character(SWWRPANAME_NEW)) %>% filter(is.na(SWWRPANAME_NEW) == F)
  table(mydata$SWWRPANAME_NEW)
  
  mydata$fFinYear <- droplevels(mydata$fFinYear)
  #mydata$FinYear <- droplevels(mydata$FinYear)
  
  mydata <- mydata %>%  complete(fFinYear, FinYear, CommonName) %>%
    filter(CommonName == key_species[j])
  
  
  sample_size = mydata %>% group_by(FinYear, fFinYear) %>% summarise(n=n()) %>%
    mutate(lab = paste0("n = ",n))
  sample_size
  sum(sample_size$n)
  
  table(mydata$Method)
  if(j %in% c(2,3,4,5)){
    plots[[j]] <- ggplot(mydata, aes(x = Length_mm/10, y = fFinYear, height = stat(density))) + 
      
      stat_density_ridges(quantile_lines = T, quantiles = 2, rel_min_height=0.01, scale=0.95, fill = "deepskyblue", from = 0)+ # , scale=0.9
      coord_flip() + #xlim(0,1500)+
      scale_y_discrete(labels=NULL, breaks=c(2000,2010,2020))+ xlab(NULL)+
      #geom_text(data=sample_size, aes(x = 1400, y = fFinYear, label=lab), inherit.aes = F, angle=90)+
      theme_classic() +  ylab(NULL)+ #xlab("Length (cm)")+
      theme(axis.title = element_text(face="bold", size = 14),
            axis.text = element_text(size=10, colour="black"),
            axis.ticks = element_line(colour="black"))
  }
  else if(j == 1){
    plots[[j]] <- ggplot(mydata, aes(x = Length_mm/10, y = fFinYear, height = stat(density))) + 
      ggtitle("Length (cm)")+
      stat_density_ridges(quantile_lines = T, quantiles = 2, rel_min_height=0.01, scale=0.95, fill = "deepskyblue", from=0)+ # , scale=0.9
      coord_flip() + #xlim(0,1500)+
      scale_y_discrete(labels=NULL, breaks=c(2000,2010,2020))+ xlab(NULL)+
      #geom_text(data=sample_size, aes(x = 1400, y = fFinYear, label=lab), inherit.aes = F, angle=90)+
      theme_classic() +  ylab(NULL)+ #xlab("Length (cm)")+
      theme(axis.title = element_text(face="bold", size = 14),
            axis.text = element_text(size=10, colour="black"),
            axis.ticks = element_line(colour="black"),
            plot.title = element_text(face="bold", size = 14, hjust = 0.5))
  }
  else{
    plots[[j]] <- ggplot(mydata, aes(x = Length_mm/10, y = fFinYear, height = stat(density))) + 
      
      stat_density_ridges(quantile_lines = T, quantiles = 2, rel_min_height=0.01, scale=0.95, fill = "deepskyblue", from =0)+ # , scale=0.9
      coord_flip() + #xlim(0,1500)+
      scale_y_discrete(breaks = c(2000,2010, 2020))+ xlab(NULL)+
      #geom_text(data=sample_size, aes(x = 1400, y = fFinYear, label=lab), inherit.aes = F, angle=90)+
      theme_classic() +  ylab("Year Ending June")+ # xlab("Length (cm)")+
      theme(axis.title = element_text(face="bold", size = 14),
            axis.text = element_text(size=10, colour="black"),
            axis.ticks = element_line(colour="black"))}
}

library(patchwork)

plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plots[[6]] +plot_layout(ncol=1)
#ggsave("test.png", width = 7, height = 28, units="cm", dpi=600)

#### Biomass Plots ####

# Overall plots timeseries
setwd("C:/Users/schilh01/OneDrive - DPIE")
library(tidyverse)
effort_dat <- read_csv("Sampling Effort for plots.csv") %>% rename(Species = CommonName)
species_sum <- effort_dat %>% group_by(Species) %>% summarise(min_Date = min(Date),
                                                              max_Date = max(Date))

key_species <- c("Murray cod", "Golden perch", "Silver perch",
                 "Macquarie perch", "Freshwater catfish", "Common carp")

golden <- read_csv("Golden perch BIOMASS updated good prediction plot data overall.csv")
murray <- read_csv("Murray cod BIOMASS updated good prediction plot data overall.csv")
catfish <- read_csv("Freshwater catfish BIOMASS updated good prediction plot data overall.csv")
silver <- read_csv("Silver perch BIOMASS updated good prediction plot data overall.csv")
mq <- read_csv("Macquarie perch BIOMASS updated good prediction plot data overall.csv")
carp <- read_csv("Common carp BIOMASS updated good prediction plot data overall.csv")

all_dat <- bind_rows(golden, murray, catfish, silver, mq, carp) %>% left_join(species_sum) %>%
  filter(Date >= min_Date) %>% filter(Date <= max_Date)

library(tidybayes)

ggplot(all_dat, aes(Date)) + facet_wrap(~Species, scales = "free_y") +
  stat_lineribbon(aes(y= .epred), alpha=0.25) + theme_classic() +
  geom_rug(data=effort_dat)+
  theme(axis.title = element_text(face="bold", size = 14),
        axis.text = element_text(size=12, colour="black"),
        axis.ticks = element_line(colour="black"),
        legend.position = "bottom") +
  scale_fill_viridis_d(name="Credible Interval")+
  ylab("Relative Biomass\n(per 90sec E-Fishing)")

# ggsave("overall biomass model plot.pdf", dpi=600, units = "cm",
#        width=21, height = 14.8)
# ggsave("overall biomass model plot.png", dpi=600, units = "cm",
#        width=21, height = 14.8)

plot_list_biom <- list()

for(i in 1:6){
  pdat <- all_dat %>% filter(Species == key_species[i])
  edat <- effort_dat %>% filter(Species == key_species[i])
  if(i %in% c(2,4,5)){
    plot_list_biom[[i]] <- ggplot(pdat, aes(Date))+
      stat_lineribbon(aes(y= .epred), alpha=0.25) + theme_classic() +
      geom_rug(data=edat)+ xlab(NULL)+
      scale_x_date(labels=NULL)+
      theme(axis.title = element_text(face="bold", size = 14),
            axis.text = element_text(size=12, colour="black"),
            axis.ticks = element_line(colour="black"),
            legend.position = "bottom") +
      scale_fill_viridis_d(name="Credible Interval")+
      ylab(NULL) # \n(per 90sec E-Fishing)
  }
  else if(i == 1){
    plot_list_biom[[i]] <- ggplot(pdat, aes(Date))+
      stat_lineribbon(aes(y= .epred), alpha=0.25) + theme_classic() +
      geom_rug(data=edat)+ xlab(NULL)+
      ggtitle("Relative\nBiomass")+
      scale_x_date(labels=NULL)+
      theme(axis.title = element_text(face="bold", size = 14),
            axis.text = element_text(size=12, colour="black"),
            axis.ticks = element_line(colour="black"),
            legend.position = "bottom",
            plot.title = element_text(face="bold", size = 14, hjust = 0.5)) +
      scale_fill_viridis_d(name="Credible Interval")+
      ylab(NULL) # \n(per 90sec E-Fishing)
  }
  else if(i==3){
    plot_list_biom[[i]] <- ggplot(pdat, aes(Date))+
      stat_lineribbon(aes(y= .epred), alpha=0.25) + theme_classic() +
      scale_y_continuous(breaks=c(0,0.01,0.02))+
      geom_rug(data=edat)+ xlab(NULL)+
      scale_x_date(labels=NULL)+
      theme(axis.title = element_text(face="bold", size = 14),
            axis.text = element_text(size=12, colour="black"),
            axis.ticks = element_line(colour="black"),
            legend.position = "bottom") +
      scale_fill_viridis_d(name="Credible Interval")+
      ylab(NULL)
  }
  else(
    plot_list_biom[[i]] <- ggplot(pdat, aes(Date))+
      stat_lineribbon(aes(y= .epred), alpha=0.25) + theme_classic() +
      geom_rug(data=edat)+
      theme(axis.title = element_text(face="bold", size = 14),
            axis.text = element_text(size=12, colour="black"),
            axis.ticks = element_line(colour="black"),
            legend.position = "bottom") +
      scale_fill_viridis_d(name="Credible Interval")+
      ylab(NULL) #\n(per 90sec E-Fishing)
  )
}

library(patchwork)
plot_list_biom[[1]] + plot_list_biom[[2]] + plot_list_biom[[3]]+
  plot_list_biom[[4]] + plot_list_biom[[5]] + plot_list_biom[[6]] +
  plot_layout(ncol=1, guides = 'collect') & theme(legend.position = "bottom")

#ggsave("biomass combined stacked plot.png", width = 7, height = 28, units="cm", dpi=600)

#### Combine plots ####


library(patchwork)
plot_list_abund[[1]] + plot_list_biom[[1]] + plots[[1]] +
  plot_list_abund[[2]] + plot_list_biom[[2]] + plots[[2]] +
  plot_list_abund[[3]]+ plot_list_biom[[3]]+ plots[[3]] +
  plot_list_abund[[4]] +plot_list_biom[[4]] + plots[[4]]+
  plot_list_abund[[5]] + plot_list_biom[[5]] + plots[[5]] +
  plot_list_abund[[6]] + plot_list_biom[[6]] + plots[[6]]+
  #plot_annotation(tag_levels = 'a')+
  plot_layout(ncol=3, guides = 'collect',
              widths = c(1, 1, 1.6)) & theme(legend.position = "bottom",
                                             axis.text = element_text(colour="black", size = 10),
                                             axis.title = element_text(face="bold", size = 12),
                                             legend.title = element_text(face="bold", size=12),
                                             legend.text = element_text(colour="black", size=10))#+
  #plot_annotation(tag_levels = 'a')

ggsave("abund and length and biomass combined stacked plot.png", width = 21, height = 28, units="cm", dpi=600)
ggsave("abund and length and biomass combined stacked plot.pdf", width = 21, height = 28, units="cm", dpi=600)
