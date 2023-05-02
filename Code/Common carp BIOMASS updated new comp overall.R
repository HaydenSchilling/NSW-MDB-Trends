### MC BPEOM REPORT
setwd("C:/Users/schilh01/OneDrive - DPIE")
# Compare brms models
library(tidyverse)
library(brms)
library(DHARMa)

# Silver perch
taxa <- c("Murray cod", "Freshwater catfish", "Golden perch",
          "Silver perch", "Common carp", "Macquarie perch")

j = 5#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))==


mydata <- read.csv("All e-catch_30_11_2022_WRPA_fixed.csv") %>% mutate(project_segment = paste0(ProjectName,":",SegmentName))

segments <- mydata %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")

mydata <- mydata %>% filter(!project_segment %in% bad_list) %>%
  filter(Method == "BTE") %>%
  mutate(EffortData = str_replace_all(EffortData, pattern = "\"\"", replacement = "\"")) %>%
  mutate(boat = str_extract(EffortData, pattern="\"ElectrofishingDuration\"......"),
         ESecs = parse_number(boat),
         boat = NULL)# %>%
table(mydata$SWWRPANAME_NEW)
table(mydata$Method)

table(mydata$BasinName)
basins <- c("Barwon-Darling Watercourse", "Gwydir", "Intersecting Streams",
            "Lachlan", "Macquarie-Castlereagh", "Murrumbidgee", "Namoi", 
            "New South Wales Border Rivers", "New South Wales Murray",
            "New South Wales Lower Darling") # murray needs more work

mydata <- mydata %>% #filter(CommonName == "Golden perch") %>%
  mutate(#Date = lubridate::mdy(Date),
    #Year = lubridate::year(Date),
    SiteID = as.factor(as.character(SiteID))) %>%
  filter(SWWRPANAME_NEW %in% basins) %>%
  mutate(SWWRPANAME_NEW = as.factor(SWWRPANAME_NEW),
         Sampling_duration = as.numeric(ESecs),
         ESecs = NULL) %>%
  filter(Sampling_duration>0) %>%
  filter(Sampling_duration < 600) %>%
  distinct()# removes a event with sample time of zero secs - there are some negatives???

table(mydata$SWWRPANAME_NEW)
table(mydata$Method)
table(mydata$Sampling_duration)



mydata <- mydata %>% ungroup() %>% dplyr::mutate(CommonName = case_when(!is.na(CommonName)~CommonName,
                                                                        T ~ "No catch"))

### Go to wide format
mydata_wide <- mydata %>% select(-5, -6, -13, -16,-17) %>% drop_na(SWWRPANAME_NEW) %>%
  # this bit needed for some dud database entries.
  group_by(ProjectName, Abbreviation, SegmentName, Method, SiteName, SiteID_old, SiteID, SampleDate,
           SamplingRecordID, OperationID, CommonName, EffortPageLetter, OperationNumber, EffortData,
           SWWRPANAME, SWWRPANAME_NEW, coords.x1, coords.x2,Sampling_duration, project_segment) %>%
  summarise(NumberCaught = sum(NumberCaught)) %>% 
  pivot_wider(names_from = CommonName, values_from = NumberCaught,values_fill = 0)


#ggplot(mydata, aes(Date, Caught)) + geom_point()+
#  geom_smooth()


mydata_wide$Golden <- mydata_wide[[taxa[j]]]
hist(mydata_wide$Golden)
mydata_wide$Date <- as.Date(mydata_wide$SampleDate)

mydata_wide$fDate <- as.factor(as.character(mydata_wide$Date))

### Can we remove sites that never got a species of interest?
GP_positive <- mydata_wide %>% filter(Golden != 0)

mydata_wide <- mydata_wide %>% ungroup() %>% filter(SiteID %in% GP_positive$SiteID) %>%
  select(Golden, SiteID, fDate, Sampling_duration, SWWRPANAME_NEW, Date, Method) %>% drop_na()# %>%
#mutate(Date = lubridate::ymd(Date))

# mdw_sum <- mydata_wide %>% group_by(SiteID, fDate, Date, SWWRPANAME_NEW, Method) %>%
#   summarise(Golden = sum(Golden),
#             Sampling_duration = sum(Sampling_duration))# %>%
# mutate(Date = lubridate::ymd(Date))

sites_per_day <- mydata_wide %>% group_by(fDate, SWWRPANAME_NEW) %>% summarise(nSites = n_distinct(SiteID)) # there are some sites sampled on the same day


sort(table(mydata_wide$SiteID, mydata_wide$fDate),decreasing = T)
#mdw_sum$days_since = as.numeric(mdw_sum$Date - min(mdw_sum$Date))
mydata_wide$days_since = as.numeric(mydata_wide$Date - min(mydata_wide$Date))

area_summary <- mydata_wide %>% group_by(SWWRPANAME_NEW) %>% summarise(total = sum(Golden, na.rm=T),
                                                                       events = n_distinct(paste(SiteID, fDate)))
area_summary

#good_area <- c("Lachlan", "Murrumbidgee", "New South Wales Border Rivers", "New South Wales Lower Darling", "New South Wales Murray")

#mydata_wide <- mydata_wide %>% filter(SWWRPANAME_NEW %in% "Intersecting Streams")

### Add biomass weights
biom <- read_csv("Common carp operation mean weights.csv") %>%
  mutate(SiteID = as.factor(as.character(SiteID)),
         Date = as.Date(SampleDate))

mydata_wide <- mydata_wide %>% left_join(biom) 
mydata_wide <- mydata_wide %>% mutate(Biomass = Golden*meanBCW/1000)
mydata_wide$Biomass <- replace_na(mydata_wide$Biomass, 0)

plot(mydata_wide$Golden, mydata_wide$Biomass)

cor.test(mydata_wide$Golden, mydata_wide$Biomass)
cor.test(mydata_wide$Golden, mydata_wide$meanBCW)
cor.test(mydata_wide$meanBCW, mydata_wide$mean_W)

library(brms)
b2 <- brm(Biomass ~  s(days_since) + # s(Month, bs="cc")+
            (1|SiteID) + (1|fDate)+
            offset(log(Sampling_duration)),
          cores=4,
          family="hurdle_gamma",
          control = list(adapt_delta = 0.95),
          data=mydata_wide,
          iter = 2000,
          file_refit = "always",
          seed=123456,
          file = paste0(taxa[j],"_updated_BIOMASS_newcomp_boat_overall2.rds")) # "../../srv/scratch/z3374139/MDB models/",
# 
# summary(b2)

#b2 <- readRDS("Golden perch_BIOMASS_newcomp_boat_WRPA.rds")

summary(b2)


library(tidybayes)

yy <- b2 %>% epred_draws(newdata = expand_grid(Method = "BTE",
                                               days_since = seq(0, max(mydata_wide$days_since), by = 50),
                                               #SWWRPANAME_NEW = levels(droplevels(mydata_wide$SWWRPANAME_NEW)),
                                               Sampling_duration = 90), 
                         re_formula = NA) %>%
  mutate(Date = days_since + min(mydata_wide$Date), Species = "Common carp")

write_csv(yy, "Common carp BIOMASS updated good prediction plot data overall.csv")

#zz <- yy %>% filter(SWWRPANAME_NEW == "Lachlan" | SWWRPANAME_NEW =="Murrumbidgee") #filter(days_since > 3000)

ggplot(yy, aes(Date)) + #facet_wrap(~SWWRPANAME_NEW, scales = "free") +
  stat_lineribbon(aes(y= .epred), alpha=0.25) + theme_classic() +
  theme(axis.title = element_text(face="bold", size = 14),
        axis.text = element_text(size=12, colour="black"),
        axis.ticks = element_line(colour="black"))

ggsave("Common carp BIOMASS good prediction plot overall.pdf", width = 21, height=14.8, units="cm", dpi =600)
ggsave("Common carp BIOMASS good prediction plot overall.png", width = 21, height=14.8, units="cm", dpi =600)