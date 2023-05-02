### Smelt REPORT
setwd("C:/Users/schilh01/OneDrive - DPIE")
# Compare brms models
library(tidyverse)
library(brms)
library(DHARMa)

# Silver perch
taxa <- c("Murray cod", "Freshwater catfish", "Golden perch",
          "Silver perch", "Common carp", "Macquarie perch", "Bony herring", "Australian smelt")

j = 3#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))


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

catch_sum <- mydata %>% filter(CommonName == taxa[j]) %>%
  group_by(Method) %>% summarise(Total_catch = sum(NumberCaught))
catch_sum # 30 percent by backpack therefore only use both boat for herring



table(mydata$BasinName)
basins <- c("Barwon-Darling Watercourse", "Gwydir", "Intersecting Streams",
            "Lachlan", "Macquarie-Castlereagh", "Murrumbidgee", "Namoi", 
            "New South Wales Border Rivers", "New South Wales Murray",
            "New South Wales Lower Darling") #

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
hist(mydata$Sampling_duration)

#mydata <- mydata %>% ungroup() %>% dplyr::mutate(NumberCaught = case_when(!is.na(NumberCaught)~NumberCaught,
#                                                     T ~ 0))

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

#no_catch <- mydata %>% filter(CommonName == "No catch")

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

area_summary <- mydata_wide %>% group_by(SWWRPANAME_NEW) %>% summarise(total = sum(Golden, na.rm=T))
area_summary

#mydata_wide <- mydata_wide %>% filter(SWWRPANAME_NEW != "Intersecting Streams")

#mydata_wide <- mydata_wide %>% filter()

b2 <- brm(Golden ~SWWRPANAME_NEW + s(days_since, by = SWWRPANAME_NEW) +
            (1|SiteID) + (1|fDate)+
            offset(log(Sampling_duration)),
          cores=4,
          family="zero_inflated_negbinomial",
          control = list(adapt_delta = 0.95),
          data=mydata_wide,
          iter = 2000,
          file_refit = "never",
          seed=1234567,
          file = paste0(taxa[j],"_updated_newcomp_boat_WRPA.rds")) # "../../srv/scratch/z3374139/MDB models/",


f2 <-  b2 #readRDS("C:/Users/schilh01/Documents/Long term abundance/Macquarie perch overall/Macquarie perch_ZINB_overall_newcomp.rds")
summary(f2)

library(tidybayes)
#yy <- new_data %>% add_epred_draws(f2)

### important to drop levels below
mydata_wide$SWWRPANAME_NEW <- droplevels(mydata_wide$SWWRPANAME_NEW)
yy <- f2 %>% epred_draws(newdata = expand_grid(Method = "BTE",
                                               days_since = seq(0, max(mydata_wide$days_since), by = 50),
                                               SWWRPANAME_NEW = levels(mydata_wide$SWWRPANAME_NEW),
                                               Sampling_duration = 90), 
                         re_formula = NA) %>%
  mutate(Date = days_since + min(mydata_wide$Date), Species = "Golden perch")

write_csv(yy, "Golden_perch_updated good prediction plot data WRPA_boat method.csv")

#zz <- yy %>% filter(SWWRPANAME_NEW == "Lachlan" | SWWRPANAME_NEW =="Murrumbidgee") #filter(days_since > 3000)

ggplot(yy, aes(Date)) + facet_wrap(~SWWRPANAME_NEW, scales = "free") +
  stat_lineribbon(aes(y= .epred), width=c(0.9), alpha=0.25) + theme_classic() +
  theme(axis.title = element_text(face="bold", size = 14),
        axis.text = element_text(size=12, colour="black"),
        axis.ticks = element_line(colour="black"))

ggsave("Golden perch_updated good prediction plot WRPA boat.pdf", width = 21, height=23, units="cm", dpi =600)
ggsave("Golden perch cod_updated good prediction plot WRPA boat.png", width = 21, height=23, units="cm", dpi =600)

#geom_line(aes(y=.epred, group = paste(SWWRPANAME_NEW, .draw)), alpha=0.25)

library(DHARMa)
model.check_f5 <- createDHARMa(
  simulatedResponse = t(posterior_predict(f2)),
  observedResponse = mydata_wide$Golden,
  fittedPredictedResponse = apply(t(posterior_epred(f2)), 1, mean),
  integerResponse = T,
  method = "traditional")

plot(model.check_f5)




#### overall has it increased
new_data <- data.frame(
  days_since = seq(min(mydata_wide$days_since), max(mydata_wide$days_since), 50),
  Method = "Boat Electrofishing",
  Sampling_duration = median(mydata_wide$Sampling_duration)
) %>% mutate(Date = days_since + min(mydata_wide$Date))

xx <- posterior_epred(f2, newdata = new_data, re_formula = NA) 
quantile(xx[,1], probs = c(0.025,0.5, 0.975))
quantile(xx[,40], probs = c(0.025,0.5, 0.975))
quantile(xx[,199], probs = c(0.025,0.5, 0.975))

day0 <- xx[,1]
day2000 <- xx[,40] # Jan 2000
dayF <- xx[,199]


### binary log ratio
diff <- log2(dayF/day0)
hist(diff)

diff2000 <- log2(dayF/day2000)
hist(diff2000)

diffDF <- diff  %>% as.data.frame() %>% rename(value = 1) %>% mutate(Species = "Macquarie perch", time = "Start")
diffDF2000 <- diff2000  %>% as.data.frame() %>% rename(value = 1) %>% mutate(Species = "Macquarie perch", time = "2000")

diffDF <- diffDF %>% bind_rows(diffDF2000)
write_csv(diffDF, "Macquarie perch overall log_ratio.csv")

library(tidybayes)
ggplot(diffDF, aes(x=value, fill=time)) + stat_slabinterval(position = "dodge") +
  geom_vline(xintercept = 0, col= "red")# I think this is how to present the final plots
