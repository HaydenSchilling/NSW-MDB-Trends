# Golden perch overall model checking

# Compare brms models
library(tidyverse)
library(brms)
library(DHARMa)

# Silver perch
taxa <- c("Murray cod", "Freshwater catfish", "Golden perch",
          "Silver perch", "Common carp", "Macquarie perch")

j = 3#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))


mydata <- read.csv("Boat_backpack_combined_with_fixed_WRPA.csv") %>%
  filter(Method == "Boat Electrofishing")
# mutate(EffortData = str_replace_all(EffortData, pattern = "\"\"", replacement = "\"")) %>%
# mutate(boat = str_extract(EffortData, pattern="\"ElectrofishingDuration\"......"),
#        ESecs = parse_number(boat),
#        boat = NULL)# %>%
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
  filter(Sampling_duration>0) %>% distinct()# removes a event with sample time of zero secs - there are some negatives???

table(mydata$SWWRPANAME_NEW)
table(mydata$Method)
table(mydata$Sampling_duration)

### Go to wide format
mydata_wide <- mydata %>% select(1:11,16,17,20:29) %>% drop_na(SWWRPANAME_NEW) %>%
  # this bit needed for some dud database entries.
  group_by(Date, SamplingRecordID, OperationID, SiteName, SampleLatitude, SampleLongitude, WaterbodyName, 
           BasinName, CatchmentName, SubCatchmentName, Altitude, EffortPageLetter, OperationNumber, Method, EffortData, PROJECT_SEGMENTS,
           SiteID, SiteID_old, SWWRPANAME, SWWRPANAME_NEW, Sampling_duration, CommonName) %>%
  summarise(Caught = sum(Caught)) %>% 
  pivot_wider(names_from = CommonName, values_from = Caught,values_fill = 0)

#ggplot(mydata, aes(Date, Caught)) + geom_point()+
#  geom_smooth()



mydata_wide$Golden <- mydata_wide[[taxa[j]]]

mydata_wide$fDate <- as.factor(as.character(mydata_wide$Date))

### Can we remove sites that never got a golden perch?
GP_positive <- mydata_wide %>% filter(Golden != 0)

mydata_wide <- mydata_wide %>% ungroup() %>% filter(SiteID %in% GP_positive$SiteID) %>%
  select(Golden, SiteID, fDate, Sampling_duration, SWWRPANAME_NEW, Date, Method) %>% drop_na() %>%
  mutate(Date = lubridate::ymd(Date))

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



f1 <- readRDS("Golden perch/Golden perch_poisson_overall_boat.rds")
f2 <- readRDS("Golden perch/Golden perch_NB_overall_boat.rds")
f3 <- readRDS("Golden perch/Golden perch_ZI_poisson_overall_boat.rds")
f4 <- readRDS("Golden perch/Golden perch_ZINB_overall_boat.rds")

summary(f1) # 1.00
summary(f2) # sd 1.00
summary(f3) # sd 1.00
summary(f4) # sd 1.00
# All converged OK

plot(f4)

### Poisson Model checks
model.check_f1 <- createDHARMa(
  simulatedResponse = t(posterior_predict(f1)),
  observedResponse = mydata_wide$Golden,
  fittedPredictedResponse = apply(t(posterior_epred(f1)), 1, mean),
  integerResponse = T,
  method = "traditional")

plot(model.check_f1)
# all significant - large deviation from normal

testOutliers(model.check_f1, type="bootstrap") # not run
testDispersion(model.check_f1) #  Yes overdispersed
testZeroInflation(model.check_f1) # Zero inflated
f1 <- NULL

### Neg binomial model checks
model.check_f2 <- createDHARMa(
  simulatedResponse = t(posterior_predict(f2)),
  observedResponse = mydata_wide$Golden,
  fittedPredictedResponse = apply(t(posterior_epred(f2)), 1, mean),
  integerResponse = T,
  method = "traditional")

plot(model.check_f2)
# better but still all the issues except dispersion

testOutliers(model.check_f2, type="bootstrap") # not run
testDispersion(model.check_f2) # perfect
testZeroInflation(model.check_f2) # slightly p = 0.001
f2 <- NULL

# THIS MODEL LOOKS better

### ZI Poisson Model checks
model.check_f3 <- createDHARMa(
  simulatedResponse = t(posterior_predict(f3)),
  observedResponse = mydata_wide$Golden,
  fittedPredictedResponse = apply(t(posterior_epred(f3)), 1, mean),
  integerResponse = T,
  method = "traditional")

plot(model.check_f3)
# normality and outlier deviation

testOutliers(model.check_f3, type="bootstrap") # 
testDispersion(model.check_f3) # OK
testZeroInflation(model.check_f3) # still zero-inflated
# 

### ZI Neg binomial model checks
model.check_f4 <- createDHARMa(
  simulatedResponse = t(posterior_predict(f4)),
  observedResponse = mydata_wide$Golden,
  fittedPredictedResponse = apply(t(posterior_epred(f4)), 1, mean),
  integerResponse = T,
  method = "traditional")

plot(model.check_f4)
# normality and outlier issues

testOutliers(model.check_f4, type="bootstrap") # too many
testDispersion(model.check_f4) # OK
testZeroInflation(model.check_f4) # yes
pp_check(f4,type = "error_scatter", ndraws = 50)
### lets go with f4 - seems the best, some normality issues still

plot(f4)



