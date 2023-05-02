# Make files showing sampling effort for overall plots

# Compare brms models
library(tidyverse)

# Silver perch
key_species <- c("Murray cod", "Freshwater catfish", "Golden perch",
                 "Silver perch", "Common carp", "Macquarie perch", "Bony herring", "Australian smelt")

#j=2
dat_list <- list()

for(j in 1:length(key_species)){
  mydata <- read.csv("All e-catch_30_11_2022_WRPA_fixed.csv") %>% mutate(project_segment = paste0(ProjectName,":",SegmentName))
  
  segments <- mydata %>% distinct(project_segment) %>% arrange(project_segment)
  
  bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
                "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
                "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
                "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
                "Murray Cod Slot Limit Assessment:2020/Extra")
  
  mydata <- mydata %>% filter(!project_segment %in% bad_list)
  
  if(j %in% c(1,3,4,7,8)){
    mydata <- mydata %>% filter(Method == "BTE")
  }
  #%>%
  mydata <- mydata %>%
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
    filter(Sampling_duration>0) %>% distinct()# removes a event with sample time of zero secs - there are some negatives???
  
  table(mydata$SWWRPANAME_NEW)
  table(mydata$Method)
  table(mydata$Sampling_duration)
  
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
  
  
  
  mydata_wide$Golden <- mydata_wide[[key_species[j]]]
  
  mydata_wide$Date <- as.Date(mydata_wide$SampleDate)
  
  mydata_wide$fDate <- as.factor(as.character(mydata_wide$Date))
  
  ### Can we remove sites that never got a species of interest?
  GP_positive <- mydata_wide %>% filter(Golden != 0)
  
  mydata_wide <- mydata_wide %>% ungroup() %>% filter(SiteID %in% GP_positive$SiteID) %>%
    select(Golden, SiteID, fDate, Sampling_duration, SWWRPANAME_NEW, Date, Method) %>% drop_na()
  
  # mdw_sum <- mydata_wide %>% group_by(SiteID, fDate, Date, SWWRPANAME_NEW, Method) %>%
  #   summarise(Golden = sum(Golden),
  #             Sampling_duration = sum(Sampling_duration))# %>%
  # mutate(Date = lubridate::ymd(Date))
  
  
  sites_per_day <- mydata_wide %>% group_by(fDate, SWWRPANAME_NEW) %>% summarise(nSites = n_distinct(SiteID)) # there are some sites sampled on the same day
  
    sort(table(mydata_wide$SiteID, mydata_wide$fDate),decreasing = T)
  #mdw_sum$days_since = as.numeric(mdw_sum$Date - min(mdw_sum$Date))
  mydata_wide$days_since = as.numeric(mydata_wide$Date - min(mydata_wide$Date))
  mydata_wide <- mydata_wide %>% dplyr::select(Date, SWWRPANAME_NEW) %>% distinct(.keep_all = T) %>% mutate(CommonName = key_species[j])
  dat_list[[j]] <- mydata_wide
}
dat_all <- bind_rows(dat_list)
write_csv(dat_all, "Sampling Effort for plots WRPA_boat.csv")
