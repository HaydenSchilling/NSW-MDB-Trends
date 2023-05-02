### Explore bio data length weight


library(tidyverse)

### select Murray cod
key_species <- c("Murray cod", "Golden perch", "Silver perch", "Freshwater catfish", "Macquarie perch", "Common carp")

mydata <- read_csv("Bio data/All electro Bio 20_12_2022.csv") %>% filter(CommonName %in% key_species) # %>%
 # filter(CommonName == "Murray cod") %>% filter(Length_mm != 0)


carp <- mydata %>% filter(CommonName == "Common carp")
murray <- mydata %>% filter(CommonName == "Murray cod")
golden <- mydata %>% filter(CommonName == "Golden perch")
silver <- mydata %>% filter(CommonName == "Silver perch")
mq <- mydata %>% filter(CommonName == "Macquarie perch")
catfish <- mydata %>% filter(CommonName == "Freshwater catfish")

# bad <- list()
# bad[[1]] <- mydata %>% filter(FishWeight == 0 | Length_mm ==0)
# bad[[2]] <- mydata %>% filter(CommonName == "Common carp" & FishWeight >15000)
# bad[[3]] <- mydata %>% filter(CommonName == "Freshwater catfish" & FishWeight > 2*CalcWeight) %>% filter(FishWeight>100)
# bad[[4]] <- golden %>% filter(FishWeight > 3*CalcWeight) %>% filter(FishWeight>80)
# bad[[5]] <- golden %>% filter(FishWeight < CalcWeight/3) %>% filter(FishWeight>80)
# bad[[6]] <- murray %>% filter(FishWeight > 10*CalcWeight) %>% filter(FishWeight > 100)
# bad[[7]] <- murray %>% filter(FishWeight < CalcWeight/10) %>% filter(Length_mm > 50)
# bad[[8]] <- silver %>% filter(Length_mm < 30 |(FishWeight<1 & Length_mm >200))
# bad[[9]] <- carp %>% filter(Length_mm <10 & FishWeight > 1000)
# bad[[10]] <- carp %>% filter(FishWeight > 20*CalcWeight)
# bad[[11]] <- golden %>% filter(FishWeight<100 & Length_mm>300)
# bad[[12]] <- carp %>% filter(FishWeight <3000 & Length_mm >900)

#bad <- bind_rows(bad)
#write_csv(bad, "Bio data/bad records/bad length weight records.csv")
bad <- read_csv("Bio data/bad records/bad length weight records.csv") %>% select(BiologicalRecordID)

mydata <- mydata %>% anti_join(bad)
write_csv(mydata, "Bio data/combined bio data all electro clean Dec22.csv")

sites <- read_csv("Bio data/Site details.csv") %>% select(SiteID, SiteName, SampleLatitude, SampleLongitude)# %>% 

mydata <- mydata %>% left_join(sites)

#summary(mydata$a) # -5.112
#summary(mydata$b) # 3.083
table(mydata$LengthFormat)

ggplot(mydata, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free")+
  scale_x_log10() + scale_y_log10() +
  geom_point(data=bad, col="blue")


ggplot(mydata, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free") +
  geom_point(data=bad, col="blue")#+
  #scale_x_log10() + scale_y_log10()



### Muray cod
library(tidyverse)

### select Murray cod
key_species <- c("Murray cod", "Golden perch", "Silver perch", "Freshwater catfish", "Macquarie Perch", "Common carp")

mydata <- read_csv("Bio data/combined bio data all electro clean Dec22.csv") %>% filter(CommonName %in% key_species)  %>%
  filter(CommonName == "Murray cod") #%>% filter(Length_mm != 0)

sites <- read_csv("Bio data/Site details.csv") %>% select(SiteID, SiteName, SampleLatitude, SampleLongitude)# %>% 

mydata <- mydata %>% left_join(sites)

#summary(mydata$a) # -5.112
#summary(mydata$b) # 3.083
table(mydata$LengthFormat)

ggplot(mydata, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free")+
  scale_x_log10() + scale_y_log10()

mydata <- mydata %>%  mutate(FishWeight = case_when(FishWeight == 0 ~ NA_real_,
                                                  T ~ FishWeight))

mydata2 <- mydata %>% drop_na(FishWeight, Length_mm)

f2 <- lm(log(FishWeight) ~ log(Length_mm), data=mydata2)
#plot(f2)

summary(f2)

## bias correction see http://derekogle.com/fishR/examples/oldFishRVignettes/LengthWeight.pdf
syx <- summary(f2)$sigma
( cf <- exp((syx^2)/2) )

cf

mydata2$PredWBias = predict(f2, newdata = mydata2)
mydata2$bias.pred.orig <- exp(mydata2$PredWBias)
mydata2$Pred_W_Fixed <- cf*mydata2$bias.pred.orig 



ggplot(mydata2, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free")+
  geom_point(aes(y=Pred_W_Fixed), col="blue")#+
  #scale_x_log10() + scale_y_log10()


mydata$PredWBias = predict(f2, newdata = mydata)
mydata$bias.pred.orig <- exp(mydata$PredWBias)
mydata$Pred_W_Fixed <- cf*mydata$bias.pred.orig 



dat_sum <- mydata %>% group_by(SiteID, SiteName, SampleDate) %>% summarise(mean_W = mean(FishWeight, na.rm=T),
                                                               #meanCW = mean(CalcWeight, na.rm=T),
                                                               meanBCW = mean(Pred_W_Fixed, na.rm=T))

write_csv(dat_sum, "Bio data/Murray cod operation mean weights.csv")

plot(dat_sum$mean_W, dat_sum$meanCW)
plot(dat_sum$meanBCW, dat_sum$meanCW)

cor(dat_sum$mean_W, dat_sum$meanCW, use = "complete.obs")
cor(dat_sum$meanBCW, dat_sum$meanCW, use = "complete.obs")


### Explore bio data length weight

library(tidyverse)

### select Golden perch
key_species <- c("Murray cod", "Golden perch", "Silver perch", "Freshwater catfish", "Macquarie Perch")

mydata <- read_csv("Bio data/combined bio data all electro clean Dec22.csv") %>% filter(CommonName %in% key_species)  %>%
  filter(CommonName == "Golden perch") %>% filter(Length_mm != 0)

sites <- read_csv("Bio data/Site details.csv") %>% select(SiteID, SiteName, SampleLatitude, SampleLongitude)# %>% 

mydata <- mydata %>% left_join(sites)

#summary(mydata$a) # -5.112
#summary(mydata$b) # 3.083
table(mydata$LengthFormat)

ggplot(mydata, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free")+
  scale_x_log10() + scale_y_log10()

mydata <- mydata %>%  mutate(FishWeight = case_when(FishWeight == 0 ~ NA_real_,
                                                  T ~ FishWeight))

# 
# library(brms)
# prior1 <- prior(normal(3.1,0.1), nlpar = "b", lb=0) +
#   prior(normal(0,0.5), nlpar = "a") 
# 
# f1 <- brm(bf((FishWeight ~ a*Length_mm^b),
#              a + b ~1,
#              nl = TRUE),
#           data=mydata,
#           prior=prior1)

mydata2 <- mydata %>% drop_na(FishWeight, Length_mm)

summary(mydata2$FishWeight)
summary(mydata2$Length_mm)

f2 <- lm(log(FishWeight) ~ log(Length_mm), data=mydata2)
#plot(f2)

summary(f2)

## bias correction see http://derekogle.com/fishR/examples/oldFishRVignettes/LengthWeight.pdf
syx <- summary(f2)$sigma
( cf <- exp((syx^2)/2) )


mydata2$PredWBias = predict(f2, newdata = mydata2)
mydata2$bias.pred.orig <- exp(mydata2$PredWBias)
mydata2$Pred_W_Fixed <- cf*mydata2$bias.pred.orig 



ggplot(mydata2, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free")+
  geom_point(aes(y=Pred_W_Fixed), col="blue")#+
#scale_x_log10() + scale_y_log10()


mydata$PredWBias = predict(f2, newdata = mydata)
mydata$bias.pred.orig <- exp(mydata$PredWBias)
mydata$Pred_W_Fixed <- cf*mydata$bias.pred.orig 



dat_sum <- mydata %>% group_by(SiteID, SiteName, SampleDate) %>% summarise(mean_W = mean(FishWeight, na.rm=T),
                                                                     #meanCW = mean(CalcWeight, na.rm=T),
                                                                     meanBCW = mean(Pred_W_Fixed, na.rm=T))

write_csv(dat_sum, "Bio data/Golden perch operation mean weights.csv")

plot(dat_sum$mean_W, dat_sum$meanCW)
plot(dat_sum$meanBCW, dat_sum$meanCW)

cor(dat_sum$mean_W, dat_sum$meanCW, use = "complete.obs")
cor(dat_sum$meanBCW, dat_sum$meanCW, use = "complete.obs")


### Explore bio data length weight

library(tidyverse)

### select Silver perch
key_species <- c("Murray cod", "Golden perch", "Silver perch", "Freshwater catfish", "Macquarie Perch")

mydata <- read_csv("Bio data/combined bio data all electro clean Dec22.csv") %>% filter(CommonName %in% key_species)  %>%
  filter(CommonName == "Silver perch") %>% filter(Length_mm != 0)

sites <- read_csv("Bio data/Site details.csv") %>% select(SiteID, SiteName, SampleLatitude, SampleLongitude)# %>% 

mydata <- mydata %>% left_join(sites)

#summary(mydata$a) # -5.112
#summary(mydata$b) # 3.083
table(mydata$LengthFormat)

ggplot(mydata, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free")+
  scale_x_log10() + scale_y_log10()

mydata <- mydata %>%  mutate(FishWeight = case_when(FishWeight == 0 ~ NA_real_,
                                                  T ~ FishWeight))

# 
# library(brms)
# prior1 <- prior(normal(3.1,0.1), nlpar = "b", lb=0) +
#   prior(normal(0,0.5), nlpar = "a") 
# 
# f1 <- brm(bf((FishWeight ~ a*Length_mm^b),
#              a + b ~1,
#              nl = TRUE),
#           data=mydata,
#           prior=prior1)

mydata2 <- mydata %>% drop_na(FishWeight, Length_mm)

summary(mydata2$FishWeight)
summary(mydata2$Length_mm)

f2 <- lm(log(FishWeight) ~ log(Length_mm), data=mydata2)
#plot(f2)

summary(f2)

## bias correction see http://derekogle.com/fishR/examples/oldFishRVignettes/LengthWeight.pdf
syx <- summary(f2)$sigma
( cf <- exp((syx^2)/2) )


mydata2$PredWBias = predict(f2, newdata = mydata2)
mydata2$bias.pred.orig <- exp(mydata2$PredWBias)
mydata2$Pred_W_Fixed <- cf*mydata2$bias.pred.orig 



ggplot(mydata2, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free")+
  geom_point(aes(y=Pred_W_Fixed), col="blue")#+
#scale_x_log10() + scale_y_log10()


mydata$PredWBias = predict(f2, newdata = mydata)
mydata$bias.pred.orig <- exp(mydata$PredWBias)
mydata$Pred_W_Fixed <- cf*mydata$bias.pred.orig 



dat_sum <- mydata %>% group_by(SiteID, SiteName, SampleDate) %>% summarise(mean_W = mean(FishWeight, na.rm=T),
                                                                     #meanCW = mean(CalcWeight, na.rm=T),
                                                                     meanBCW = mean(Pred_W_Fixed, na.rm=T))

write_csv(dat_sum, "Bio data/Silver perch operation mean weights.csv")

plot(dat_sum$mean_W, dat_sum$meanCW)
plot(dat_sum$meanBCW, dat_sum$meanCW)

cor(dat_sum$mean_W, dat_sum$meanCW, use = "complete.obs")
cor(dat_sum$meanBCW, dat_sum$meanCW, use = "complete.obs")


### Explore bio data length weight

library(tidyverse)

### select Macquarie perch
key_species <- c("Murray cod", "Golden perch", "Silver perch", "Freshwater catfish", "Macquarie perch")

mydata <- read_csv("Bio data/combined bio data all electro clean Dec22.csv") %>% filter(CommonName %in% key_species)  %>%
  filter(CommonName == "Macquarie perch") %>% filter(Length_mm != 0)

sites <- read_csv("Bio data/Site details.csv") %>% select(SiteID, SiteName, SampleLatitude, SampleLongitude)# %>% 

mydata <- mydata %>% left_join(sites)

#summary(mydata$a) # -5.112
#summary(mydata$b) # 3.083
table(mydata$LengthFormat)

ggplot(mydata, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free")+
  scale_x_log10() + scale_y_log10()

mydata <- mydata %>%  mutate(FishWeight = case_when(FishWeight == 0 ~ NA_real_,
                                                  T ~ FishWeight))

# 
# library(brms)
# prior1 <- prior(normal(3.1,0.1), nlpar = "b", lb=0) +
#   prior(normal(0,0.5), nlpar = "a") 
# 
# f1 <- brm(bf((FishWeight ~ a*Length_mm^b),
#              a + b ~1,
#              nl = TRUE),
#           data=mydata,
#           prior=prior1)

mydata2 <- mydata %>% drop_na(FishWeight, Length_mm)

summary(mydata2$FishWeight)
summary(mydata2$Length_mm)

f2 <- lm(log(FishWeight) ~ log(Length_mm), data=mydata2)
#plot(f2)

summary(f2)

## bias correction see http://derekogle.com/fishR/examples/oldFishRVignettes/LengthWeight.pdf
syx <- summary(f2)$sigma
( cf <- exp((syx^2)/2) )


mydata2$PredWBias = predict(f2, newdata = mydata2)
mydata2$bias.pred.orig <- exp(mydata2$PredWBias)
mydata2$Pred_W_Fixed <- cf*mydata2$bias.pred.orig 



ggplot(mydata2, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free")+
  geom_point(aes(y=Pred_W_Fixed), col="blue")#+
#scale_x_log10() + scale_y_log10()


mydata$PredWBias = predict(f2, newdata = mydata)
mydata$bias.pred.orig <- exp(mydata$PredWBias)
mydata$Pred_W_Fixed <- cf*mydata$bias.pred.orig 



dat_sum <- mydata %>% group_by(SiteID, SiteName, SampleDate) %>% summarise(mean_W = mean(FishWeight, na.rm=T),
                                                                     #meanCW = mean(CalcWeight, na.rm=T),
                                                                     meanBCW = mean(Pred_W_Fixed, na.rm=T))

write_csv(dat_sum, "Bio data/Macquarie perch operation mean weights.csv")

plot(dat_sum$mean_W, dat_sum$meanCW)
plot(dat_sum$meanBCW, dat_sum$meanCW)

cor(dat_sum$mean_W, dat_sum$meanCW, use = "complete.obs")
cor(dat_sum$meanBCW, dat_sum$meanCW, use = "complete.obs")

###############################

library(tidyverse)

### select Catfish
key_species <- c("Murray cod", "Golden perch", "Silver perch", "Freshwater catfish", "Macquarie perch")

mydata <- read_csv("Bio data/combined bio data all electro clean Dec22.csv") %>% filter(CommonName %in% key_species)  %>%
  filter(CommonName == "Freshwater catfish") %>% filter(Length_mm != 0)

sites <- read_csv("Bio data/Site details.csv") %>% select(SiteID, SiteName, SampleLatitude, SampleLongitude)# %>% 

mydata <- mydata %>% left_join(sites)

#summary(mydata$a) # -5.112
#summary(mydata$b) # 3.083
table(mydata$LengthFormat)

ggplot(mydata, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free")+
  scale_x_log10() + scale_y_log10()

mydata <- mydata %>%  mutate(FishWeight = case_when(FishWeight == 0 ~ NA_real_,
                                                  T ~ FishWeight))

# 
# library(brms)
# prior1 <- prior(normal(3.1,0.1), nlpar = "b", lb=0) +
#   prior(normal(0,0.5), nlpar = "a") 
# 
# f1 <- brm(bf((FishWeight ~ a*Length_mm^b),
#              a + b ~1,
#              nl = TRUE),
#           data=mydata,
#           prior=prior1)

mydata2 <- mydata %>% drop_na(FishWeight, Length_mm)

summary(mydata2$FishWeight)
summary(mydata2$Length_mm)

f2 <- lm(log(FishWeight) ~ log(Length_mm), data=mydata2)
#plot(f2)

summary(f2)

## bias correction see http://derekogle.com/fishR/examples/oldFishRVignettes/LengthWeight.pdf
syx <- summary(f2)$sigma
( cf <- exp((syx^2)/2) )


mydata2$PredWBias = predict(f2, newdata = mydata2)
mydata2$bias.pred.orig <- exp(mydata2$PredWBias)
mydata2$Pred_W_Fixed <- cf*mydata2$bias.pred.orig 



ggplot(mydata2, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free")+
  geom_point(aes(y=Pred_W_Fixed), col="blue")#+
#scale_x_log10() + scale_y_log10()


mydata$PredWBias = predict(f2, newdata = mydata)
mydata$bias.pred.orig <- exp(mydata$PredWBias)
mydata$Pred_W_Fixed <- cf*mydata$bias.pred.orig 



dat_sum <- mydata %>% group_by(SiteID, SiteName, SampleDate) %>% summarise(mean_W = mean(FishWeight, na.rm=T),
                                                                    # meanCW = mean(CalcWeight, na.rm=T),
                                                                     meanBCW = mean(Pred_W_Fixed, na.rm=T))

write_csv(dat_sum, "Bio data/Freshwater catfish operation mean weights.csv")

plot(dat_sum$mean_W, dat_sum$meanCW)
plot(dat_sum$meanBCW, dat_sum$meanCW)

cor(dat_sum$mean_W, dat_sum$meanCW, use = "complete.obs")
cor(dat_sum$meanBCW, dat_sum$meanCW, use = "complete.obs")

###############################

library(tidyverse)

### select Carp
key_species <- c("Murray cod", "Golden perch", "Silver perch", "Freshwater catfish", "Macquarie perch", "Common carp")

mydata <- read_csv("Bio data/combined bio data all electro clean Dec22.csv") %>% filter(CommonName %in% key_species)  %>%
  filter(CommonName == "Common carp") %>% filter(Length_mm != 0)

sites <- read_csv("Bio data/Site details.csv") %>% select(SiteID, SiteName, SampleLatitude, SampleLongitude)# %>% 

mydata <- mydata %>% left_join(sites)

#summary(mydata$a) # -5.112
#summary(mydata$b) # 3.083
table(mydata$LengthFormat)

ggplot(mydata, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free")+
  scale_x_log10() + scale_y_log10()

mydata <- mydata %>%  mutate(FishWeight = case_when(FishWeight == 0 ~ NA_real_,
                                                  T ~ FishWeight))

# 
# library(brms)
# prior1 <- prior(normal(3.1,0.1), nlpar = "b", lb=0) +
#   prior(normal(0,0.5), nlpar = "a") 
# 
# f1 <- brm(bf((FishWeight ~ a*Length_mm^b),
#              a + b ~1,
#              nl = TRUE),
#           data=mydata,
#           prior=prior1)

mydata2 <- mydata %>% drop_na(FishWeight, Length_mm)

summary(mydata2$FishWeight)
summary(mydata2$Length_mm)

f2 <- lm(log(FishWeight) ~ log(Length_mm), data=mydata2)
#plot(f2)

summary(f2)

## bias correction see http://derekogle.com/fishR/examples/oldFishRVignettes/LengthWeight.pdf
syx <- summary(f2)$sigma
( cf <- exp((syx^2)/2) )


mydata2$PredWBias = predict(f2, newdata = mydata2)
mydata2$bias.pred.orig <- exp(mydata2$PredWBias)
mydata2$Pred_W_Fixed <- cf*mydata2$bias.pred.orig 



ggplot(mydata2, aes(Length_mm, FishWeight)) + geom_point() + 
  geom_point(aes(y=CalcWeight), col="red") + facet_wrap(~CommonName, scales = "free")+
  geom_point(aes(y=Pred_W_Fixed), col="blue")#+
#scale_x_log10() + scale_y_log10()


mydata$PredWBias = predict(f2, newdata = mydata)
mydata$bias.pred.orig <- exp(mydata$PredWBias)
mydata$Pred_W_Fixed <- cf*mydata$bias.pred.orig 



dat_sum <- mydata %>% group_by(SiteID, SiteName, SampleDate) %>% summarise(mean_W = mean(FishWeight, na.rm=T),
                                                                     #meanCW = mean(CalcWeight, na.rm=T),
                                                                     meanBCW = mean(Pred_W_Fixed, na.rm=T))

write_csv(dat_sum, "Bio data/Common carp operation mean weights.csv")

plot(dat_sum$mean_W, dat_sum$meanCW)
plot(dat_sum$meanBCW, dat_sum$meanCW)

cor(dat_sum$mean_W, dat_sum$meanCW, use = "complete.obs")
cor(dat_sum$meanBCW, dat_sum$meanCW, use = "complete.obs")
