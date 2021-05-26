library(tidyverse)
library(lme4)
library(here)
library(rio)
library(equatiomatic)
# library(easystats)
library(performance)
library(readr)
library(here)

#now having problems importing my data
year7 <- read_csv(here("data", "year7.csv"))
 view(year7)

#I think data types (unknown) don't work for MLM, need factors.
#Tried to turn into factors, but didn't work
year7$PrimarySubstanceType.1 <- as.factor(year7$PrimarySubstanceType.1)



year7 <- year7 %>%
   mutate(subtype = ifelse(PrimarySubstanceType.1 == 13, "Tobacco",
                     ifelse(PrimarySubstanceType.1 == 14, "Alcohol", "Drugs"))) %>%
year7$Alcohol.1 <- as.factor(year7$Alcohol.1) %>%
year7$Drugs.1 <- as.factor(year7$Drugs.1) %>%
year7$Tobacco.1 <- as.factor(year7$Tobacco.1) %>%
year7$RaceEthnicity <- as.factor(year7$RaceEthnicity) %>%
year7$School <- as.factor(year7$School)


# Getting errors trying to conslidate categories,need to figure out how to make missing
# year7 <- year7 %>%
#   mutate(RaceEthnicity = ifelse(RaceEthnicity == 1, "Native",
#                                 ifelse(RaceEthnicity == 2, "Asian",
#                                 ifelse(RaceEthnicity == 3, "Latinx"))))
#
#                   ifelse(RaceEthnicity == 4, "Black",
#                                 ifelse(RaceEthnicity == 5, "White",
#                                 ifelse(RaceEthnicity == 8, "Pacific_Islander",
#                                 ifelse(RaceEthnicity == 44, "Multi"))))
#
#
#                                 # ifelse(RaceEthnicity == 6, is.na(),
#                                 # ifelse(RaceEthnicity == 7, is.na(), )

#giving me an error for district
year7 <- year7 %>%
  year7$District <- as.factor(year7$District)

#crosstabs race by ethnicity
tablesubxrace <- year7 %>%
  group_by(RaceEthnicity1, subtype) %>%
  summarise(n=n()) %>%
  spread(subtype, n)
view(tablesubxrace)


#unconditional models

uncond <- glmer(OSS_1stRef ~ 1 + (1|School), data = year7,
                family = binomial)

uncond1 <- glmer(OSS_1stRef ~ 1 + (1|School)+ (1|District), data = year7,
                family = binomial)

#look at distribution schools per district: 84% have 2 or fewer
#robust check, remove schools with only 1, run ICC again

#conditional models
drug <- glmer(OSS_1stRef ~  1 + Drugs.1  + (1|School)+ (1|District),
             data= year7,
             family = binomial)
alc <- glmer(OSS_1stRef ~  1 + Alcohol.1 + (1|School)+ (1|District),
             data= year7,
             family = binomial)
tob <- glmer(OSS_1stRef ~  1 + Tobacco.1 + (1|School)+ (1|District),
             data= year7,
             family = binomial)
sub <- glmer(OSS_1stRef ~  1 + Alcohol.1 + Drugs.1 + Tobacco.1 + (1|School),
      data= year7,
      family = binomial)
subnonest <- glm(OSS_1stRef ~  1 + Alcohol.1 + Drugs.1 + Tobacco.1 ,
                   data= year7,
                   family = binomial)

summary(subnonest)
summary(sub)
icc(sub)
confint(sub)

exp(fixef(drug))
exp(fixef(alc))
exp(fixef(tob))
exp(fixef(sub))
summary(subnonest) #find code for

#
# sub1 <- glmer(OSS_1stRef ~  1 + Alcohol.1 + Drugs.1 + (1|School),
#              data= year7,
#              family = binomial)
race <- glmer(OSS_1stRef ~  RaceEthnicity + (1|School),
              data= year7,
              family = binomial)
subxrace <- glmer(OSS_1stRef ~  1 + Alcohol.1 + Drugs.1 + Tobacco.1 +
                    RaceEthnicity + RaceEthnicity:Alcohol.1 +
                    RaceEthnicity:Drugs.1 + RaceEthnicity:Tobacco.1 +
                    (1|School),
             data= year7,
             family = binomial) #specify reference group

summary(uncond)
summary(sub)
summary(sub1)
summary(race)
summary(subxrace)

#save race as factor
#vary randomly across school/district as another step

summary(uncond1)
icc(sub, by_group = TRUE)
summary(sub1)
?icc

#New dataset restricting to only most diverse schools.
# Probably only necessary for school-level analysis
racedata <- year7[ which(year7$BLW_Sample==TRUE), ]
view(racedata)