replication <- read.csv(file = "data/HomeworkOneDataSet.csv")
require(tidyverse)
summary(replication)
str(replication)

# There are four primary assumptions in ANOVA:
#   
# -The responses for each factor level have a normal population distribution.
# -These distributions have the same variance.  (Ran a Lavene's Pr (>F)=.26 which is less than 1)
# -The data are independent (Maybe a problem if he connected data at conference)
# -No significant outliers in any cell of the design (outliers exist... fuck)

#I have renamed the variables so they are easier to work with

# $ Participant                : int  1 3 4 5 7 8 9 10 11 12 ...
# $ WouldReccomend             : chr  "2" "2" "1" "1" ...
# $ LackOriginality            : int  55 51 19 0 40 40 83 89 38 100 ...
# $ GeneralizePrevious         : int  80 39 50 100 90 100 100 63 55 47 ...
# $ QuestionOriginal           : int  53 40 51 100 80 80 78 81 41 100 ...
# $ NotPriority                : int  56 85 21 53 80 94 90 90 7 100 ...
# $ BuildReputation            : int  72 24 20 74 60 84 7 27 55 49 ...
# $ VerifyResults              : int  75 50 51 100 20 82 95 80 36 100 ...
# $ NegativePublisherEvaluation: int  59 50 23 NA 30 86 39 87 60 75 ...
# $ ExpandResults              : int  67 50 95 83 90 88 100 77 52 90 ...
# $ EstablishKnowledge         : int  65 70 82 52 50 85 68 91 48 100 ...
# $ LearnMethods               : int  63 80 88 88 85 85 97 85 45 100 ...
# $ RelevantResearch           : int  56 70 83 97 91 65 54 84 80 100 ...
# $ StrengthenDiscipline       : int  55 80 86 100 86 77 80 60 62 100 ...
# $ ConsolidateResults         : int  56 50 84 100 46 83 89 81 62 100 ...
# $ ValuableToField            : int  59 70 85 100 100 81 58 78 80 100 ...
# $ LackInnovation             : int  36 50 19 0 13 28 43 79 42 96 ...
# $ IncreaseOrDecrease         : int  3 3 3 3 3 1 3 2 3 3 ...
# $ HaveReplicated             : int  1 0 0 1 0 1 1 1 0 1 ...
# $ WantToReplicate            : chr  "" "Yes" "Yes" "" ...
# $ Country                    : chr  "China" "China" "China" "China" ...
# $ PublishPerYear             : int  1 0 NA NA NA 0 0 1 NA 2 ...
# $ CoursesTaken               : int  2 2 1 1 2 0 2 1 5 0 ...
# $ ResearchType               : int  3 4 4 2 2 NA 4 3 2 2 ...

# Where respondents were from
# rev(sort(table(replication$Country)))

# USA                   United Kingdom 
# 130                               22 
# Germany                       Brazil 
# 21                               20 
# Canada                        China 
# 19                               14 
# Spain                       Belgium 
# 11                                9 
# Netherlands                   Japan 
# 8                                8 

# switched to dplyr rev(sort(table(round(((replication$Country)/length(replication$Country)), digits = 2)))


topCountries <-  replication %>%
  select(Country) %>% 
  group_by(Country) %>%
  na.omit() %>% 
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

# I want to use more functions and classes in R.  Any tips how to do so?

function(r)
{
  r + 1
}
# How to add logical statements to mutate
# https://stackoverflow.com/questions/24459752/can-dplyr-package-be-used-for-conditional-mutating


library(scales)
replication %>%
  select(WouldReccomend, HaveReplicated) %>%
  mutate(ReplicatedAndReccomended = case_when(
    WouldReccomend == 1 & HaveReplicated == 1 ~ 1,
    WouldReccomend == 0 & HaveReplicated == 1 ~ 0)) %>% 
  na.omit() %>%
  summarise(PercentOfDoneAndReccomend = 
              percent(round(mean(ReplicatedAndReccomended),digits=2)))

# Almost all individuals who had completed a replication study would
# reccomend it to others... kind of obvious.

# PercentOfDoneAndReccomend
# 1                       98%

#dplyr makes working with data much easier.

topCountries <- as.data.frame(topCountries)

# Country        frequency
# <chr>              <int>
# 1 USA                  130
# 2 United Kingdom        22
# 3 Germany               21
# 4 Brazil                20
# 5 Canada                19
# 6 China                 14
# 7 Spain                 11
# 8 Belgium                9
# 9 Japan                  8
# 10 Netherlands           8


# Amount of respondents who would recommend replicating research to others.
table(replication$WouldReccomend)/length(replication$WouldReccomend)

# 1           2           No response 
# 0.91150442  0.04424779  0.02359882

# table(replication$WantToReplicate)/length(replication$WantToReplicate)

#dplyr is more intuitive

publishOrPerish <- replication %>%
  select(PublishPerYear) %>%
  na.omit() %>% 
  # summarize(AveragePerYear = mean(PublishPerYear)) %>% 
  group_by(PublishPerYear) %>%
  summarise(amount = n()) %>%
  arrange(desc(amount))

publishOrPerish


# Of the sample, average number of papers published per person per year.
# AveragePerYear
# 1       1.711191

#Subtract 50 from all values to centralize, then make responses to negative
# statements negative so all agreement flows in the same direction.
# Three or four were excluded due to neutrality.

#Grouped by Zero Publications, Average or Below Publications and Above Average

PerceptionCentralized <- replication %>%
  select(c(LackOriginality, NotPriority, BuildReputation, VerifyResults, 
           NegativePublisherEvaluation, ExpandResults, RelevantResearch, 
           StrengthenDiscipline, ValuableToField, LackInnovation, PublishPerYear)) %>%
  mutate(across(c("BuildReputation", "VerifyResults", "ExpandResults", 
                  "RelevantResearch", "StrengthenDiscipline", 
                  "ValuableToField", "LackOriginality", "NotPriority", 
                  "NegativePublisherEvaluation", "LackInnovation"), 
                ~replace(., is.na(.), 50))) %>%
  na.omit() %>% #this omit removes any rows that have an NA in PublishPerYear
  mutate(across(c("BuildReputation", "VerifyResults", "ExpandResults", 
                  "RelevantResearch", "StrengthenDiscipline", 
                  "ValuableToField",), ~ .x - 50)) %>% 
  mutate(across(c("LackOriginality", "NotPriority", 
                  "NegativePublisherEvaluation", "LackInnovation"),
                ~ -(.x - 50))) %>%
  mutate(OverallPerception = (rowSums(.))/10) %>%
  mutate(publicationTier = case_when(
    PublishPerYear == 0 ~ "NoPublications",
    PublishPerYear != 0 & PublishPerYear <= 2 ~ "AverageOrBelow", 
    PublishPerYear > 2 ~ "AboveAverage")) %>% 
    group_by(publicationTier) #%>%
  # summarize(ReplicationSentiment = mean(OverallPerception))

# publicationTier ReplicationSentiment
# <chr>                          <dbl>
# 1 AboveAverage                    22.3
# 2 AverageOrBelow                  19.7
# 3 NoPublications                  19.8

PerceptionCentralized  

library(car)

# Using leveneTest()
result = leveneTest(OverallPerception ~ publicationTier, PerceptionCentralized)

# print the result
print(result)

WhatIsNormal <- PerceptionCentralized %>%
  select(OverallPerception, publicationTier)

shapiro.test(PerceptionCentralized$OverallPerception)
# data:  PerceptionCentralized$OverallPerception
# W = 0.97823, p-value = 0.00031
#Overall is not normal....

library(tidyr)
spreadNormals <- spread(WhatIsNormal, publicationTier, OverallPerception)


library(ggpubr)
ggqqplot(PerceptionCentralized$OverallPerception)
#This looks like it's not good for Overall

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   2  1.3398 0.2636
#       274  

#So, the variance is close enough to run an Anova

TierOverallPerception <- PerceptionCentralized %>%
  select(publicationTier, OverallPerception)

AnovaPublish <- aov(OverallPerception ~ publicationTier, data = TierOverallPerception)

summary(AnovaPublish)

#                   Df Sum Sq Mean Sq F value Pr(>F)
# publicationTier   2    360   180.0   1.016  0.363
# Residuals       274  48526   177.1       

#Could not reject the null hypothesis.

boxplot(OverallPerception ~ publicationTier, data = TierOverallPerception, 
        ylab = "Overall Sentiment", xlab = "Publication Tiers")

# library("ggpubr")
# ggboxplot(TierOverallPerception, x = "publicationTier", y = "OverallPerception", 
#           color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
#           order = c("AboveAverage", "AverageOrBelow", "NoPublications"), 
#           ylab = "Amount of Publications Per Year", xlab = "Overall Sentiment")

###9/3/2023: mutate can be used together with across and concatenate
###to apply functions to each cell in the desired variables.  rowSums() is great
###for adding up a row.  I need to find a way to do na omission based on specific
###columns.  

#666666666666666666666666666666  Ran same code again with 6 gropus 1 way anove

PerceptionCentralizedSixGroups <- replication %>%
  select(c(LackOriginality, NotPriority, BuildReputation, VerifyResults, 
           NegativePublisherEvaluation, ExpandResults, RelevantResearch, 
           StrengthenDiscipline, ValuableToField, LackInnovation, PublishPerYear)) %>%
  mutate(across(c("BuildReputation", "VerifyResults", "ExpandResults", 
                  "RelevantResearch", "StrengthenDiscipline", 
                  "ValuableToField", "LackOriginality", "NotPriority", 
                  "NegativePublisherEvaluation", "LackInnovation"), 
                ~replace(., is.na(.), 50))) %>%
  na.omit() %>% #this omit removes any rows that have an NA in PublishPerYear
  mutate(across(c("BuildReputation", "VerifyResults", "ExpandResults", 
                  "RelevantResearch", "StrengthenDiscipline", 
                  "ValuableToField",), ~ .x - 50)) %>% 
  mutate(across(c("LackOriginality", "NotPriority", 
                  "NegativePublisherEvaluation", "LackInnovation"),
                ~ -(.x - 50))) %>%
  mutate(OverallPerception = (rowSums(.))/10) %>%
  group_by(PublishPerYear) #%>%

# PublishPerYear ReplicationSentiment
#               <int>             <dbl>
# 1              0                 19.8
# 2              1                 20.4
# 3              2                 18.7
# 4              3                 24.7
# 5              4                 18.4
# 6              5                 19.5

SixGroupsOverallPerception <- PerceptionCentralizedSixGroups %>%
  select(PublishPerYear, OverallPerception)

AnovaPublishSixGroups <- aov(OverallPerception ~ PublishPerYear, data = SixGroupsOverallPerception)

summary(AnovaPublishSixGroups)

boxplot(OverallPerception ~ PublishPerYear, data = SixGroupsOverallPerception, 
        ylab = "Overall Sentiment", xlab = "Number of Publications")



# Df Sum Sq Mean Sq F value Pr(>F)
# publicationTier   2    360   180.0   1.016  0.363
# Residuals       274  48526   177.1     

#The sample data that comes with some of these packages is great.

  

starwars %>%
  select(height, mass, gender, species) %>%
  filter(species == "Human") %>%
  na.omit() %>%
  mutate(height = height / 100) %>%
  mutate(bmi = mass / height^2) %>%
  group_by(gender) %>% 
  summarise(Average_BMI = mean(bmi))

