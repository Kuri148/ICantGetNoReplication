library(tidyverse)
library(ggpubr)
library(scales)
library(car)

replication <- read.csv(file = "data/HomeworkOneDataSet.csv")

#---------------------------------------------------------General Comments
# In the General Data Wrangling Section, I selected the variables that I could
# set on a common scale and centralized them by subract 50 from each and then
# applying a negative to the values that would have a positive value towards
# replication if a number closer to zero was selected. All non-replies where
# originally set to 50 and then ended up being zeroed out in the equation
# to create a neutral value.

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
#-----------------------------------------------------------Code for 2.1
increaseOrDecrease <- replication %>%
  select(IncreaseOrDecrease) %>%
  replace(is.na(.), 0) %>% 
  group_by(IncreaseOrDecrease) %>%
  na.omit() %>%
  summarize(frequency = n()) %>%
  mutate(percentage = percent(frequency/sum(frequency)))

increaseOrDecrease

#---------------------------------------------------------General Data Wrangling

CentralizedPerceptionData <- replication %>%
  select(c(LackOriginality, NotPriority, BuildReputation, VerifyResults, 
           NegativePublisherEvaluation, ExpandResults, RelevantResearch, 
           StrengthenDiscipline, ValuableToField, LackInnovation, 
           HaveReplicated, ResearchType)) %>%
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
  group_by(HaveReplicated)

#-----------------------------------------HaveReplicated Specific Data Wrangling

PerceptionHaveSlice <- CentralizedPerceptionData %>%
  select(OverallPerception, HaveReplicated) %>%
  mutate(HaveReplicated = case_when(HaveReplicated == 0 ~ "No",
                                   HaveReplicated == 1 ~ "Yes"))

HaveSliceDescriptive <- PerceptionHaveSlice %>%
  select(OverallPerception, HaveReplicated) %>%
  group_by(HaveReplicated) %>%
  na.omit() %>%
  summarize(frequency = n()) %>%
  mutate(percentage = percent(frequency/sum(frequency)))

HaveSliceMean <- PerceptionHaveSlice %>%
  select(OverallPerception, HaveReplicated) %>%
  group_by(HaveReplicated) %>%
  summarize(OverallPerceptionMean = mean(OverallPerception))

#--------------------------------HaveReplicated OneWay Anova Assumption Checking

# Normality – that each sample is taken from a normally distributed population
shapiroresult1 <- shapiro.test(CentralizedPerceptionData$OverallPerception)
# Shapiro-Wilk normality test
# 
# data:  JustNo$OverallPerception
# W = 0.97527, p-value = 0.008934

# Sample independence – that each sample has been drawn independently of the 
# other samples
#TRUE

# Variance equality – that the variance of data in the different groups should be 
# the same
leveneresult1 = leveneTest(OverallPerception ~ HaveReplicated,  PerceptionHaveSlice)

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   1   0.671 0.4133
#       337       

# Your dependent variable – here, “weight”, should be continuous – that is, 
# measured on a scale which can be subdivided using increments 
# (i.e. grams, milligrams)
#TRUE


#----------------------------------------------------HaveReplicated Oneway Anova

AnovaHaveReplicated <- aov(OverallPerception ~ factor(HaveReplicated), 
                          data = CentralizedPerceptionData)
summary(AnovaHaveReplicated)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
# HaveReplicated   1   3940    3940   22.31 3.41e-06 ***
#   Residuals      337  59517     177                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# H0 Rejected .0001


#----------------------------------------------------------HaveReplicated Graphs

ggboxplot(PerceptionHaveSlice, x = "HaveReplicated", y = "OverallPerception")


# ------------------------------------------ResearchType Specific Data Wrangling

PerceptionTypeSlice <- CentralizedPerceptionData %>%
  select(OverallPerception, ResearchType) %>% 
  mutate(ResearchType = case_when(ResearchType == 1 ~ "All Quantitative",
                                  ResearchType == 2 ~ "Mostly Quantitative", 
                                  ResearchType == 3 ~ "Equal Amounts", 
                                  ResearchType == 4 ~ "Mostly Qualitative", 
                                  ResearchType == 5 ~ "All Qualitative"))

PerceptionTypeDescriptive <- PerceptionTypeSlice %>%
  select(OverallPerception, ResearchType) %>%
  group_by(ResearchType) %>%
  na.omit() %>%
  summarize(frequency = n()) %>%
  mutate(percentage = percent(frequency/sum(frequency)))

TypeSliceMean <- PerceptionTypeSlice %>%
  select(OverallPerception, ResearchType) %>%
  group_by(ResearchType) %>%
  summarize(OverallPerceptionMean = mean(OverallPerception))


# --------------------------------ResearchType One Way ANOVA Assumption Checking

# Normality – See HaveReplicated Assumptions above.


# Sample independence – See HaveReplicated Assumptions above.

# Variance equality – that the variance of data in the different groups should be 
# the same
leveneresult2 = leveneTest(OverallPerception ~ ResearchType,  PerceptionTypeSlice)
leveneresult2
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   4  0.6611 0.6194
#       313   

# Your dependent variable – See HaveReplicated Assumptions above.


#-----------------------------------------------------ResearchType One Way ANOVA

AnovaResearchType <- aov(OverallPerception ~ factor(ResearchType), 
                           data = CentralizedPerceptionData)
summary(AnovaResearchType)

#                       Df Sum Sq Mean Sq F value Pr(>F)   
# factor(ResearchType)   4   2628   657.0   3.665 0.0062 **
#   Residuals           313  56109   179.3                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# H0 Rejected .01


#------------------------------------------------------------ResearchType Graphs
PerceptionTypeSlice$ResearchType <- factor(PerceptionTypeSlice$ResearchType , 
                                           levels=c("All Quantitative", 
                                                    "Mostly Quantitative",
                                                    "Equal Amounts",
                                                    "Mostly Qualitative",
                                                    "All Qualitative"))

ggboxplot(PerceptionTypeSlice, x = "ResearchType", y = "OverallPerception")


#------------------------------------------------------------Post hoc Tukey Test
tukey.test <- TukeyHSD(AnovaResearchType)
# Tukey multiple comparisons of means
# 99% family-wise confidence level
# 
# Fit: aov(formula = OverallPerception ~ factor(ResearchType), data = CentralizedPerceptionData)
# 
# $`factor(ResearchType)`
# diff        lwr        upr     p adj
# 2-1   0.9944549  -5.876800  7.8657096 0.9895451
# 3-1   2.1367512  -5.966724 10.2402268 0.9092131
# 4-1  -6.4226190 -15.395142  2.5499036 0.1321522
# 5-1  -8.8202381 -27.702168 10.0616923 0.5413986
# 3-2   1.1422963  -5.481538  7.7661302 0.9798277
# 4-2  -7.4170739 -15.079647  0.2454988 0.0140197
# 5-2  -9.8146930 -28.110489  8.4811031 0.3982632
# 4-3  -8.5593702 -17.343856  0.2251160 0.0131148
# 5-3 -10.9569892 -29.750295  7.8363162 0.3119455
# 5-4  -2.3976190 -21.581680 16.7864414 0.9940285


plot(tukey.test)

par(cex.axis=.7)
