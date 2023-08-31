replication <- read.csv(file = "data/HomeworkOneDataSet.csv")
require(tidyverse)
summary(replication)
str(replication)

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

function(r)
{
  r + 1
}

replication %>%
  select(WouldReccomend, HaveReplicated) %>%
  na.omit() %>% 
  group_by(WouldReccomend) %>%
  mutate(if(WouldReccomend == 1 && HaveReplicated == 1){ReplicatedAndReccomended = 1}else{ReplicatedAndReccomended = 0})
  # summarise(frequency = n()) %>%


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


table(round(replication$Country)/length(replication$Country), digits = 2)

# Amount of respondents who would recommend replicating research to others.
table(replication$WouldReccomend)/length(replication$WouldReccomend)

# 1           2           No response 
# 0.91150442  0.04424779  0.02359882

# table(replication$WantToReplicate)/length(replication$WantToReplicate)

#dplyr is more intuitive

starwars %>%
  select(height, mass, gender, species) %>%
  filter(species == "Human") %>%
  na.omit() %>%
  mutate(height = height / 100) %>%
  mutate(bmi = mass / height^2) %>%
  group_by(gender) %>% 
  summarise(Average_BMI = mean(bmi))



