# install library
#install.packages(tidyverse)
#install.packages('caret')
#install.packages('e1071')
#install.packages("ranger")

# Call library
library(tidyverse) # package for data tidying
library(dplyr) # package for data tidying
library(caret) # package for spliting data
library(e1071) # package for ploting confussion heat map
library(ranger) # package for randforest training 

# Set working directory at Suicidal assigment
setwd("~/Desktop/Assigment 2")

# Load dataset to Depository
suicide_data <- read.csv("suicidal data.csv")
# Renames of some of the countries to match country names
suicide_data <- suicide_data %>%
  mutate(country = fct_recode(country, "Cape Verde" = "Cabo Verde"),
         country = fct_recode(country, "Russia" = "Russian Federation"),
         country = fct_recode(country, "South Korea" = "Republic of Korea"),
         country = fct_recode(country, "Saint Vincent and the Grenadines" = "Saint Vincent and Grenadines")
  )

countries <- read.csv("countries.csv")
names(countries)[names(countries) == "country"] <- "country_code"
names(countries)[names(countries) == "name"] <- "country"

# add longtitude and lantitudes
suicide_data <- left_join(suicide_data, countries, by = "country", all.x = TRUE)
# Data tidying
names(suicide_data)[names(suicide_data) == "gdp_for_year...."] <- "yGDP"
names(suicide_data)[names(suicide_data) == "gdp_per_capita...."] <- "cGDP"
names(suicide_data)[names(suicide_data) == "age"] <- "age_group"

# rearrange the level of the age range level
suicide_data$age_group <- factor(suicide_data$age_group,levels(suicide_data$age)[c(4,1,2,3,5,6)])
# 1: 5-14 years; 2: 15-24 years; 3: 25-34 years; 4: 35-54 years; 5: 55-74 years; 6: 75+ years 
suicide_data$age_group <- as.numeric(suicide_data$age_group)
# reassign Generation
# 1 = Boomers; 2 = G.I. Generation; 3 = Generation X
# 4 = Generation Z; 5 = Millenials; 6 = Silent
suicide_data$generation_level <- as.numeric(suicide_data$generation)


# tendency of suicide classification:
# High = suicide no. > 12.82/ 100K population
# low = suicide no. < 12.82/100K population
suicide_data <- suicide_data %>%
  mutate(
    suicide_tendency_level = case_when(
      suicides.100k.pop > 12.82 ~ 1,
      suicides.100k.pop <= 12.82 ~ 0
    ),
    
    suicide_tendency = case_when(
      suicide_tendency_level == 1 ~ "High",
      suicide_tendency_level == 0 ~ "Low",
    )
  )

suicide_data$suicide_tendency <- as.factor(suicide_data$suicide_tendency)
# check whether NA in data set
summary(suicide_data$suicide_tendency)

# select variables country, sex, age_group
suicide_ML <- select(suicide_data, country, sex, age_group, suicide_tendency)

# Down sampling of the imbalanced data
down_suicide_ML <- suicide_ML %>% arrange(suicide_tendency)
keep_idx<- sample(8758:nrow(down_suicide_ML), 8757)
keep_no <- down_suicide_ML[keep_idx, ]
keep_no
sorted_suicide_ML <- rbind(keep_no, down_suicide_ML[1:8757,])
summary(sorted_suicide_ML$suicide_tendency)

# Split data for model training
set.seed(1234)
suicide_select_ML <- createDataPartition(sorted_suicide_ML$suicide_tendency, p = 0.7, list = FALSE)
train_data_ml <- suicide_ML[suicide_select_ML,]
test_data_ml <- suicide_ML[-suicide_select_ML,]

# random forest model traing
suicide_ml_rf_mod <- ranger(train_data_ml$suicide_tendency ~ .,
                  data= train_data_ml,
                  num.trees=1000,
                  mtry=ncol(train_data_ml)/3,
                  importance="impurity",
                  write.forest=TRUE,
                  min.node.size=4,
                  max.depth=6
)
suicide_ml_rf_mod
suicide_ml_rf_mod$variable.importance

vi <- tibble(x = suicide_ml_rf_mod$variable.importance, attri=attr(suicide_ml_rf_mod$variable.importance, 'name'))
vi <- vi %>% dplyr::arrange(desc(x)) 
vi %>%
  ggplot(aes(reorder(attri, x), x)) +
  geom_col() +
  coord_flip() + 
  ggtitle("Variable Importance")

# Prediction 
Rf_pred <- predict(suicide_ml_rf_mod, data=test_data_ml)
head(Rf_pred)
confusionMatrix(Rf_pred$predictions, test_data_ml$suicide_tendency, mode = 'prec_recall') 

# created file with the selected columns as the dataframe for application creation
write.csv(suicide_data, file = 'suicidedata.csv')
# save the trained rf model in rda file 
save(suicide_ml_rf_mod, file = 'suicideMLrandomforest.rda')

