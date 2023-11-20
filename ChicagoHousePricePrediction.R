# Random forest regression

# Installing and loading libs

if(!require("tidyverse")) install.packages("tidyverse", repos = "https://cloud.r-project.org")
if(!require("randomForest")) install.packages("randomForest", repos = "https://cloud.r-project.org")
if(!require("car")) install.packages("car", repos = "https://cloud.r-project.org")

library(tidyverse)
library(randomForest)
library(car)

# ------------------- Data import -------------------
dt = readr::read_csv("database/realest.csv")

# ------------------- Data cleaning -------------------

# The choice was to delete missing data
# Amount of missing information and total row in the database
cbind(na = sum(is.na(dt)), rows = nrow(dt))

# delete missing data
dt <- dt %>% drop_na()

# Number of lines removed
cbind(na = sum(is.na(dt)), rows = nrow(dt))

# ------------------- Normality test -------------------
# Significance level
SIG <- 0.05

list_names <- names(dt)
# Teste de Shapiro-Wilk 
for (name in list_names){
  result <- 
    dt[,name] %>%
    unlist() %>%
    as.numeric() %>%
    shapiro.test()
  if(result[2] > SIG){str_c(name , " = Normally distributed")}
  else{str_c(name , " = Not normally distributed")} %>%
    print()
}
rm(SIG)
rm(list_names)
rm(result)
rm(name)

# ------------------- Graphic normality test -------------------
qqPlot(dt$Price, dist='norm',envelope=.95)
qqPlot(dt$Space, dist='norm',envelope=.95)
qqPlot(dt$Tax, dist='norm',envelope=.95)

# ------------------- Correlation Test -------------------
for (i in c(2:length(dt))){
  correlation <- cor(y = dt$Price, x = dt[,i], method = "spearman")
  print(str_c(names(dt[,i]), " = ",correlation))
}
rm(i)
rm(correlation)

# Separating the model variables
dt <- dt[,c("Space", "Tax", "Price")]

# make it reproducible
set.seed(1)

# Separating 70% for training and 30% for testing
train <- dt %>% sample_frac(0.70)
test  <- anti_join(dt, train)

# ------------------- Random Forest regression -------------------
# make it reproducible
set.seed(1)

model = randomForest(
  formula = Price ~ .,
  data = train,
  ntree=800,
  nodesize = 3,
  keep.forest= TRUE, 
  random_state = 0,
  xtest = test[,1:2],
  ytest = test$Price
)
model

# ------------------- Validating the random forest regression model -------------------

# Assumptions for the Random Forest Algorithm:
#  Absence of multicollinearity;
#  Identical distribution.

# Checking multicollinearity
cor(dt$Space, dt$Tax, method = "spearman")

# Result: They all follow the same distribution, there is a low correlation, but the model remains valid.

# Database source link: https://www.kaggle.com/datasets/tawfikelmetwally/chicago-house-price/