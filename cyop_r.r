#__________________________
#Download required packages
#__________________________

if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) 
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(e1071)) 
  install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) 
  install.packages("randomForest", repos = "http://cran.us.r-project.org")

#_____________________________________________________________
# Part 1: Downloading data, preprocessing and Data exploration
#_____________________________________________________________

# Downloading and Reading data

if(!file.exists("adult.data")){
  download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data","adult.data")
}
income_data <- read.csv("adult.data",header = FALSE)

# Set column names
colnames(income_data) <- c("age","workclass","fnlwgt","education","education_num","marital_status",
                           "occupation","relationship","race","sex","capital_gain","capital_loss", 
                           "hours_per_week","native_country","income")

# Explore data

# To get the dimension of data
dim(income_data)

# To check the structure of data
str(income_data)

# To get summary of data
summary(income_data)

# To explore the levels in each column
sapply(income_data, levels)

# Convert " ?" data to NA and the remove rows with NA
income_data <- read.csv("adult.data",na.strings = c(" ?"),header = FALSE)

income_data <- na.omit(income_data)

# Set column names again
colnames(income_data) <- c("age","workclass","fnlwgt","education","education_num","marital_status",
                           "occupation","relationship","race","sex","capital_gain","capital_loss", 
                           "hours_per_week","native_country","income")

# To get the dimension of new data
dim(income_data)

# Create Train and validation set with validation set having 10% data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead

test_index <- createDataPartition(income_data$income,p = 0.1,
                                  times = 1,list = FALSE)
trainset <- income_data[-test_index, ]
validation <- income_data[test_index,]

# To get dimension of train and validation data
dim(trainset)
dim(validation)

#___________________________________________
# Part 2: Visualization
#___________________________________________

# 1. age

# Summary of age
summary(trainset$age)

# Group age
trainset <- trainset %>% 
  mutate(
    age_group =case_when(
      age > 10 & age <= 20 ~ "17-20",
      age > 20 & age <= 30 ~ "21-30",
      age > 30 & age <= 40 ~ "31-40",
      age > 40 & age <= 50 ~ "41-50",
      age > 50 & age <= 60 ~ "51-60",
      age > 60 & age <= 70 ~ "61-70",
      age > 70 & age <= 80 ~ "71-80",
      age > 80 & age <= 90 ~ "81-90"
    ))

# This plot displays age distribution
trainset %>% ggplot(aes(age)) + 
  geom_histogram(binwidth = 5, col = "black", fill = "grey") +
  scale_x_continuous(breaks = seq(0, 95, 5)) +   
  scale_y_continuous(breaks = seq(0, 5000, 500)) +
  ggtitle("Age distribution")

# This plot displays age_group vs income
trainset %>% ggplot(aes(x = age_group,fill = income)) + 
  geom_bar(width = 0.3, col = "black") +
  scale_y_continuous(breaks = seq(0,9000,1000)) +
  ggtitle("Age - Income distribution")

# 2. workclass

# Summary of workclass
summary(trainset$workclass)

# This plot displays workclass distribution
trainset  %>% ggplot(aes(workclass)) + 
  geom_bar(col = "black", fill = "grey") +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Workclass distribution")

# This plot displays workclass vs income
trainset  %>% ggplot(aes(workclass,fill = income)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) + 
  ggtitle("Workclass - Income distribution")

# 3. Final weight

# Summary of fnlwgt
summary(trainset$fnlwgt)

# This plot displays boxplot distribution of fnlwgt
trainset %>% ggplot(aes(factor(0),fnlwgt)) + 
  geom_boxplot() + ggtitle("Final Weight distribution")

# This plot displays boxplot distribution of fnlwgt vs income
trainset %>% ggplot(aes(income,fnlwgt)) +
  geom_boxplot() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  stat_summary(fun.y = mean, geom = "point") +
  ggtitle("Final Weight - Income distribution")

# 4.Education

# Summary of education
summary(trainset$education)

# This plot displays education distribution
trainset  %>% ggplot(aes(education)) + 
  geom_bar(col = "black", fill = "grey") +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Education distribution")

# This plot displays education vs income
trainset  %>% ggplot(aes(education,fill = income)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Education - Income distribution")

# 5. Eduation Number

# Summary of education_num
summary(trainset$education_num)

# Group education and education_num by education
trainset %>% select(education,education_num) %>% 
  group_by(education_num) %>% count(education_num,education)

# 6. Marital status

#Summary of marital_status
summary(trainset$marital_status)

# This plot displays marital_status distribution
trainset  %>% ggplot(aes(marital_status)) + 
  geom_bar(col = "black", fill = "grey") +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Marital Status distribution")

# This plot displays marital_status vs income
trainset  %>% ggplot(aes(marital_status,fill = income)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Marital Status- Income distribution")

# 7. Occupation

# Summary of occupation
summary(trainset$occupation)

# This plot displays occupation distribution
trainset  %>% ggplot(aes(occupation)) + 
  geom_bar(col = "black", fill = "grey") +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Occupation distribution")

# This plot displays occupation vs income
trainset  %>% ggplot(aes(occupation,fill = income)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Occupation - Income distribution")

# 8. Relationship

# Summary of relationship
summary(trainset$relationship)

# This plot displays relationship distribution
trainset  %>% ggplot(aes(relationship)) + 
  geom_bar(col = "black", fill = "grey") +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Relationship distribution")

# This plot displays relationship vs income
trainset  %>% ggplot(aes(relationship,fill = income)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Relationship - Income distribution")

# 9. Race

# Summary of race
summary(trainset$race)

# This plot displays race distribution
trainset  %>% ggplot(aes(race)) + 
  geom_bar(col = "black", fill = "grey") +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Race distribution")

# This plot displays race vs income distribution
trainset  %>% ggplot(aes(race,fill = income)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Race - Income distribution")

# 10. Sex

# Summary of sex
summary(trainset$sex)

# This plot displays sex distribution
trainset  %>% ggplot(aes(sex)) + 
  geom_bar(col = "black", fill = "grey") +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Sex distribution")

# This plot displays sex vs income
trainset  %>% ggplot(aes(sex,fill = income)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) + 
  ggtitle("Sex - Income distribution")

# This plot displays education vs sex
trainset  %>% ggplot(aes(education,fill = sex)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle=90,hjust = 1)) + 
  ggtitle("Education - sex distribution")

# 11. Capital_gain

# Summary of capital_gain
summary(trainset$capital_gain)

# This plot displays capital gain distribution
trainset %>% ggplot(aes(capital_gain)) + 
  geom_histogram(col = "black", fill = "grey") +
  ggtitle("Capital-gain distribution")
 
# This plot displays boxplot of capital gain distribution
trainset %>% ggplot(aes(factor(0),capital_gain)) + 
  geom_boxplot() +
  ggtitle("Capital-gain distribution")

# 12. Capital_loss

# Summary of capital_loss
summary(trainset$capital_loss)

# This plot displays capital loss distribution
trainset %>% ggplot(aes(capital_loss)) + 
  geom_histogram(col = "black", fill = "grey") +
  ggtitle("Capital-loss distribution")

# This plot displays boxplot of capital loss distribution
trainset %>% ggplot(aes(factor(0),capital_loss)) + 
  geom_boxplot() +
  ggtitle("Capital-loss distribution")

# 13. Hours per week

# Summary of hour_per_week
summary(trainset$hours_per_week)

# This plot displays hours_per_week distribution
trainset %>% ggplot(aes(hours_per_week)) + 
  geom_histogram(binwidth = 5, col = "black", fill = "grey") +
  scale_x_continuous(breaks = seq(1, 99, 5)) +   
  scale_y_continuous(breaks = seq(0, 15000, 1000)) +
  ggtitle("Hours-per-week distribution")

# This plot displays hours_per_week vs income
trainset %>% ggplot(aes(x = hours_per_week,fill = income)) + 
  geom_bar(width = 0.5, col = "black") +
  scale_y_continuous(breaks = seq(0,15000,1000)) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  ggtitle("Hours-per-week - Income distribution")

# 14. Native country

# Summary of native_country
summary(trainset$native_country)

# This plot displays native country distribution
trainset  %>% ggplot(aes(native_country)) + 
  geom_bar(col = "black", fill = "grey") +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Native-country distribution")

# This plot displays native country vs income
trainset  %>% ggplot(aes(native_country,fill = income)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Native-country - Income distribution")

# 15. Income

# Summary of income
summary(trainset$income)

# To calculate proportions for each factor level
percentage <- 100 * prop.table(table(trainset$income)) 

# Display in table format
cbind(freq = table(trainset$income), percentage = percentage)

#___________________________________________
# Part 3: Data modeling
#___________________________________________

# 1. Logistic Regression

set.seed(1, sample.kind="Rounding")

# Fit data using caret package, method - glm, family - binomial
train_lr <- train(income ~ age + workclass + education + occupation + 
                    relationship+ hours_per_week + native_country +
                    race + sex + marital_status + capital_gain + capital_loss, 
                  data=trainset, 
                  method = "glm", 
                  family="binomial")

# Predict income using the above fitted model
pred_lr <- predict(train_lr,validation)

# Save results of Confusion Matrix
lr_acc <- confusionMatrix(pred_lr,validation$income)

# Add results to a table
results <- tibble(Method="Logistic Regression",  
                  Accuracy_Train = lr_acc$overall["Accuracy"],
                  F1_Train = lr_acc$byClass[7])
results %>% knitr::kable()

# 2. Support Vector Classifier

set.seed(3, sample.kind="Rounding")

## Fit data using svm from base package
svc <- svm(income ~ age + workclass + education + capital_gain +
             occupation + relationship + race + sex + capital_loss +
             hours_per_week + native_country + marital_status, 
           data=trainset)

# Predict income using the above fitted model
pred_svc <- predict(svc,validation)

# Save results of Confusion Matrix
cm_svc <- confusionMatrix(pred_svc,validation$income)

# Add results to the results table
results <- bind_rows(results, tibble(Method="Support Vector Classifier",  
                                     Accuracy_Train = cm_svc$overall["Accuracy"],
                                     F1_Train = cm_svc$byClass[7]))
results %>% knitr::kable()

# 3. Random Forest Classifier

set.seed(4, sample.kind="Rounding")

## Fit data using rf from base package
raf <- randomForest(income ~ age + workclass + education + capital_gain + 
                      occupation + relationship + race + sex + capital_loss +
                      hours_per_week + native_country + marital_status,
                    data = trainset)

# Predict income using the above fitted model
pred_raf <- predict(raf ,validation)

# Save results of Confusion Matrix
cm_raf <- confusionMatrix(pred_raf,validation$income)

# Add results to the results table
results <- bind_rows(results, tibble(Method="Random Forest Classifier",  
                                     Accuracy_Train = cm_raf$overall["Accuracy"],
                                     F1_Train = cm_raf$byClass[7]))
results %>% knitr::kable()

# 4. Gradient Boosting Classifier

set.seed(6, sample.kind="Rounding")

# Fit data using caret package, method - gbm
gbc <- train(income ~ age + workclass + education + capital_gain +
               occupation + relationship + race + sex + capital_loss +
               hours_per_week + native_country + marital_status, 
             data=trainset, 
             method = "gbm")

# Predict income using the above fitted model
pred_gbc <- predict(gbc,validation)

# Save results of Confusion Matrix
cm_gbc <- confusionMatrix(pred_gbc,validation$income)

# Add results to the results table
results <- bind_rows(results,tibble(Method="Gradient Boosting Classifier",  
                                    Accuracy_Train = cm_gbc$overall["Accuracy"],
                                    F1_Train = cm_gbc$byClass[7]))
results %>% knitr::kable()

#_______________________________________________
# Part 4: Results / Check the models on test set
#_______________________________________________

# Downloading and Reading test data

if(!file.exists("adult.test")){
  download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test","adult.test")
}
income_test <- read.csv("adult.test",header = FALSE,skip=1)

# Set column names
colnames(income_test) <- c("age","workclass","fnlwgt","education","education_num","marital_status",
                           "occupation","relationship","race","sex","capital_gain","capital_loss", 
                           "hours_per_week","native_country","income")

# explore test data

# To check the structure of test data
str(income_test)

# To get the dimension of test data
dim(income_test)

# To get summary of test data
summary(income_test)

# To explore the levels in each column of test data
sapply(income_test, levels)

# Convert " ?" data to NA and the remove rows with NA
income_test <- read.csv("adult.test",na.strings = c(" ?"),header = FALSE,skip=1)
income_test <- na.omit(income_test)

# Set column names again
colnames(income_test) <- c("age","workclass","fnlwgt","education","education_num","marital_status",
                           "occupation","relationship","race","sex","capital_gain","capital_loss", 
                           "hours_per_week","native_country","income")

# Assign levels of income to test data
levels(income_test$income)[1] <- " <=50K"
levels(income_test$income)[2] <- " >50K"

# To ensure levels of native_country in train and test data are same
levels(income_test$native_country) <- levels(trainset$native_country)

# 1. Logistic Regression

# Use test data to predict income using the above fitted logistic regression model
pred_lrtest <- predict(train_lr,income_test)

# Save results of Confusion Matrix
lr_test <- confusionMatrix(pred_lrtest,income_test$income)

# Add results to the table
test_results <- tibble(Accuracy_Test = lr_test$overall["Accuracy"],
                       F1_Test = lr_test$byClass[7])
test_results %>% knitr::kable()

# 2. Support Vector Classifier

set.seed(3, sample.kind="Rounding")

# Use test data to predict income using the above fitted svm model
pred_svctest <- predict(svc,income_test)

# Save results of Confusion Matrix
svc_test <- confusionMatrix(pred_svctest,income_test$income)

# Add results to the table
test_results <- bind_rows(test_results, tibble(  
  Accuracy_Test = svc_test$overall["Accuracy"],
  F1_Test = svc_test$byClass[7]))
test_results %>% knitr::kable()

# 3. Random Forest Classifier

set.seed(4, sample.kind="Rounding")

# Use test data to predict income using the above fitted random forest model
pred_raftest <- predict(raf,income_test)

# Save results of Confusion Matrix
raf_test <- confusionMatrix(pred_raftest,income_test$income)

# Add results to the table
test_results <- bind_rows(test_results, tibble(  
  Accuracy_Test = raf_test$overall["Accuracy"],
  F1_Test = raf_test$byClass[7]))
test_results %>% knitr::kable()

# 4. Gradient Boosting Classifier

set.seed(6, sample.kind="Rounding")

# Use test data to predict income using the above fitted gradient boosting model
pred_gbctest <- predict(gbc,income_test)

# Save results of Confusion Matrix
gbc_test <- confusionMatrix(pred_gbctest,income_test$income)

# Add results to the table
test_results <- bind_rows(test_results, tibble(  
  Accuracy_Test = gbc_test$overall["Accuracy"],
  F1_Test= gbc_test$byClass[7]))
test_results %>% knitr::kable()

# Add test results to train results table
results <- bind_cols(results,test_results)
results %>% knitr::kable()



