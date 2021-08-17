# Multiple Lineaar Regression

# Data Preprocessing

# Preparing Dataset
dataset = read.csv('50_Startups.csv')

# Encoding Categorized Data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# Spliting Dataset into Training And Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Model To Training set
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting values
y_pred = predict(regressor, newdata = test_set)


# Peacticing-------------------------
#regressor1 = lm(formula = Profit ~ R.D.Spend,
 #              data = training_set)

#y1_pred = predict(regressor1, newdata = test_set)

# BAckward Elimination
regressor_be = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State ,
               data = dataset)
summary(regressor_be)


regressor_be = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend ,
                  data = dataset)
summary(regressor_be)


regressor_be = lm(formula = Profit ~ R.D.Spend + Marketing.Spend ,
                  data = dataset)
summary(regressor_be)


regressor_be = lm(formula = Profit ~ R.D.Spend ,
                  data = dataset)
summary(regressor_be)
