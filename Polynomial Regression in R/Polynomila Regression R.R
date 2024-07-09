# Polynomial Regression

# Data Preprocessing

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]


# No splitting because the dataset is very small (just 10 lines)

# #Splitting the data into the Training and Test set
# #install.packages('caTools')
# library('caTools')
# set.seed(123)
# #primer parametro es y, la variable dependiente,
# #segundo el tamaño del training set. devuelve true si va a training set y false si va a test set
# split = sample.split(dataset$Purchased, SplitRatio=0.8) 
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Fitting Linear Regression to the dataset
lin_reg = lm(formula= Salary ~.,
             data = dataset)
summary(lin_reg)

# Fitting Polynomial Regression to the dataset
dataset$Level2sq = dataset$Level^2 #creating new columns in a dataset
dataset$Level3sq = dataset$Level^3
dataset$Level4sq = dataset$Level^4

poly_reg = lm(formula= Salary ~.,
             data = dataset)
summary(poly_reg)


# Visualising the Linear Regression results
library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line (aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
             colour = 'blue') +
  ggtitle('Linear Regression') +
  xlab('Level')+
  ylab('Salary')



# Visualising the Polynomial Regression results
library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line (aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
             colour = 'blue') +
  ggtitle('Polynomial Regression') +
  xlab('Level')+
  ylab('Salary')

# Predicting a new result with Linear Regression

y_pred = predict(lin_reg, data.frame(Level = 6.5)) #adding a level (value) not existing before


# Predicting a new result with Polynomial Regression
y_pred = predict(poly_reg, data.frame(Level = 6.5, 
                                      Level2sq = 6.5^2,
                                      Level3sq = 6.5^3,
                                      Level4sq = 6.5^4))
