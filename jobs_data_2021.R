#############################################################################
# Analysis of data science jobs                                             #
#                                                                           #
# Author: Sandro Mauricio Peirano                                           #
#                                                                           #
# Description: Exploratory analysis and application of predictive modelling #
# to have a better understanding at 2021 data science market.               #
#############################################################################

# Load libraries

library(tidyverse) 
library(MASS)
library(leaps)
library(glmnet)

# Set directory and read data
setwd("C:/Users/Mauca/Google Drive/Trabajo/Varios")

data <- read_csv("data_cleaned_2021.csv")

# Rename names that might causa problems

data <- data %>% rename("lower_salary" = "Lower Salary")
data <- data %>% rename("upper_salary" = "Upper Salary")
data <- data %>% rename("avg_salary"   = "Avg Salary(K)")

#######################
# Exploratoy analysis #
#######################

names(data)
dim(data)

# Summaries
summary(data[,c("lower_salary","upper_salary","avg_salary")])
summary(data[data$Rating != -1,c("Rating")])
summary(data[data$Age != -1,c("Age")])
sort(round(apply(data[,24:38], 2 , FUN = mean) * 100,2),
     decreasing = TRUE)

# Creating variable: Total knowledge (nº of programs required)
data$total_know = rowSums(data[24:38])

ggplot(data, aes(x = total_know)) +
  geom_histogram(binwidth = 1, color = "black", fill = "dark orange",
                 center = 0.5) +
  scale_x_continuous(breaks = seq(0,15,1)) +
  scale_y_continuous(expand = c(0,0)) +
  xlab("Number of programs required") +
  ylab("Frequency") +
  labs(title = "Histogram - Informatic knowledge") +
  coord_cartesian(ylim = c(-2,260)) +
  theme_light()

ggplot(data, aes(x = total_know, y = lower_salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(data$total_know, cbind(data$lower_salary,
                           data$avg_salary,
                           data$upper_salary))
  
ggplot(data, aes(x = total_know, y = upper_salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Creating variable: Better than average salaries

  # Lower bound
    data$best_lower = rep(0, nrow(data))
    data$best_lower = (data$lower_salary>mean(data$lower_salary))
    data$best_lower = as.numeric(data$best_lower)
    data$best_lower = factor(data$best_lower, levels = c(0,1),
                             labels = c("Below","Above"))
  # Upper bound
    data$best_upper = rep(0, nrow(data))
    data$best_upper = (data$upper_salary>mean(data$upper_salary))
    data$best_upper = as.numeric(data$best_upper)
    data$best_upper = factor(data$best_upper, levels = c(0,1),
                             labels = c("Below","Above"))

summary(data[,44:45])

########################
# Predictive modelling #
########################

# Linear regression - which programs contribute the most to salary?     
###################################################################

## Lower salary
###############

df_linear <- data %>% dplyr::select("lower_salary","upper_salary",
                                    "avg_salary" ,24:38)


linear_fit <- lm(lower_salary ~ . - upper_salary - avg_salary,
                 data = df_linear)

summary(linear_fit)
names(summary(linear_fit)$coefficients[,4][summary(
  linear_fit
  )$coefficients[,4] < 0.05]) # Significant variables

## Selection of best subset of variables

set.seed(2022)
train <- sample(c(TRUE, FALSE), nrow(data),replace = TRUE)
test  <- (!train)
          
subset <- regsubsets(lower_salary ~ . - upper_salary - avg_salary,
                     data = df_linear[train,], nvmax = 15)

x_linear_test <- model.matrix(lower_salary ~ . - upper_salary - avg_salary,
                              data = df_linear[test,])

val_errors <- rep(NA, 15)
for (i in 1:15) {
  coefi = coef(subset,i)
  y = x_linear_test[,names(coefi)] %*% coefi
  val_errors[i] = mean((y - data$lower_salary[test])^2)
}

which.min(val_errors)
plot(val_errors, xlab = "Nº of predictors", ylab = "MSE", type = "l",
     main = "Number of variable selection - MSE")
points(which.min(val_errors), val_errors[which.min(val_errors)],
       col = "red", cex = 1.5, pch = 20)

print(paste0("Number of variables: ",which.min(val_errors)))

names(coef(subset,6))

best_linear_model <- lm(data$lower_salary ~ Python + aws + sql + sas + tableau
                        + scikit,
                        data = df_linear) 

summary(best_linear_model)

predict(best_linear_model, data.frame(
  Python = 1,
  aws = 0,
  sql = 0,
  sas = 1,
  tableau = 0,
  scikit = 1
))


# Upper salary
##############

linear_fit <- lm(upper_salary ~ . - lower_salary - avg_salary,
                 data = df_linear)

summary(linear_fit)
names(summary(linear_fit)$coefficients[,4][summary(
  linear_fit
)$coefficients[,4] < 0.05]) # Significant variables

## Selection of best subset of variables ##

subset <- regsubsets(upper_salary ~ . - lower_salary - avg_salary,
                     data = df_linear[train,], nvmax = 15)

x_linear_test <- model.matrix(upper_salary ~ . - lower_salary - avg_salary,
                              data = df_linear[test,])

val_errors <- rep(NA, 15)
for (i in 1:15) {
  coefi = coef(subset,i)
  y = x_linear_test[,names(coefi)] %*% coefi
  val_errors[i] = mean((y - data$upper_salary[test])^2)
}

which.min(val_errors)
plot(val_errors, xlab = "Nº of predictors", ylab = "MSE", type = "l",
     main = "Number of variable selection - MSE")
points(which.min(val_errors), val_errors[which.min(val_errors)],
       col = "red", cex = 1.5, pch = 20)

print(paste0("Number of variables: ",which.min(val_errors)))

names(coef(subset,8))

best_linear_model <- lm(data$upper_salary ~ Python + aws + sql + sas + 
                          pytorch + scikit + tensor + tableau,
                        data = df_linear) 

summary(best_linear_model)
coef(best_linear_model)

predict(best_linear_model, data.frame(
  Python = 1,
  aws = 0,
  sql = 0,
  sas = 1,
  pytorch = 0,
  scikit = 1,
  tensor = 1,
  tableau = 0
))

# Average salary
################

linear_fit <- lm(avg_salary ~ . - lower_salary - upper_salary,
                 data = df_linear)

summary(linear_fit)
names(summary(linear_fit)$coefficients[,4][summary(
  linear_fit
)$coefficients[,4] < 0.05]) # Significant variables

## Selection of best subset of variables ##

subset <- regsubsets(avg_salary ~ . - lower_salary - upper_salary,
                     data = df_linear[train,], nvmax = 15)

x_linear_test <- model.matrix(avg_salary ~ . - lower_salary - upper_salary,
                              data = df_linear[test,])

val_errors <- rep(NA, 15)
for (i in 1:15) {
  coefi = coef(subset,i)
  y = x_linear_test[,names(coefi)] %*% coefi
  val_errors[i] = mean((y - data$upper_salary[test])^2)
}

which.min(val_errors)
plot(val_errors, xlab = "Nº of predictors", ylab = "MSE", type = "l",
     main = "Number of variable selection - MSE")
points(which.min(val_errors), val_errors[which.min(val_errors)],
       col = "red", cex = 1.5, pch = 20)

print(paste0("Number of variables: ",which.min(val_errors)))

names(coef(subset,11))

best_linear_model <- lm(data$upper_salary ~ Python + aws + excel + sql +
                        sas + pytorch + scikit + tensor + tableau + bi +
                        flink, data = df_linear) 

summary(best_linear_model)
coef(best_linear_model)

predict(best_linear_model, data.frame(
  Python = 1,
  aws = 0,
  excel = 0,
  sql = 0,
  sas = 0,
  pytorch = 0,
  scikit = 0,
  tensor = 0,
  tableau = 0,
  bi = 0,
  flink =0
))


# Including interactions for lower salary #
###########################################

x_mod <- model.matrix(lower_salary ~ (. - upper_salary - avg_salary)^2,
                      data = df_linear)
y_mod <- data$lower_salary
  
# Ridge regression
##################

inter_fit <- lm(lower_salary ~ (. - upper_salary - avg_salary)^2 ,
                     data = df_linear)
summary(inter_fit)

grid <-10^seq(-2,2, length.out = 100) 

ridge_intr_fit <- glmnet(x_mod, y_mod, alpha = 0, lambda = grid)
summary(ridge_intr_fit)

set.seed(2)
bestlam <- cv.glmnet(x_mod, y_mod, alpha = 0)$lambda.min
plot(cv.glmnet(x_mod, y_mod, alpha = 0))

ridge_intr_fit <- glmnet(x_mod, y_mod, alpha = 0, lamda = bestlam)
pred <- predict(ridge_intr_fit, s = bestlam, x_mod[test,])

mse_ridge <- mean((data$lower_salary[test] - pred)^2)

# Lasso regression
##################

plot(glmnet(x_mod, y_mod, alpha = 1, lambda = grid))

set.seed(2)
bestlam <- cv.glmnet(x_mod, y_mod, alpha = 1)$lambda.min
plot(cv.glmnet(x_mod, y_mod, alpha = 1))

lasso_intr_fit <- glmnet(x_mod, y_mod, alpha = 1, lamda = bestlam)
pred <- predict(lasso_intr_fit, s = bestlam, x_mod[test,])

mse_lasso <- mean((data$lower_salary[test] - pred)^2)

best_model <- glmnet(x_mod, y_mod, alpha = 1, lambda = bestlam)
lasso_coef <- coef(best_model)

print(paste0("Number of variables that are zero: ",
             length(coef(best_model)[coef(best_model) == 0])-1))
print(paste0("Number of variables left: ",
             length(coef(best_model)[coef(best_model) != 0])))

lasso_coef <- tibble(
  names = lasso_coef@Dimnames[[1]],
  coef = as.numeric(coef(best_model)))

lasso_coef_notzero <- lasso_coef[lasso_coef$coef != 0,]
view(lasso_coef_notzero)
      
