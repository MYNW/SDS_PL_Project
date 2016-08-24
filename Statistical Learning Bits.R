## Statistical Learning - Premier League Performance Prediction

## Load the neccesary libraries
library(glmnet)
library(leaps)
library(rio)
require(parallel)
require(data.table)
require(plyr)

## Load needed data frame
filepath = "https://github.com/MYNW/SDS_PL_Project/blob/master/SL_PL.csv"
PL_SL <- import(filepath)

## Generate a new dataframe with only the variables we need as an alternative
#PL_SL <- PL_promotion %>%
#  select(points,
#         points_last, 
#         avg_age,
#         club_transfer_ratio,
#         manager_change,
#         star_players,
#         played_internationally,
#         squad_size,
#         foreigners,
#         promotion)

## Generate our in- and outputs
x = model.matrix(points ~ . - 1, data = PL_SL)
y = PL_SL$points

## Set seed
set.seed(2016)

##--------------------------------------------------------
## Classic OLS - Might be unneccesary 
##--------------------------------------------------------

MLR <- lm(y ~ x)
MLR.sm <- summary(MLR)
MLR.rmse <- function(MLR.sm) 
  sqrt(mean(MLR.sm$residuals^2))
MLR.rmse(MLR.sm)  # RMSE = 10.81154


##--------------------------------------------------------
## Ridge regression - Standard Model
##--------------------------------------------------------

fit.ridge = glmnet(x, y, alpha = 0)
plot(fit.ridge, xvar = "lambda", label = TRUE)

# 10 fold cross validation
cv.ridge = cv.glmnet(x, y, alpha = 0, nfolds = 10)
plot(cv.ridge)
coef(cv.ridge) # Coefficients when choosing lambda by MSE

# Calculate RMSE in a new data frame
cv.ridge.df <- data.frame(mse = cv.ridge$cvm, 
                          rmse = sqrt(cv.ridge$cvm), 
                          lambda =cv.ridge$lambda)

# Plot it
ggplot(cv.ridge.df, aes( x = log(lambda), y = rmse)) + 
  geom_point() + geom_line() +
  geom_point(data = cv.ridge.df %>% 
               filter(rmse == min(rmse)),
             color = "red") + 
  labs(title = "Ridge Regression")

# Find the lambda that minimizes RMSE
best.ridge.lambda = cv.ridge.df %>% 
  filter(rmse == min(rmse))
best.ridge.lambda  # is 0.9637915

# Find the coefficients for this model
coef(cv.ridge,s= 0.9637915)


##--------------------------------------------------------
## Lasso Regression - Standard Model
##--------------------------------------------------------

# Do and plot the Lasso regression
fit.lasso = glmnet(x, y, alpha = 1)
plot(fit.lasso, xvar = "lambda", label = TRUE)

# Do 10 fold cross validation (based on MSE)
cv.lasso = cv.glmnet(x, y, alpha = 1, nfolds = 10)
plot(cv.lasso)
coef(cv.lasso)  # Foreigners and promotion are excluded 

## Run 10-Fold Cross Validation 100 times
## and get the lambda with the minimum RMSE on average over the runs.
# Create a function that returns the MSE, RMSE and lambdas for 10 k-fold cv
Lasso.Lambdas <- function(x, y) {
  cv <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
  return(data.table(MSE = cv$cvm, 
                    RMSE = sqrt(cv$cvm),
                    lambda=cv$lambda))
}

# Create a function repeats cv 100 times and returns the best lambda
OptimLambda <- function(k, x, y) {
  RMSEs <- data.table(rbind.fill(mclapply(seq(k), 
                                         function(dummy) Lasso.Lambdas(x,y))))
  return(RMSEs[, list(mean.RMSE=mean(RMSE)), lambda][order(mean.RMSE)][1]$lambda)
}

# Average RMSE per lambda
RMSE.Lambda <- function(k, x, y) {
  RMSEs <- data.table(rbind.fill(mclapply(seq(k), 
                                          function(dummy) Lasso.Lambdas(x,y))))
  return(RMSEs[, list(mean.RMSE=mean(RMSE)), lambda])
}

# Run through the code to get the optimal lambda that minimizes RMSE on average
Lasso.df = Lasso.Lambdas(x = x, y = y)
Best.Lasso.df = OptimLambda(k = 100, x = x, y = y)  # lambda is 0.3083426
RMSE.Lambda.df = RMSE.Lambda(k = 100, x = x, y = y)

# Plot the lambda that minimizes RMSE
ggplot(RMSE.Lambda.df, aes( x = log(lambda), y = mean.RMSE)) + 
  geom_point() + geom_line() +
  geom_point(data = RMSE.Lambda.df %>% 
               filter(mean.RMSE == min(mean.RMSE)),
             color = "red") + 
  labs(title = "Lasso Regression")

# Get lambda value that minimizes RMSE
best.lasso.lambda = RMSE.Lambda.df %>% 
  filter(mean.RMSE == min(mean.RMSE))
best.lasso.lambda

# What coefficients do we get?
coef(fit.lasso,s = 0.3083426)

##---------------------------------------
## Smart Monkey - Doing simple linear OLS regression. 
SLR <- lm(y ~ PL_SL$points_last)
SLR.sm <- summary(SLR)
SLR.rmse <- function(SLR.sm) 
  sqrt(mean(SLR.sm$residuals^2))
SLR.rmse(SLR.sm)  # RMSE is 13.04625


## 10-fold cross validation with simple OLS model - Possibly a better approach
folds = sample(rep(1:10, length = nrow(PL_SL)))
folds
table(folds)
cv.errors = matrix(NA, 10)
for (k in 1:10) {
  best.fit = lm(points ~ points_last, data = PL_SL[folds != k, ])
  pred = predict(best.fit, PL_SL[folds == k, ])
  cv.errors[k] = mean((PL_SL$points[folds == k] - pred)^2)
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))  # RMSE is 13.14936
rmse.cv


##--------------------------------------------------------
## New X - 8 variables, with transfer spending excluded
##--------------------------------------------------------
PL_X <- PL_SL %>%
  select(points_last, 
         avg_age,
         manager_change,
         star_players,
         played_internationally,
         squad_size,
         foreigners,
         promotion)
x1 <- data.matrix(PL_X)

##--------------------------------------------------------
## Ridge regression - No transfer ratio
##--------------------------------------------------------

fit.ridge1 = glmnet(x1, y, alpha = 0)
plot(fit.ridge1, xvar = "lambda", label = TRUE)

# 10 fold cross validation
cv.ridge1 = cv.glmnet(x1, y, alpha = 0, nfolds = 10)
plot(cv.ridge1)
coef(cv.ridge1) # Coefficients when choosing lambda by MSE

# Calculate RMSE in a new data frame
cv.ridge1.df <- data.frame(mse = cv.ridge1$cvm, 
                          rmse = sqrt(cv.ridge1$cvm), 
                          lambda =cv.ridge1$lambda)

# Plot it
ggplot(cv.ridge1.df, aes( x = log(lambda), y = rmse)) + 
  geom_point() + geom_line() +
  geom_point(data = cv.ridge1.df %>% 
               filter(rmse == min(rmse)),
             color = "red") + 
  labs(title = "Ridge Regression")

# Find the lambda that minimizes RMSE
best.ridge1.lambda = cv.ridge1.df %>% 
  filter(rmse == min(rmse))
best.ridge1.lambda  # is 0.9637915

# Find the coefficients for this model
coef(cv.ridge,s= 0.9637915)

## Conclusion: Lambda unchanged, but higher RMSE


##--------------------------------------------------------
## Lasso Regression - Without transfer ratio
##--------------------------------------------------------

# Do and plot the Lasso regression
fit.lasso1 = glmnet(x1, y, alpha = 1)
plot(fit.lasso1, xvar = "lambda", label = TRUE)

# Do 10 fold cross validation (based on MSE)
cv.lasso1 = cv.glmnet(x1, y, alpha = 1, nfolds = 10)
plot(cv.lasso1)
coef(cv.lasso1)  # Foreigners and promotion are still excluded 

## Run 10-Fold Cross Validation 100 times
## and get the lambda with the minimum RMSE on average over the runs.

# Run through the code to get the optimal lambda that minimizes RMSE on average
Lasso1.df = Lasso.Lambdas(x = x, y = y)
Best.Lasso1.df = OptimLambda(k = 100, x = x, y = y)  # lambda is 0.3083426
RMSE.Lambda1.df = RMSE.Lambda(k = 100, x = x, y = y)

# Plot the lambda that minimizes RMSE
ggplot(RMSE.Lambda1.df, aes( x = log(lambda), y = mean.RMSE)) + 
  geom_point() + geom_line() +
  geom_point(data = RMSE.Lambda1.df %>% 
               filter(mean.RMSE == min(mean.RMSE)),
             color = "red") + 
  labs(title = "Lasso Regression")

# Get lambda value that minimizes RMSE
best.lasso.lambda1 <- RMSE.Lambda1.df %>% 
  filter(mean.RMSE == min(mean.RMSE))
best.lasso.lambda1

# What coefficients do we get?
coef(fit.lasso1,s = 0.3083426)

## Conlcusion: Lambda is unchanged, RMSE is actually lower,
## and only foreigners are excluded.
