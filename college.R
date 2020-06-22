# libraries import
library(ISLR)
library(dplyr)
library(tidyverse)
library(leaps)
library(car)
library(gbm)
library(vctrs)
library(glmnet)
library(plyr)
set.seed(1234)
############################################################
# VISUAL ANALYSIS
############################################################
# data import
data(College)
# showing top 6 observations
head(College)

# column order change, so that explained variable is the first column in order to make plotting easier
College <- College[,c(2,1,3:18)]

# checking variable types
glimpse(College)
# variables properly encoded

# deleting row names
rownames(College) <- c()
# are there missings in the data set?
sum(is.na(College))
# no missings

# checking basic statistics
summary(College)

# plotting dependent variables versus indepentent variable with division on the variable Private: yes/no
College %>%
  gather(-Private, -Apps, key = "var", value = "variables") %>% 
  ggplot(aes(x = variables, y = Apps, color = Private)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()+
  scale_color_manual(values=c("orangered","forestgreen"))+
  scale_fill_manual(values=c("orangered","forestgreen")) +
  geom_smooth(method = 'lm', color = "black", se = F) +
  ggtitle("Scatterplot of dependent variables versus indepentent variable
along with regression lines")

# these variables are probably positively correlated with variable Apps:
# Accept, Enroll, Top10perc, Top25perc, F.Undergrad, 
# P.Undergrad, PhD, Terminal

# Accept i Enroll determine the number of accepted applications and enrolled students, therefore they can not be used in biulding a model
colnames(College)
# histograms of categorical and continuous variables
College %>%
  gather(-Private, key = "var", value = "variables") %>% 
  ggplot(aes(x = variables)) +
  geom_histogram(color="forestgreen", fill="yellowgreen",
                 bins = 30)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()+
  ggtitle("Histograms of depentent variables")

############################################################
# PREPARING DATA FOR ANALYSIS
############################################################

# encoding Private into 0 and 1 in order to make modelling easier
College$Private <- ifelse(College$Private == "Yes", 1, 0)
College$Private <- as.factor(College$Private)

# variables with outliers:
# Apps > 40000, P.Undergrad > 15000, S.F.Ratio > 35, Personal > 6000, Books > 1800
# deleting these observations
College <- subset(College, Apps < 40000 & P.Undergrad < 15000
                  & S.F.Ratio < 35 & Personal < 6000
                  & Books < 1800)

# split into train, vaildation and test sets
idx <- sample(seq(1, 3), size = nrow(College), replace = TRUE, prob = c(.7, .15, .15))
train <- (1:nrow(College))[idx == 1]
test <- (1:nrow(College))[idx == 2]
valid <- (1:nrow(College))[idx == 3]

############################################################
# VARIABLE SUBSET SELECTION FOR LINEAR REGRESSION
############################################################
colnames(College)
# variable subset selection for regression with the method stepwise regression selection (method = 'seqrep' in the function regsubsets
 
lm.subset.fit <- regsubsets(Apps~., 
                            data = College,
                            nvmax = 17, 
                            method = "seqrep", 
                            subset = train,
                            force.out =  c(2,3))
reg.summary <- summary(lm.subset.fit)

reg <- data.frame(
  RSS = reg.summary$rss,
  Adj.R2 = reg.summary$adjr2,
  Cp = reg.summary$cp,
  BIC = reg.summary$bic
)
RSS <- ggplot(data=reg, aes(x=1:nrow(reg), y=RSS)) +
  geom_line(linetype = "dashed")+
  theme_bw()+
  geom_point()+
  labs(x = "number of variables", y = "RSS")

Adj.R2 <- ggplot(data=reg, aes(x=1:nrow(reg), y=Adj.R2)) +
  geom_line(linetype = "dashed")+
  theme_bw()+
  geom_point()+
  labs(x = "number of variables", y = "Adjusted-R^2")

Cp <- ggplot(data=reg, aes(x=1:nrow(reg), y=Cp)) +
  geom_line(linetype = "dashed")+
  theme_bw()+
  geom_point()+
  labs(x = "number of variables", y = "Cp")

BIC <- ggplot(data=reg, aes(x=1:nrow(reg), y=BIC)) +
  geom_line(linetype = "dashed")+
  theme_bw()+
  geom_point()+
  labs(x = "number of variables", y = "BIC")

grid.arrange(RSS, Adj.R2, Cp, BIC, nrow = 2, ncol = 2,
             top = "Variable selection")

# The plots show, that models with less than 8 variables should be rejected. Models woth 11, 13 or 14 would also not be optimal (there are bends on the plots of information criteria. In order to decide on a numerical basis and not only plots, a data frame with an optimal number of variables for each information criterion has been created (i.e. minima for Cp and BIC and maxima for RSS and adjusted-R^2 respectively)
for (i in names(reg)) {
  print(c(i, which.min(reg[,i])))
}

data.frame(
  RSS = which.min(reg$RSS),
  Adj.R2 = which.max(reg$Adj.R2),
  CP = which.min(reg$Cp),
  BIC = which.min(reg$BIC
                  )
)
# In order not to make a model too complicated and make interpretation easier a subset of 8 variables has been chosen:
colnames(reg.summary$which)[reg.summary$which[which.min(reg.summary$bic),]==TRUE]

############################################################
# LINEAR REGRESSION
############################################################

# a model taking into account all dependent variables except Enroll and Accept

lm.model.1 <- lm(Apps~.-Enroll -Accept, data = College, subset = train)
summary(lm.model.1)

lm.fit.1 <- predict(lm.model.1, College[valid,])
summary(lm.fit.1)

lm.err.1 <- mean((College$Apps[valid] - lm.fit.1)^2)

actuals_preds <- data.frame(cbind(actuals=College[valid,]$Apps, predicteds=lm.fit.1))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
head(actuals_preds)

vif(lm.model.1) %>%    
  knitr::kable()

# The table shows, variables Top10perc and Top25perc are collinear, because their VIF is greater than 5.

############################################################
# STEPWISE REGRESSION
############################################################

# linear regression with the variables chosen with the best subset selection algorithm
lm.model.2 <- lm(Apps~ Private+Top10perc+F.Undergrad+
                   P.Undergrad+Room.Board+
                   perc.alumni+Expend+Grad.Rate,
                data = College, subset = train)


summary(lm.model.2)
# The table Coefficients shows that all the variables taken to the linear regression model are statistically significant 

lm.fit.2 <- predict(lm.model.2, College[valid,])
summary(lm.fit.2)

lm.err.2 <- mean((College$Apps[valid] - lm.fit.2)^2)

# prediction rating of the linear regression model 
actuals_preds <- data.frame(cbind(actuals=College[valid,]$Apps, predicteds=lm.fit.2))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
head(actuals_preds)

# checkig possible collinearities
vif(lm.model.2) %>%    
  knitr::kable()

# For all variables VIF is less than 5, so there is no collinearity in the model

# in order to dispose of collinearities in another way, LASSO and ridge regression have been applied

############################################################
# RIDGE REGRESSION
############################################################

# creating matrices that are required by the algorithm
train.mat <- model.matrix(Apps~.-Enroll -Accept, data=College[train,])
valid.mat <- model.matrix(Apps~.-Enroll -Accept, data=College[valid,])
test.mat <- model.matrix(Apps~.-Enroll -Accept, data=College[test,])

# Regresja brzegowa 
# wyznaczenie zbioru parametrow lambda, sposrod ktorych
# zostanie wybrany najlepszy
grid <- 10 ^ seq(4, -2, length=100)

# wybor najlepszego parametru lambda za pomoc? walidacji krzy?owej
mod.ridge <- cv.glmnet(train.mat, College$Apps[train], alpha=0,
                       lambda=grid, thresh=1e-12)
lambda.best.1 <- mod.ridge$lambda.min

# najlepsza wartosc parametru lambda dla regresji brzegowej
lambda.best.1

# zbudowanie modelu z nowym parametrem lambda
ridge.pred <- predict(mod.ridge, newx=valid.mat, s=lambda.best.1)

# blad sredniokwadratowy regresji brzegowej na zbiorze walidacyjnym:
lm.err.3 <- mean((College$Apps[valid] - ridge.pred)^2)

# wspolczynniki modelu regresji brzegowej wyznaczone na calym zbiorze danych:
mod.ridge <- glmnet(model.matrix(Apps~.-Enroll -Accept, data=College), College[, "Apps"], alpha=0)
ridge.coef <- predict(mod.ridge, s=lambda.best.1, type="coefficients")
ridge.coef

############################################################
# LASSO REGRESSION
############################################################

# best lambda parameter selection by cross validation
mod.lasso <- cv.glmnet(x=train.mat, 
                       y=College$Apps[train],
                       alpha=1, 
                       lambda=grid, 
                       thresh=1e-12)
lambda.best.2 <- mod.lasso$lambda.min

# best lambda parameter for LASSO regression
lambda.best.2

# MSE of LASSO regression on validation set
lasso.pred <- predict(mod.lasso, newx=valid.mat, s=lambda.best.2)
lm.err.4 <- mean((College$Apps[valid] - lasso.pred)^2)

# coefficients of the LASSO regression model determined on the whole data set:
mod.lasso <- glmnet(model.matrix(Apps~.-Enroll -Accept, data=College), College[, "Apps"], alpha=1)
lasso.coef <- predict(mod.lasso, s=lambda.best.2, type="coefficients")
lasso.coef


#checking which regression model works best on a validation set (model 1 cannot be taken into account due to the statistical irrelevance of some variables and the coincidence of variables detected)
which.min(c(lm.err.2,
          lm.err.3,
          lm.err.4))


# Model 4 of the linear regression, or LASSO regression, is the best among all linear regression models. Its parameters are as follows:
lasso.coef

############################################################
# GRADIENT BOOSTING METHODS
############################################################

# gradient boosting model on all explanatory variables
boost.model.1 <- gbm(Apps~. -Accept -Enroll, data = College[train,],
                   distribution = "gaussian",
                   n.trees = 5000,
                   cv.folds = 10)

# optimal number of trees in the gradient  boosting model 1
ntree.opt.oob.1 <- gbm.perf(boost.model.1, method = "OOB", plot.it = F)

# the optimal size differs significantly from that adopted for boost.model.1, which is why the boost.model.2 model with fewer iterations was built to avoid over-learning the model

# reducing the number of iterations:
boost.model.2 <- gbm(Apps~. -Accept -Enroll, data = College[train,],
                     distribution = "gaussian",
                     n.trees = round_any(ntree.opt.oob.1, 
                                         100, f = ceiling), 
		     cv.folds = 10)

# optimal number of trees in the gradient  boosting model 2
ntree.opt.oob.2 <- gbm.perf(boost.model.2, method = "OOB", plot.it = F)

# now the out-of-bag size does not differ much from the built model boost.model.2, but the number of divisions for cross-validation can still be increased
boost.model.3 <- gbm(Apps~ .-Accept -Enroll, data = College[train,],
                     distribution = "gaussian",
                     n.trees = round_any(ntree.opt.oob.1, 
                                         100, f = ceiling),
                     cv.folds = round_any(ntree.opt.cv.2,
                                          10, f = ceiling))


# comparing the quality of model prediction on a validation set
pred.boost.1 <- predict.gbm(boost.model.1, 
                            newdata = College[valid, ], 
                            n.trees = 5000)
boost.err.1 <- mean((pred.boost.1-College[valid,]$Apps)^2)
pred.boost.2 <- predict.gbm(boost.model.2, 
                            newdata = College[valid, ], 
                            n.trees = round_any(ntree.opt.oob.1, 
                                                100, f = ceiling))
boost.err.2 <- mean((pred.boost.2-College[valid,]$Apps)^2)
pred.boost.3 <- predict.gbm(boost.model.3, 
                            newdata = College[valid, ], 
                            n.trees = round_any(ntree.opt.oob.1, 
                                                100, f = ceiling))
boost.err.3 <- mean((pred.boost.3-College[valid,]$Apps)^2)
which.min(c(boost.err.1,
          boost.err.2,
          boost.err.3))

# the lowest error on the validation set among the tree models gradient boosting is achieved by model boost.model.3
# graph of the importance of individual variables in the gradient boosting method

summary(boost.model.3, las = 2, xlim = c(0,100), order = FALSE)

############################################################
# CHOOSING THE BEST MODEL
############################################################

# comparing the prediction of the best models on the test set:
# LASSO regression:
lasso.pred.f <- predict(mod.lasso, newx=test.mat, s=lambda.best.2)
lm.err.f <- mean((College$Apps[test] - lasso.pred.f)^2)

# gradient boosting:
pred.boost.f <- predict.gbm(boost.model.3, 
                            newdata = College[test, ], 
                            n.trees = round_any(ntree.opt.oob.1, 
                                                100, f = ceiling))
boost.err.f <- mean((pred.boost.f-College[test,]$Apps)^2)

# comparing the prediction errors of both models between each other and choosing the best model:
error.values <- c(lm.err.f,boost.err.f)
which.min(error.values)
# the best prediction on the test set is made by the linear regression model with L1 regularization (the lowest mean square error) and this model was used for interpretation

############################################################
# INTERPRETATION OF THE MODEL
############################################################

# The parameters of the best model are as follows:
lasso.coef

# The mean squared error is:
lm.err.4

# coefficient of determination is:
1-lm.err.4/var(College[test,]$Apps)
# thus the L1 linear regression model explains 79.9% of the variation in the data

# The parameters next to each variable indicate that
# that the following variables are most strongly related to the number of university applications:
#   Private, Top10perc, perc.alumni, Grad.Rate and Terminal.
# The values of the model parameters show that:
#   
# private universities have a smaller number of applications than public ones
# on average 603 (variable Private)
# 
# more new students recruiting by 1 percentage point
# from 10% of the best high schools, increases the number of applications by 18.9
# (Top10perc variable)
# 
# each additional full-time student increases the number of applications by 0.6
# (variable F. Undergrad), and each additional one-off decline in the number of applications by 0.1
# (variable P. Undergrad)
# 
# An additional $1 in funding results in a 0.04 drop in applications (Outstate variable)
# 
# $1 increase in campus accommodation costs
# causes a decrease in the number of applications by 0.3 (Room.Board variable)
# 
# $1 per person increase in spending
# causes a decrease in the number of applications by 0.1 (Personal variable)
# 
# increase in the percentage of professors at a given university
# in relation to other lecturers by 1 percentage point
# causes a decrease in the number of applications by 3.6. (Terminal variable)
# 
# each subsequent graduate causes an increase in the number of applications for universities,
# 19.2 (Grad.Rate variable)
# 
# $1 increase in tuition costs leads to a decrease in the number of applications by 0.09
# 
# increase in the percentage of graduates who support universities financially
# by 1 percentage point results in 23.1 fewer university applications,
# (variable perc.alumni)