
#clear environment
rm(list = ls(all = TRUE))

#set seed to 123  
set.seed(123)

#install packages for regression
library(MASS)
install.packages("glmnet")
library(glmnet)

#----------------------------------------------exercise1----------------------------------------------------------------

#For the four conditions, correlation was set to 0, 0.2, 0.95, 0.999999
correlation <- 0

#set loop counter
S = 100

#create empty vectors to store results
rep(NA, S)
b1 = c()
b2 = c()
b1r = c()
b2r = c()
b1l = c()
b2l = c()

#generate data and regression scores. (mean = 0, variance = 0.01)
for (i in 1:S){
  mu <- c(0,0)
  Sigma <- matrix(c(1,correlation,correlation,1),2,2)
  pred_scores1 <- mvrnorm(100, mu = mu, Sigma = Sigma, empirical = TRUE)
  pred_scores1
  mean(pred_scores1)
  noise <- rnorm(n = 100, mean = 0, sd = 0.1)
  mean(noise)
  
  #For case 2, we set Beta2 to 0 by using the following code fragment: "y1 = pred_scores1[,1]*1 + pred_scores1[,2]*0+noise"
  y1 = pred_scores1[,1]*1 + pred_scores1[,2]*1+noise
  
  ### OLS ####
  out1 <- lm(y1 ~ pred_scores1[,1] + pred_scores1[,2])
  summary(out1)
  b1 = c(b1,out1$coefficients[2])
  b2 = c(b2,out1$coefficients[3])
  
  ### Ridge ###
  out2 <- glmnet(pred_scores1[,1:2],y1, alpha = 0, lambda = 0.1)
  b1r = c(b1r,coef(out2)[2,])
  b2r = c(b2r,coef(out2)[3,])
  
  ### Lasso ###
  out3 <- glmnet(pred_scores1[,1:2],y1, alpha = 1, lambda = 0.1)
  summary(out3)
  b1l = c(b1l,coef(out3)[2,])
  b2l = c(b2l,coef(out3)[3,])
}

#OLS regression results
bias = mean(b1) - 1
variance = var(b1)
MSE = variance + (bias*bias)
bias;variance;MSE

# for case2,bias2 = mean(b2) - 0
bias2 = mean(b2) - 1
variance2 = var(b2)
MSE2 = variance2 + (bias2*bias2)
bias2;variance2;MSE2

#ridge regression results
biasr = mean(b1r) - 1
variancer = var(b1r)
MSEr = variancer + (biasr*biasr)
biasr;variancer;MSEr

# for case2, we used "biasr2 = mean(b2r) - 0" instead of "biasr2 = mean(b2r) - 1"
biasr2 = mean(b2r) - 1
variancer2 = var(b2r)
MSEr2 = variancer2 + (biasr2*biasr2)
biasr2;variancer2;MSEr2

#lasso regression results
biasl = mean(b1l) - 1
variancel = var(b1l)
MSEl = variancel + (biasl*biasl)
biasl;variancel;MSEl

#for case 2, we used "biasl2 = mean(b2l) - 0" instead of "biasr2 = mean(b2r) - 1"
biasl2 = mean(b2l) - 1
variancel2 = var(b2l)
MSEl2 = variancel2 + (biasl2*biasl2)
biasl2;variancel2;MSEl2

#calculate and examine the relative efficiency of the regression methods 
RE1LassoOLS = MSEl / MSE
RE1LassoRidge = MSEl / MSEr
RE1RidgeOLS = MSEr / MSE
RE2LassoOLS = MSEl2 / MSE2
RE2LassoRidge = MSEl2 / MSEr2
RE2RidgeOLS = MSEr2 / MSE2
RE1LassoOLS;RE1LassoRidge;RE1RidgeOLS;RE2LassoOLS;RE2LassoRidge; RE2RidgeOLS 


#-------------------------------------------------exercise2------------------------------------------------------------------
# Change Mean to 0 or 1 to create both results
rm(list = ls(all = TRUE))

set.seed(123)
S <- 100
n1 = c(10, 100, 200)
s2 = c(1, 2, 10)

### H1 ###

RESULT <- c()
for (j in 1:3){
  for (k in 1:3){
    h1_result_Welch <- c()
    h1_result_Student <- c()
    for (i in 1:S){
      #Create groups
      h1_group_n1.1<- rnorm(n1[j], mean = 0, sd = sqrt(1))
      h1_group_n2.1 <- rnorm(200, mean = 0, sd = sqrt(s2[k]))
      
      #Welch test
      h1_ttest_Welch1 <- t.test(h1_group_n1.1, h1_group_n2.1, paired = FALSE, var.equal = FALSE)
      h1_result_Welch <- c(h1_result_Welch,h1_ttest_Welch1[3])
      
      #Student test
      h1_ttest_Student1 <- t.test(h1_group_n1.1, h1_group_n2.1, paired = FALSE, var.equal = TRUE)
      h1_result_Student <- c(h1_result_Student,h1_ttest_Student1[3])
    }
    
    h1_power_Welch <- sum(h1_result_Welch<0.05)/S
    h1_power_Student <- sum(h1_result_Student<0.05)/S
    
    res <- c(h1_power_Welch,h1_power_Student)
    RESULT <- rbind(RESULT,res)
  }
}
RESULT
