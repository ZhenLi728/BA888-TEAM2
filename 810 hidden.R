library(tidyverse)
library(readr)
rawData<-read_csv('team project/OnlineNewsPopularity/OnlineNewsPopularity.csv')

head(rawData)
##remove non-predictive variable: url & timedelta
news<-rawData %>% select(-c(url,timedelta))

##remove rows with 'zero value' for n_tokens_content
news<-news %>% filter(n_tokens_content!=0)

##binary variables (delete superfluous two)
news <- news %>% select(-c(is_weekend,weekday_is_sunday))

## some strange variables with extreme skewness: check if there are errorness
news %>% filter(n_unique_tokens>1) %>% nrow  #[1] 1
news %>% filter(n_non_stop_words>1) %>% nrow #[1] 1
news %>% filter(n_non_stop_unique_tokens>1) %>% nrow ##[1] 1
news<-news %>% filter(n_unique_tokens<=1)

  #news %>% filter(kw_max_min<0) %>% nrow #[1] 0
  #news %>% filter(kw_avg_min<0) %>% nrow #[1] 789
  #news %>% filter(kw_min_avg<0) %>% nrow #[1] 5
  #news %>% filter(kw_min_min<0) %>% nrow

news<-news %>% filter(kw_avg_min>=0) %>% filter(kw_min_avg>=0) %>% select(-c(kw_min_min))
nrow(news) #[1] 16549  37668

##take log 
news<-news %>% mutate(log_shares=log(shares)) %>% select(-c(shares))
#news<-news %>% mutate(log_self_reference_min_shares=log(self_reference_min_shares)) %>% select(-c(self_reference_min_shares))
#news<-news %>% mutate(log_self_reference_max_shares=log(self_reference_max_shares)) %>% select(-c(self_reference_max_shares))
#news<-news %>% mutate(log_self_reference_avg_sharess=log(self_reference_avg_sharess)) %>% select(-c(self_reference_avg_sharess))

#######try all regression methods########

set.seed(810)
test_index<-sample(nrow(news),nrow(news)*0.3,replace=F)
news.train<-news[-test_index,]  #[1] 11585 [1] 26368
news.test<-news[test_index,]  #[1] 4964 [1] 11300
y.train<-news.train$log_shares
y.test<-news.test$log_shares 

###multiple linear regression (OLS)###
f<-as.formula(log_shares~ .)
fit.lm<-lm(f,news.train)
summary(fit.lm)
yhat.train.lm<-predict(fit.lm,news.train)
length(yhat.train.lm)
yhat.test.lm<-predict(fit.lm,news.test)
length(yhat.test.lm)

mse.train.lm<-mean((y.train-yhat.train.lm)^2)
mse.train.lm #[1] 0.7416378
mse.test.lm<-mean((y.test-yhat.test.lm)^2)
mse.test.lm  ###########[1] 0.7588266 ############ [1] 0.8123508 (kw-min-min>0)  [1] 0.758636 (remove kw-min-min)  [1] 0.758636(take log of indep)

###forward stepwise###
xnames<- colnames(news)
xnames<-xnames[xnames!='log_shares']

   ##start by fitting an intercept only model
fit_fw<-lm(log_shares~1,news.train)
yhat.train.fw<-predict(fit_fw,news.train)
mse.train.fw<-mean((y.train-yhat.train.fw)^2) 
mse.train.fw  #[1] 0.8473858

yhat.test.fw<-predict(fit_fw,news.test)
mse.test.fw<-mean((y.test-yhat.test.fw)^2)  
mse.test.fw  #[1] 0.8777721

   ##store the value so that we can check later
fw_result<-tibble(
  xname='intercept',
  model=deparse(fit_fw$call),
  mse_train=mse.train.fw,
  mse_test=mse.test.fw
)
  ## add one variable with smallest test mse each time
while (length(xnames)>0) {
  best_mse_train<-NA
  best_mse_test<-NA
  best_fit_fw<-NA
  best_xname<-NA
  
  for (xname in xnames) {
    fit_fw_tmp<-update(fit_fw,as.formula(paste0('. ~ . + ', xname)))
    # compute train mse
    yhat_train_tmp<-predict(fit_fw_tmp,news.train)
    mse_train_tmp<-mean((y.train-yhat_train_tmp)^2)
    # compute test mse
    yhat_test_tmp<-predict(fit_fw_tmp,news.test)
    mse_test_tmp<-mean((y.test-yhat_test_tmp)^2)
    # if it is the first predictor or adding it produces the currently lowest test mse
    # then store this predictor as the current best predictor
    if (is.na(best_mse_test) | mse_test_tmp<best_mse_test) {
      best_mse_train<-mse_train_tmp
      best_mse_test<-mse_test_tmp
      best_fit_fw<-fit_fw_tmp
      best_xname<-xname
    }
  }
  fw_result<-fw_result %>% add_row(
    xname=best_xname,
    model=paste0(deparse(best_fit_fw$call), collapse = ""),
    mse_train = best_mse_train,
    mse_test = best_mse_test
  )
  # adopt the best model for the next iteration
  fit_fw<-best_fit_fw
  # remove the current best predictor from the xnames list
  xnames<-xnames[xnames!=best_xname]
}

View(fw_result)

min(fw_result[,'mse_test']) #########[1] 0.7571899 ########### [1] 0.805682  [1] 0.7564137
min(fw_result[,'mse_train']) # [1] 0.7399837
fw_result[which.min(fw_result$mse_test),]

###backward stepwise###
xnames<- colnames(news)
xnames<-xnames[xnames!='log_shares']

  ##start by fitting all predictors in a model
fit_bw<-lm(log_shares~.,news.train)
yhat.train.bw<-predict(fit_bw,news.train)
mse.train.bw<-mean((y.train-yhat.train.bw)^2) 
mse.train.bw  #[1] 0.7416378

yhat.test.bw<-predict(fit_bw,news.test)
mse.test.bw<-mean((y.test-yhat.test.bw)^2)  
mse.test.bw  #[1] 0.7588266

  ##store the value so that we can check later
bw_result<-tibble(
  xname='intercept',
  model=deparse(fit_bw$call),
  mse_train=mse.train.fw,
  mse_test=mse.test.fw
)
  ##remove one variable with smallest test mse each time
while (length(xnames)>0) {
  best_mse_train<-NA
  best_mse_test<-NA
  best_fit_bw<-NA
  best_xname<-NA
  
  # select the next predictor to be removed
  for (xname in xnames) {
    fit_bw_tmp<-update(fit_bw, as.formula(paste0(". ~ . - ", xname)))
    yhat_train_tmp<-predict(fit_bw_tmp,news.train)
    mse_train_tmp<-mean((y.train-yhat_train_tmp) ^ 2)
    
    yhat_test_tmp<-predict(fit_bw_tmp,news.test)
    mse_test_tmp<-mean((y.test-yhat_test_tmp) ^ 2)
    
    if (is.na(best_mse_test) | mse_test_tmp<best_mse_test) {
      best_mse_test<-mse_test_tmp
      best_mse_train<-mse_train_tmp
      best_xname <- xname
      best_fit_bw <- fit_bw_tmp
    }
  }
  bw_result<-bw_result %>% add_row(
    xname=best_xname,
    model=paste0(deparse(best_fit_bw$call),collapse = ''),
    mse_train=best_mse_train,
    mse_test=best_mse_test
  )
  # store the current model for next iteration
  fit_bw<-best_fit_bw
  # remove the predictor from the xnames list
  xnames<-xnames[xnames!=best_xname]
}

View(bw_result)g


###lasso###
library(glmnet)
xnames<- colnames(news)
xnames<-xnames[xnames!='log_shares']

loopformular<-'log_shares~n_tokens_title'
for (i in 2:length(xnames)) {
  loopformular<-paste(loopformular,'+',xnames[i],collapse = '')
}

loopformular
f<-as.formula(loopformular)

x_train<-model.matrix(f,news.train)[,-1]
x_test<-model.matrix(f,news.test)[,-1]

fit_lasso<-cv.glmnet(x_train,y.train,alpha=1,nfolds = 10)
yhat_train_lasso <- predict(fit_lasso, x_train, s = fit_lasso$lambda.min)
mse_train_lasso <- mean((y.train - yhat_train_lasso)^2) 
yhat_test_lasso <- predict(fit_lasso, x_test, s = fit_lasso$lambda.min) 
mse_test_lasso <- mean((y.test - yhat_test_lasso)^2)
mse_train_lasso #[1] 0.7651388
mse_test_lasso #########[1] 0.78203 ########

coef(fit_lasso)

###ridge###

xnames<- colnames(news)
xnames<-xnames[xnames!='log_shares']

loopformular<-'log_shares~n_tokens_title'
for (i in 2:length(xnames)) {
  loopformular<-paste(loopformular,'+',xnames[i],collapse = '')
}

loopformular
f<-as.formula(loopformular)

x_train<-model.matrix(f,news.train)[,-1]
x_test<-model.matrix(f,news.test)[,-1]

fit_ridge<-cv.glmnet(x_train,y.train,alpha=0,nfolds = 10)
yhat_train_ridge <- predict(fit_ridge, x_train, s = fit_ridge$lambda.min)
mse_train_ridge <- mean((y.train - yhat_train_ridge)^2) 
yhat_test_ridge <- predict(fit_ridge, x_test, s = fit_ridge$lambda.min) 
mse_test_ridge <- mean((y.test - yhat_test_ridge)^2)
mse_train_ridge #[1] 0.8473858
mse_test_ridge #########[1] 0.8777721 ########

coef(fit_ridge)

#####Tree######
library(rpart)
library(rpart.plot)
fit_tree<-rpart(log_shares~.,news.train,control = rpart.control(cp=0.001))
yhat_train_tree<-predict(fit_tree,news.train)
mse_train_tree<-mean((y.train-yhat_train_tree)^2)
yhat_test_tree<-predict(fit_tree,news.test)
mse_test_tree<-mean((y.test-yhat_test_tree)^2)
mse_test_tree  #[1] 0.7816188

   #plot the decision tree(single decision tree)
rpart.plot(fit_tree)  #Error in plot.new() : figure margins too large

######bagging#######
install.packages('randomForest')
library(randomForest)
ncol(news)

fit_bag<-randomForest(log_shares~.,data=news.train,mtry=3,importance=T)
yhat.bag.test<-predict(fit_bag,news.test)

plot(yhat.bag.test,boston.test)
abline(0,1)
mse.test<-mean((y.test-yhat.bag.test)^2)
mse.test  # 13.50808

importance(bag.boston)
varImpPlot(bag.boston)

#######random forest#######
fit_bag<-randomForest(log_shares~n_tokens_title+n_tokens_content+num_imgs+num_videos,data=news.train,mtry=2,importance=T)
fit_bag<-randomForest(log_shares~.,data=news.train,mtry=8,importance=T)
#######boosting#######
install.packages('gbm')
library(gbm)
set.seed(810)
fit_boost<-gbm(log_shares~.,news.train,distribution = 'gaussian')
summary(fit_boost)

yhat_boost_train<-predict(fit_boost,news.train,n.trees = 50)
mse_boost_train<-mean((yhat_boost_train-y.train)^2)
yhat_boost_test<-predict(fit_boost,news.test,n.trees = 100)
mse_boost_test<-mean((yhat_boost_test-y.test)^2)
mse_boost_test #[1] 0.7519992
