## GAM Models
##------------
library(gratia)
library(psych)
library(mgcv)
library(dplyr)
library(caret)
library(bartMachine)

# Functions
fun_metrics<-function(df){
  l_model<-list(gam_m,gbm_m,bart_m)
  names_m<-c("gam","Gbm","Bart")
  l_df<-list()
  for(i in 1:length(l_model)){
    pred<-predict(l_model[[i]],newdata = df)
    rmse<-RMSE(pred,df$y)
    mae<-MAE(pred,df$y)
    l_df[[i]]<-c(names_m[[i]],rmse,mae)
  }
  b_df<-do.call("rbind",l_df) %>% as.data.frame()
  names(b_df)<-c("model","rmse","mae")
  b_df[,c(2,3)]<-sapply(b_df[,c(2,3)],as.numeric)
  return(b_df)
}


# Data
df <- data_sim("eg1", seed = 42)
pairs.panels(df)

# Gam Model
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df,
         method = "REML")
summary(m)

# Plot
draw(m)
sm <- smooth_estimates(m) #Data to draw plots

# Model diagnostics
appraise(m)
draw(m, residuals = TRUE) # check for oversmoothing

# Penalty matrices
penalty(m)
m %>%
  penalty() %>%
  draw()

## Predictive power
## train/test
indx<-createDataPartition(df$y,p=0.75, list=F)
train<-df[indx,]
test<-df[-indx,]
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)
### gam model
gam_m<-train(y ~ x0 + x1 + x2 + x3,
             data=train,
             method="gam",
             trControl=fitControl)

### gbm model
gbm_m<-train(y ~ x0 + x1 + x2 + x3,
             data=train,
             method="gbm",
             trControl=fitControl)

### BART model
bartGrid <- expand.grid(num_trees = c(10, 15, 20, 100), k = 2, alpha = 0.95, beta = 2, nu = 3)
bart_m<-train(y ~ x0 + x1 + x2 + x3,
              data=train,
              method="bartMachine",
              tuneGrid = bartGrid,
              trControl=fitControl)

### Results
## On train test
results <- resamples(list(gam=gam_m,
                          gbm=gbm_m,
                          bart=bart_m))
bwplot(results,metric = "RMSE")
bwplot(results,metric = "MAE")
bwplot(results,metric = "Rsquared")

## test set
fun_metrics(test)

