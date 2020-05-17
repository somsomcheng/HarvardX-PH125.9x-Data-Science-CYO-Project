## ----setup, include=FALSE------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----include=FALSE-------------------------------------------------------------------------------------------------------------
if(!require(bsts)) install.packages("bsts")
if(!require(e1071)) install.packages("e1071")
if(!require(knitr)) install.packages("knitr")
if(!require(glmnet)) install.packages("glmnet")
if(!require(caTools)) install.packages("caTools")
if(!require(corrplot)) install.packages("corrplot")
if(!require(lubridate)) install.packages("lubridate")
if(!require(data.table)) install.packages("data.table")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(randomForest)) install.packages("randomForest")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
library(bsts)
library(dplyr)
library(caret)
library(e1071) 
library(knitr)
library(scales)
library(glmnet) 
library(caTools) 
library(ggplot2)
library(ggthemes)
library(corrplot)
library(lubridate) 
library(tidyverse)
library(data.table)
library(kableExtra)
library(randomForest)
library(RColorBrewer)
 
# Load the dataset
train<-read.csv("train.csv")
set.seed(123, sample.kind="Rounding")

# Convert NA to numeric 0 for modeling purposes
train_set<-train%>% mutate_all(~replace(., is.na(.), 0))

#Converting character variables to numeric
train$regshape[train$LotShape == "Reg"] <- 1
train$regshape[train$LotShape != "Reg"] <- 0
train$paved[train$Street == "Pave"] <- 1 
train$paved[train$Street != "Pave"] <- 0

# Separate the data set into two sets: training_set and testing_set
test_index<-createDataPartition(y = train_set$SalePrice, times = 1, p = 0.1, list = FALSE)
training_set<-train_set[-test_index,]
testing_set<-train_set[test_index,]



# Take a glance at the training_set
summary(training_set)



## ------------------------------------------------------------------------------------------------------------------------------
ggplot(data=train, aes(x=SalePrice)) +
  geom_histogram(fill="skyblue2", binwidth = 2000) +
  scale_x_continuous(breaks= seq(0, 1000000, by=90000),labels = comma) +
  labs(title="House Sale Prices Distribution")+
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------------------
# Identify index vector of numeric variables
numeric_vars<- which(sapply(training_set, is.numeric)) 

# Identify the column names of numeric variables
numeric_vars_col<-data.table(names(numeric_vars))  
temp<-training_set[,numeric_vars]

# Get the correlations of all numeric variables
corr_all<-cor(temp,use="pairwise.complete.obs")

# Sort the correlations with SalePrice on a descending order
corr_sorted<-as.matrix(sort(corr_all[,'SalePrice'], decreasing = TRUE)) 

# Select high corelations only
high_corr<-names(which(apply(corr_sorted, 1, function(x) abs(x)>0.5))) 
high_corr_col<-data.table(high_corr)
temp2<- corr_all[high_corr, high_corr]

# Plot the correlations
corrplot(temp2, addrect = 2, type = "lower", col = brewer.pal(n = 8, name = "RdBu"))


## ------------------------------------------------------------------------------------------------------------------------------
training_set%>%ggplot(aes(x=factor(OverallQual),y=SalePrice))+
  geom_boxplot(col='coral1')+ 
  labs(x='OverallQual')+ 
  scale_y_continuous(breaks= seq(0, 1000000, by=90000),labels = comma)+ 
  labs(title="Corrrelation between OverallQual and SalePrice")+
  theme_minimal()



## ------------------------------------------------------------------------------------------------------------------------------
training_set%>%ggplot(aes(x = SalePrice,fill = as.factor(OverallQual)))+
  geom_histogram(position = "stack", binwidth = 10000)+
  ggtitle("Distribution of SalePrice by OverallQual")+
  ylab("Count")+
  xlab("SalePrice")+ 
  scale_x_continuous(breaks= seq(0, 1000000, by=90000),labels = comma)+
  scale_fill_discrete(name="OverallQual")+
  theme(plot.title = element_text(hjust = 0.1), 
        legend.position=c(0.9,0.7),
        legend.background = element_rect(fill="grey90",size=0.5,linetype="solid"))                      
                                                                                


## ------------------------------------------------------------------------------------------------------------------------------
# Run the Multiple Linear Regression Model
fit<-lm(SalePrice~OverallQual+GrLivArea+GarageCars+GarageArea+TotalBsmtSF+X1stFlrSF+FullBath+TotRmsAbvGrd+YearBuilt+YearRemodAdd,data=training_set)

summary(fit)


## ------------------------------------------------------------------------------------------------------------------------------
# Predict SalePrice in testing set using the model
SalePrice_prediction<-predict(fit,testing_set)

# Scale the data
log_SalePrice_Prediction<-log(SalePrice_prediction)
log_SalePrice_Real<-log(testing_set$SalePrice)

#Calculate RMSLE
RMSLE<-RMSE(log_SalePrice_Prediction,log_SalePrice_Real)
RMSLE


## ------------------------------------------------------------------------------------------------------------------------------
rf<-randomForest(SalePrice~OverallQual+GrLivArea+GarageCars+GarageArea+TotalBsmtSF+X1stFlrSF+FullBath+TotRmsAbvGrd+YearBuilt+YearRemodAdd,data=training_set)
SalePrice_prediction2<-predict(rf,testing_set)

log_SalePrice_Prediction2<-log(SalePrice_prediction2)
log_SalePrice_Real<-log(testing_set$SalePrice)
RMSLE2<- RMSE(log_SalePrice_Real,log_SalePrice_Prediction2)
RMSLE2


## ------------------------------------------------------------------------------------------------------------------------------
plot(log_SalePrice_Prediction2,log_SalePrice_Real,main = "log Predicted SalePrice VS. log Actual Saleprice")+abline(0,1)


## ------------------------------------------------------------------------------------------------------------------------------
bind_rows(tibble(Method="Multiple Linear Regression Model",RMSLE=RMSE(log_SalePrice_Prediction,log_SalePrice_Real)),
          tibble(Method="Random Forests Model",RMSLE=RMSE(log_SalePrice_Real,log_SalePrice_Prediction2)))

