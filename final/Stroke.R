rm(list=ls())
#import the library 
library(dplyr)
library("tidyverse")
library("ggplot2")
library("gridExtra")
library("dummies")
library("gplots")
library(gains)
library(caret)
library(ROCR)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(ranger)
library(DMwR2)
library(smotefamily)
library(ROSE)
library(pROC)


#import the dataset
df<- read.csv("stroke.csv")

#head of stroke 
head(df)

#Names of df 
names(df)

#dropping the id column
df<-subset(df,select=(-id))
head(df)

df<- df%>%
  filter(gender!="Other")


#EDA 
#View of df
View(df)

#Checking the structure of df 
str(df)

#BMI has NA values 
df$bmi<- as.integer(df$bmi)

#Changing the datatype of the gender, ever_married, Smoking status to factors 
df$gender<-as.factor(df$gender)
df$ever_married<-as.factor(df$ever_married)
df$work_type<-as.factor(df$work_type)
df$Residence_type<-as.factor(df$Residence_type)
df$smoking_status<-as.factor(df$smoking_status)
df$stroke<-as.factor(df$stroke)
df$heart_disease<-as.factor(df$heart_disease)
df$hypertension<-as.factor(df$hypertension)

#checking if there is any null value in the dataset 
any(is.na(df))

#number of na's in bmi 
length(which(is.na(df$bmi)))

#Doing median imputation for fixing null value 
df1<-df
df_mean<-df
nrow(df)

#removing the missing values of the bmi 
df2<-na.omit(df1)

#Checking for na after the values are dropped
any(is.na(df2))
nrow(df2)

#calculating the median
med<-median(df2$bmi);med
Mean<-mean(df2$bmi);Mean

#imputing the mean value 
df_mean$bmi[is.na(df_mean$bmi)]<-Mean

#Imputing the median value 
df1$bmi[is.na(df1$bmi)]<-med

#checking the value for bmi 
any(is.na(df1$bmi))

#Checking the structure of the dataframe
str(df1)

unique(df1$work_type)


#df1 => median imputed value 
#df2 => missing values dropped 

#Performing the Exploratory Data Analysis
#Creating a bar plot fot gender VS stroke
ggplot(df1,aes(x=gender,fill=stroke))+geom_bar(position="dodge")

#creating a bar plot for hypertension vs stroke
ggplot(df1,aes(x=hypertension,fill=stroke))+geom_bar(position="dodge")


#Creating the bar plot for heart disease
ggplot(df1,aes(x=heart_disease,fill=stroke))+geom_bar(position="dodge")

#Creating the bar plot for worktype 
ggplot(df1,aes(x=work_type,fill=stroke))+geom_bar(position="dodge")


#Creating a barplot for the marital status of the people
ggplot(df1,aes(x=ever_married,fill=stroke))+geom_bar(position="dodge")

#Creating a bar plot for the smoking status and the stroke 
ggplot(df1,aes(x=smoking_status,fill=stroke))+geom_bar(position = "dodge")

#Inference from the above plot is that the number of people who get stroke when compared to the entire population

#So now comparing the within the population of people who got stroke 

#Comparing the proportions within the gender among the people who got stroke
data_proportion_gender<- df1 %>%
                  group_by(gender)%>%
                  summarise(prop=sum(stroke==1)/length(gender))

g1<-ggplot(data_proportion_gender,aes(x=gender,y=prop,fill=gender))+geom_col()
g1
#comparing the proportions within the different married type who got stroke 
data_proportion_married<- df1%>%
                          group_by(ever_married)%>%
                          summarise(prop=sum(stroke==1)/length(ever_married))

g2<-ggplot(data_proportion_married,aes(x=ever_married,y=prop,fill=ever_married))+geom_col()

#Comparing the proportions of people who have hypertension  who got stroke 
data_proportion_hypertension<- df1%>%
  group_by(hypertension)%>%
  summarise(prop=sum(stroke==1)/length(hypertension))

g3<-ggplot(data_proportion_hypertension,aes(x=hypertension,y=prop,fill=hypertension))+geom_col()


#Comparing the proportions of people who have the heart diseases who got stroke 
data_proportion_heart_disease<- df1%>%
  group_by(heart_disease)%>%
  summarise(prop=sum(stroke==1)/length(heart_disease))

g4<-ggplot(data_proportion_heart_disease,aes(x=heart_disease,y=prop,fill=heart_disease))+geom_col()
g4

#Comparing the proportions of people who have different worktype who got a stroke 
data_proportion_work_type<- df1%>%
  group_by(work_type)%>%
  summarise(prop=sum(stroke==1)/length(work_type))

g5<-ggplot(data_proportion_work_type,aes(x=work_type,y=prop,fill=work_type))+geom_col()
g5

names(df1)

#Comparing the proportion of people who have different resident type 
data_proportion_Residence_type<- df1%>%
  group_by(Residence_type)%>%
  summarise(prop=sum(stroke==1)/length(Residence_type))

g5<-ggplot(data_proportion_Residence_type,aes(x=Residence_type,y=prop,fill=Residence_type))+geom_col()
g5

#comparing people with different smoking status type who got stroke 

data_proportion_smoking_status<- df1%>%
  group_by(smoking_status)%>%
  summarise(prop=sum(stroke==1)/length(smoking_status))


g6<-ggplot(data_proportion_smoking_status,aes(x=smoking_status,y=prop,fill=smoking_status))+geom_col()
g6

#Plotting together in the form of a grid 
grid.arrange(grobs=list(g1,g2,g3,g4,g5,g6),ncol=3, top = "Proportion of Strokes for Each Factor")

#Gender and the residence type does not have much difference in occurrence of strokes. Those with hypertension, heart diseases and those who have been married have higher proportion of strokes 

#Children and the people who have never worked have very low occurrence of stroke. People who are self_employed have a greater proportion of getting the stroke.

#People who are currently smoking also has a greater percentage of getting the stroke when compared to the rest of the population

#Removing other from the dataframe 
#df1<- df1%>%
#  filter(gender!="Other")

#df2<- df2%>%
#  filter(gender!="Other")

#df_mean<- df_mean%>%
#  filter(gender!="Other")



#Comparing the boxplot for different factors  with age 
b1<-df1 %>%
    ggplot(aes(x=gender,y=age,color=stroke))+geom_boxplot()
b1

b2<-df1 %>%
  ggplot(aes(x=hypertension,y=age,color=stroke))+geom_boxplot()
b2

b3<-df1 %>%
  ggplot(aes(x=ever_married,y=age,color=stroke))+geom_boxplot()
b3

b4<-df1 %>%
  ggplot(aes(x=heart_disease,y=age,color=stroke))+geom_boxplot()
b4

b5<-df1 %>%
  ggplot(aes(x=smoking_status,y=age,color=stroke))+geom_boxplot()
b5

b6<-df1 %>%
  ggplot(aes(x=work_type,y=age,color=stroke))+geom_boxplot()
b6

b7<-df1 %>%
  ggplot(aes(x=Residence_type,y=age,color=stroke))+geom_boxplot()
b7

#plotting the boxplots in the form of a grid 
grid.arrange(grobs=list(b1,b2,b3,b4,b5,b6,b7),ncol=3,top="Box plots for Stroke and Age across various factors")


#From the above plot we can infer that most of the people who got stroke are the people who are older
#people who are self employed are also older than rest of the population 
#People who had stroke and smokes are younger than those who never smoked

#Comparing the strokes for glucose level across various factors 
q1<-df1 %>%
  ggplot(aes(x=gender,y=avg_glucose_level,color=stroke))+geom_boxplot()
q1

q2<-df1 %>%
  ggplot(aes(x=hypertension,y=avg_glucose_level,color=stroke))+geom_boxplot()
q2

q3<-df1 %>%
  ggplot(aes(x=ever_married,y=avg_glucose_level,color=stroke))+geom_boxplot()
q3

q4<-df1 %>%
  ggplot(aes(x=heart_disease,y=avg_glucose_level,color=stroke))+geom_boxplot()
q4

q5<-df1 %>%
  ggplot(aes(x=smoking_status,y=avg_glucose_level,color=stroke))+geom_boxplot()
q5

q6<-df1 %>%
  ggplot(aes(x=work_type,y=avg_glucose_level,color=stroke))+geom_boxplot()
q6

q7<-df1 %>%
  ggplot(aes(x=Residence_type,y=avg_glucose_level,color=stroke))+geom_boxplot()
q7

#plotting the boxplots in the form of a grid 
grid.arrange(grobs=list(q1,q2,q3,q4,q5,q6,q7),ncol=3,top="Box plots for Average Glucose level across various factors")

# We can see that the glucose level is right skewed 
#The Inter Quartile Range is higher for people who gets stroke 

#Comparing the strokes for BMI across various factors 
w1<-df1 %>%
  ggplot(aes(x=gender,y=bmi,color=stroke))+geom_boxplot()
w1

w2<-df1 %>%
  ggplot(aes(x=hypertension,y=bmi,color=stroke))+geom_boxplot()
w2

w3<-df1 %>%
  ggplot(aes(x=ever_married,y=bmi,color=stroke))+geom_boxplot()
w3

w4<-df1 %>%
  ggplot(aes(x=heart_disease,y=bmi,color=stroke))+geom_boxplot()
w4

w5<-df1 %>%
  ggplot(aes(x=smoking_status,y=bmi,color=stroke))+geom_boxplot()
w5

w6<-df1 %>%
  ggplot(aes(x=work_type,y=bmi,color=stroke))+geom_boxplot()
w6

w7<-df1 %>%
  ggplot(aes(x=Residence_type,y=bmi,color=stroke))+geom_boxplot()
w7

#Creating the grid for all the plots 
grid.arrange(grobs=list(w1,w2,w3,w4,w5,w6,w7),ncol=3,top="Boxplots for Stroke and BMI accross various factors")


#There is no much difference between the person who got stroke and person who didn't get stroke. Even the BMI is right skewed.

#plotting the density plot and the histogram for the continuous parameters
e1<- ggplot(df1,aes(x=age,fill=stroke))+geom_density(alpha=0.5)
e1 
e2<- ggplot(df1,aes(x=avg_glucose_level,fill=stroke))+geom_density(alpha=0.5)
e2
e3<- ggplot(df1,aes(x=bmi,fill=stroke))+geom_density(alpha=0.5)
e3
e4<-ggplot(df1,aes(x=age,fill=stroke))+geom_histogram()
e4
e5<-ggplot(df1,aes(x=avg_glucose_level,fill=stroke))+geom_histogram()
e5
e6<-ggplot(df1,aes(x=bmi,fill=stroke))+geom_histogram()
e6

grid.arrange(grobs=list(e1,e2,e3,e4,e5,e6),nrow=3, top=" Distribution of continuous variable")

#From this plots we can infer that the number of stroke increases as the age increases, 
#From the histogram we cam infer that the average number of strokes is bimodal for avg_glucose_level for both distribution of people who received stroke and who didn't receive stroke

#Correlation matrix
df4<-dummy.data.frame(df1)

cor(df4)
heatmap.2(cor(df4), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(df4,2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))




#Splitting the dataset 
set.seed(101)

#creating the test-train split 
number_of_rows<- nrow(df1);number_of_rows
train.index<-sample(number_of_rows,number_of_rows*.7)
train.df<-df1[train.index,]
head(train.df)
test.df<-df1[-train.index,]

names(test.df)

nrow(train.df)

#SMOTE
train.df<-ovun.sample(stroke~.,data=train.df,method = "over",N=6000)$data
nrow(train.df)
View(train.df)
head(train.df)
nrow(train.df)

#----------------------------------------------------------------------------------------------------------------------
#logistic Regression with data frame which is median imputed 
glm_stroke <- glm(stroke ~ gender + age + hypertension + heart_disease + ever_married + 
                    work_type + Residence_type + avg_glucose_level + bmi + smoking_status, data = train.df, family = "binomial") 
summary(glm_stroke)

#Predicting using the logistics regression
glm_pred<-predict(glm_stroke,test.df[,-11],type = "response")


data.frame(actual = test.df$stroke[1:10], predicted = glm_pred[1:10])
confusionMatrix(table(predict(glm_stroke, newdata = test.df, 
                              type="response") >= 0.7, test.df$stroke == 1))


#Gains calculations
gain <- gains(as.numeric(test.df$stroke), glm_pred, groups=10);gain

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(as.numeric(test.df$stroke)))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(as.numeric(test.df$stroke)))~c(0, dim(test.df)[1]), lty=2)


# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(as.numeric(test.df$stroke))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

export.df <- data.frame(test.df, glm_pred)
t.df <- data.frame("Predicted" = glm_pred, "Label" = test.df$stroke)
View(t.df)

pred <- prediction(t.df$Predicted, t.df$Label)
perf <- performance( pred, "tpr", "fpr" )
plot( perf )

#-----------------------------------------------------------------------------------------------------------------------------
#Model for Dropped columns
set.seed(101)
#Creating a test train split for values which is dropped instead of imputation
number_of_rows1<- nrow(df2);number_of_rows1
train_dropped.index<-sample(number_of_rows1,number_of_rows1*.7)
train_dropped.df<-df1[train_dropped.index,]
head(train_dropped.df)
test_dropped.df<-df1[-train_dropped.index,]


#SMOTE
train_dropped.df<-ovun.sample(stroke~.,data=train_dropped.df,method = "over",N=6000)$data

#Creating a logistic model for dropped Stroke 
glm_stroke_dropped<-glm(stroke~ gender + age + hypertension + heart_disease + ever_married + 
                          work_type + Residence_type + avg_glucose_level + bmi + smoking_status,data=train_dropped.df,family="binomial")
summary(glm_stroke_dropped)

glm_dropped_pred<-predict(glm_stroke_dropped,test_dropped.df[,-11],type="response")

data.frame(actual = test_dropped.df$stroke[1:10], predicted = glm_dropped_pred[1:10])
confusionMatrix(table(predict(glm_stroke_dropped, newdata = test_dropped.df, 
                              type="response") > 0.65, test_dropped.df$stroke == 1))

r.dt<-roc(test_dropped.df$stroke,glm_dropped_pred,)
plot(r.dt)
auc(r.dt)
.#--------------------------------------------------------------------------------------------------------------------------------------------
#model for mean imputations
set.seed(101)
#Creating a train test split for which is mean imputed
number_of_rows2<- nrow(df_mean);number_of_rows2
train_mean.index<-sample(number_of_rows2,number_of_rows2*.7)
train_mean.df<-df_mean[train_mean.index,]
head(train_mean.df)
test_mean.df<-df1[-train_mean.index,]

#Smote
train_mean.df<-ovun.sample(stroke~.,data=train_mean.df,method = "over",N=6000)$data


#Creation  of model 
glm_stroke_mean<-glm(stroke~.,data=train_mean.df,family='binomial')

summary(glm_stroke_mean)

#Making predictions
glm_mean_pred<- predict(glm_stroke_mean,test_mean.df[,-11],type="response")

data.frame(actual = test_mean.df$stroke[1:10], predicted = glm_mean_pred[1:10])
confusionMatrix(table(predict(glm_stroke_mean, newdata = test_mean.df, type="response") > 0.65, test_mean.df$stroke == 1))

#Gains calculations
gain <- gains(as.numeric(test_mean.df$stroke), glm_mean_pred, groups=10);gain

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(as.numeric(test_mean.df$stroke)))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(as.numeric(test_mean.df$stroke)))~c(0, dim(test_mean.df)[1]), lty=2)


# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(as.numeric(test_mean.df$stroke))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

export.df <- data.frame(test.df, glm_pred)
t.df <- data.frame("Predicted" = glm_pred, "Label" = test.df$stroke)

pred <- prediction(t.df$Predicted, t.df$Label)
perf <- performance( pred, "tpr", "fpr" )
plot( perf )



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Random Forest
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library("metrics")

#Random forest with dropped values 
rf_dropped<-randomForest(stroke ~ ., data = train_dropped.df, 
                          ntree = 1000, mtry = 10, nodesize = 1, importance = TRUE, sampsize = 3000)

#plot the variables by order of importance
varImpPlot(rf_dropped, type = 1)

#create a confusion matrix
rf_dropped.pred <- predict(rf_dropped, test_dropped.df)
confusionMatrix(rf_dropped.pred, test_dropped.df$stroke)

#RMSE(rf_dropped.pred,test_dropped.df$stroke)
rf_dropped.pred
test_dropped.df$stroke
default_rmse<-mean((as.numeric(rf_dropped.pred)-as.numeric(test_dropped.df$stroke))^2)
default_rmse

n_features <- length(setdiff(names(train_dropped.df), "stroke"));n_features

hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .25, .333, .4,.5)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA                                               
)



for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = stroke ~ ., 
    data            = train_dropped.df,
    num.trees       = n_features * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 42,
    respect.unordered.factors = 'order'
  )
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}

hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(100)

#------------------------------------------------------------------------------------------------------------------

#Random Forest with Median Imputation 
rf_median<-randomForest(stroke ~ ., data = train.df, 
                         ntree = 1000, mtry = 10, nodesize = 1, importance = TRUE, sampsize = 3000)

#plot the variables by order of importance
varImpPlot(rf_median, type = 1)

#create a confusion matrix
rf_median.pred <- predict(rf_median, test.df)
confusionMatrix(rf_median.pred, test.df$stroke)


#Random Forest with Mean Imputation
rf_mean<- randomForest(stroke~.,data=train_mean.df,ntree=1000,mtry=10,nodesize=1,importance=TRUE,sampsize=3000)

#plotting the variable by order of importance 
varImpPlot(rf_mean,type=1)

#Creating a confusion matrix 
rf_mean.pred<-predict(rf_mean,test_mean.df)
confusionMatrix(rf_mean.pred,test_mean.df$stroke)


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Decision Tree and best Pruned Tree 
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Creating a best pruned tree for dropped rows 
cv.ct_dropped <- rpart(stroke ~ ., data = train_dropped.df, method = "class", 
               control = rpart.control(cp = 0.001, minsplit = 5, xval = 10))

# use printcp() to print the table. 
printcp(cv.ct_dropped)
prp(cv.ct_dropped, type = 1, extra = 1, split.font = 1, varlen = -10)  

#prune the tree using the lowest value for xerror
#Note: the prune function requires cp as a parameter so we need to get cp for lowest value of xerror
pruned.ct_dropped <- prune(cv.ct_dropped, 
                   cp = cv.ct_dropped$cptable[which.min(cv.ct_dropped$cptable[,"xerror"]),"CP"])

prp(pruned.ct_dropped, type = 1, extra = 1, split.font = 1, varlen = -10,
    box.col=ifelse(pruned.ct_dropped$frame$var == "<leaf>", 'gray', 'white'))  

prune.pred_dropped <- predict(pruned.ct_dropped, test_dropped.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(prune.pred_dropped, test_dropped.df$stroke)

#Best Pruned tree with median imputation

#Creating a best pruned tree for median imputed rows 
cv.ct_median <- rpart(stroke ~ ., data = train.df, method = "class", 
                       control = rpart.control(cp = 0.001, minsplit = 5, xval = 10))

# use printcp() to print the table. 
printcp(cv.ct_median)
prp(cv.ct_median, type = 1, extra = 1, split.font = 1, varlen = -10)  

#prune the tree using the lowest value for xerror
#Note: the prune function requires cp as a parameter so we need to get cp for lowest value of xerror
pruned.ct_median <- prune(cv.ct_median, 
                           cp = cv.ct_median$cptable[which.min(cv.ct_median$cptable[,"xerror"]),"CP"])

prp(pruned.ct_median, type = 1, extra = 1, split.font = 1, varlen = -10,
    box.col=ifelse(pruned.ct_median$frame$var == "<leaf>", 'gray', 'white'))  

prune.pred_median <- predict(pruned.ct_median, test.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(prune.pred_median, test.df$stroke)


#Best Pruned Tree with mean Imputation

#Creating a best pruned tree for mean imputed rows 
cv.ct_mean <- rpart(stroke ~ ., data = train_mean.df, method = "class", 
                      control = rpart.control(cp = 0.001, minsplit = 5, xval = 10))

# use printcp() to print the table. 
printcp(cv.ct_mean)
prp(cv.ct_mean, type = 1, extra = 1, split.font = 1, varlen = -10)  

#prune the tree using the lowest value for xerror
#Note: the prune function requires cp as a parameter so we need to get cp for lowest value of xerror
pruned.ct_mean <- prune(cv.ct_mean, 
                          cp = cv.ct_mean$cptable[which.min(cv.ct_mean$cptable[,"xerror"]),"CP"])

prp(pruned.ct_mean, type = 1, extra = 1, split.font = 1, varlen = -10,
    box.col=ifelse(pruned.ct_mean$frame$var == "<leaf>", 'gray', 'white'))  

prune.pred_mean <- predict(pruned.ct_mean, test_mean.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(prune.pred_mean, test_mean.df$stroke)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Decisipn Tree
#Median Imputation 

#Creation of model
.ct_median <- rpart(stroke ~ ., data = train.df, method = "class", cp = 0.0001, maxdepth = 30, minsplit = 1)

# print tree summary and plot tree. try different values for extra
printcp(.ct_median)
prp(.ct_median, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)


# classify records in the validation data using the classification tree.
# set argument type = "class" in predict() to generate predicted class membership.
ct.pred_median <- predict(.ct_median, test.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(ct.pred_median, test.df$stroke)

#--------------------------------------------------------------------------------------------------------------------------
#Decision tree with Dropped rows 
.ct_dropped<- rpart(stroke ~ ., data = train_dropped.df, method = "class", cp = 0.0001, maxdepth = 10, minsplit = 1)

# print tree summary and plot tree. try different values for extra
printcp(.ct_dropped)
prp(.ct_dropped, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)


# classify records in the validation data using the classification tree.
# set argument type = "class" in predict() to generate predicted class membership.
ct.pred_dropped <- predict(.ct_dropped, test_dropped.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(ct.pred_dropped, test_dropped.df$stroke)

#---------------------------------------------------------------------------------------------------------------------------------
#Decision tree with mean imputation
.ct_mean<- rpart(stroke ~ ., data = train_mean.df, method = "class", cp = 0.0001, maxdepth = 10, minsplit = 5)

# print tree summary and plot tree. try different values for extra
printcp(.ct_mean)
prp(.ct_mean, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)


# classify records in the validation data using the classification tree.
# set argument type = "class" in predict() to generate predicted class membership.
ct.pred_mean <- predict(.ct_mean, test_mean.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(ct.pred_mean, test_mean.df$stroke)

#---------------------------------------------------------------------------------------------------------------------------------------------------------
#SVM
#Creating model with missing values dropped 
model<-svm(stroke~.,data=train_dropped.df)
summary(model)

#predicting values 
pred_dropped<-predict(model,test_dropped.df)

#Confusion Matrix 
confusionMatrix(pred_dropped,test_dropped.df$stroke)

#Tuning model to fix the discrepancy 
tune.results<-tune(svm,train.x=stroke~.,data=train_dropped.df,kernel="radial",range=list(cost=c(1,10),gamma=c(0.1,1)))
summary(tune.results)

model_tuned<-svm(stroke~.,data=train_dropped.df,cost=10,gamma=.1)

pred_dropped_tuned<-predict(model_tuned,test_dropped.df)

confusionMatrix(pred_dropped_tuned,test_dropped.df$stroke)

#------------------------------------------------------------------------------------

#Creating model with imputing the median values 
model_median<-svm(stroke~.,data=train.df)
summary(model_median)

#predicting values 
pred_median<-predict(model_median,test.df)

#Confusion Matrix 
confusionMatrix(pred_median,test.df$stroke)

#Tuning model to fix the discrepancy 
tune.results_median<-tune(svm,train.x=stroke~.,data=train.df,kernel="radial",range=list(cost=c(1,10),gamma=c(0.1,1)))
summary(tune.results_median)

model_tuned_median<-svm(stroke~.,data=train.df,cost=10,gamma=.1)

pred_median_tuned<-predict(model_tuned_median,test.df)

confusionMatrix(pred_median_tuned,test.df$stroke)

#---------------------------------------------------------------------------------------

#Creating model with imputing the mean value 
model_mean<-svm(stroke~.,data=train_mean.df)
summary(model_mean)

#predicting values 
pred_mean<-predict(model_mean,test.df)

#Confusion Matrix 
confusionMatrix(pred_mean,test_mean.df$stroke)

#Tuning model to fix the discrepancy 
tune.results_mean<-tune(svm,train.x=stroke~.,data=train_mean.df,kernel="radial",range=list(cost=c(1,10),gamma=c(0.1,1)))
summary(tune.results_mean)

model_tuned_mean<-svm(stroke~.,data=train_mean.df,cost=10,gamma=.1)

pred_mean_tuned<-predict(model_tuned_mean,test_mean.df)

confusionMatrix(pred_mean_tuned,test_mean.df$stroke)


