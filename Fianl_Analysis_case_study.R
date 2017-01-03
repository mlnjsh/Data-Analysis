                                  ### Case Study ###


#Loading Data

dta <-read.csv("C:/Users/milan.joshi/Desktop/Interview_Project/Intervw_Data.csv")

dta<-dta[,-c(1,13)]

# Devide the data into training set and test set (50%-50%)

library(caret)

set.seed(998)

# Partition data in 50-50% as train test

inTraining <- createDataPartition(dta$TargetBuy, p = .50, list = FALSE)

train <- dta[ inTraining,]

test <- dta[-inTraining,]

head(train)

head(test)

summary(train)

#train<-apply(train,2,function(x) gsub("^$|^ $",NA,x))

# Data Type of each column

str(train)

# Here we can see that, out of 13 variables, 
# there are 7 continuous (int) variables and 6 categorical (Factor) variables,
# where the last one (TargetBuy) is the outcome (also known as dependent variable) itself.
# we want TargetBuy to be categorical.Since it is classification problem

train$TargetBuy<-as.factor(train$TargetBuy)
train$DemClusterGroup<-as.factor(train$DemClusterGroup)
train$DemGender<-as.factor(train$DemGender)
train$DemReg <-as.factor(train$DemReg )
train$DemTVReg <-as.factor(train$DemTVReg )
train$PromClass <-as.factor(train$PromClass )

test$TargetBuy<-as.factor(test$TargetBuy)
test$DemClusterGroup<-as.factor(test$DemClusterGroup)
test$DemGender<-as.factor(test$DemGender)
test$DemReg <-as.factor(test$DemReg )
test$DemTVReg <-as.factor(test$DemTVReg )
test$PromClass <-as.factor(test$PromClass )

# Lets Group Continuous and categorical variables

train_count<- subset(train,select = c(ID,DemAffl,DemAge,DemCluster,PromSpend,PromTime ))

library(AppliedPredictiveModeling)
transparentTheme(trans = .4)

library(caret)
featurePlot(x = train_count,
            y = train$TargetBuy,
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

transparentTheme(trans = .9)
featurePlot(x = train_count,
            y = train$TargetBuy,
            plot = "density",
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            adjust = 1.5,
            pch = "|",
            layout = c(4,1),
            auto.key = list(columns = 3))

featurePlot(x =train_count,
            y = train$TargetBuy,
            plot = "box",
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),
            layout = c(4,1 ),
            auto.key = list(columns = 2))


par(mfrow=c(2, 3))

colnames <- dimnames(train_count)[[2]]

for (i in 1:5) {
  
  hist(train_count[,i],  main=colnames[i], probability=TRUE, col="blue", border="white")
  
  d <- density(train_count[,i])
  
  lines(d, col="red")
}
train_cat<-subset(train,select = -c(ID,DemAffl,DemAge,DemCluster,PromSpend,PromTime ))

# Quick summary of all numeric variables

summary(train_count)   # First quartile 25% third quartile 75%

library("pastecs")

# set singificant digits and get detail summary

options(scipen = 100)

options(digits=4)

stat.desc(train_count)
#nbr.val - shows number of values in a variable
#nbr.null - shows number of null(missing) values
#nbr.na - shows number of NA(missing) values
#CI.mean.0.95 - considers 95% confidence interval

#lets check the number of unique values in each categorical variable.

apply(train_cat, 2 , function(x){length(unique(x))})

# DemTvReg has more that 10 unique values but thats not the problem

# Calculating Proportion of Dependent variable

table(train$TargetBuy)

prop.table(table(train$TargetBuy))  #25% Buy 75% Not Buy

# Calculating DemClusterGroup

table(train$DemClusterGroup) # 471 blanks

as.matrix(prop.table(table(train$DemClusterGroup))) # category U is just 0.2%, 3% blank 20% of C

#Calculating DemGender

table(train$DemGender) # 1731 Blanks and Unknown U 1747

as.matrix(prop.table(table(dta$DemGender))) # F= 54% M= 26% U= 7% Blanks = 11%

#Calculating DemReg

table(train$DemReg) # 324 Blanks

as.matrix(prop.table(table(dta$DemReg)))  

round(sort(prop.table(table(dta$DemReg))*100,decresing=T),3) #3% blank 39% of South East 30% midlands

#Calculating DemTVReg

table(train$DemTVReg)   # 321 Missing( 2%) 

as.matrix(prop.table(table(dta$DemTVReg))) # 27% Londan

#Calculating PromClass

table(train$PromClass) 

as.matrix(prop.table(table(dta$PromClass))) # 39% Silver 29% tin 28% gold

#we look at the cross-tabulation or confusion matrix of the two variables

MyTable<-table(train$TargetBuy,train$DemClusterGroup)

round(prop.table(MyTable),2)

margin.table(MyTable,1) # Row marginal frq

margin.table(MyTable,2)#col marginal freq

round(prop.table(MyTable),2)

round(prop.table(MyTable,1),2)

round(prop.table(MyTable,2),2)

chisq.test(MyTable)
#==========================================================================================
MyTable<-table(train$TargetBuy,train$DemGender)

round(prop.table(MyTable),5)

margin.table(MyTable,1) # Row marginal frq

margin.table(MyTable,2)#col marginal freq

round(prop.table(MyTable),2)

round(prop.table(MyTable,1),2)

round(prop.table(MyTable,2),2)

chisq.test(MyTable)
#==========================================================================================
MyTable<-table(train$TargetBuy,train$DemReg)

round(prop.table(MyTable),5)

margin.table(MyTable,1) # Row marginal frq

margin.table(MyTable,2)#col marginal freq

round(prop.table(MyTable),2)

round(prop.table(MyTable,1),2)

round(prop.table(MyTable,2),2)

chisq.test(MyTable)     #DemReg and targetBuy has no association
#==========================================================================================
MyTable<-table(train$TargetBuy,train$DemTVReg)

round(prop.table(MyTable),5)

margin.table(MyTable,1) # Row marginal frq

margin.table(MyTable,2)#col marginal freq

round(prop.table(MyTable),2)

round(prop.table(MyTable,1),2)

round(prop.table(MyTable,2),2)

chisq.test(MyTable)  #DemTv Reg has No assocation with TargetBUy
#==========================================================================================
MyTable<-table(train$TargetBuy,train$PromClass) 

round(prop.table(MyTable),5)

margin.table(MyTable,1) # Row marginal frq

margin.table(MyTable,2)#col marginal freq

round(prop.table(MyTable),2)

round(prop.table(MyTable,1),2)

round(prop.table(MyTable,2),2)

chisq.test(MyTable)
#=========================================================================================================

# Exploratory Graphs

library(ggplot2)

# categorical vs Continuous

library(ggplot2)

qplot(factor(train$TargetBuy), data=train, geom="bar", fill=factor(TargetBuy)) 

qplot(factor(train$TargetBuy), data=train, geom="bar", fill=factor(DemGender))

# More female tend to buy than male

qplot(factor(train$TargetBuy), data=train, geom="bar", fill=factor(PromClass)) 

# People carring Tin and Silver tend to buy 



p<-ggplot(train,aes(factor(TargetBuy),DemAffl))

p + geom_boxplot(fill = "grey80", colour = "#3366FF")

#Buyers have High median DemAffl than nonBuyer so DemAffl is signifivcant variable

qplot(factor(TargetBuy), DemAge, data = train, geom = "boxplot",colour = I("#3366FF")) 

# From Above  People around 30 to 42 tend to buy

#Heatmaps And Mosiac plot

#categorical vs Categorical plots

MyTable1<-table(train$TargetBuy,train$DemClusterGroup)

mosaicplot(MyTable1,main="ClusterGroup Buy NotBUy", xlab="Buy NotBuy",ylab="ClusterGroup",col = c(3,4,5))

MyTable2<-table(train$TargetBuy,train$DemGender)

mosaicplot(MyTable2,main="DemGender Buy NotBUy", xlab="Buy NotBuy",ylab="demGender",col = c(3,6,5))

MyTable3<-table(train$TargetBuy,train$DemReg)

mosaicplot(MyTable3,main="DemReg Buy NotBUy", xlab="Buy NotBuy",ylab="demGender",col = c(1,4,6))

MyTable4<-table(train$TargetBuy,train$PromClass)

mosaicplot(MyTable4,main="DemReg Buy NotBUy", xlab="Buy NotBuy",ylab="demGender",col = c(5,3,6))

# lets Bin the age in 5 categories

summary(train$DemAge)

c1 <- cut(train$DemAge, breaks = 5)

table(c1)

qplot(factor(train$TargetBuy), data=train, geom="bar", fill=factor(c1)) # target Middle age (30,40)

c2<- cut(train$DemAffl, breaks = 5)

table(c2)

qplot(factor(train$TargetBuy), data=train, geom="bar", fill=factor(c2)) # Most of Buyers are in range 7-14

c3<-cut(as.numeric(train$Demcluster),9)


library(vcd)

assoc(MyTable1, shade=TRUE)

# Mosiac plot with more folds

# Gender Age and Demaffl

M<-split(train, list(train$TargetBuy,train$DemAffl, train$DemGender,train$DemAge))

ftable(sort(M))

#Checking missing values

table(is.na(train))

colSums(is.na(train))

library(VIM)

aggr_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  
                  labels=names(train), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


#The plot helps us understanding that almost 70% of the samples are not missing any information,
#8% are missing the DemAge, and the remaining ones show other missing patterns. 
#Through this approach the situation looks a bit clearer in my opinion.


aggr_plot <- aggr(test, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  
                  labels=names(test), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


# Missing value Treatment Train Set/Test Set

library(mlr)

imputed_data<- impute(train, classes = list(integer = imputeMean(), factor = imputeMode()))
                         

train<-imputed_data$data

colSums(is.na(train))

dim(train)

summary(train)

#train<-apply(train,2,function(x) gsub("^$|^ $",NA,x))


imputed_test_data<-impute(test,classes = list(integer = imputeMean(),factor=imputeMode()))

test<-imputed_test_data$data

colSums(is.na(test))

train$TargetBuy<-as.factor(train$TargetBuy)

imp = impute(train, target = "TargetBuy", cols = list(DemReg = imputeLearner(makeLearner("classif.rpart")),
                                                      
      DemGender = imputeLearner(makeLearner("classif.rpart"))), dummy.cols = c("DemReg", "DemGender"))


apply(train,2,function(x) sum(x== ""))

apply(test,2,function(x) sum(x==""))

train_count<- subset(train,select = c(ID,DemAffl,DemAge,DemCluster,PromSpend,PromTime ))

cor(train_count) # Not so interesting correlation.

pairs(train_count)

#### INteresting
train$TargetBuy<- as.factor(train$TargetBuy)
task<-makeClassifTask(data=train , target = "TargetBuy")
print(task)
lrn=makeLearner("classif.rpart")
print(lrn)
getParamSet(lrn)
# Feature Engineering

library(Boruta)

set.seed(123)

boruta.train <- Boruta(TargetBuy~.-ID, data = train, doTrace = 2)
                       
print(boruta.train)

#Boruta performed 41 iterations in 32.95664 mins.

#2 attributes confirmed unimportant: DemReg, DemTVReg.

plot(boruta.train, xlab = "", xaxt = "n")

lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  
boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])

names(lz) <- colnames(boruta.train$ImpHistory)

Labels <- sort(sapply(lz,median))

axis(side = 1,las=2,labels = names(Labels),
     
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
#===========================================================================================================

# Predictve Model Building

# Building Decision tree

library(rpart)

set.seed(333)

train.tree<-rpart(TargetBuy~.,data=train, method="class")             
            

summary(train.tree)


library(rpart.plot)

rpart.plot(train.tree)

train.tree

prediction_train<-predict(train.tree,newdata=train,type="class")

prediction_test<-predict(train.tree,newdata=test,type="class")

library(caret)
library(doSnow)
set.seed(2348)
cv.10.folds <- createMultiFolds(train$TargetBuy, k = 10, times = 10)
table(train$TargetBuy)
table(train$TargetBuy[cv.10.folds[[33]]])
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.folds)

rf.5.cv.1 <- train(x = train, y = train$TargetBuy, method = "rf", tuneLength = 3,
                   ntree =100, trControl = ctrl.1)
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

rf.5.cv.1

plot(rf.5.cv.1)

confusionMatrix(prediction_train,train$TargetBuy)

probs <- predict(train.tree, test[,-12], type = "prob")

head(probs)

confusionMatrix(prediction_test,test$TargetBuy)

head(probs)

plotcp(train.tree)  # 0.017


# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(train.tree) # visualize cross-validation results    

# Prune the tree with the optimal cp value

pTree<- prune(train.tree, 0.017)

# draw the pruned tree

rpart.plot(pTree)

pTree

prediction_train<-predict(pTree,newdata=train,type="class")

prediction_test<-predict(pTree,newdata=test[,-12],type="class")

probs<-predict(pTree,newdata=test[,-12],type="prob")

confusionMatrix(prediction_train,train$TargetBuy)

head(prediction_test)

confusionMatrix(prediction_test,test$TargetBuy)

#ROC for Decision Tree

library(ROCR)

ROCRpred <- prediction(predict, test$TargetBuy)

ROCRperf <- performance(ROCRpred, 'tpr','fpr')

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))


library(pROC)

g<-roc(test$TargetBuy~probs[,2],data=test[,-12])

plot(g,col='blue',lwd=4)  # area Under ROC curve 0.704

#===============================================================================

    #####   RANDOM FORSET    #########

library(randomForest)

model.randomForest<- randomForest(TargetBuy ~ ., data = train,importance=TRUE,proximity=TRUE)

# look at variable importance

round(importance(model.randomForest),2)

pred.randomForest <- predict(model.randomForest, test[,-12],type="prob")

head(pred.randomForest)

library(pROC)

g<-roc(test$TargetBuy~pred.randomForest[,2],data=test[,-12])

plot(g,col='blue',lwd=4)  # area Under ROC curve 0.704

#======================================================================================
library(e1071)

 model.naiveBayes <- naiveBayes(TargetBuy ~ ., data = train, laplace = 3)

 pred.naiveBayes <- predict(model.naiveBayes, test[,-12],type="raw")

 head(pred.naiveBayes)

g<-roc(test$TargetBuy~pred.randomForest[,2],data=test[,-12])

plot(g,col='blue',lwd=4)  # area Under ROC curve 0.704

#=========================================================================================

# build the models with two di??????erent kernel functions respectively

model.svm.linear <- svm(TargetBuy ~ ., data = train, kernel="linear",probability = TRUE)

model.svm.radial <- svm(TargetBuy ~ ., data = train, kernel="radial",probability = TRUE)

# prediction with the two models respectively

pred.svm.linear <- predict(model.svm.linear, test[,-1],probability=TRUE)

attr(pred.svm.linear, "probabilities")[1:6,]

pred.svm.radial <- predict(model.svm.radial, test[,-1],probability=TRUE)

attr(pred.svm.linear, "probabilities")[1:6,]
library(caret)
confusionMatrix(data = test_set$pred, reference = test_set$obs)

#========================================================================================
xtab <- confusionMatrix(iris$Species, sample(iris$Species))
as.matrix(xtab)
# Neural Network Model

library(neuralnet)

library(NeuralNetTools)

n <- names(train)

f <- as.formula(paste("TargetBuy ~", paste(n[!n %in% "TargetBuy"], collapse = " + ")))
                
f

parse_train <- model.matrix(~ ID+DemAffl +DemAge+ DemCluster+DemClusterGroup+DemGender+DemReg
                            +DemTVReg+PromClass+ PromSpend+ PromTime+TargetBuy ,data = train)


maxs <- apply(parse_train, 2, max) 
mins <- apply(parse_train, 2, min)

scaled <- as.data.frame(scale(parse_train, center = mins, scale = maxs - mins))

train_ <- scaled[d,]
test_ <- scaled[-d,]



nn <- neuralnet(f,data=train_, hidden=2,err.fct="ce",threshold=0.01,linear.output=FALSE)

#===================================================================================================

logit.model<-glm(TargetBuy~.,data=train,family=binomial(logit))

summary(logit.model)

prob.train<-predict(logit.model,data=train[-12],type="response")

prob.train[1:10]

prob.test<-predict(logit.model,data=test[-12],type="response")

prob.test[1:10]   

for (T in seq(0.1,0.9,0.1)){
  
  class1<-ifelse(prob.train>T,1,0)
  
  # Confusion Matrix
  
  cat("Threshhold = ", T,"\n")
  
  confusion = table(class1,train$TargetBuy)
  
  cat("Confusion Matrix :" , "\n" ) 
              

print(confusion)             

pctcorrect<-100*(confusion[1,1]+confusion[2,2])/sum(confusion)

False.Positive.Rate<-confusion[[3]]/(confusion[[1]]+confusion[[3]])

False.Negative.Rate<-confusion[[2]]/(confusion[[2]]+confusion[[4]])

cat("% correct =",round(pctcorrect,1),"\n")

print("------------------------")

cat("False Positive Rate =", round(False.Positive.Rate,2),"\n")

print("------------------------")

cat("False Negative Rate =", round(False.Positive.Rate,2),"\n")

print("------------------------")

}

library(pROC)

g<-roc(TargetBuy~prob.train,data=train)

plot(g)

#==========================================================================================================

# XGBoost Algorithm

install.packages("drat", repos="https://cran.rstudio.com")

drat:::addRepo("dmlc")

install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

require(xgboost)

set.seed(1)

data(agaricus.train, package='xgboost')

data(agaricus.test, package='xgboost')

train <- agaricus.train

test  <- agaricus.test

param <- list("objective" = "binary:logistic",
              "eval_metric" = "logloss",
              "eta" = 1, "max.depth" = 2)

bst.cv = xgb.cv(param=param, data = as.matrix(train$data), label = train$label, nfold = 10, nrounds = 20)

plot(log(bst.cv$test.logloss.mean),type = "l")

bst <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2, eta = 1, nround = 5,
               nthread = 2, objective = "binary:logistic")

preds=predict(bst,test$data)

print(-mean(log(preds)*test$label+log(1-preds)*(1-test$label)))

trees = xgb.model.dt.tree(dimnames(train$data)[[2]],model = bst)

# Get the feature real names

names <- dimnames(train$data)[[2]]

importance_matrix <- xgb.importance(names, model = bst)

xgb.plot.importance(importance_matrix[1:10])

xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)


#===========================================================================================
library(caret)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

library(gbm)

set.seed(825)

gbmFit1 <- train(train$TargetBuy ~ ., data = train,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

#===========================================

# model building

library(caret)

sparse_matrix <- model.matrix(TargetBuy ~ ., TargetBuy)

model_xgb <- xgboost(data=as.matrix(Dumy_Train), 
                     label=TargetBuy1, 
                     objective="binary:logistic", nrounds=100, 
                     eta=0.02, max_depth=5, subsample=0.6, colsample_bytree=0.85,
                     min_child_weight=1, eval_metric="auc")

