           # Random Rotation and RREBoost on IRIS#
Scaled.iris <- c()
scale<-function(x){
 for(i in 1:length(x)) x[i]<-min(1,max(0,(x[i]-min(x))/(max(x)-min(x))))
 Scaled<-c(Scaled.iris,x[i])
}
scale(iris[,-5])
#################################################################
Sc.SL<-c()
x<-iris[,1]
for (i in 1:length(x)){
x[i]<-min(1,max(0,(x[i]-min(x))/(max(x)-min(x))))
Sc.SL<- c(Sc.SL, x[i])}
head(Sc.SL)
length(Sc.SL)
summary(Sc.SL)
#################################################################
Sc.SW<-c()
x<-iris[,2]
for (i in 1:length(x)){
  x[i]<-min(1,max(0,(x[i]-min(x))/(max(x)-min(x))))
  Sc.SW<- c(Sc.SW, x[i])}
head(Sc.SW)
length(Sc.SW)
summary(Sc.SW)
##################################################################
Sc.PL<-c()
x<-iris[,3]
for (i in 1:length(x)){
  x[i]<-min(1,max(0,(x[i]-min(x))/(max(x)-min(x))))
  Sc.PL<- c(Sc.PL, x[i])}
head(Sc.PL)
length(Sc.PL)
summary(Sc.PL)
##################################################################
Sc.PW<-c()
x<-iris[,2]
for (i in 1:length(x)){
  x[i]<-min(1,max(0,(x[i]-min(x))/(max(x)-min(x))))
  Sc.PW<- c(Sc.PW, x[i])}
head(Sc.PW)
length(Sc.PW)
summary(Sc.PW)
###################################################################
iris$Species<-ifelse(iris$Species=="setosa",1, 
       ifelse(iris$Species=="versicolor",2,3)) 

N.species<-iris$Species

iris.scaled<-data.frame(Sc.SL,Sc.SW,Sc.PL,Sc.PW,iris$Species)

dim(iris.scaled)

head(iris.scaled)
str(iris.scaled)
random_rotation_matrix_incl_flip<-function(n)
{
  QR <-qr (matrix(rnorm(n^2),ncol=n)) # A = QR
  M <-qr.Q(QR)%*%diag(sign(diag(qr.R(QR)))) # d i a g (R) > 0
  return (M)
}
# generate random member of specialorthogonal group SO( n )
Random_rotation_matrix<-function(n)
{
  M <-random_rotation_matrix_incl_flip(n)
  if(det(M)<0) M[,1]<-M[,1 ] # det(M) = +1
  return(M)
}
R<-Random_rotation_matrix(4)
dim(iris.scaled)
L=as.matrix(iris.scaled[,-5])
Rotated.Space<-L%*%R
head(Rotated.Space)
N.species<-as.factor((N.species))
Rotated.Space<-data.frame(Rotated.Space,iris$Species)
head(Rotated.Space)
names(Rotated.Space)<- c("Sc.SL","Sc.SW","Sc.PL","Sc.PW","N.species")
head(Rotated.Space)
dim(Rotated.Space)
summary(Rotated.Space)
str(Rotated.Space)
# Load the party package. It will automatically load other required packages.
library(party)
library(randomForest)
library(caret)
trainIndex<-createDataPartition(Rotated.Space$N.species, p = .6,list = FALSE,times = 1)
RirisTrain <- Rotated.Space[trainIndex,]
RirisTest  <- Rotated.Space[-trainIndex,]


fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE)

set.seed(1002)
                           
rfFit<- train(N.species~.,data = RirisTrain,method = "rf", 
              trControl = fitControl,metric = "ROC")
rfFit
plot(rfFit)     

RirisTest$pred<-predict(rfFit, newdata =RirisTest)

confusionMatrix(data = RirisTest$pred, reference = RirisTest$N.species)




set.seed(2)
library(caret)
trainIndex<-createDataPartition(Riris$Species, p = .6,list = FALSE,times = 1)
Train <- Riris[trainIndex,]
Test  <- Riris[-trainIndex,]


#Iris example
library(rpart)
str(RirisTrain)
RirisTrain$N.species <- factor( RirisTrain$N.species)
iris.adaboost <- boosting(factor(N.Species)~., data=RirisTrain, mfinal=10)
margins(iris.adaboost,Train)->iris.margins # training set
plot.margins(iris.margins)
# test set
iris.predboosting<- predict.boosting(iris.adaboost, newdata=Test)
margins(iris.predboosting,Test)->iris.predmargins
plot.margins(iris.predmargins,iris.margins)

iris.predboosting$prob
iris.predboosting$confusion
iris.predboosting$error
iris.adaboost<-boosting.cv(Species~., data=Train, v = 10, boos = TRUE, mfinal = 100,
            coeflearn = "Breiman", control=rpart.control(cp=0.01))
