rm(list=ls())
install.packages("C50")
require(C50)
data1 <- read.csv("/Users/narensham/Documents/Data Analysis/IDA/Biomechanical_Data_column_2C_weka.csv",h=T)
head(data1)

set.seed(9850)
g<- runif(nrow(data1))
data1shuffled<- data1[order(g),]


attach(data1shuffled)
head(data1shuffled)
dt1<-rpart(class~.,data=data1shuffled[1:210,],method="class",parms = list(split="information"))

summary(dt1)

?rpart

dt25<-rpart(class~.,data=data1shuffled[1:210,],method="class",control = rpart.control(minbucket = 25),parms = list(split="information"))
summary(dt25)

dt20<-rpart(class~.,data=data1shuffled[1:210,],method="class",control = rpart.control(minbucket = 20),parms = list(split="information"))
summary(dt20)


dt15<-rpart(class~.,data=data1shuffled[1:210,],method="class",control = rpart.control(minbucket = 15),parms = list(split="information"))
summary(dt15)

dt10<-rpart(class~.,data=data1shuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))
summary(dt10)

dt5<-rpart(class~.,data=data1shuffled[1:210,],method="class",control = rpart.control(minbucket = 5),parms = list(split="information"))
summary(dt5)

rpart.plot(dt25)

rpart.plot(dt20)

rpart.plot(dt15)

rpart.plot(dt10)

rpart.plot(dt5)

p25<-predict(dt25,data1shuffled[211:310,],type="class")
t25<-table(data1shuffled[211:310,7],predicted=p25)
diag = diag(t25) # number of correctly classified instances per class 
n=sum(t25)
rowsums = apply(t25, 1, sum) # number of instances per class
colsums = apply(t25, 2, sum) # number of predictions per class
accuracy25 = sum(diag) /  n
precision25 = diag / colsums 
recall25 = diag / rowsums 
accuracy25
precision25
recall25


p20<-predict(dt20,data1shuffled[211:310,],type="class")
t20<-table(data1shuffled[211:310,7],predicted=p20)
diag = diag(t20) # number of correctly classified instances per class 
n=sum(t20)
rowsums = apply(t20, 1, sum) # number of instances per class
colsums = apply(t20, 2, sum) # number of predictions per class
accuracy20 = sum(diag) /  n
precision20 = diag / colsums 
recall20 = diag / rowsums 
accuracy20
precision20
recall20



p15<-predict(dt15,data1shuffled[211:310,],type="class")
t15<-table(data1shuffled[211:310,7],predicted=p15)
diag = diag(t15) # number of correctly classified instances per class 
n=sum(t15)
rowsums = apply(t15, 1, sum) # number of instances per class
colsums = apply(t15, 2, sum) # number of predictions per class
accuracy15 = sum(diag) /  n
precision15 = diag / colsums 
recall15 = diag / rowsums 
accuracy15
precision15
recall15

p10<-predict(dt10,data1shuffled[211:310,],type="class")
t10<-table(data1shuffled[211:310,7],predicted=p10)
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy10 = sum(diag) /  n
precision10 = diag / colsums 
recall10 = diag / rowsums 
accuracy10
precision10
recall10


p5<-predict(dt5,data1shuffled[211:310,],type="class")
t5<-table(data1shuffled[211:310,7],predicted=p5)
diag = diag(t5) # number of correctly classified instances per class 
n=sum(t5)
rowsums = apply(t5, 1, sum) # number of instances per class
colsums = apply(t5, 2, sum) # number of predictions per class
accuracy5 = sum(diag) /  n
precision5 = diag / colsums 
recall5 = diag / rowsums 
accuracy5
precision5
recall5

Accuracyvector<-c(accuracy5,accuracy10,accuracy15,accuracy20,accuracy25)
names(Accuracyvector)<- c("5","10","15","20","25")
barplot(Accuracyvector[5:1],ylab ="% of Accuracy",xlab="Minimum no of observation at any leaf node",main="Accuracy")

Precisionvector_abnormal<-c(precision25[1],precision20[1],precision15[1],precision10[1],precision5[1])
names(Precisionvector_abnormal)<-c("25","20","15","10","5")
barplot(Precisionvector_abnormal)

Precisionvector_normal<-c(precision25[2],precision20[2],precision15[2],precision10[2],precision5[2])
names(Precisionvector_normal)<-c("25","20","15","10","5")
barplot(Precisionvector_normal)

Recallvector_abnormal<-c(recall25[1],recall20[1],recall20[1],recall10[1],recall5[1])
names(Recallvector_abnormal)<-c("25","20","15","10","5")
barplot(Recallvector_abnormal,ylab ="% of Recall",xlab="Minimum no of observation at any leaf node",main="Recall")

detach(data1shuffled)

set.seed(9851)
g<- runif(nrow(data1))
data1reshuffled<- data1[order(g),]
attach(data1reshuffled)
dt10<-rpart(class~.,data=data1reshuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))
summary(dt10)

p10<-predict(dt10,data1reshuffled[211:310,],type="class")
t10<-table(data1reshuffled[211:310,7],predicted=p10)
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy = sum(diag) /  n
precision = diag / colsums 
recall = diag / rowsums 
accuracy
precision
recall

detach(data1reshuffled)

set.seed(9852)
g<- runif(nrow(data1))
data1reshuffled<- data1[order(g),]
attach(data1reshuffled)
dt10<-rpart(class~.,data=data1reshuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))
summary(dt10)

p10<-predict(dt10,data1reshuffled[211:310,],type="class")
t10<-table(data1reshuffled[211:310,7],predicted=p10)
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy = sum(diag) /  n
precision = diag / colsums 
recall = diag / rowsums 
accuracy
precision
recall

detach(data1reshuffled)


set.seed(4)
g<- runif(nrow(data1))
data1reshuffled<- data1[order(g),]
attach(data1reshuffled)
dt10<-rpart(class~.,data=data1reshuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))
summary(dt10)

p10<-predict(dt10,data1reshuffled[211:310,],type="class")
t10<-table(data1reshuffled[211:310,7],predicted=p10)
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy = sum(diag) /  n
precision = diag / colsums 
recall = diag / rowsums 
accuracy
precision
recall


detach(data1reshuffled)


set.seed(4264)
g<- runif(nrow(data1))
data1reshuffled<- data1[order(g),]
attach(data1reshuffled)
dt10<-rpart(class~.,data=data1reshuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))
summary(dt10)

p10<-predict(dt10,data1reshuffled[211:310,],type="class")
t10<-table(data1reshuffled[211:310,7],predicted=p10)
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy = sum(diag) /  n
precision = diag / colsums 
recall = diag / rowsums 
accuracy
precision
recall

detach(data1reshuffled)


set.seed(7890)
g<- runif(nrow(data1))
data1reshuffled<- data1[order(g),]
attach(data1reshuffled)
dt10<-rpart(class~.,data=data1reshuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))
summary(dt10)

p10<-predict(dt10,data1reshuffled[211:310,],type="class")
t10<-table(data1reshuffled[211:310,7],predicted=p10)
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy = sum(diag) /  n
precision = diag / colsums 
recall = diag / rowsums 
accuracy
precision
recall

sd(c(0.76,0.84,0.84,0.75,0.77))

detach(data1shuffled)

data2 <- read.csv("/Users/narensham/Documents/Data Analysis/IDA/BiomechanicalData_column_3C_weka.csv",h=T)

head(data2)
set.seed(9850)
g<- runif(nrow(data2))
data2shuffled<- data2[order(g),]
attach(data2shuffled)

dt25<-rpart(class~.,data=data2shuffled[1:210,],method="class",control = rpart.control(minbucket = 25),parms = list(split="information"))
summary(dt25)

dt20<-rpart(class~.,data=data2shuffled[1:210,],method="class",control = rpart.control(minbucket = 20),parms = list(split="information"))
summary(dt20)


dt15<-rpart(class~.,data=data2shuffled[1:210,],method="class",control = rpart.control(minbucket = 15),parms = list(split="information"))
summary(dt15)

dt10<-rpart(class~.,data=data2shuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))
summary(dt10)

dt5<-rpart(class~.,data=data2shuffled[1:210,],method="class",control = rpart.control(minbucket = 5),parms = list(split="information"))
summary(dt5)


rpart.plot(dt25)

rpart.plot(dt20)

rpart.plot(dt15)

rpart.plot(dt10)

rpart.plot(dt5)


p25<-predict(dt25,data2shuffled[211:310,],type="class")
t25<-table(data2shuffled[211:310,7],predicted=p25)
diag = diag(t25) # number of correctly classified instances per class 
n=sum(t25)
rowsums = apply(t25, 1, sum) # number of instances per class
colsums = apply(t25, 2, sum) # number of predictions per class
accuracy25 = sum(diag) /  n
precision25 = diag / colsums 
recall25 = diag / rowsums 
accuracy25
precision25
recall25

p25probs<-predict(dt25,data2shuffled[211:310,],type="prob")
auc<-auc(data2shuffled$class,p25probs)
plot(multiclass.roc(data2shuffled$class,p25probs,levels = 3))


p20<-predict(dt20,data2shuffled[211:310,],type="class")
t20<-table(data2shuffled[211:310,7],predicted=p20)
diag = diag(t20) # number of correctly classified instances per class 
n=sum(t20)
rowsums = apply(t20, 1, sum) # number of instances per class
colsums = apply(t20, 2, sum) # number of predictions per class
accuracy20 = sum(diag) /  n
precision20 = diag / colsums 
recall20 = diag / rowsums 
accuracy20
precision20
recall20



p15<-predict(dt15,data2shuffled[211:310,],type="class")
t15<-table(data2shuffled[211:310,7],predicted=p15)
diag = diag(t15) # number of correctly classified instances per class 
n=sum(t15)
rowsums = apply(t15, 1, sum) # number of instances per class
colsums = apply(t15, 2, sum) # number of predictions per class
accuracy15 = sum(diag) /  n
precision15 = diag / colsums 
recall15 = diag / rowsums 
accuracy15
precision15
recall15

p10<-predict(dt10,data2shuffled[211:310,],type="class")
t10<-table(data2shuffled[211:310,7],predicted=p10)
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy10 = sum(diag) /  n
precision10 = diag / colsums 
recall10 = diag / rowsums 
accuracy10
precision10
recall10


p5<-predict(dt5,data2shuffled[211:310,],type="class")
t5<-table(data2shuffled[211:310,7],predicted=p5)
diag = diag(t5) # number of correctly classified instances per class 
n=sum(t5)
rowsums = apply(t5, 1, sum) # number of instances per class
colsums = apply(t5, 2, sum) # number of predictions per class
accuracy5 = sum(diag) /  n
precision5 = diag / colsums 
recall5 = diag / rowsums 
accuracy5
precision5
recall5

Accuracyvector<-c(accuracy5,accuracy10,accuracy15,accuracy20,accuracy25)
names(Accuracyvector)<- c("5","10","15","20","25")
barplot(Accuracyvector[5:1],ylab ="% of Accuracy",xlab="Minimum no of observation at any leaf node",main="Accuracy")
install.packages("pROC")
require(pROC)





detach(data2shuffled)

head(data1)

install.packages("Hmisc")
library(Hmisc) # cut2

data1$pelvic_incidence_grp<-as.numeric(cut2(data1$pelvic_incidence, g=4))

#plot(data1$pelvic_incidence~data1$pelvic_incidence_grp)


length(which(data1$pelvic_incidence_grp==1))
length(which(data1$pelvic_incidence_grp==2))
length(which(data1$pelvic_incidence_grp==3))
length(which(data1$pelvic_incidence_grp==4))

aggregate(data1$pelvic_incidence, by=list(Category=data1$pelvic_incidence_grp), FUN=min)
aggregate(data1$pelvic_incidence, by=list(Category=data1$pelvic_incidence_grp), FUN=max)


data1$pelvic_tilt_grp<-as.numeric(cut2(data1$pelvic_tilt.numeric, g=4))
aggregate(data1$pelvic_tilt.numeric, by=list(Category=data1$pelvic_tilt_grp), FUN=min)
aggregate(data1$pelvic_tilt.numeric, by=list(Category=data1$pelvic_tilt_grp), FUN=max)

length(which(data1$pelvic_tilt_grp==1))
length(which(data1$pelvic_tilt_grp==2))
length(which(data1$pelvic_tilt_grp==3))
length(which(data1$pelvic_tilt_grp==4))

data1$lumbar_lordosis_grp<-as.numeric(cut2(data1$lumbar_lordosis_angle, g=4))
aggregate(data1$lumbar_lordosis_angle, by=list(Category=data1$lumbar_lordosis_grp), FUN=min)
aggregate(data1$lumbar_lordosis_angle, by=list(Category=data1$lumbar_lordosis_grp), FUN=max)


length(which(data1$lumbar_lordosis_grp==1))
length(which(data1$lumbar_lordosis_grp==2))
length(which(data1$lumbar_lordosis_grp==3))
length(which(data1$lumbar_lordosis_grp==4))

data1$sacral_grp<-as.numeric(cut2(data1$sacral_slope, g=4))
aggregate(data1$sacral_slope, by=list(Category=data1$sacral_grp), FUN=min)
aggregate(data1$sacral_slope, by=list(Category=data1$sacral_grp), FUN=max)


length(which(data1$sacral_grp==1))
length(which(data1$sacral_grp==2))
length(which(data1$sacral_grp==3))
length(which(data1$sacral_grp==4))

data1$pelvic_radius_grp<-as.numeric(cut2(data1$pelvic_radius, g=4))
aggregate(data1$pelvic_radius, by=list(Category=data1$pelvic_radius_grp), FUN=min)
aggregate(data1$pelvic_radius, by=list(Category=data1$pelvic_radius_grp), FUN=max)


length(which(data1$pelvic_radius_grp==1))
length(which(data1$pelvic_radius_grp==2))
length(which(data1$pelvic_radius_grp==3))
length(which(data1$pelvic_radius_grp==4))


data1$degree_spondylolisthesis_grp<-as.numeric(cut2(data1$degree_spondylolisthesis, g=4))
aggregate(data1$degree_spondylolisthesis, by=list(Category=data1$degree_spondylolisthesis_grp), FUN=min)
aggregate(data1$degree_spondylolisthesis, by=list(Category=data1$degree_spondylolisthesis_grp), FUN=max)


length(which(data1$degree_spondylolisthesis_grp==1))
length(which(data1$degree_spondylolisthesis_grp==2))
length(which(data1$degree_spondylolisthesis_grp==3))
length(which(data1$degree_spondylolisthesis_grp==4))


head(data1)
data3<-(data1[7:13])

data3$pelvic_incidence_grp<-data3$pelvic_incidence_grp-1
data3$pelvic_tilt_grp<-data3$pelvic_tilt_grp-1
data3$lumbar_lordosis_grp<-data3$lumbar_lordosis_grp-1
data3$sacral_grp<-data3$sacral_grp-1
data3$pelvic_radius_grp<-data3$pelvic_radius_grp-1
data3$degree_spondylolisthesis_grp<-data3$degree_spondylolisthesis_grp-1

head(data3)
attach(data3)


set.seed(9850)
g<- runif(nrow(data3))
data3shuffled<- data3[order(g),]


dt10<-rpart(class~.,data=data3shuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))
summary(dt10)

rpart.plot(dt10)


p10<-predict(dt10,data3shuffled[211:310,],type="class")
t10<-table(data3shuffled[211:310,1],predicted=p10)
t10
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy = sum(diag) /  n
precision = diag / colsums 
recall = diag / rowsums 
accuracy
precision
recall


