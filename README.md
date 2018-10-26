# Decision-Tree-Variance-with-Accuracy
A graphical view of how the accuracy of the classification algorithm changes with leaf node sizes



####The code is performed in R using package : Rpart and all the code is explained below. Please note that the markdown document has comments beginning with "#" accompanying code explaining the code. All the code is inserted in the document as the document is knitted in a serial manner and certain outputs cannot be masked or ommited from being displayed.

##Question 1: Using 2 Classes.

#### Q1: Take Data2 and split it into randomly selected 210 training instances and remaining 100 as test instance. Create decision trees using the training set and the “minimum records per leaf node” values of 5, 10, 15, 20, and 25. 
```{r}
require(rpart)#For making decision tree models
require(rpart.plot)#For making decision tree diagrams

data1 <- read.csv("/Users/narensham/Documents/Data Analysis/IDA/Biomechanical_Data_column_2C_weka.csv",h=T) # Reading the dataset
head(data1)#Preview of Data

#Shuffling the data

set.seed(9850)# For randomizing the data
g<- runif(nrow(data1)) #Index
data1shuffled<- data1[order(g),] #Subset of data


attach(data1shuffled) 

dt25<-rpart(class~.,data=data1shuffled[1:210,],method="class",control = rpart.control(minbucket = 25),parms = list(split="information"))

#The decision trees are computed for the first 210 observations of the shuffled dataset. The minimum observation at leaf node is set to 25 here, and the Information Gain method is used in computation.



dt20<-rpart(class~.,data=data1shuffled[1:210,],method="class",control = rpart.control(minbucket = 20),parms = list(split="information"))
 # For minimum observation of leaf node =20



dt15<-rpart(class~.,data=data1shuffled[1:210,],method="class",control = rpart.control(minbucket = 15),parms = list(split="information"))
 # For minimum observation of leaf node =15



dt10<-rpart(class~.,data=data1shuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))
 # For minimum observation of leaf node =10



dt5<-rpart(class~.,data=data1shuffled[1:210,],method="class",control = rpart.control(minbucket = 5),parms = list(split="information"))
 # For minimum observation of leaf node =5



```
---------------------


#### 1.a.	Show the tree for the value 25. Comment on what you notice about the five trees. R Markdown
```{r}

rpart.plot(dt25)

```

### Here, it is observed that as the minimum number of observations at any leaf node is reduced, the grain of the model becomes higher and the tree has more branches.

---------------------

#### 1.b.	For each tree compute and report the accuracy, precision, and recall values. Comment on the comparison of these values and show these values on a plot.

### To perform the following, we will have to predict the values for each tree and draw out the confusion matrix. 
```{r}

# For 25 minimum observations at any leaf node
p25<-predict(dt25,data1shuffled[211:310,],type="class")
t25<-table(data1shuffled[211:310,7],predicted=p25)
diag = diag(t25) # number of correctly classified instances per class 
n=sum(t25)
rowsums = apply(t25, 1, sum) # number of instances per class
colsums = apply(t25, 2, sum) # number of predictions per class
accuracy25 = sum(diag) /  n
precision25 = diag / colsums 
recall25 = diag / rowsums 
accuracy25 # Accuracy For 25 minimum observations at any leaf node
precision25 # Precision for 25 minimum observations at any leaf node
recall25 # Recall for 25 minimum observations at any leaf node



# For 20 minimum observations at any leaf node
p20<-predict(dt20,data1shuffled[211:310,],type="class")
t20<-table(data1shuffled[211:310,7],predicted=p20)
diag = diag(t20) # number of correctly classified instances per class 
n=sum(t20)
rowsums = apply(t20, 1, sum) # number of instances per class
colsums = apply(t20, 2, sum) # number of predictions per class
accuracy20 = sum(diag) /  n
precision20 = diag / colsums 
recall20 = diag / rowsums 
accuracy20 # Accuracy For 20 minimum observations at any leaf node
precision20 # Precision for 20 minimum observations at any leaf node
recall20 # Recall for 20 minimum observations at any leaf node


# For 15 minimum observations at any leaf node
p15<-predict(dt15,data1shuffled[211:310,],type="class")
t15<-table(data1shuffled[211:310,7],predicted=p15)
diag = diag(t15) # number of correctly classified instances per class 
n=sum(t15)
rowsums = apply(t15, 1, sum) # number of instances per class
colsums = apply(t15, 2, sum) # number of predictions per class
accuracy15 = sum(diag) /  n
precision15 = diag / colsums 
recall15 = diag / rowsums 
accuracy15 # Accuracy For 15 minimum observations at any leaf node
precision15 # Precision For 15 minimum observations at any leaf node
recall15 # Recall For 15 minimum observations at any leaf node


# For 10 minimum observations at any leaf node
p10<-predict(dt10,data1shuffled[211:310,],type="class")
t10<-table(data1shuffled[211:310,7],predicted=p10)
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy10 = sum(diag) /  n
precision10 = diag / colsums 
recall10 = diag / rowsums 
accuracy10 # Accuracy For 10 minimum observations at any leaf node
precision10 # Precision For 10 minimum observations at any leaf node
recall10 # Recall For 10 minimum observations at any leaf node

# For 5 minimum observations at any leaf node
p5<-predict(dt5,data1shuffled[211:310,],type="class")
t5<-table(data1shuffled[211:310,7],predicted=p5)
diag = diag(t5) # number of correctly classified instances per class 
n=sum(t5)
rowsums = apply(t5, 1, sum) # number of instances per class
colsums = apply(t5, 2, sum) # number of predictions per class
accuracy5 = sum(diag) /  n
precision5 = diag / colsums 
recall5 = diag / rowsums 
accuracy5 # Accuracy For 5 minimum observations at any leaf node
precision5 # Precision For 5 minimum observations at any leaf node
recall5 # Recall For 5 minimum observations at any leaf node

# Below are the confusion matrices for each :

t25 #For 25 minimum observations at any leaf node
t20 #For 20 minimum observations at any leaf node
t15 #For 15 minimum observations at any leaf node
t10 #For 10 minimum observations at any leaf node
t5 #For 5 minimum observations at any leaf node

# Examining the accuracy
Accuracyvector<-c(accuracy5,accuracy10,accuracy15,accuracy20,accuracy25)
names(Accuracyvector)<- c("5","10","15","20","25")
barplot(Accuracyvector[5:1],ylab ="% of Accuracy",xlab="Minimum no of observation at any leaf node",main="Accuracy")

```

### We notice that the Accuracy is the least for minbucket=25. Here, we notice that when minbucket=20, the accuracy is highest. However, this is not the result that we need as we need to ensure that more number of abnormal cases should not be predicted as normal.

#### Let us take a look at the precision and recall for all models.
```{r}

Precisionvector_abnormal<-c(precision25[1],precision20[1],precision15[1],precision10[1],precision5[1])
names(Precisionvector_abnormal)<-c("25","20","15","10","5")
barplot(Precisionvector_abnormal,ylab ="% of Precision",xlab="Minimum no of observation at any leaf node",main="Precision: Abnormal")

Precisionvector_normal<-c(precision25[2],precision20[2],precision15[2],precision10[2],precision5[2])
names(Precisionvector_normal)<-c("25","20","15","10","5")
barplot(Precisionvector_normal,ylab ="% of Precision",xlab="Minimum no of observation at any leaf node",main="Precision: Normal")


Recallvector_abnormal<-c(recall25[1],recall20[1],recall20[1],recall10[1],recall5[1])
names(Recallvector_abnormal)<-c("25","20","15","10","5")
barplot(Recallvector_abnormal,ylab ="% of Recall",xlab="Minimum no of observation at any leaf node",main="Recall for Abnormal Class")


```



## Hence, with decreasing the minimum at any leaf node, we notice that there is a reduction in misclassification of class abnormal.

---------------------


#### 1.c.	Now limit yourself to the case of 10 minimum records per leaf node. Repeat the tree learning exercise five times by randomly choosing different sets of 210 training instances. Report the accuracy, precision, and recall values for each run and also their averages and standard deviations. Comment on the variability of the values as the random sample changes. 


### The data is reshuffled and the process is repeated for 5 iterations.
```{r}



set.seed(9851)# for randomizing
g<- runif(nrow(data1))
data1reshuffled<- data1[order(g),]
attach(data1reshuffled)
dt10<-rpart(class~.,data=data1reshuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))

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
#summary(dt10)

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
#summary(dt10)

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
#summary(dt10)

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
#summary(dt10)

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

detach(data1shuffled)

```

### We notice that the accuracy, precision and recall values largly remain the same through the iterations. The accuracy is at a mean of 76% with a standard deviation of 4.43%.

---------------------

## Question 2

#### 2. Repeat the same tasks as done in Question-1 above for Data3. In addition to reporting results for 2a, 2b, and 2c, comment on the comparison of results obtained for 1c and 2c. Give your analysis for the differences in results. Label this answer as 2d.

#### 2.a 
```{r}

data2 <- read.csv("/Users/narensham/Documents/Data Analysis/IDA/BiomechanicalData_column_3C_weka.csv",h=T)

head(data2)
set.seed(9850)
g<- runif(nrow(data2))
data2shuffled<- data2[order(g),]
attach(data2shuffled)
 

dt25<-rpart(class~.,data=data2shuffled[1:210,],method="class",control = rpart.control(minbucket = 25),parms = list(split="information"))# For minimum observation of leaf node =25

dt20<-rpart(class~.,data=data2shuffled[1:210,],method="class",control = rpart.control(minbucket = 20),parms = list(split="information"))# For minimum observation of leaf node =20

dt15<-rpart(class~.,data=data2shuffled[1:210,],method="class",control = rpart.control(minbucket = 15),parms = list(split="information"))# For minimum observation of leaf node =15

dt10<-rpart(class~.,data=data2shuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))# For minimum observation of leaf node =10

dt5<-rpart(class~.,data=data2shuffled[1:210,],method="class",control = rpart.control(minbucket = 5),parms = list(split="information"))# For minimum observation of leaf node =5


rpart.plot(dt25)



```
#### 2.b To plot the accuracies as done before and obtain the confusion matrices.
```{r}

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


t25 #For 25 minimum observations at any leaf node
t20 #For 20 minimum observations at any leaf node
t15 #For 15 minimum observations at any leaf node
t10 #For 10 minimum observations at any leaf node
t5 #For 5 minimum observations at any leaf node


```
#### The accuracy is clearly increasing with reducing the minimum observation at any leaf node. There is a distinct reduction with each step of decrease as observed in the plot above.




#### 2.c : Repeat the model for 5 iterations and obtain the accuracy precison and recall for each iteration

```{r}

set.seed(9851)
g<- runif(nrow(data2))
data2reshuffled<- data2[order(g),]
attach(data2reshuffled)
dt10<-rpart(class~.,data=data2reshuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))

# Iteration 1 

p10<-predict(dt10,data2reshuffled[211:310,],type="class")
t10<-table(data2reshuffled[211:310,7],predicted=p10)
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

detach(data2reshuffled)

set.seed(9852)
g<- runif(nrow(data2))
data2reshuffled<- data2[order(g),]
attach(data2reshuffled)
dt10<-rpart(class~.,data=data2reshuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))
# Iteration 2

p10<-predict(dt10,data2reshuffled[211:310,],type="class")
t10<-table(data2reshuffled[211:310,7],predicted=p10)
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

detach(data2reshuffled)

# Iteration 3

set.seed(4)
g<- runif(nrow(data1))
data2reshuffled<- data2[order(g),]
attach(data2reshuffled)
dt10<-rpart(class~.,data=data2reshuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))

p10<-predict(dt10,data2reshuffled[211:310,],type="class")
t10<-table(data2reshuffled[211:310,7],predicted=p10)
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


detach(data2reshuffled)
# Iteration 4

set.seed(4264)
g<- runif(nrow(data2))
data2reshuffled<- data2[order(g),]
attach(data2reshuffled)
dt10<-rpart(class~.,data=data2reshuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))

p10<-predict(dt10,data2reshuffled[211:310,],type="class")
t10<-table(data2reshuffled[211:310,7],predicted=p10)
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

detach(data2reshuffled)

# Iteration 5
set.seed(7890)
g<- runif(nrow(data2))
data2reshuffled<- data2[order(g),]
attach(data2reshuffled)
dt10<-rpart(class~.,data=data2reshuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))

p10<-predict(dt10,data2reshuffled[211:310,],type="class")
t10<-table(data2reshuffled[211:310,7],predicted=p10)
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
```

### 2.d Give your analysis for the differences in results. 
## Here, we notice that the accuracy, precision and recall of the models increase as there are 3 seperate classes to bin each of the obsevations. By training the models 5 times, the accuracy more or less remains the same.
---------------------

## Question 3

#### Take Data2 for this question. Partition each column into four sets of equal widths of values. Assign these intervals as values 0, 1, 2, and 3 and replace each value by its corresponding interval value. 

```{r}
require(Hmisc) # cut2

data1$pelvic_incidence_grp<-as.numeric(cut2(data1$pelvic_incidence, g=4)) # Grouping of equal widths


# To show the length of each group
length(which(data1$pelvic_incidence_grp==1))
length(which(data1$pelvic_incidence_grp==2))
length(which(data1$pelvic_incidence_grp==3))
length(which(data1$pelvic_incidence_grp==4))


#Repeating for other values
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



```

####. 3.a.	Show the boundaries for each interval for each attribute.
```{r}
# For Pelvic Incidence
pelvic_incidence_intervals<-aggregate(data1$pelvic_incidence, by=list(Category=data1$pelvic_incidence_grp), FUN=min)
pelvic_incidence_intervals[,3]<-aggregate(data1$pelvic_incidence, by=list(Category=data1$pelvic_incidence_grp), FUN=max)[2]
#Lower and upper limits
pelvic_incidence_intervals

# For Pelvic Tilt

data1$pelvic_tilt_grp<-as.numeric(cut2(data1$pelvic_tilt.numeric, g=4))
pelvic_tilt_int<-aggregate(data1$pelvic_tilt.numeric, by=list(Category=data1$pelvic_tilt_grp), FUN=min)
pelvic_tilt_int[,3]<-aggregate(data1$pelvic_tilt.numeric, by=list(Category=data1$pelvic_tilt_grp), FUN=max)[2]
#Lower and upper limits
pelvic_tilt_int


# For lumbar lordosis
data1$lumbar_lordosis_grp<-as.numeric(cut2(data1$lumbar_lordosis_angle, g=4))
lumbar_lordosis_int<-aggregate(data1$lumbar_lordosis_angle, by=list(Category=data1$lumbar_lordosis_grp), FUN=min)
lumbar_lordosis_int[,3]<-aggregate(data1$lumbar_lordosis_angle, by=list(Category=data1$lumbar_lordosis_grp), FUN=max)[2]
#Lower and upper limits
lumbar_lordosis_int

# For sacral slope
data1$sacral_grp<-as.numeric(cut2(data1$sacral_slope, g=4))
sacral_int<-aggregate(data1$sacral_slope, by=list(Category=data1$sacral_grp), FUN=min)
sacral_int[,3]<-aggregate(data1$sacral_slope, by=list(Category=data1$sacral_grp), FUN=max)[2]
#Lower and upper limits
sacral_int


#For Pelvic Radius
data1$pelvic_radius_grp<-as.numeric(cut2(data1$pelvic_radius, g=4))
pelvic_radius_int<-aggregate(data1$pelvic_radius, by=list(Category=data1$pelvic_radius_grp), FUN=min)
pelvic_radius_int[,3]<-aggregate(data1$pelvic_radius, by=list(Category=data1$pelvic_radius_grp), FUN=max)[2]
#Lower and upper limits
pelvic_radius_int


# For degree of spondylolisthesis
data1$degree_spondylolisthesis_grp<-as.numeric(cut2(data1$degree_spondylolisthesis, g=4))
degree_spondylolisthesis_int<-aggregate(data1$degree_spondylolisthesis, by=list(Category=data1$degree_spondylolisthesis_grp), FUN=min)
degree_spondylolisthesis_int[,3]<-aggregate(data1$degree_spondylolisthesis, by=list(Category=data1$degree_spondylolisthesis_grp), FUN=max)[2]
#Lower and upper limits

degree_spondylolisthesis_int

```

## 3.b. Learn a decision tree with this transformed data and compute performance parameters in the same way as done for 1c and 2c.


```{r}

set.seed(9876)
g<- runif(nrow(data3))
data3shuffled<- data3[order(g),]


dt10<-rpart(class~.,data=data3shuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))


p10<-predict(dt10,data3shuffled[211:310,],type="class")
t10<-table(data3shuffled[211:310,1],predicted=p10)
t10
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy1 = sum(diag) /  n
precision = diag / colsums 
recall = diag / rowsums 
accuracy1
precision
recall


set.seed(8931)
g<- runif(nrow(data3))
data3shuffled<- data3[order(g),]


dt10<-rpart(class~.,data=data3shuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))



p10<-predict(dt10,data3shuffled[211:310,],type="class")
t10<-table(data3shuffled[211:310,1],predicted=p10)
t10
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy2 = sum(diag) /  n
precision = diag / colsums 
recall = diag / rowsums 
accuracy2
precision
recall


set.seed(9850)
g<- runif(nrow(data3))
data3shuffled<- data3[order(g),]


dt10<-rpart(class~.,data=data3shuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))


p10<-predict(dt10,data3shuffled[211:310,],type="class")
t10<-table(data3shuffled[211:310,1],predicted=p10)
t10
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy3 = sum(diag) /  n
precision = diag / colsums 
recall = diag / rowsums 
accuracy3
precision
recall


set.seed(8896)
g<- runif(nrow(data3))
data3shuffled<- data3[order(g),]


dt10<-rpart(class~.,data=data3shuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))



p10<-predict(dt10,data3shuffled[211:310,],type="class")
t10<-table(data3shuffled[211:310,1],predicted=p10)
t10
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy4 = sum(diag) /  n
precision = diag / colsums 
recall = diag / rowsums 
accuracy4
precision
recall


set.seed(1234)
g<- runif(nrow(data3))
data3shuffled<- data3[order(g),]


dt10<-rpart(class~.,data=data3shuffled[1:210,],method="class",control = rpart.control(minbucket = 10),parms = list(split="information"))


p10<-predict(dt10,data3shuffled[211:310,],type="class")
t10<-table(data3shuffled[211:310,1],predicted=p10)
t10
diag = diag(t10) # number of correctly classified instances per class 
n=sum(t10)
rowsums = apply(t10, 1, sum) # number of instances per class
colsums = apply(t10, 2, sum) # number of predictions per class
accuracy5 = sum(diag) /  n
precision = diag / colsums 
recall = diag / rowsums 
accuracy5
precision
recall

barplot(c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5))

```


#### The accuracies vary around 80% as shown above, for the 5 iterations.

#### 3.c.	Compare these results with those obtained for 1c. Analyze the differences in performance and give your intuitive reasons why these differences are observed. 


By dividing the dataset into 4 intervals of equal width, the accuracy of the model increases as we are categorizing each of the continuous variables and binning them into equal widths. This is due to making each value into fixed boundaries based on quantiles, we reduced the continuous variables which in turn increased the efficiancy of the model.


