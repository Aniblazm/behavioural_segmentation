#Import the neccesary libraries
library(dplyr)
library(VIM)
library(Hmisc)
library(corrplot)
library(cluster)
library(HSAUR)
library(factoextra)
library(rpart)
library(rpart.plot)
library(pastecs)

#Setting the working environment and loading the data
setwd("/.")
data_imported <- read.csv("data.csv", header=TRUE, colClasses=c("character","numeric","factor","character"))



# Modify the variables to get the frequency, recency and monetary value
rfm <-  data_imported %>%
  group_by(user_id) %>%
  summarise(
    frequency = n(),
    amount = sum(amount),
    latest_purchase = max(date)
  )
rfm$recency <- Sys.Date() - as.Date(rfm$latest_purchase)


# Convert all variables to numeric and check missing values
aggr(rfm, numbers=T,sortVars=T)
instanceconvert <- colnames(rfm[,-c(1,4)])
for (i in instanceconvert)
{
  rfm[[i]] <- as.numeric(rfm[[i]])
}
str(rfm)


# Check a correlation matrix
rfm_num = rfm[,-c(1,4)] 
corrplot(cor(rfm_num), method = "pie", type = "upper")

# Normalize the data and apply the kmeans algorithm to get 3 clusters
scaled_rfm_num= scale(rfm_num)
set.seed(20)
rfmCluster3 <- kmeans(scaled_rfm_num[, 1:3], 3, nstart = 20)
rfmCluster3

rfm$cluster3 = rfmCluster3$cluster
scaled_rfm_num$cluster3 = rfmCluster3$cluster




# Create a variable for each cluster
cluster_1 = rfm %>%   
  filter(cluster3==1)
dim(cluster_1)


cluster_2 = rfm %>%   
  filter(cluster3==2)
dim(cluster_2)


cluster_3 = rfm %>%   
  filter(cluster3==3)
dim(cluster_3)

# Get some statistics about each cluster
scores_cluster1<- rfm %>%
  filter(cluster3==1)%>%
  select(frequency,amount,recency)

scores_cluster2<- rfm %>%
  filter(cluster3==2)%>%
  select(frequency,amount,recency)

scores_cluster3<- rfm %>%
  filter(cluster3==3)%>%
  select(frequency,amount,recency)

options(scipen=100)
options(digits=2)

stat.desc(scores_cluster1)
stat.desc(scores_cluster2)
stat.desc(scores_cluster3)

# NOW WE ARE CREATING A DECISION TREE TO UNDERSTAND WHICH VARIABLES USED THE KMEANS TO CLUSTERIZE OUR SEGMENT
#We want to balance the presence of the 3 groups in our train test so we are taking just 800 users form each group to generate the train cluster
# Create train and test groups from cluster 1
row.cluster_1=dim(cluster_1)[1]
ind.cluster_1=1:row.cluster_1
ind.train.cluster_1=sort(sample(ind.cluster_1,800))  #indices train
ind.test.cluster_1=ind.cluster_1[-ind.train.cluster_1] #indices de test
cluster_1.train = cluster_1[ind.train.cluster_1,]
cluster_1.test=cluster_1[ind.test.cluster_1,]

# Create train and test groups from cluster 2
row.cluster_2=dim(cluster_2)[1]
ind.cluster_2=1:row.cluster_2
ind.train.cluster_2=sort(sample(ind.cluster_2,800))  #indices train
ind.test.cluster_2=ind.cluster_2[-ind.train.cluster_2] #indices de test
cluster_2.train = cluster_2[ind.train.cluster_2,]
cluster_2.test=cluster_2[ind.test.cluster_2,]

# Create train and test groups from cluster 3
row.cluster_3=dim(cluster_3)[1]
ind.cluster_3=1:row.cluster_3
ind.train.cluster_3=sort(sample(ind.cluster_3,800))  #indices train
ind.test.cluster_3=ind.cluster_3[-ind.train.cluster_3] #indices de test
cluster_3.train = cluster_3[ind.train.cluster_3,]
cluster_3.test=cluster_3[ind.test.cluster_3,]

# Merge 3 groups to get final train and test groups
modelo.train = rbind(cluster_1.train,cluster_2.train, cluster_3.train)
modelo.test = rbind(cluster_1.test,cluster_2.test, cluster_3.test)

colnames(modelo.train)

# Create the decision tree and draw it
modelo.rp <- rpart(modelo.train$cluster3 ~., data=modelo.train[,-c(1,4,6)], cp=0.04, parms=list(split="information"), method='class')
modelo.rp
rpart.plot(modelo.rp)


# Validate with the train group
k=dim(modelo.train)[1]
y.pred.train=predict(modelo.rp, modelo.train[,-c(1,4,6)])

factores=colnames(y.pred.train)
y.pred.train.fact = matrix(0,k,1)

for (i in 1:k){
  y.pred.train.fact[i]= factores[which.max(y.pred.train[i,])]
}

y.real.train.fact = as.matrix(modelo.train[,6])

table.rpart.train=table(y.pred.train.fact,y.real.train.fact)
prop.table(table.rpart.train, 2) # column percentages

# Validate with the test group
k.test=dim(modelo.test)[1]
y.pred.test = predict(modelo.rp, modelo.test[,-c(1,4,6)])

factores=colnames(y.pred.test)
y.pred.test.fact = matrix(0,k.test,1)

for (i in 1:k.test){
  y.pred.test.fact[i]= factores[which.max(y.pred.test[i,])]
}

y.real.test.fact = as.matrix(modelo.test[,6])

table.rpart.test=table(y.pred.test.fact,y.real.test.fact)
prop.table(table.rpart.test, 2) # column percentages
table.rpart.test

# Extract results as a csv file
write.table(rfm, file = "./rfm.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")
