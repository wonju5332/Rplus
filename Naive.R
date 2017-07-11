movie<-read.csv('movie.csv',header =T)
install.packages('e1071')
# 빈 기술대학의 통계학과에서 개발
library(e1071)
# 맨 끝 장르게 레벨
# 5번째 컬럼까지가 데이터
movie
nm <- naiveBayes(movie[1:5],movie$장르, laplace=0)
nm
result <- predict(nm, movie[1:5])
result

cbind(movie,result)

##########################################################


nm <- naiveBayes(zoo2[2:1])



kbody2
install.packages("e1071")
library(e1071)
df<-read.csv('kbody2.csv',header=F, stringsAsFactors = T)
str(df)
library(caret)
kbody
train<-df[, ]
test<-df[-intrain, ]


nb_model <- naiveBayes(V19~.,data = train)
nb_model
nbpred <- predict(nb_model, test)
nbpred
confusionMatrix(nbpred, test$V19)







################sms spam 실습 ############


sms_raw <- read.csv('sms_spam.csv',stringsAsFactors = F, strip.white = T)
sms_raw





###########################


install.packages("Rcpp")
install.packages("wordcloud")



##
library(e1071)
kbody <- read.csv('kbody2.csv' , header = F , stringsAsFactors = T)
kbody
str(kbody)

nrow(kbody)
test <- kbody[nrow(kbody),-19]
train <- kbody[,-19]
train
label <- kbody[,19]
label
nm <- naiveBayes(train, label, laplace = 1)
body_pred <- predict(nm , test)
body_pred
install.packages('caret')
library(caret)
confusionMatrix(body_pred)

#


mushroom <- read.csv("mushroom.csv", header=F,
                     stringsAsFactors=F)
mushroom[which(mushroom$V12=='?'), 12]<- NA 		#imputation of missing values 
for (i in (15:23)) {
  mushroom[which(mushroom[,i]==''), i]<- NA
}

for (i in 1:23) {											#convert chr to factor
  mushroom[,i] <-factor(mushroom[,i]) }

####dim(mushroom)									#do not need to do #get the number of rows, columns
####V1 column: e(edible), p(poisonous)


####################### data split
set.seed(1)							#random number generator. using this function we can get the same ramdom numbers
train_cnt <-round(0.75*dim(mushroom)[1])	# partitioning a data set into train set and test set # 0.75 is the splitting ratio for train set
train_index <- sample(1:dim(mushroom)[1], train_cnt,replace=F) 

mushroom_train <- mushroom[train_index,]
mushroom_test <- mushroom[-train_index,]

###edible <- subset(mushroom, V1=="e") # do not need to do #return subsets of vectors,matrices or data frames which meet conditions
###poisonous <- subset(mushroom, V1=="p")


######################## Training a model on the data
install.packages('e1071')
library(e1071)
nb_mushroom<- naiveBayes(V1~. , data= mushroom_train)
nb_mushroom
#p159 classifier
#nb_mushroom<- naiveBayes(mushroom_train , mushroom_train$V1)

######################## Evaluating model performance
mushroom_test_pred<- predict(nb_mushroom, mushroom_test[,-1])
#mushroom_test_pred<- predict(nb_mushroom, mushroom_test) 

nb_table<-table(mushroom_test_pred, mushroom_test[,1]) 
sum(diag(nb_table))/sum(nb_table)
# or 
library(gmodels)
CrossTable(mushroom_test_pred, mushroom_test$V1 )

######################## Improving model performance
nb_mushroom2<-naiveBayes(mushroom_train, mushroom_train$V1, laplace=1)
mushroom_test_pred2 <- predict(nb_mushroom2, mushroom_test[-1])
nb_table2 <-table(mushroom_test_pred2, mushroom_test[,1])
sum(diag(nb_table2))/sum(nb_table2) # we can see that accuracy increases up to 96.7%


########################summarize Naive bayes code########################################
mushroom <- read.csv("/Users/misoni/Desktop/mushroom.csv", header=F, stringsAsFactors=F)

#mushroom[which(mushroom$V12=='?'), 12]<- NA 	
#for (i in (15:23)) {
#	mushroom[which(mushroom[,i]==''), i]<- NA
#}

#for (i in 1:23) {																
#mushroom[,i] <-factor(mushroom[,i]) }


train_cnt <-round(0.75*dim(mushroom)[1])		
mushroom_train <- mushroom[1:train_cnt,]
mushroom_test <- adult[(train_cnt+1):nrow(mushroom),]

nb_mushroom<- naiveBayes(V1~. , data= mushroom_train)
mushroom_test_pred<- predict(nb_mushroom, mushroom_test[,-1])
nb_table<-table(mushroom_test_pred, mushroom_test[,1]) 
sum(diag(nb_table))/sum(nb_table)

nb_mushroom2<-naiveBayes(mushroom_train, mushroom_train$V1, laplace=1)
mushroom_test_pred2 <- predict(nb_mushroom2, mushroom_test[-1])
nb_table2 <-table(mushroom_test_pred2, mushroom_test[,1])
sum(diag(nb_table2))/sum(nb_table2) 


