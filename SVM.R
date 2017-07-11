### 중산층 데이터 로드####
svm1 <- read.csv('set1.csv',header = T, stringsAsFactors = F)

###현재 데이터 상태 plot 그리기 ###
plot(svm1)


### svm관련 패키지 설치 ###

install.packages('MASS')
library(MASS)

### Two- dimensional Kernal Density Estimation

density <- kde2d(svm1$food, svm1$book, n=400)
plot(density)
image(density, xlab='food',ylab='book')


### SVM 알고리즘 적용 ###

install.packages('e1071')
library(e1071)

m1 <- svm(status ~ food + book + cul + cloth + travel,
          type = "C-classification", data= svm1)
### 결과
m1

### 모델 성능 평가

library(caret)
a <- predict(m1, svm1)
confusionMatrix(table(svm1$status, a))



###### Mnist 실습  SVM 으로 풀기 #####

install.packages("caret")
install.packages("doParallel")
install.packages("kernlab")
install.packages("ggplot2")
install.packages("lattice")
library(ggplot2)
library(lattice)
library(kernlab)
library(caret)
library(doParallel)

# Enable parallel processing.

cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('train-images.idx3-ubyte')
  test <<- load_image_file('t10k-images.idx3-ubyte')
  
  train$y <<- load_label_file('train-labels.idx1-ubyte')
  test$y <<- load_label_file('t10k-labels.idx1-ubyte')  
}

show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

train <- data.frame()
test <- data.frame()

# Load data.
setwd('d:/R_data/mnist')
load_mnist()

# Normalize: X = (X - min) / (max - min) => X = (X - 0) / (255 - 0) => X = X / 255.

train$x <- train$x / 255

# Setup training data with digit and pixel values with 60/40 split for train/cv.

inTrain = data.frame(y=train$y, train$x)
inTrain$y <- as.factor(inTrain$y)
trainIndex = createDataPartition(inTrain$y, p = 0.60,list=FALSE)
training = inTrain[trainIndex,]
cv = inTrain[-trainIndex,]

# SVM. 95/94.

fit <- train(y ~ ., data = head(training, 1000), method = 'svmRadial', tuneGrid = data.frame(sigma=0.0107249, C=1))

results <- predict(fit, newdata = head(cv, 1000))
results

confusionMatrix(results, head(cv$y, 1000))


show_digit(as.matrix(training[5,2:785]))

# Predict the digit.

predict(fit, newdata = training[5,])

# Check the actual answer for the digit.

training[5,1]


## 붓꽃 데이터 ##
data(iris)
install.packages("ggplot2")
library(ggplot2)
qplot(Petal.Length, Petal.Width, data=iris, color = Species) #실제 데이터를 확인 
library(e1071)
s<-sample(150,100) 	# 1 ~ 150 까지 중복되지 않는 랜덤수를 100개 생성 
col<- c("Petal.Length", "Petal.Width", "Species") 	# 컬럼명 지정 꽃잎 길이 , 꽃잎 폭, 종류
iris_train <- iris[s,col] 	# 랜덤으로 뽑은 100개의 트레이닝 셋
iris_test <- iris[-s,col]	 # 나머지 테스트 셋

# 리니어 커널방식으로 트레이닝 셋을 종류별로 모델링한다.
iris_svm <- svm(Species ~. , data=iris_train, cost=1,kernel ="linear") 
plot(iris_svm, iris_train[,col])

p<- predict(iris_svm,iris_test[,col],type="class") 
plot(p)
table(p, iris_test[,3])
mean( p == iris_test[,3])






########광학식 문자 인식 #########


# 각 글자모양의 속성들을 수치화 한 데이터 
# 데이터 읽기와 구조
letters <- read.csv("letterdata.csv", stringsAsFactors = T)
str(letters)

# 훈련 데이터와 테스터 데이터 구분
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]
## 3단계 : 데이터로 모델 훈련 ----
# 단순 선형 SVM을 훈련으로 시작
install.packages('kernlab')
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")
# 모델에 대한 기본 정보 확인
letter_classifier


## 4단계 : 모델 성능 평가 ----
# 테스트 데이터셋에 대한 예측
letter_predictions <- predict(letter_classifier, letters_test)

head(letter_predictions)

table(letter_predictions, letters_test$letter)

# 일치/불일치 예측을 표시하는 TRUE/FALSE 벡터 생성
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

## 5단계 : 커널을 바꿔서 모델 성능을 향상시켜보자 ----
## 가우시안 RBF 커널을 사용
##  RBF ? 링크 -> [Radial Basis Function(방사 기저 함수)]
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf)) 
prop.table(table(agreement))
## 83%에서 93%로 향상되었다.




################ 산불 데이터 ##############

install.packages("dplyr")
install.packages("kernlab")
install.packages("ROCR")
install.packages("caret")
install.packages("e1071")
library(dplyr)
library(kernlab)
library(ROCR)
library(caret)
library(e1071)

mydata <- read.csv('forestfires.csv',header =T)
hist(mydata$area)
rug(mydata$area)

# area값들을 분석하기 쉽게 변경 log값을 씌워서 y컬럼으로 추가한다. 
mydata <- mutate(mydata, y = log(area + 1))

normalise <- function(x) {  #정규화함수
  return((x - min(x)) / (max(x) - min(x))) }

#분석할 값들을 정규화한다.
mydata$temp <- normalise(mydata$temp)
mydata$rain <- normalise(mydata$rain)
mydata$RH <- normalise(mydata$RH)
mydata$wind <- normalise(mydata$wind)


#산불 넓이가 5헥타르 이상인 것을 large 아니면 small로 놓고 분석한다.
mydata$size <- factor(ifelse(mydata$y< 2, 1, 0),
                      labels = c("large", "small"))
#tail(mydata[, c("size", "area")])  
train <- sample(x = nrow(mydata), size = 400, replace = FALSE) 
# 각 커널 방식 비교 
# polydot커널 방식으로 svm
m.poly <- ksvm(size ~ temp + RH + wind + rain,
               data = mydata[train, ],
               kernel = "polydot", C = 1)

pred <- predict(m.poly, newdata = mydata[-train, ], type = "response")

table(pred, mydata[-train, "size"])
mean(pred == mydata[-train, "size"])


# tanhdot 커널 방식으로 svm
m.tan <- ksvm(size ~ temp + RH + wind + rain,
              data = mydata[train, ],
              kernel = "tanhdot", C = 1)

pred <- predict(m.tan, newdata = mydata[-train, ], type = "response")

table(pred, mydata[-train, "size"])
mean(pred == mydata[-train, "size"])


# rbfdot 커널 방식으로 svm
m.rad <- ksvm(size ~ temp + RH + wind + rain,
              data = mydata[train, ],
              kernel = "rbfdot", C = 1)
pred <- predict(m.rad, newdata = mydata[-train, ], type = "response")

table(pred, mydata[-train, "size"])
mean(pred == mydata[-train, "size"])


