concrete <- read.csv("concrete.csv", stringsAsFactors = T)
str(concrete)

# 정규화 함수
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# 전체 데이터 프레임에 정규화 적용
concrete_norm <- as.data.frame(lapply(concrete, normalize))

# 0과1 사이에 범위 확인
summary(concrete_norm$strength)

# 본래 데이터의 최소값, 최대값 비교
summary(concrete$strength)

# 훈련과 테스트 데이터 생성
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

## 3단계 : 데이터로 모델 훈련 ----
# neuralnet 모델 훈련
install.packages("neuralnet")
library(neuralnet)

# 하나의 은닉 뉴런에 대한 단순한 ANN
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                            data = concrete_train)

# 망(network) 시각화
plot(concrete_model)

## 4단계 : 모델 성능 평가 ----

# 모델 결과
model_results <- compute(concrete_model, concrete_test[1:8])

# 강도값 예측
predicted_strength <- model_results$net.result

# 예측값과 실제값간의 상관 관계 확인
cor(predicted_strength, concrete_test$strength)  #80% 적중률


## 5단계 : 모델 성능 향상 ----
# 5개 은닉 뉴런인 복잡한 뉴런망
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)

# 망(network) 시각화
plot(concrete_model2)

# 결과 평가
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)  # 90% 적중률  즉 , 은닉층 수정으로 10% 의 적중률이 향상됨.





#보스턴 주택가격 데이터

boston <- read.csv('boston.csv',header = T, stringsAsFactors = T)


# 정규화 함수
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# 전체 데이터 프레임에 정규화 적용
boston_norm <- as.data.frame(lapply(boston, normalize))
boston_norm
# 0과1 사이에 범위 확인
summary(boston_norm)

# 본래 데이터의 최소값, 최대값 비교
summary(boston_norm$CAT..MEDV)
nrow(boston)
# 훈련과 테스트 데이터 생성
set.seed(12345)
boston_land <- boston_norm[order(runif(506)), ]
boston_train <- boston_norm[1:400,]
boston_tst <- boston_norm[401:506,]

## 3단계 : 데이터로 모델 훈련 ----
# neuralnet 모델 훈련
install.packages("neuralnet")
library(neuralnet)
boston
# 하나의 은닉 뉴런에 대한 단순한 ANN
n <- colnames(boston)
f <- as.formula(paste("MEDV ~", paste(n[!n %in% c("MEDV", 'CAT..MEDV')], collapse = " + ")))
boston_model <- neuralnet(f,data=boston_train,hidden=c('5','3'),linear.output=T)

# 망(network) 시각화
plot(boston_model)

boston
# 모델 결과
model_results <- compute(boston_model, boston_tst[,1:13])
co
# 강도값 예측
predicted_strength <- model_results$net.result


###  보스턴 데이터

bos <- read.csv('boston.csv', header = T, stringsAsFactors = T)


normalize <- function(x) {
  return ((x-min(x)) / (max(x)-min(x) ) )
}

bos_scale <- as.data.frame(lapply(bos,normalize))

train_cnt <- round(0.75*nrow(bos))
train_idx <- sample(1:nrow(bos_scale),train_cnt,replace=F)
bos_train <- bos_scale[train_idx,]
bos_test <- bos_scale[-train_idx,]
bos_test2 <- bos_test[,-c(1,4,9,15)]
bos_test2
bos_model <- neuralnet(formula = MEDV~ZN+INDUS+NOX+RM+AGE+DIS+TAX+PTRATIO+B+LSTAT,data=bos_train,hidden=10)
result <- compute(bos_model,bos_test2[1:10])
pred <- result$net.result
cor(pred,bos_test$MEDV)




# 화력 발전 데이터

plant <- read.csv('plant.csv', header =T, stringsAsFactors = T)
plant

summary(plant$energy_output)

#결측값이 잇는지 확인하기.
apply(plant, 2, function(x) sum(is.na(x)))
#없음

#인덱스 만들기
index <- sample(1:nrow(plant),round(0.75*nrow(plant)))
train <- plant[index,]
test <- plant[-index,]

#회귀 그리기?
lm.fit <- glm(energy_output~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
# 작을 수록 원본과의 오차가 적은 것이므로 추측한 값의 정확성이 높은 것.
# MSE = 0이다.
# 즉, 원본과의 오차가 적은 것..?


#정규화하기.
maxs <- apply(plant, 2, max)  #각 컬럼의 최대값
mins <- apply(plant, 2, min)  #각 컬럼의 최소값

scaled <- as.data.frame(scale(plant, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

summary(test_)
# 파라메터

n <- colnames(train_)
f <- as.formula(paste("energy_output ~", paste(n[!n %in% "energy_output"], collapse = " + ")))
plant_model <- neuralnet(formula = f, data= train_, hidden=1)
plant_model
plot(plant_model)

model_results <- compute(plant_model, test[-5])
predicted_strength <- model_results$net.result
cor(predicted_strength, test_$energy_output)



# 새로운 신경망
plant_model2 <- neuralnet(formula = f, data= train_, hidden=c(3,3))

# 망(network) 시각화
plot(plant_model2)

# 모델 결과
model_results2 <- compute(plant_model2, test_[-5])

# 강도값 예측
predicted_strength2 <- model_results2$net.result

# 예측값과 실제값간의 상관 관계 확인
cor(predicted_strength2, test_$energy_output)

# 두 신경망 비교 
plot(plant_model)
plot(plant_model2)
cor(predicted_strength, plant_test$energy_output)
cor(predicted_strength2, plant_test$energy_output)









###### Mnist 실습  신경망 으로 풀기 #####

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

############### Mnist 신경망으로 풀기 ##########
install.packages("neuralnet")
library(neuralnet)
head(training)
str(training)
training <- data.matrix(training)
training
n <- colnames(training)
f <- as.formula(paste("y ~", paste(n[!n %in% c('y')], collapse = " + ")))
fit2 <- neuralnet(formula = f, data = head(training, 1000), hidden = c('5','5'))
cv = inTrain[-trainIndex,]

plot(fit2)
str(cv)
head(cv[,2:785], 1000)
cv <- data.matrix(cv)

model_results <- compute(fit2,head(cv[,training[-1]],1000))

