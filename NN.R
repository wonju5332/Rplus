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









########mnist
# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call: show_digit(train$x[5,]) to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

load_image_file <- function(conn) {
  readBin(conn, 'integer', n=1, size=4, endian='big')
  n = readBin(conn, 'integer', n=1, size=4, endian='big')
  nrow = readBin(conn, 'integer', n=1, size=4, endian='big')
  ncol = readBin(conn, 'integer', n=1, size=4, endian='big')
  x = readBin(conn, 'integer', n=n*nrow*ncol, size=1, signed=F)
  x = matrix(x,  ncol=nrow * ncol,  byrow=T)
  close(conn)
  return(x)
}

load_label_file <- function(conn) {
  readBin(conn, 'integer', n=1, size=4, endian='big')
  n = readBin(conn, 'integer', n=1, size=4, endian='big')
  y = readBin(conn, 'integer', n = n, size=1, signed=F)
  close(conn)
  return(y)
}

download.mnist <- function(range = c(0, 1), global = FALSE) {
  mnist <- list(
    train = list(),
    test = list()
  )
  
  mnist$train$x <- load_image_file(gzcon(url("http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz", "rb")))
  mnist$test$x <- load_image_file(gzcon(url("http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz", "rb")))
  mnist$train$y <- load_label_file(gzcon(url("http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz", "rb")))
  mnist$test$y <- load_label_file(gzcon(url("http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz", "rb")))
  
  # Normalize the x's - only if needed
  if (!isTRUE(all.equal(c(0, 255), range))) {
    if (! is.numeric(range))
      stop("'range' must be a numeric vector")
    if (length(range) != 2)
      range <- range(range)
    
    mnist$train$x <-  mnist$train$x / (255 / diff(range)) + range[1]
    mnist$test$x <-  mnist$test$x / (255 / diff(range)) + range[1]
    
    # Convert to integer if possible
    s <- seq(range[1], range[2], length.out = 256)
    if (isTRUE(all.equal(s, as.integer(s)))) {
      storage.mode(mnist$train$x) <- "integer"
      storage.mode(mnist$test$x) <- "integer"
    }
  }
  
  if (global) {
    save(mnist, file = paste(system.file(package="mnist"), "data", "mnist.RData", sep=.Platform$file.sep))
    assign("mnist", mnist, envir = globalenv())
  }
  
  return(mnist)
}
plot.mnist <- function(x = mnist$test$x, label = mnist$test$y + 1, model = prcomp(mnist$train$x),
                       predictions = predict(model, x)[,1:2], reconstructions = tcrossprod(predictions, model$rotation[,1:2]),
                       show.reconstructions = TRUE,
                       highlight.digits = c(11, 3, 2, 91, 20, 188, 92, 1, 111, 13),
                       digits.col = c("#FF0000FF", "#0000FFFF", "#964B00FF", "#FF00FFFF", "#00AAAAFF", "#00EE00FF", "#000000FF", "#000000FF", "#AAAAAAFF", "#FF9900FF"),
                       digits.alphas.reverse = c(FALSE, TRUE)[c(1, 1, 1, 1, 1, 1, 2, 1, 1, 1)],
                       pch = 21,
                       pch.col = c("black", "white")[c(1, 2, 1, 1, 1, 1, 1, 2, 1, 1)],
                       pch.bg = c("#FF0000FF", "#0000FFFF", "#964B00FF", "#FF00FFFF", "#00AAAAFF", "#00EE00FF", "#FFFFFFFF", "#000000FF", "#AAAAAAFF", "#FF9900FF"),
                       cex = 1.5,
                       cex.axis = .5,
                       cex.lab = .5,
                       cex.highlight = 2.5,
                       xlab = "Node 1",
                       ylab = "Node 2",
                       xlim = NULL,
                       ylim = NULL,
                       ncol = 10,
                       ...
) {
  
  n.highlight <- length(highlight.digits)
  layout(cbind(seq_along(highlight.digits), matrix(1 + n.highlight + ifelse(show.reconstructions, n.highlight, 0), nrow=n.highlight, ncol=ncol), if (show.reconstructions) seq_along(highlight.digits) + n.highlight))
  
  opar <- par(cex=cex)
  
  alphas.gen <- expand.grid(c(0:9, LETTERS[1:6]), c(0:9, LETTERS[1:6]))
  alphas <- paste(alphas.gen[,2], alphas.gen[,1], sep="")
  
  sapply(seq_along(highlight.digits), function(i) {show.digit(x[highlight.digits[i],], col=paste(substring(digits.col[label[highlight.digits[i]]], 1, 7), if (digits.alphas.reverse[i]) rev(alphas) else alphas, sep=""))})
  if (show.reconstructions) {
    sapply(seq_along(highlight.digits), function(i) {show.digit(reconstructions[highlight.digits[i],], col=paste(substring(digits.col[label[highlight.digits[i]]], 1, 7), if (digits.alphas.reverse[i]) rev(alphas) else alphas, sep=""))})
  }
  
  
  par(cex=cex, mar=c(3, 3, 0, 0), mgp=c(2, 1, 0))
  plot(predictions[, 1], predictions[, 2], xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim,
       pch = pch, bg = pch.bg[label], col=pch.col[label], cex.axis=cex.axis, cex.lab=cex.lab)
  points(predictions[highlight.digits, 1], predictions[highlight.digits, 2], pch = pch, bg = pch.bg[label[highlight.digits]], col = pch.col[label[highlight.digits]], cex=cex.highlight)
  
  par(opar)
}

plot.mnist()

.onAttach <- function(lib, pkg) {
  # It makes no sense to request the user to say data(mnist) after loading the package
  # with library(mnist), so load it automatically
  mnist_data_file <- system.file("data/mnist.RData", package="mnist")
  if (mnist_data_file == "") {
    packageStartupMessage("Downloading mnist dataset...")
    dir.create(paste(system.file(package="mnist"), "data", sep=.Platform$file.sep))
    mnist <- download.mnist(global = TRUE)
  }
  packageStartupMessage("Use data(mnist) to load the MNIST dataset.")
}
.onAttach()


