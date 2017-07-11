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



