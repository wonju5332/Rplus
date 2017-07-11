##data load

zoo <- read.csv('zoo3.csv', header = F, stringsAsFactors = F)
zoo
str(zoo)

na.omit(zoo)
colnames(zoo)
#id 제거
zoo2 <- zoo[-1]
zoo2
# zoo$V18 <- factor(zoo$v18 , levels = c('1','2','3','4','5','6','7'), 
#                   labels = c('포유류','조류','파충류','어류','양서류','곤충','갑각류'))


nrow(zoo)
ncol(zoo)
zoo2

#zoo2는 id가 빠진 상태 , 진단 콜롬이 17번째이다.


#트레이닝, 테스트 데이터 생성
zoo_train <- zoo2[1:100,-17]
zoo_test <- zoo2[101,-17]

zoo_train
zoo_test


str(zoo_train)
str(zoo_test)

#라벨있는 데이터 만들기
zoo_train_label <- zoo2[1:100,17]
zoo_test_label <- zoo2[101,17]
zoo_test_label
zoo_train_label

is.na(zoo_test)
library(class)
zoo_pred <-knn(train=zoo_train, test=zoo_test, cl=zoo_train_label, k=3, prob =T)
zoo_pred

table(zoo$V18)




