c <- c(3, 4, 1, 5, 7, 9, 5, 4, 6, 8,4,5,9,8,7,8,6,7,2,1)
row <- c("A", "B", "C", "D", "E","F","G","H","I","J")
col <- c("X", "Y")
data <- matrix(c, nrow=10, ncol=2, byrow=TRUE, dimnames=list(row, col))

plot(data, xlim=range(0:10), ylim=range(0:10))


dat
key = round(sqrt(10/2))

km <- kmeans(data,key)
km

# 플롯으로 센터가 어떻게 잡혔는지 확인하자.

cbind(data, km$cluster)
km$centers



plot(round(km$center), col=km$center,pch=22, bg="dark blue",
     xlim=range(0:10), ylim=range(0:10) )
par(new=T)
plot(data, col = km$cluster + 1,
     xlim=range(0:10), ylim=range(0:10) ) 



## 국영수 데이터##\


academy <- read.csv('academy.csv' , header= T)
academy <- academy[-1]

academy2 <- academy[ , c(2,3)]
plot(academy2)

km <- kmeans(academy2, 4)
summary(km)
km
unique(km$cluster)
plot(km$centers)
graphics.off()
plot(round(km$center), col=c(1:4),pch=km$cluster, bg="dark blue",
     xlim=range(20:100), ylim=range(20:100) )
par(new=T)
plot(academy2, col = km$cluster + 1,
     xlim=range(20:100), ylim=range(20:100) ) 

x <- cbind(academy2, km$cluster)
x





#### SNS ####
teens <- read.csv('snsdata.csv')

table(teens$gender, useNA = 'ifany')

# 이상치 제거  (13 ~ 18살 이외는 모두 NA 처리)
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
teens$age
summary(teens$age)

teens$female <- ifelse(teens$gender=="F" & !is.na(teens$gender),1,0)
teens$no_gender <- ifelse(is.na(teens$gender),1,0)

table( teens$gender, useNA = 'ifany')
table( teens$female, useNA = 'ifany')
table( teens$no_gender, useNA = 'ifany')

# 집단 별 평균 나이
mean(teens$age)
mean(teens$, na.rm=T)

aggregate(data= teens, age ~ gradyear, mean, na.rm = T)


# 각 개인에 대한 예측된 나이 계산

ave_age <- ave(teens$age, teens$gradyear,
               FUN = function(x) mean(x, na.rm=T))

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)



# interest 변수에 담기
interests <- teens[5:40]
#정규화 해야한다.
interests_z <- as.data.frame(lapply(interests, scale))
interests_z


# kmeans 로 위 관심사  5개로 분류


teens_clusters <- kmeans(interests_z, 5, nstart = 5,iter.max = 10)
teens_clusters$iter

teens_clusters$cluster

# 분류한 컬럼을 teens$interest 에 추가

teens$interest <- teens_clusters$cluster
teens[1:5, c('interest','gender','age','friends')]
plot(teens_clusters$centers)
