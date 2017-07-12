###  맥주 데이터 ###
x <- data.frame(
  beer=c(0,1,1,1,0),
  bread=c(1,1,0,1,1),
  cola=c(0,0,1,0,1),
  diapers=c(0,1,1,1,1),
  eggs=c(0,1,0,0,0),
  milk=c(1,0,1,1,1) )

# 설치 

install.packages('arules')
library(arules)

# 데이터프레임을 행열데이터로 전환
trans <- as.matrix(x, "Transaction")


# 아프리오리 알고리즘 실행

rules1 <- apriori(trans, parameter = list(supp=0.2, conf=0.6, target="rules"))

rules1   # set of 49 rules  = 49개의 규칙을 발견했다!

# 연관규칙 검사
inspect(sort(rules1))







#### 병원 과 약국의 연관규칙 찾기 ####

hospital <- read.csv('building.csv', header= T , stringsAsFactors = F)
hospital[is.na(hospital)] <- 0
hospital<- hospital[-1]
hospital
# 매트릭스로 전환
trans <- as.matrix(hospital, "Transaction")

#apriori 알고리즘
aa <- apriori(trans, parameter = list(supp=0.1, conf=0.6, target="rules"))
aa
## 검사해보기
bb <- inspect(sort(aa))

bb2 <- subset(aa, items %in% '보습')



### 식료품점 #####

groc <- read.transactions('groceries.csv', sep=',')
groc

### data summary ###
summary(groc)

### draw plot / top 20 ###

itemFrequencyPlot(groc, topN=20)

### 매트릭스 다이어그램 ###

image(sample(groc, 500, replace = F), main = 'matrix diagram')

### 아프리올리 알고리즘 실행 ###

# 하루에 두번씩(약 60번) 구매되는 제품이라면 지지도 레벨을 계산하는데 쓸 수 있다.
# 총 9835에서 60은 0.006이기 때문에 지지도를 0.006으로 한다.
# 결과에 포함되기 위해서 적어도 25% 정확도의 규칙을 뜻하는 0.25인 신뢰도 경계 값으로 시작한다.
# 두 제품보다 적게 포함되는 규칙을 제거하기 위한 minlen을 2로 설정한다.
rules <- apriori(groc, parameter=list(support = 0.006, confidence=0.25, minlen=2))

summary(rules)
itemFrequency(groc[,1:3])

str(groc)

plot(rules, method = "graph",
     control = list(type="items"),
     vertex.label.cex = 0.7,
     edge.arrow.size = 0.3,
     edge.arrow.width = 2)


##### 점심문제 ! ####


x <- data.frame(
  영화=c(0,1,1,1,0,1,1,1,0,1,1,1,1),
  오징어=c(1,0,1,1,1,1,0,0,0,1,0,0,1),
  맥주=c(0,0,1,0,1,0,0,0,1,1,0,0,1),
  음료=c(1,1,0,1,1,0,0,1,0,0,1,1,0),
  스낵=c(0,1,0,0,0,0,0,0,0,1,1,0,0),
  팝콘=c(0,1,1,1,1,1,1,1,0,1,1,0,1))

## 매트릭스로 변환 ##

trans <- as.matrix(x, "Transaction")

## 지지도 레벨 확인 ##
str(x)
itemFrequency(trans[,1:5])
str(trans)

rule <- apriori(trans, parameter = list(supp=0.01, conf=0.6, target="rules"))

##graph
install.packages('arulesViz')
library(arulesViz)






####### Epub

help(Epub)


data(Epub)
summary(Epub)


##check itemsets in sparse matrix

inspect(Epub[1:10])

#앞의 10개 품목 빈도 확인
itemFrequency(Epub[ , 1:10])

#지지도 1% 이상의 item 에 대해 막대그래프 그리기
itemFrequencyPlot(Epub, support=0.01, main = '지지율 1% 이상 품목 빈도 그래프')

# support 상위 30개의 막대그래프 그리기
itemFrequencyPlot(Epub, topN=30, main='support top 30 items')


image(sample(Epub, 500, replace = F), main= '다이어그램')



# 연관규칙 분석

##
Epub_rule2 <- apriori(data = Epub,
                     parameter = list(support = 0.001,
                                      confidence = 0.20,
                                      minlen = 2) )

# 연관규칙 개략적 파악하기
summary(Epub_rule2)

# 연관규칙 평가해보기
inspect(Epub_rule2[1:20])


# 위처럼 개별 규칙을 쭈~욱 나열해 놓으면, 사람이 평가하기가 쉽지 않다. 그럴땐?
# 1) sort함수를 써서 분석가가 보고자 하는 기준에 맞게 by 매개변수로 정렬하여 우선순위를 두고 보면 유용하다.

inspect(sort(Epub_rule2, by = "lift"))
inspect(sort(Epub_rule2, by = "support"))
inspect(sort(Epub_rule2, by = "confidence"))



# 2) subset 함수를 써서 관심이 있는 item 이 포함된 연관규칙만 보기

rule_interest <- subset(Epub_rule2, items %in% c("doc_72f", "doc_4ac"))
inspect(sort(rule_interest, by = "lift", decreasing = F))



# partial subset : %pin%

rule_interest_pin <- subset(Epub_rule2, items %pin% c('60e'))
inspect(rule_interest_pin)


# %ain% 을 사용해서 
rule_interest_lhs_ain <- subset(Epub_rule2, lhs %ain% c('doc_6e8','doc_6e9'))
inspect(rule_interest_lhs_ain)



## visualize

install.packages('arulesViz')
library(arulesViz)

## scatter plot of association rules

plot(Epub_rule2)


## Grouped matrix for assocation rules
plot(sort(Epub_rule2, by = "support")[1:20], method = "grouped")


# Network Graph for assocation rules

plot(Epub_rule2, method = 'graph', control = list(type='items'))



# replot

plot(Epub_rule2, method = 'graph', control = list(type='items'),
    vertex.label.cex = 0.7,
    edge.arrow.size = 0.3,
    edge.arrow.width = 2)


# 20개만 선별해서 그래프 그려보기

plot(Epub_rule2[55:65], method = "graph", control = list(type="items"))
