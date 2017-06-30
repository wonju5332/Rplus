### OJDBC를 이용하여 오라클 데이터를 가져오는 방법


install.packages('DBI')
install.packages('RJDBC')
library(RJDBC)
library(DBI)

driver <- JDBC('oracle.jdbc.driver.OracleDriver', 'ojdbc6.jar')
oracle_db <- dbConnect(driver, 'jdbc:oracle:thin:@//127.0.0.1:1522/orcl3', 'scott', 'tiger')
emp_query <- 'select * from emp'
emp_data <- dbGetQuery(oracle_db, emp_query)



# 166. 직업, 직업별 토탈월급을 출력하는데, 직업별 토탈월급들의 평균값보다 더 큰 값만 출력되게 하시오.

q166 <- 'select * from (select job, sum_sal, avg(sum_sal) over() avg_sal from (select job, sum(sal) sum_sal from emp group by job)) where sum_sal > avg_sal'

q166_ans <- dbGetQuery(oracle_db, q166)
q166_ans


# 166. sh 계정의 sales테이블의 data를 sales 라는 변수에 담고 전체 건수를 count하시오.

oracle_db <- dbConnect(driver, 'jdbc:oracle:thin:@//127.0.0.1:1522/orcl3', 'sh','sh')
sales_query <- 'select * from sales'
sales_data <- dbGetQuery(oracle_db, sales_query)
nrow(sales_data)
