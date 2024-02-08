## 경영경제테이터분석소프트웨어 과제물 2

# 평가요소
# 1. 제공된 자료를 이용하여 본인이 흥미 있는 요소를 3개 분석(제출 1점, 1개당 3점, 총 10점)
# 2. 필수 포함 분석기법: t검정, ANOVA검정, 회귀분석
# 3. 개별 분석에 대하여 분석문제, R 소스코드, 결과물(표, 그래프), 자료분석 결과 해석
# 4. 여러개의 분석을 수행해 보고 의미 있는 결과를 선택하여 최종적으로 3개의 분석만 제출

library(readxl)
data <- read_excel("2014년_주거실태조사.xlsx", sheet = 1)

# 1. 서울, 부산의 아파트 가격 차이에 대한 t 검정(기초자료 검토, 분석결과 및 해석)
library(dplyr)
apt <- data %>%
	filter(sido %in% c(11, 21)) %>% 
	filter(!is.na(q12_1) & q12_1 != 9999999) %>% 
	select(sido, q12_1)
seoul_apt <- apt %>% 
	filter(sido == 11)
busan_apt <- apt %>% 
	filter(sido == 21)

var.test(seoul_apt$q12_1, busan_apt$q12_1) # F-test -> not equal variance
t.test(seoul_apt$q12_1, busan_apt$q12_1, var.equal = F) # p < 2.2e-16 -> H1 accepted

# 2. 서울의 동작구, 강남구, 마포구 아파트 전세보증금 차이에 대한 ANOVA 검정
# q40_2 : 전세보증금
apt <- data %>% 
	filter(q4 == 4 & sigungu %in% c("동작구", "강남구", "마포구")) %>% 
	filter(!is.na(q40_2) & q40_2 != 9999999) %>% 
	select(sigungu, q40_2)
apt <- rename(apt, deposit = q40_2)

boxplot(deposit~sigungu, data = apt)
bartlett.test(deposit~sigungu, data = apt)
# oneway.test(deposit~sigungu, data = apt, var.equal = T)

anova = aov(deposit~sigungu, data = apt)
anova
summary(anova)
TukeyHSD(anova, "sigungu", conf.level = 0.95) #사후검정


# 3. 전국의 주택가격에 영향이 있을 것으로 생각되는 변수를 사용하여 회귀분석
# q12_1 : 현재 주택 가격(만원)
# q49_1 : 근로, 사업 소득
data1 <- data %>% 
	filter(!is.na(q49_1) & q49_1 != 9999999 & q49_1 != 0) %>% 
	filter(!is.na(q12_1) & q12_1 != 9999999) %>% 
	select(q49_1, q12_1)
data1 <- rename(data1, income = q49_1, hprice = q12_1)
head(data1)
plot(hprice~income, data = data1)
cor(data1$income, data1$hprice, method = "pearson")


# q52_4 : 총자산
data3 <- data %>% 
	filter(!is.na(q52_4) & q52_4 != 9999999) %>% 
	filter(!is.na(q12_1) & q12_1 != 9999999) %>% 
	select(q52_4, q12_1)
data3 <- rename(data3, assets = q52_4, hprice = q12_1)
head(data3)
plot(hprice~assets, data = data3)
cor(data3$assets, data3$hprice, method = "pearson")

regression = lm(hprice~assets, data = data3)
summary(regression)
