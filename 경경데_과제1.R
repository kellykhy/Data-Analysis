# 경영경제테이터분석소프트웨어 과제물 1
# 20200874 김희영

# 1번 정답
# dplyr, ggplot2, readxl 패키지를 설치하고 스크립트에서 해당 패키지를 사용할 수 있도록 처리하고, “2014년_주거실태조사.xlsx” 엑셀파일 “조사결과” 시트를 불러와서 “exam_data”로 저장하고 “sido”의 숫자 데이터를 엑셀파일 “항목정의” 시트에 있는 문자로 변경하시오. (예: 11 -> 서울특별시 … 39 -> 제주특별자치도)
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
library(dplyr)
library(ggplot2)
library(readxl)

exam_data <- read_excel("2014년_주거실태조사.xlsx", sheet = 1)
#table(exam_data$sido)
#summary(exam_data)
exam_data$sido <- 
	ifelse(exam_data$sido == 11, "서울특별시",
				 ifelse(exam_data$sido == 21, "부산광역시", 
				 			 ifelse(exam_data$sido == 22, "대구광역시",
				 			 			 ifelse(exam_data$sido == 23, "인천광역시",
				 			 			 			 ifelse(exam_data$sido == 24, "광주광역시",
				 			 			 			 			 ifelse(exam_data$sido == 25, "대전광역시",
				 			 			 			 			 			 ifelse(exam_data$sido == 26, "울산광역시",
				 			 			 			 			 			 			 ifelse(exam_data$sido == 29, "세종특별자치시",
				 			 			 			 			 			 			 			 ifelse(exam_data$sido == 31, "경기도",
				 			 			 			 			 			 			 			 			 ifelse(exam_data$sido == 32, "강원도",
				 			 			 			 			 			 			 			 			 			 ifelse(exam_data$sido == 33, "충청북도",
				 			 			 			 			 			 			 			 			 			 			 ifelse(exam_data$sido == 34, "충청남도",
				 			 			 			 			 			 			 			 			 			 			 			 ifelse(exam_data$sido == 35, "전라북도",
				 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(exam_data$sido == 36,
				 			 			 			 			 			 			 			 			 			 			 			 			 			 "전라남도",
				 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(exam_data$sido == 37, "경상북도",
				 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse(exam_data$sido == 38, "경상남도", "제주특별자치도"))))))))))))))))

#table(exam_data$sido)

# 2번 정답
# 점유형태(q7) 자가, 전세, 보증금 있는 월세만 선택, 금융기관 대출금(q53_1_1)의 데이터가 없거나 모름/무응답 제외 후, 자가 전세, 월세의 금융기관 대출금 평균을 type_housing_loan에 저장하고 자가, 전세, 월세 순으로 세로 막대 그래프를 그리시오.
df_loan <- exam_data %>%
	filter(q7 %in% c(1, 2, 3)) %>% 
	filter(!is.na(q53_1_1) & q53_1_1 != 9999999) %>% 
	group_by(q7) %>% 
	summarise(type_housing_loan = mean(q53_1_1))

ggplot(data = df_loan, aes(x = q7, y = type_housing_loan)) + geom_col()

# 3번 정답
# 광역시(sido)만 선택, 주택유형(q4) 아파트만 선택, 현재주택가격(q12_1)의 데이터가 없거나 모름/무응답 제외, 전용면적_평(q20_1_a)의 데이터가 없거나 모름/무응답 제외 후, 전국 광역시 아파트 평당 가격의 평균을 계산하여 가격이 비싼 3개 광역시와 평당 가격 평균을 apt_price에 저장하시오.
apt_price <- exam_data %>% 
	filter(sido %in% c("부산광역시", "대구광역시", "인천광역시", "광주광역시", "대전광역시", "울산광역시")) %>% 
	filter(q4 == 4) %>% 
	filter(!is.na(q12_1) & q12_1 != 9999999) %>% 
	filter(!is.na(q20_1_a) & q20_1_a != 9999999) %>% 
	mutate(price_per_s = (q12_1 / q20_1_a)) %>% 
	group_by(sido) %>% 
	summarise(mean_pps = mean(price_per_s)) %>% 
	arrange(desc(mean_pps)) %>% 
	head(3)
apt_price

# 4번 정답
# 서울(sido) 자치구(sigungu)만 선택, 전세(q7)만 선택, 전세보증금(q15_1)의 데이터가 없거나 모름/무응답 제외 후, 서울 자치구별 전세보증금 평균을 계산하여 rant_deposit에 저장하고, 전세보증금이 큰 자지구부터 작은 자치구 순으로 정렬된 가로 막대 그래프를 그리시오.
#table(exam_data$sigungu)
df_rant <- exam_data %>% 
	filter(sido == "서울특별시" & q7 == 2) %>% 
	filter(!is.na(q15_1) & q15_1 != 9999999) %>% 
	group_by(sigungu) %>% 
	summarise(rant_deposit = mean(q15_1)) %>% 
	arrange(desc(rant_deposit))
ggplot(data = df_rant, aes(x = reorder(sigungu, -rant_deposit), y = rant_deposit)) +
	geom_col() +
	coord_flip()


# 5번 정답
# 주택유형(q4) 일반단독주택, 다가구단독주택, 영업겸용단독주택, 아파트, 연립주택, 다세대주택 만 선택, 총 자산(q52_4)의 데이터가 없거나 모름/무응답 제외, 총 부채(q53_1_4)의 데이터가 없거나 모름/무응답 제외 후, 일반단독주택, 다가구단독주택, 영업겸용단독주택 은 모두 단독주택으로 데이터 변경하여 주택유형별 총 자산과 총 부채 데이터를 housing_asset에 저장하시오. 주택유형별 총 자산 평균과 총 부채 평균을 분리된 가로 막대 그래프로 그리시오.
housing_asset <- exam_data %>% 
	filter(q4 %in% c(1, 2, 3, 4, 5, 6)) %>% 
	filter(!is.na(q52_4) & q52_4 != 9999999) %>% 
	filter(!is.na(q53_1_4) & q53_1_4 != 9999999)

housing_asset$q4 <- ifelse(housing_asset$q4 %in% c(1, 2, 3), "단독주택", 
													 ifelse(housing_asset$q4 == 4, "아파트",
													 			 ifelse(housing_asset$q4 == 5, "연립주택", "다세대주택")))
mean_q52_4 <- housing_asset %>% 
	group_by(q4) %>% 
	summarise(mean = mean(q52_4)) %>% 
	mutate(type = "총 자산 평균")

mean_q53_1_4 <- housing_asset %>% 
	group_by(q4) %>% 
	summarise(mean = mean(q53_1_4)) %>% 
	mutate(type = "총 부채 평균")

#mean_q52_4
#mean_q53_1_4
housing_asset <- bind_rows(mean_q52_4, mean_q53_1_4)
#housing_asset

ggplot(data = housing_asset, aes(x = q4, y = mean, fill = type)) + 
	geom_col(position = "dodge")
