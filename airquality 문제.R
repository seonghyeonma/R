library(dplyr)
dim(airquality)
data(airquality)
head(airquality)
airquality_df <- tbl_df(airquality)


#(1) 9월달 중 온도가 90F 이상인 날이 몇일 있었나?
filter(airquality_df, Month ==9, Temp>=90)
  #4일동안 있었다. 

#(2) 가장 온도가 낮은 날은 언제였나?
minTemp <- max(airquality$Temp)
airquality$Temp == min(airquality$Temp)
airquality [airquality$Temp == minTemp,]
airquality [airquality$Temp == min(airquality$Temp), ]

#(3) 7,8월 중 가장 온도가 높은 날은 언제였나? 이때 온도는? (화씨 기준)
maxtemp <- filter(airquality, Month == 7 | Month== 8)
airquality$Temp == max(maxtemp$Temp)
airquality[airquality$Temp==maxtemp,]
airquality [airquality$Temp == max(airquality$Temp), ]


#(4) 온도를 섭씨로 바꾸어 temp_celsius 변수로 저장하라.섭씨 
#온도 30C이상인 날은 hot, 이하는 mild라고 할 때, hot한 날의 빈도와 mild한 날의 빈도를 알아내고 막대그래프로 그리시오
#1. 온도를 섭씨로 바꾸기
airquality_df
cal_celsius <- function(Temp) {
  celsius <- (Temp - 30)/2
  return(celsius)
}
temp_celsius <-  (cal_celsius(airquality$Temp))
hot<-airquality_df2 %>% filter (temp_celsius>=30)
mild<- airquality_df2 %>% filter (temp_celsius<=30)
airquality_df2<- airquality_df%>% mutate(temp_celsius)
mean(airquality_df2$temp_celsius, na.rm = T)
airquality_df2$temp_celsius <- ifelse(is.na(airquality_df2$temp_celsius), 24, airquality_df2$temp_celsius)  #결측치 처리
table(is.na(airquality_df2$temp_celsius))  

airquality_df2

table(airquality_df2$temp_celsius)
library(ggplot2)
qplot(airquality_df2$temp_celsius<=30)#날씨가 mild한 경우를 보여준다 True의 결과값을 참고
qplot(airquality_df2$temp_celsius>=30)#날씨가 hot한 경우를 보여준다 True의 결과값을 참고

#hot mild의 비율은 찾았지만 그래프로 어떻게 표현해야할지 모르겠습니다 ㅠㅠ

is.na(airquality_df2) 
table(is.na(airquality_df2))
table(is.na(airquality_df2$Ozone))
mean(airquality_df2$Ozone)
sum(airquality_df2$Ozone)






