install.packages("DataExplorer")

library(foreign)
library(DataExplorer)
library(dplyr)

#spsse데이터 파일 읽어 오기
health <- read.spss('/Users/seonghyeonma/r_practice/hn20_all.sav', to.data.frame=T, reencode='UTF-8')
introduce(health)
#결측치 확인하기
profile_missing(health)
#결측치가 90%이상인것만 추출하기
dat_missing <- profile_missing(health)
subset(dat_missing, pct_missing > 0.9)
arrange(subset(dat_missing, pct_missing>0.9), desc(pct_missing))

var <- c('ID','sex','age','DE1_31','HE_glu','HE_HbA1c','HE_DM_HbA1c')
df_health <- select(health, var)  
df_health

#결측치 확인 및 제거
introduce(df_health)
df_health <- na.omit(df_health)
head(df_health, 5)

#이론:당뇨환자에서 인슐린 투여군과 비투여군은 차이가 있을것이다

#당뇨병 여부의 분리, 정상 1, 공복형당장애 2 = 정상(),당뇨병(3)이면 당뇨병(1)로 변경
df_health$HE_DM2 <- ifelse(df_health$HE_DM==3, 1, 0)
#인슐린 투여여부 분리
df_subject <- subset(df_health, df_health$HE_DM2==1 & df_health$DE1_31==0 | df_health$DE1_31==1)
#대상자 추출
nrow(df_subject)
head(df_subject, 10)

#시각화 분석 
#QQplot 
#공복혈당에 대한 분포이다, 근접하게 분포하면 정규성을 가진다
qqnorm(df_subject$HE_glu)
qqline(df_subject$HE_glu)


#정규분포를 따르지 않는다
var.test(df_subject$HE_glu ~ df_subject$DE1_31)

t.test(df_subject$HE_glu ~ df_subject$DE1_31, paired=F, var.equal=F, conf.level=0.95)


      
#anova 분석
aov.model1=aov(HE_glu~as.factor(HE_DM_HbA1c),data=df_health)
summary(aov.model1)
t.test(HE_glu ~ as.factor(DE1_31), data=df_subject, paired=F, var.equal=T)
TukeyHSD(aov.model1)



