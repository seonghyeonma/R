#구글맵을 이용한 시각화 

library (ggplot2)
library (ggmap)
dev.off()
mykey = "Use personal key"
register_google(key=mykey)

##Test1 
gc <- geocode("waco,texas",source ="google")
center <- as.numeric(gc)
ggmap(get_googlemap(center=center,color="bw",scale=2),extent="device"
  
##Test2
gc<-geocode(enc2utf8("제주도"))
center <- as.numeric(gc)
ggmap(get_googlemap(center=center,maptype="roadmap",
                    scale=2,
                    markers = gc),extent="device")


##Test3 - mark 추가
gc<-geocode(enc2utf8("제주도"))
center <- as.numeric(gc)
ggmap(get_googlemap(center=center,maptype="roadmap",
                    scale=2,
                    markers = gc),extent="device")

##Test4 - 여러군데 찍기 
  names <- c("해운대","광안리","태종대","감천문화마을","동백섬")
  gc <- geocode(enc2utf8(names))
  df<- data.frame(name=names,lon=gc$lon, lat=gc$lat)
  center <- c(mean(df$lon),mean(df$lat))
  gmap<- ggmap(get_googlemap(center=center,maptype="roadmap",
                             scale=2,zoom=12,
                             markers = gc),extent="device")
  gmap

##마크에 텍스트 추가 
library(ggplot2)
theme_set(theme_gray(base_family='AppleMyungjo'))
gmap+geom_text(data = df, aes(x=lon,y=lat),size=5,label=df$name)+
  
#원추가 및 범례 넣기
df <- head(quakes,500)
cen<-c(mean(df$long),mean(df$lat))
cen

gc<- data.frame(lon =df$long,lat=df$lat)
gc$lon<-ifelse(gc$lon>180,-(360-gc$lon),gc$lon)
gc

map<- get_googlemap(center=cen,maptype="roadmap",zoom=4,scale=1)
ggmap(map,fullpage=TRUE)+geom_point(data=df,aes(x=long,y=lat,size=mag),alpha=0.5)

# google API key 입력하기

googleAPIkey = 'AIzaSyCjzeKUA70bkbPnz1k3sYQZICAw8xxDMmg'
register_google(googleAPIkey)
gg_seoul<-get_googlemap('seoul', zoom=6, maptype = 'roadmap')
ggmap(gg_seoul)

## 지도 위에 산점도 그리기
library(dplyr)
library(ggplot2)

# 한글 검색을 위해 utf8로 변환한 후 위도와 경도 데이터를 geo_doce 변수에 할당
geo_code<-enc2utf8('대전역') %>% geocode()
geo_data<-as.numeric(geocode)  

# 대전역의 위치정보를 가져온 후 글 지도 호출

get_googlemap(center=geo_data, maptype = 'roadmap', zoom=13) %>% ggmap()+
  geom_point(data=geo_code, aes(x=geo_code$lon, y=geo_code$lat))


getwd()
library(ggmap)
install.packages('stringr')
library(stringr)
loc<-read.csv('Data/서울_강동구_공영주차장_위경도.csv', header=T)
loc
kd<-get_map('Amsa_dong', zoom=13, maptype = 'roadmap')
kor.map<-ggmap(kd)+geom_point(data=loc, aes(x=LON, y=LAT),size=3, alpha=0.7, color='red')
kor.map+geom_text(data=loc, aes(x=LON, y=LAT+0.001, label=주차장명),size=2)
