#Google Map ApI
install.packages("ggmap")
install.packages('ggplot2')
library(ggmap)
locs<-c('홍익대학교','서강대학교','이화여자대학교','연세대학교','명지대학교')
locs
addr<-c('서울특별시 마포구 창전동 와우산로 94','서울특별시 마포구 신수동 백범로 35',
        '서울특별시 서대문구 신촌동 이화여대길 52','서울특별시 서대문구 신촌동 연세로 50',
        '서울특별시 서대문구 거북골로 34')
gcode<-geocode(enc2utf8(addr))
gcode
df.loc<-data.frame(name=locs,'경도'=gcode$lon,'위도'=gcode$lat)
df.loc
center<-c(mean(df.loc[,2]),mean(df.loc[,3]))
center
map<-get_googlemap(center=center, maptype='roadmap',zoom=13,marker=gcode)
myMap<-ggmap(map)
myMap+geom_text(data=df.loc,aes(x=경도,y=위도), size=5, label=df.loc$name)
