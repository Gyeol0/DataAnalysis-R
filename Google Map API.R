#Google Map ApI
install.packages("ggmap")
install.packages('ggplot2')
library(ggmap)
locs<-c('ȫ�ʹ��б�','�������б�','��ȭ���ڴ��б�','�������б�','�������б�')
locs
addr<-c('����Ư���� ������ â���� �Ϳ��� 94','����Ư���� ������ �ż��� ����� 35',
        '����Ư���� ���빮�� ���̵� ��ȭ����� 52','����Ư���� ���빮�� ���̵� ������ 50',
        '����Ư���� ���빮�� �źϰ�� 34')
gcode<-geocode(enc2utf8(addr))
gcode
df.loc<-data.frame(name=locs,'�浵'=gcode$lon,'����'=gcode$lat)
df.loc
center<-c(mean(df.loc[,2]),mean(df.loc[,3]))
center
map<-get_googlemap(center=center, maptype='roadmap',zoom=13,marker=gcode)
myMap<-ggmap(map)
myMap+geom_text(data=df.loc,aes(x=�浵,y=����), size=5, label=df.loc$name)