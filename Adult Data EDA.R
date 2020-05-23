#Adult DAta 불러오기
getwd()
setwd('C:/Users/user/Desktop/김한결')
adult<-read.csv('adult data.csv')
str(adult)
#기술통계 분석
class(adult)
summary(adult)
#Histogram & Density
Age<-adult$Age
hist(Age,main='Age')
plot(density(Age),main='Age',ylim=c(0,0.03))
#BOXPLOT
CG<-adult$Capital.Gain
boxplot(Age,xlab='Age',main='Age Box Plot',horizontal=T)
boxplot(CG,xlab='Capital Gain',main='Capital Gain Box Plot',horizontal=T)
#Pie Chart(Race)
Race<-adult$Race
table(Race)
x01<-c(311,1039,3124,271,27816)
lbls01<-c('Amer-Indian-Eskimo', 'Asian-Pac-Islander', 'Black', 'Other', 'White')
percent01<-(x01/sum(x01))*100
lbls001<-paste(lbls01,'\n', percent01,'%')    
pie(x01,labels=lbls001,main='Race Pie Chart')
#Pie Chart(Sex)
Sex<-adult$Sex
table(Sex)
x02<-c(10771,21790)
lbls02<-c('Female0','Male')
percent02<-(x02/sum(x02))*100
lbls002<-paste(lbls02,'\n',percent02,'%')
pie(x02, labels=lbls002,main='Sex Pie Chart')
#Target <=50K(Race) Pie Chart
Target<-adult$Target
table(Race,Target)
x03<-c(275,763,2737,246,20699)
percent03<-(x03/sum(x03))*100
lbls03<-lbls01
lbls003<-paste(lbls03,'\n',percent03,'%')
pie(x03,labels=lbls003,main='Target <=50K(Race)')
#Target >50K(Race) Pie Chart
x04<-c(36,276,387,25,7117)
percent04<-(x04/sum(x04))*100
lbls04<-lbls03
lbls004<-paste(lbls04,'\n',percent04,'%')
pie(x04,labels=lbls004,main='Target >50K(Race)')
#Target <=50K(Sex) Pie Chart
table(Sex,Target)
x05<-c(9592,15128)
percent05<-(x05/sum(x05))*100
lbls05<-c('Female','Male')
lbls005<-paste(lbls05,'\n',percent05,'%')
pie(x05,labels=lbls005,main='Target <=50K(Sex)')
#Target >50K(Sex) Pie Chart
x06<-c(1179,6662)
percent06<-(x06/sum(x06))*100
lbls06<-c('Female','Male')
lbls006<-paste(lbls06,'\n',percent06,'%')
pie(x06,labels=lbls006,main='Target >50K(Sex)')
#Scatterplot
Hpw<-adult$Hours.per.week
Edn<-adult$Education.N
data01<-data.frame(Age,Hpw,CG,Edn)
pairs(data01,main='adult Scatter Matrix')
#Radar Diagram
install.packages('fmsb')
library(fmsb)
radar1<-data.frame(Age=c(90,17),Edu.N=c(16,1),Capital.Gain=c(99999,0),
                   Hours.per.week=c(99,1))
radar2<-data.frame(Age=sample(Age,1),Edu.N=sample(Edn,1),Capital.Gain=
                     sample(CG,1),Hours.per.week=sample(Hpw,1))
radar<-rbind(radar1,radar2)
radarchart(radar,axistype=1,seg=5,plty=1,title
           ='첫번째 타입')
radarchart(radar,axistype=2,pcol=topo.colors(3),plty=1,
           title='두번째 타입')
radarchart(radar,axistype=3,pty=32,plty=1,axislabcol='grey',
           na.itp=FALSE,title='세번째 타입')
radarchart(radar,axistype=0,plwd=1:5,pcol=1,
           title='네번째 타입')

#Parallel Coordinate
data01
library(MASS)
parcoord(data01[1:100,])

#Mosaic Plot(Target, Race)
tb1<-table(Target,Sex)
tb1
mosaicplot(tb1,main='Target$Sex',color=TRUE)
#Mosaic Plot(Target, Education)
EDNumNew<-Edn
EDNumNew[9<=Edn]<-3
EDNumNew[Edn<9]<-2
EDNumNew[Edn<5]<-1
EDNumNew
tb2<-table(Target,EDNumNew)
tb2
mosaicplot(tb2,main='Target$Education',color=TRUE)
#Mosaic Plot(Target, Education-Num)
tb3<-table(Target,Edn)
tb3
mosaicplot(tb3,main='Target$Education-Num',color=TRUE,las=1)
