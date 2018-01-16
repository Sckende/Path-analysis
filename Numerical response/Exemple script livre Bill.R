install.packages('ggm')

require(ggm)

#####création d'un DAG correspondant à l'exemple de Bill page 25#####

fig26<-DAG (
  U~X+V,
  S1~U,
  W~V+Y,
  S2~W, order=FALSE
)

#dsep of X et V given an empty conditioning vertices set
dSep(fig26, first = "X", second = "V", cond = NULL)

#dsep of X et V given conditioning vertices U and W
dSep(fig26, first = "X", second = "V", cond = c("U","W"))

#####Papier Bill 2009#####
#simulation
x<-rnorm(1000)
y<-rnorm(1000)
e<-rnorm(1000, 0, 0.5)
z<-x+y+e
plot(x,y)
#To proof the contre-intuitive consequence
pairs(data.frame(x,y,z))#x & Y no correlation and Y&Z and X&Z correlation
#with statistic test
#dependance between x & Z / y & z
summary(lm(x~z))
summary(lm(y~z))
#independance between x & y
summary(lm(x~y))
#but what eslse between x & Y given z
summary(lm(x~z+y))#dependance between x & Y !!!!!

setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
require(lme4)
require(nlme)

Shipley<-read.table("Shipley.txt",sep = "\t", dec = ".", h=T)
summary(Shipley)

#Independence claim: (Date,lat)|{DD} 
fit1<-lme(Date~DD+lat,data=Shipley,random=~1|site/tree,na.action=na.omit)
summary(fit1)

#Independence claim: (Growth,lat)|{Date} 
fit2<-lme(Growth~Date+lat,data=Shipley,random=~1|site/tree,na.action=na.omit) 
summary(fit2)

#Independence claim: (Growth,DD)|{Date,lat| 
fit3<-lme(Growth~Date+lat+DD,data=Shipley,random=~1|site/tree,na.action=na.omit)
summary(fit3)

#Independence claim: (Live,lat)|{Growth}
fit4<-lmer(Live~Growth+lat+(1|site)+(1|tree),data=Shipley, na.action=na.omit, family=binomial(link="logit"))
summary(fit4)
