rm(list=ls())
library(data.table)
library(ncdf4)
library(useful)
library(caret)
library(lubridate)
library(xgboost)

load('allData1Hot_modelOnly.Rdata')
load('allData1Hot_SHAP_500000.Rdata')
bst.shap.pred1 <- bst.shap.pred
idx1 <- idxz
load('allData1Hot_SHAP_1000000.Rdata')
bst.shap.pred2 <- bst.shap.pred
idx2 <- idxz
load('allData1Hot_SHAP_1539621.Rdata')
bst.shap.pred3 <- bst.shap.pred
idx3 <- idxz


# Takes ~ 30 sec
bst.pred <- predict(bst, testX)
m.test.dt$bst.pred <- bst.pred

m.test.dt$predCorr <- 0
m.test.dt[bst.pred==diff]$predCorr <- 1
predIdx <- which(m.test.dt$predCorr==1)



formulaPred <- diff ~ U + V + Temp + QITOT + QICN + QI + Q + OMEGA + IWC +
  CNV_CVW + BYNCY + ZLFC + ZLCL + ZCBL + SDMZ + Q600 + LWI + IWP + INHB +
  CN_PRCP + CNV_TOPP + CNV_BASEP + CLDTT + CAPE + lat + lon - 1

lm.pred <- lm(formulaPred,m.test.dt)
lm.pred <- round(predict(lm.pred, m.test.dt))

sum(m.test.dt$diff==bst.pred)/dim(m.test.dt)[1]
sum(m.test.dt$diff==lm.pred)/dim(m.test.dt)[1]


###
shapLow <- bst.shap.pred1[[1]]
shapLow <- rbind(shapLow,bst.shap.pred2[[1]])
shapLow <- rbind(shapLow,bst.shap.pred3[[1]])

shapMed <- bst.shap.pred1[[2]]
shapMed <- rbind(shapMed,bst.shap.pred2[[2]])
shapMed <- rbind(shapMed,bst.shap.pred3[[2]])

shapHigh <- bst.shap.pred1[[3]]
shapHigh <- rbind(shapHigh,bst.shap.pred2[[3]])
shapHigh <- rbind(shapHigh,bst.shap.pred3[[3]])

r <- data.table(apply(abs(shapLow),c(2),mean))
names(r) <- 'low'
r$names <- names(colMeans(shapLow))
r$med <- data.table(apply(abs(shapMed),c(2),mean))
r$high <- data.table(apply(abs(shapHigh),c(2),mean))

tp.r <- melt(r,id.var='names')

v <- data.table(apply(abs(shapLow),c(2),sd))
names(v) <- 'low'
v$names <- names(colMeans(shapLow))
v$med <- data.table(apply(abs(shapMed),c(2),sd))
v$high <- data.table(apply(abs(shapHigh),c(2),sd))

tp.v <- melt(r,id.var='names')
names(tp.v) <- c('names','variable','v.std')

tp.r <- merge(tp.v,tp.r)

ggplot(tp.r[names!='BIAS'],aes(x=names,y=value,group=variable,fill=variable)) +
  geom_bar(position="dodge",stat='identity') +
  coord_flip()
# 
# names(tp.r)
# 
# nr <- r
# nr$low <- nr$low/max(nr[names!='BIAS']$low)
# nr$med <- nr$med/max(nr[names!='BIAS']$med)
# nr$high <- nr$high/max(nr[names!='BIAS']$high)
# tp.nr <- melt(nr,id.var='names')
# 
# tp.nr[,mean(value),by=names][order(V1)]
# tp.nr$name <- tp.nr$name
# tp.nr[names=='Temp']$name <- 'Temperature'
# tp.nr[names=='U']$name <- 'Eastward Wind Component'
# tp.nr[names=='V']$name <- 'Northward Wind Component'
# tp.nr[names=='QITOT']$name <- 'Grid Box Mass Fraction of Cloud Ice Water'
# tp.nr[names=='QICN']$name <- 'Mass Fraction of Convective Cloud Ice Water'
# tp.nr[names=='QI']$name <- 'In-Cloud Cloud Ice'
# tp.nr[names=='Q']$name <- 'Specific Humidity'
# tp.nr[names=='OMEGA']$name <- 'Vertical Pressure Velocity'
# tp.nr[names=='IWC']$name <- 'Ice Water Content'
# tp.nr[names=='CNV_CVW']$name <- 'Updraft Vertical Velocity'
# tp.nr[names=='BYNCY']$name <- 'Buoyancy of Surface Parcel'
# tp.nr[names=='ZLFC']$name <- 'Level of Free Convection'
# tp.nr[names=='ZLCL']$name <- 'Lifting Condensation Level'
# tp.nr[names=='ZCBL']$name <- 'Height of Cloud Base Layer'
# tp.nr[names=='SDMZ']$name <- 'Sedimentation Loss of Cloud Ice'
# tp.nr[names=='Q600']$name <- 'Q at 600mb'
# tp.nr[names=='LWI']$name <- 'Land/Water/Ice Flag'
# tp.nr[names=='IWP']$name <- 'Ice Water Path'
# tp.nr[names=='INHB']$name <- 'Inhibition for Surface Parcel'
# tp.nr[names=='CNV_TOPP']$name <- 'Pressure at Convective Cloud Top'
# tp.nr[names=='CNV_BASEP']$name <- 'Pressure at Convective Cloud Base'
# tp.nr[names=='CLDTT']$name <- 'Total Cloud Area Fraction'
# tp.nr[names=='CAPE']$name <- 'CAPE for Surface Parcel'
# tp.nr[names=='CN_PRCP']$name <- 'Convective Precipitation'
# tp.nr[names=='lat']$name <- 'Latitude'
# tp.nr[names=='lon']$name <- 'Longitude'
# 
# 
# cols <- c("low" = 'lightgrey', "med" = 'darkGrey', 'high' = 'Black')
# ggplot(tp.nr[names!='BIAS'],aes(x=reorder(name,value),y=value,
#                                 group=variable,fill=as.factor(variable))) +
#   geom_bar(position=position_dodge(),stat='identity',col='black',width=0.7) + 
#   coord_flip() + labs(y='mean(|SHAP|)',x='',title='') + 
#   theme_Publication() + guides(fill=F) +
#   scale_fill_manual(values = cols)
#   
# 
# ggplot(tp.r[names!='BIAS'],aes(x=reorder(names,value),y=value,group=variable,fill=variable)) +
#   geom_bar(position="dodge",stat='identity') + coord_flip()

predLowIdx <- which(m.test.dt$diff==0)
predMedIdx <- which(m.test.dt$diff==1)
predHighIdx <- which(m.test.dt$diff==2)
shapPred <- rbind(shapLow[predLowIdx,],shapMed[predMedIdx,],shapHigh[predHighIdx,])

r <- data.table(apply(abs(shapPred),c(2),median))
names(r) <- 'Pred'
r$names <- names(colMeans(shapLow))[1:27]

v <- data.table(apply(abs(shapPred),c(2),quantile, probs=0.25))
names(v) <- 'p.low'
v$names <- names(colMeans(shapLow))[1:27]
v$p.high <- data.table(apply(abs(shapPred),c(2),quantile, probs=0.75))


tp.nr <- merge(v,r)
tp.nr[,mean(value),by=names][order(V1)]
tp.nr$name <- tp.nr$name
tp.nr[names=='Temp']$name <- 'Temperature'
tp.nr[names=='U']$name <- 'Eastward Wind Component'
tp.nr[names=='V']$name <- 'Northward Wind Component'
tp.nr[names=='QITOT']$name <- 'Grid Box Mass Fraction of Cloud Ice Water'
tp.nr[names=='QICN']$name <- 'Mass Fraction of Convective Cloud Ice Water'
tp.nr[names=='QI']$name <- 'In-Cloud Cloud Ice'
tp.nr[names=='Q']$name <- 'Specific Humidity'
tp.nr[names=='OMEGA']$name <- 'Vertical Pressure Velocity'
tp.nr[names=='IWC']$name <- 'Ice Water Content'
tp.nr[names=='CNV_CVW']$name <- 'Updraft Vertical Velocity'
tp.nr[names=='BYNCY']$name <- 'Buoyancy of Surface Parcel'
tp.nr[names=='ZLFC']$name <- 'Level of Free Convection'
tp.nr[names=='ZLCL']$name <- 'Lifting Condensation Level'
tp.nr[names=='ZCBL']$name <- 'Height of Cloud Base Layer'
tp.nr[names=='SDMZ']$name <- 'Sedimentation Loss of Cloud Ice'
tp.nr[names=='Q600']$name <- 'Q at 600mb'
tp.nr[names=='LWI']$name <- 'Land/Water/Ice Flag'
tp.nr[names=='IWP']$name <- 'Ice Water Path'
tp.nr[names=='INHB']$name <- 'Inhibition for Surface Parcel'
tp.nr[names=='CNV_TOPP']$name <- 'Pressure at Convective Cloud Top'
tp.nr[names=='CNV_BASEP']$name <- 'Pressure at Convective Cloud Base'
tp.nr[names=='CLDTT']$name <- 'Total Cloud Area Fraction'
tp.nr[names=='CAPE']$name <- 'CAPE for Surface Parcel'
tp.nr[names=='CN_PRCP']$name <- 'Convective Precipitation'
tp.nr[names=='lat']$name <- 'Latitude'
tp.nr[names=='lon']$name <- 'Longitude'

ggplot(tp.nr[names!='BIAS'],aes(x=reorder(name,Pred),y=Pred)) +
#  geom_bar(stat='identity',alpha=0.5) + 
  geom_pointrange(aes(ymin = p.low, ymax = p.high),lwd = 0.75) +
  coord_flip() + labs(y='median(|SHAP|)',x='',title='') + 
  theme_Publication()  

ggplot(tp.nr[names %in% c('CAPE','CN_PRCP','ZLCL','CNV_CVW','LWI','Q600')],
       aes(x=reorder(name,Pred),y=Pred)) +
  #  geom_bar(stat='identity',alpha=0.5) + 
  geom_pointrange(aes(ymin = p.low, ymax = p.high),lwd = 0.75) +
  coord_flip() + labs(y='median(|SHAP|)',x='',title='') + 
  theme_Publication()  


ggplot(tp.nr[names!='BIAS'],aes(x=reorder(name,Pred),y=p.high-p.low)) +
  #  geom_bar(stat='identity',alpha=0.5) + 
  geom_point() + coord_flip() + labs(y='median(|SHAP|)',x='',title='') + 
  theme_Publication()  

##
#
##
shapLow <- data.table(shapLow)
shapLow$pred <- m.test.dt$bst.pred
shapLow$truth <- m.test.dt$diff

shapLow$predCorr <- 0
shapLow[pred==truth]$predCorr <- 1

shapMed <- data.table(shapMed)
shapMed$pred <- m.test.dt$bst.pred
shapMed$truth <- m.test.dt$diff

shapMed$predCorr <- 0
shapMed[pred==truth]$predCorr <- 1

shapHigh <- data.table(shapHigh)
shapHigh$pred <- m.test.dt$bst.pred
shapHigh$truth <- m.test.dt$diff

shapHigh$predCorr <- 0
shapHigh[pred==truth]$predCorr <- 1

shapLow$v.CAPE <- m.test.dt$CAPE
shapMed$v.CAPE <- m.test.dt$CAPE
shapHigh$v.CAPE <- m.test.dt$CAPE

shapLow$v.CNV_CVW <- m.test.dt$CNV_CVW
shapMed$v.CNV_CVW <- m.test.dt$CNV_CVW
shapHigh$v.CNV_CVW <- m.test.dt$CNV_CVW

shapLow$v.LWI <- m.test.dt$LWI
shapMed$v.LWI <- m.test.dt$LWI
shapHigh$v.LWI <- m.test.dt$LWI

shapLow$v.ZLCL <- m.test.dt$ZLCL
shapMed$v.ZLCL <- m.test.dt$ZLCL
shapHigh$v.ZLCL <- m.test.dt$ZLCL

shapLow$v.Q600 <- m.test.dt$Q600
shapMed$v.Q600 <- m.test.dt$Q600
shapHigh$v.Q600 <- m.test.dt$Q600

shapLow$v.CN_PRCP <- m.test.dt$CN_PRCP
shapMed$v.CN_PRCP <- m.test.dt$CN_PRCP
shapHigh$v.CN_PRCP <- m.test.dt$CN_PRCP

shapLow$v.Temp <- m.test.dt$Temp
shapHigh$v.Temp <- m.test.dt$Temp
shapMed$v.Temp <- m.test.dt$Temp

shapLow$v.CNV_TOPP <- m.test.dt$CNV_TOPP
shapMed$v.CNV_TOPP <- m.test.dt$CNV_TOPP
shapHigh$v.CNV_TOPP <- m.test.dt$CNV_TOPP





table(shapLow$predCorr)
1165093/(1165093 + 374528)


x <- confusionMatrix(reference = as.factor(shapLow$truth),data=as.factor(shapLow$pred))
x$byClass
x$overall

sum(shapLow$truth == 0 & shapLow$pred == 0 )/dim(shapLow[truth == 0])[1]
sum(shapLow$truth != 0 & shapLow$pred != 0 )/dim(shapLow[truth != 0])[1]
sum(shapLow$truth != 0 & shapLow$pred == 0 )/dim(shapLow[truth != 0])[1]
sum(shapLow$truth == 0 & shapLow$pred != 0 )/dim(shapLow[truth == 0])[1]

sum(shapLow$truth == 1 & shapLow$pred == 1 )/dim(shapLow[truth == 1])[1]
sum(shapLow$truth != 1 & shapLow$pred != 1 )/dim(shapLow[truth != 1])[1]
sum(shapLow$truth != 1 & shapLow$pred == 1 )/dim(shapLow[truth != 1])[1]
sum(shapLow$truth == 1 & shapLow$pred != 1 )/dim(shapLow[truth == 1])[1]

sum(shapLow$truth == 2 & shapLow$pred == 2 )/dim(shapLow[truth == 2])[1]
sum(shapLow$truth != 2 & shapLow$pred != 2 )/dim(shapLow[truth != 2])[1]
sum(shapLow$truth != 2 & shapLow$pred == 2 )/dim(shapLow[truth != 2])[1]
sum(shapLow$truth == 2 & shapLow$pred != 2 )/dim(shapLow[truth == 2])[1]

table(shapLow$truth)/dim(shapLow)[1] *100


sum(shapLow[pred!=0]$truth == shapLow[pred!=0]$pred)/dim(shapLow[pred!=0])[1]



sum(shapLow$truth == shapLow$pred)/dim(shapLow)[1]
sum(shapLow[pred==0]$truth == shapLow[pred==0]$pred)/dim(shapLow[pred==0])[1]
sum(shapLow[pred==1]$truth == shapLow[pred==1]$pred)/dim(shapLow[pred==1])[1]
sum(shapLow[pred==2]$truth == shapLow[pred==2]$pred)/dim(shapLow[pred==2])[1]

sum(shapLow[pred!=0]$truth == shapLow[pred!=0]$pred)/dim(shapLow[pred!=0])[1]
sum(shapLow[pred!=1]$truth == shapLow[pred!=1]$pred)/dim(shapLow[pred!=1])[1]
sum(shapLow[pred!=2]$truth == shapLow[pred!=2]$pred)/dim(shapLow[pred!=2])[1]

table(shapLow$truth)/dim(shapLow)[1] *100

randomTruth <- sample(c(0,1,2),size = dim(shapLow)[1],replace = T,
                      prob = table(shapLow$truth)/dim(shapLow)[1])
x <- confusionMatrix(reference = as.factor(shapLow$truth),data=as.factor(randomTruth))
x$byClass
x$overall

randomTruth <- sample(c(0,1,2),size = dim(shapLow)[1],replace = T,prob=c(1/3,1/3,1/3))
x <- confusionMatrix(reference = as.factor(shapLow$truth),data=as.factor(randomTruth))
temp <- x$byClass[,11]
p0 <- c()
p1 <- c()
p2 <- c()
for(i in 1:50){
  randomTruth <- sample(c(0,1,2),size = dim(shapLow)[1],replace = T,prob=c(1/3,1/3,1/3))
  p0 <- c(p0, sum(shapLow[pred==0]$truth == randomTruth[shapLow$pred==0])/dim(shapLow[pred==0])[1])
  p1 <- c(p1, sum(shapLow[pred==1]$truth == randomTruth[shapLow$pred==1])/dim(shapLow[pred==1])[1])
  p2 <- c(p2, sum(shapLow[pred==2]$truth == randomTruth[shapLow$pred==2])/dim(shapLow[pred==2])[1])
}


x$overall
sum(shapLow$truth == shapLow$pred)/dim(shapLow)[1]
sum(shapLow[pred==0]$truth == randomTruth[shapLow$pred==0])/dim(shapLow[pred==0])[1]
sum(shapLow[pred==1]$truth == randomTruth[shapLow$pred==1])/dim(shapLow[pred==1])[1]
sum(shapLow[pred==2]$truth == randomTruth[shapLow$pred==2])/dim(shapLow[pred==2])[1]

cols <- c("-1" = 'Blue', "1" = 'Red', '0' = 'Black')
theme_Publication <- function(base_size=14, base_family="") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = 'Black'),
            strip.background = element_blank(),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.text = element_text(face="bold")
    ))
  
}

###
tpLow <- shapLow[predCorr<=1,.(mVal = median(CAPE),
                               sdVal = sqrt(var(CAPE)),
                               lowVal = quantile(CAPE,probs=0.25),
                               highVal = quantile(CAPE,probs=0.75),
                               n=length(CAPE)),
                 by=.(v.CAPE=floor(v.CAPE/250)*250)]

tpMed <- shapMed[predCorr<=1,.(mVal = median(CAPE),
                               sdVal = sqrt(var(CAPE)),
                               lowVal = quantile(CAPE,probs=0.25),
                               highVal = quantile(CAPE,probs=0.75),
                               n=length(CAPE)),
                 by=.(v.CAPE=floor(v.CAPE/250)*250)]

tpHigh <- shapHigh[predCorr<=1,.(mVal = median(CAPE),
                                 sdVal = sqrt(var(CAPE)),
                                 lowVal = quantile(CAPE,probs=0.25),
                                 highVal = quantile(CAPE,probs=0.75),
                                 n=length(CAPE)),
                   by=.(v.CAPE=floor(v.CAPE/250)*250)]

tpLow$val <- '-1'
tpMed$val <- '0'
tpHigh$val <- '1'
tp <- rbind(rbind(tpLow,tpMed),tpHigh)

ggplot(tp,aes(x=mVal-sdVal,y=lowVal)) + geom_point()

# tp$lowVal <- tp$mVal-tp$sdVal
# tp$highVal <- tp$mVal+tp$sdVal
tp$sign <- 0
tp[sign(lowVal) == -1 & sign(highVal) == -1]$sign <- -1
tp[sign(lowVal) == 1 & sign(highVal) == 1]$sign <- 1
tp.CAPE <- tp

ggplot(tp[n > 10],aes(x=v.CAPE,y=mVal,col=factor(sign))) +   
  geom_pointrange(aes(ymin = lowVal, ymax = highVal), fatten = 2.5) +
  geom_hline(yintercept = 0) + theme_Publication() +
  scale_colour_manual(values = cols) +
  facet_wrap(~val)+ 
  labs(y='SHAP Value',x='CAPE',title='') + guides(color=F)

###
tpLow <- shapLow[predCorr<=1,.(mVal = median(CNV_CVW),
                               sdVal = sqrt(var(CNV_CVW)),
                               lowVal = quantile(CNV_CVW,probs=0.25),
                               highVal = quantile(CNV_CVW,probs=0.75),
                               n=length(CNV_CVW)),
                 by=.(v.CNV_CVW=floor(v.CNV_CVW*100)/100)]
tpMed <- shapMed[predCorr<=1,.(mVal = median(CNV_CVW),
                               sdVal = sqrt(var(CNV_CVW)),
                               lowVal = quantile(CNV_CVW,probs=0.25),
                               highVal = quantile(CNV_CVW,probs=0.75),
                               n=length(CNV_CVW)),
                 by=.(v.CNV_CVW=floor(v.CNV_CVW*100)/100)]
tpHigh <- shapHigh[predCorr<=1,.(mVal = median(CNV_CVW),
                                 sdVal = sqrt(var(CNV_CVW)),
                                 lowVal = quantile(CNV_CVW,probs=0.25),
                                 highVal = quantile(CNV_CVW,probs=0.75),
                                 n=length(CNV_CVW)),
                   by=.(v.CNV_CVW=floor(v.CNV_CVW*100)/100)]

tpLow$val <- '-1'
tpMed$val <- '0'
tpHigh$val <- '1'
tp <- rbind(rbind(tpLow,tpMed),tpHigh)

# tp$lowVal <- tp$mVal-tp$sdVal
# tp$highVal <- tp$mVal+tp$sdVal
tp$sign <- 0
tp[sign(lowVal) == -1 & sign(highVal) == -1]$sign <- -1
tp[sign(lowVal) == 1 & sign(highVal) == 1]$sign <- 1
tp.CNV_CVW <- tp 

ggplot(tp[n > 10],aes(x=v.CNV_CVW,y=mVal,col=factor(sign))) +   
  geom_pointrange(aes(ymin = lowVal, ymax = highVal), fatten = 2.5) +
  geom_point() + geom_hline(yintercept = 0) + theme_Publication() +
  scale_colour_manual(values = cols) +
  facet_wrap(~val)+ 
  labs(y='SHAP Value',x='CNV_CVW') + guides(color=F) 

###
tpLow <- shapLow[predCorr<=1,.(mVal = median(CN_PRCP),
                               sdVal = sqrt(var(CN_PRCP)),
                               lowVal = quantile(CN_PRCP,probs=0.25),
                               highVal = quantile(CN_PRCP,probs=0.75),
                               n=length(CN_PRCP)),
                 by=.(v.CN_PRCP=floor(v.CN_PRCP*3e4)/3e4)]
tpMed <- shapMed[predCorr<=1,.(mVal = median(CN_PRCP),
                               sdVal = sqrt(var(CN_PRCP)),
                               lowVal = quantile(CN_PRCP,probs=0.25),
                               highVal = quantile(CN_PRCP,probs=0.75),
                               n=length(CN_PRCP)),
                 by=.(v.CN_PRCP=floor(v.CN_PRCP*3e4)/3e4)]
tpHigh <- shapHigh[predCorr <= 1,.(mVal = median(CN_PRCP),
                                   sdVal = sqrt(var(CN_PRCP)),
                                   lowVal = quantile(CN_PRCP,probs=0.25),
                                   highVal = quantile(CN_PRCP,probs=0.75),
                                   n=length(CN_PRCP)),
                   by=.(v.CN_PRCP=floor(v.CN_PRCP*3e4)/3e4)]

tpLow$val <- '-1'
tpMed$val <- '0'
tpHigh$val <- '1'
tp <- rbind(rbind(tpLow,tpMed),tpHigh)

# tp$lowVal <- tp$mVal-tp$sdVal
# tp$highVal <- tp$mVal+tp$sdVal
tp$sign <- 0
tp[sign(lowVal) == -1 & sign(highVal) == -1]$sign <- -1
tp[sign(lowVal) == 1 & sign(highVal) == 1]$sign <- 1
tp.CN_PRCP <- tp

dim(m.test.dt[CNV_CVW==0 & obs == 1])/dim(m.test.dt[obs == 1])
dim(m.test.dt[CNV_CVW==0 & LFR_GCC == 1])/dim(m.test.dt[obs == 1])


ggplot(tp[n > 10],aes(x=v.CN_PRCP,y=mVal,col=factor(sign))) +   
  geom_pointrange(aes(ymin = lowVal, ymax = highVal), fatten = 2.5) +
  geom_point() + geom_hline(yintercept = 0) + theme_Publication() +
  scale_colour_manual(values = cols) +
  facet_wrap(~val) + labs(y='SHAP Value',x='CN_PRCP',title='') + 
  guides(color=F) + scale_x_continuous(label=scientific_format())

###
tpLow <- shapLow[predCorr<=1,.(mVal = median(LWI),
                               sdVal = sqrt(var(LWI)),
                               lowVal = quantile(LWI,probs=0.25),
                               highVal = quantile(LWI,probs=0.75),
                               n=length(LWI)),
                 by=.(v.LWI=floor(v.LWI*50)/50)]
tpMed <- shapMed[predCorr<=1,.(mVal = median(LWI),
                               sdVal = sqrt(var(LWI)),
                               lowVal = quantile(LWI,probs=0.25),
                               highVal = quantile(LWI,probs=0.75),
                               n=length(LWI)),
                 by=.(v.LWI=floor(v.LWI*50)/50)]
tpHigh <- shapHigh[predCorr<=1,.(mVal = median(LWI),
                                 sdVal = sqrt(var(LWI)),
                                 lowVal = quantile(LWI,probs=0.25),
                                 highVal = quantile(LWI,probs=0.75),
                                 n=length(LWI)),
                   by=.(v.LWI=floor(v.LWI*50)/50)]

tpLow$val <- '-1'
tpMed$val <- '0'
tpHigh$val <- '1'
tp <- rbind(rbind(tpLow,tpMed),tpHigh)

# tp$lowVal <- tp$mVal-tp$sdVal
# tp$highVal <- tp$mVal+tp$sdVal
tp$sign <- 0
tp[sign(lowVal) == -1 & sign(highVal) == -1]$sign <- -1
tp[sign(lowVal) == 1 & sign(highVal) == 1]$sign <- 1
tp.LWI <- tp

ggplot(tp.LWI,aes(x=v.LWI,y=mVal,col=factor(sign))) +   
  geom_errorbar(aes(ymin = mVal-sdVal, ymax = mVal+sdVal)) +
  geom_point() + geom_hline(yintercept = 0) + theme_Publication() +
  scale_colour_manual(values = cols) +
  facet_wrap(~val)+ 
  labs(y='SHAP Value',x='LWI') + guides(color=F)

###
tpLow <- shapLow[predCorr<=1,.(mVal = median(ZLCL),
                               sdVal = sqrt(var(ZLCL)),
                               lowVal = quantile(ZLCL,probs=0.25),
                               highVal = quantile(ZLCL,probs=0.75),
                               n=length(ZLCL)),
                 by=.(v.ZLCL=floor(v.ZLCL/100)*100)]

tpMed <- shapMed[predCorr<=1,.(mVal = median(ZLCL),
                               sdVal = sqrt(var(ZLCL)),
                               lowVal = quantile(ZLCL,probs=0.25),
                               highVal = quantile(ZLCL,probs=0.75),
                               n=length(ZLCL)),
                 by=.(v.ZLCL=floor(v.ZLCL/100)*100)]

tpHigh <- shapHigh[predCorr<=1,.(mVal = median(ZLCL),
                                 sdVal = sqrt(var(ZLCL)),
                                 lowVal = quantile(ZLCL,probs=0.25),
                                 highVal = quantile(ZLCL,probs=0.75),
                                 n=length(ZLCL)),
                   by=.(v.ZLCL=floor(v.ZLCL/100)*100)]

tpLow$val <- '-1'
tpMed$val <- '0'
tpHigh$val <- '1'
tp <- rbind(rbind(tpLow,tpMed),tpHigh)

# tp$lowVal <- tp$mVal-tp$sdVal
# tp$highVal <- tp$mVal+tp$sdVal
tp$sign <- 0
tp[sign(lowVal) == -1 & sign(highVal) == -1]$sign <- -1
tp[sign(lowVal) == 1 & sign(highVal) == 1]$sign <- 1
tp.ZLCL <- tp 

ggplot(tp,aes(x=v.ZLCL,y=mVal,col=factor(sign))) +   
  geom_errorbar(aes(ymin = mVal-sdVal, ymax = mVal+sdVal)) +
  geom_point() + geom_hline(yintercept = 0) + theme_Publication() +
  scale_colour_manual(values = cols) +
  facet_wrap(~val)+ 
  labs(y='SHAP Value',x='ZLCL',title='Pred: 1') + guides(color=F)


###
tpLow <- shapLow[predCorr<=1,.(mVal = median(Q600),
                               sdVal = sqrt(var(Q600)),
                               lowVal = quantile(Q600,probs=0.25),
                               highVal = quantile(Q600,probs=0.75),
                               n=length(Q600)),
                 by=.(v.Q600=floor(v.Q600*5e3)/5e3)]
tpMed <- shapMed[predCorr<=1,.(mVal = median(Q600),
                               sdVal = sqrt(var(Q600)),
                               lowVal = quantile(Q600,probs=0.25),
                               highVal = quantile(Q600,probs=0.75),
                               n=length(Q600)),
                 by=.(v.Q600=floor(v.Q600*5e3)/5e3)]
tpHigh <- shapHigh[predCorr >= 0,.(mVal = median(Q600),
                                   sdVal = sqrt(var(Q600)),
                                   lowVal = quantile(Q600,probs=0.25),
                                   highVal = quantile(Q600,probs=0.75),
                                   n=length(Q600)),
                   by=.(v.Q600=floor(v.Q600*5e3)/5e3)]

tpLow$val <- '-1'
tpMed$val <- '0'
tpHigh$val <- '1'
tp <- rbind(rbind(tpLow,tpMed),tpHigh)

# tp$lowVal <- tp$mVal-tp$sdVal
# tp$highVal <- tp$mVal+tp$sdVal
tp$sign <- 0
tp[sign(lowVal) == -1 & sign(highVal) == -1]$sign <- -1
tp[sign(lowVal) == 1 & sign(highVal) == 1]$sign <- 1

tp.Q600 <- tp

ggplot(tp,aes(x=v.Q600,y=mVal,col=factor(sign(mVal)))) +   
  geom_errorbar(aes(ymin = mVal-sdVal, ymax = mVal+sdVal)) +
  geom_point() + geom_hline(yintercept = 0) + theme_Publication() +
  scale_colour_manual(values = cols) +
  facet_wrap(~val) + labs(y='SHAP Value',x='Q600',title='') + 
  guides(color=F)


###

tp.CAPE$varN <- 'CAPE'
tp.CNV_CVW$varN <- 'CNV_CVW'
tp.LWI$varN <- 'LWI'
tp.ZLCL$varN <- 'ZLCL'
tp.Q600$varN <- 'Q600'
tp.CN_PRCP$varN <- 'CN_PRCP'

names(tp.CAPE)[1] <- 'var'
names(tp.CNV_CVW)[1] <- 'var'
names(tp.LWI)[1] <- 'var'
names(tp.ZLCL)[1] <- 'var'
names(tp.Q600)[1] <- 'var'
names(tp.CN_PRCP)[1] <- 'var'

tp.all <- rbind(tp.CAPE,tp.CNV_CVW)
tp.all <- rbind(tp.all,tp.LWI)
tp.all <- rbind(tp.all,tp.ZLCL)
tp.all <- rbind(tp.all,tp.Q600)
tp.all <- rbind(tp.all,tp.CN_PRCP)

library(cowplot)
library(grid)
library(gridExtra)

 
  
p1 <- ggplot(tp.all[varN=='CAPE'],aes(x=var,y=mVal,col=factor(sign(mVal)))) +   
  geom_errorbar(aes(ymin = mVal-sdVal, ymax = mVal+sdVal)) +
  geom_point() + geom_hline(yintercept = 0) + theme_Publication() +
  scale_colour_manual(values = cols) + #coord_cartesian(ylim=c(-6.5,2)) + 
  facet_wrap(~val) + labs(y='SHAP Value',x='CN_PRCP',title='') + 
  guides(color=F)
p2 <- ggplot(tp.all[varN=='CNV_CVW'],aes(x=var,y=mVal,col=factor(sign(mVal)))) +   
  geom_errorbar(aes(ymin = mVal-sdVal, ymax = mVal+sdVal)) +
  geom_point() + geom_hline(yintercept = 0) + theme_Publication() +
  scale_colour_manual(values = cols) + coord_cartesian(ylim=c(-6.5,2)) + 
  facet_wrap(~val) + labs(y='SHAP Value',x='CN_PRCP',title='') + 
  guides(color=F)



c.plot <- plot_grid(p1, p2, ncol=1)
grid.arrange(arrangeGrob(c.plot))


###
tpLow <- shapLow[predCorr<=1,.(mVal = median(Temp),
                               sdVal = sqrt(var(Temp)),
                               lowVal = quantile(Temp,probs=0.25),
                               highVal = quantile(Temp,probs=0.75),
                               n=length(Temp)),
                 by=.(v.Temp=floor(v.Temp))]

tpMed <- shapMed[predCorr<=1,.(mVal = median(Temp),
                               sdVal = sqrt(var(Temp)),
                               lowVal = quantile(Temp,probs=0.25),
                               highVal = quantile(Temp,probs=0.75),
                               n=length(Temp)),
                 by=.(v.Temp=floor(v.Temp))]

tpHigh <- shapHigh[predCorr<=1,.(mVal = median(Temp),
                                 sdVal = sqrt(var(Temp)),
                                 lowVal = quantile(Temp,probs=0.25),
                                 highVal = quantile(Temp,probs=0.75),
                                 n=length(Temp)),
                   by=.(v.Temp=floor(v.Temp))]

tpLow$val <- '-1'
tpMed$val <- '0'
tpHigh$val <- '1'
tp <- rbind(rbind(tpLow,tpMed),tpHigh)


# tp$lowVal <- tp$mVal-tp$sdVal
# tp$highVal <- tp$mVal+tp$sdVal
tp$sign <- 0
tp[sign(lowVal) == -1 & sign(highVal) == -1]$sign <- -1
tp[sign(lowVal) == 1 & sign(highVal) == 1]$sign <- 1
tp.Temp <- tp

ggplot(tp[n > 10],aes(x=v.Temp,y=mVal,col=factor(sign))) +   
  geom_errorbar(aes(ymin = lowVal, ymax = highVal)) +
  geom_point() + geom_hline(yintercept = 0) + theme_Publication() +
  scale_colour_manual(values = cols) +
  facet_wrap(~val)+ 
  labs(y='SHAP Value',x='Temp',title='') + guides(color=F)


#
#
#

table(m.test.dt[CNV_CVW == 0]$diff)
table(m.test.dt[CNV_CVW != 0]$diff)
table(m.test.dt$obs)

table(m.test.dt[CNV_CVW == 0]$diff)/sum(table(m.test.dt[CNV_CVW == 0]$diff))
table(m.test.dt[CNV_CVW != 0]$diff)/sum(table(m.test.dt[CNV_CVW != 0]$diff))


