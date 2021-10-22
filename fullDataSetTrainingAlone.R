rm(list=ls())
library(ncdf4)
library(data.table)
library(caret)
library(useful)
library(shapr)
library(xgboost)

model.filenames <- list.files(path = '', full.names=T, pattern='.nc4')

fracData <- 50
fidx <- 1:length(model.filenames)
#fidx <- sample(length(model.filenames),floor(length(model.filenames)/fracData))

obs.filenames <- list.files(path = '', full.names=T, pattern='.nc4')

qqq <- 1
for(f in fidx){
  print(f)
  inFile <- model.filenames[f]
  file.nc <- nc_open(inFile)
  CNV_CVW_MAX <- ncvar_get(file.nc,'CNV_CVW_MAX')
  U <- ncvar_get(file.nc,'U')
  V <- ncvar_get(file.nc,'V')
  Temp <- ncvar_get(file.nc,'T')
  QITOT <- ncvar_get(file.nc,'QITOT')
  QICN <- ncvar_get(file.nc,'QICN')
  QI <- ncvar_get(file.nc,'QI')
  Q <- ncvar_get(file.nc,'Q')
  OMEGA <- ncvar_get(file.nc,'OMEGA')
  IWC <- ncvar_get(file.nc,'IWC')
  CNV_CVW <- ncvar_get(file.nc,'CNV_CVW')
  BYNCY <- ncvar_get(file.nc,'BYNCY')
  lev <- ncvar_get(file.nc,'lev')
  ZLFC <- ncvar_get(file.nc,'ZLFC')
  ZLCL <- ncvar_get(file.nc,'ZLCL')
  ZCBL <- ncvar_get(file.nc,'ZCBL')
  SDMZ <- ncvar_get(file.nc,'SDMZ')
  Q600 <- ncvar_get(file.nc,'Q600')
  LWI <- ncvar_get(file.nc,'LWI')
  LFR <- ncvar_get(file.nc,'LFR') #km-2 s-1
  LFR_GCC <- ncvar_get(file.nc,'LFR_GCC') #km-2 s-1
  IWP <- ncvar_get(file.nc,'IWP')
  INHB <- ncvar_get(file.nc,'INHB')
  CN_PRCP <- ncvar_get(file.nc,'CN_PRCP')
  CNV_TOPP <- ncvar_get(file.nc,'CNV_TOPP')
  CNV_BASEP <- ncvar_get(file.nc,'CNV_BASEP')
  CLDTT <- ncvar_get(file.nc,'CLDTT')
  CAPE <- ncvar_get(file.nc,'CAPE')
  lat <- matrix(rep(ncvar_get(file.nc,'lat'),each=360),nrow = 360,ncol = 181)
  lon <- matrix(rep(ncvar_get(file.nc,'lon'),length.out=360*181),nrow = 360,ncol = 181)
  nc_close(file.nc)
  
  inFile <- obs.filenames[f]
  file.nc <- nc_open(inFile)
  GLM_LFR <- ncvar_get(file.nc,'GLM_LFR')
  GLM_ENERGY <- ncvar_get(file.nc,'GLM_ENERGY')
  nc_close(file.nc)
  
  if(qqq==1){
    m.dt <- data.table(CNV_CVW_MAX=c(CNV_CVW_MAX),
                       U=c(U),
                       V=c(V),
                       Temp=c(Temp),
                       QITOT=c(QITOT),
                       QICN=c(QICN),
                       QI=c(QI),
                       Q=c(Q),
                       OMEGA=c(OMEGA),
                       IWC=c(IWC),
                       CNV_CVW=c(CNV_CVW),
                       BYNCY=c(BYNCY),
                       ZLFC=c(ZLFC),
                       ZLCL=c(ZLCL),
                       ZCBL=c(ZCBL),
                       SDMZ=c(SDMZ),
                       Q600=c(Q600),
                       LWI=c(LWI),
                       IWP=c(IWP),
                       INHB=c(INHB),
                       CN_PRCP=c(CN_PRCP),
                       CNV_TOPP=c(CNV_TOPP),
                       CNV_BASEP=c(CNV_BASEP), 
                       CLDTT=c(CLDTT),
                       CAPE=c(CAPE),
                       obs=c(GLM_LFR),
                       lat=c(lat),
                       lon=c(lon),
		       month = as.numeric(substr(model.filenames[f],68,69)),
                       LFR_GCC=c(LFR_GCC))
    
    m.dt <- m.dt[lat > -60 & lat < 60 & lon > -150 & lon < -30 & LWI <= 1 ]
    m.dt[LFR_GCC > 0]$LFR_GCC <- 1
    m.dt[obs > 0]$obs <- 1
    
    m.dt <- m.dt[complete.cases(m.dt)]
    
    
  } else{
    temp.dt <- data.table(CNV_CVW_MAX=c(CNV_CVW_MAX),
                          U=c(U),
                          V=c(V),
                          Temp=c(Temp),
                          QITOT=c(QITOT),
                          QICN=c(QICN),
                          QI=c(QI),
                          Q=c(Q),
                          OMEGA=c(OMEGA),
                          IWC=c(IWC),
                          CNV_CVW=c(CNV_CVW),
                          BYNCY=c(BYNCY),
                          ZLFC=c(ZLFC),
                          ZLCL=c(ZLCL),
                          ZCBL=c(ZCBL),
                          SDMZ=c(SDMZ),
                          Q600=c(Q600),
                          LWI=c(LWI),
                          IWP=c(IWP),
                          INHB=c(INHB),
                          CN_PRCP=c(CN_PRCP),
                          CNV_TOPP=c(CNV_TOPP),
                          CNV_BASEP=c(CNV_BASEP), 
                          CLDTT=c(CLDTT),
                          CAPE=c(CAPE),
                          obs=c(GLM_LFR),
                          lat=c(lat),
                          lon=c(lon),
                          month = as.numeric(substr(model.filenames[f],68,69)),
                          LFR_GCC=c(LFR_GCC))
    
    temp.dt <- temp.dt[lat > -45 & lat < 45 & lon > -120 & lon < -30 & LWI <= 1 ]
    temp.dt[LFR_GCC > 0]$LFR_GCC <- 1
    temp.dt[obs > 0]$obs <- 1
    
    temp.dt <- temp.dt[complete.cases(temp.dt)]
    
    m.dt <- rbind(m.dt,temp.dt)
  }
  qqq <- 2
}


###
# Preprocessing
###
set.seed(072590)

m.learn.dt <- m.dt

m.learn.dt <- m.learn.dt[complete.cases(m.learn.dt)]
m.learn.dt$obs <- m.learn.dt$obs

no.dt <- m.learn.dt[obs==0]
yes.dt <- m.learn.dt[obs==1]


m.learn.dt <- rbind(yes.dt,
                    no.dt[sample(nrow(no.dt),size = nrow(yes.dt))])
m.learn.dt$diff <- m.learn.dt$obs - m.learn.dt$LFR_GCC + 1

trainIdx <- sort(sample(nrow(m.learn.dt), nrow(m.learn.dt)*.7))

m.train.dt <- m.learn.dt[month %in% c(1,3,4,6,7,9,10,12)]
m.test.dt <- m.learn.dt[month %in% c(2,5,8,11)]

valIdx <- sort(sample(nrow(m.train.dt), nrow(m.train.dt)*.1))
m.val.dt <- m.train.dt[valIdx,]
m.train.dt <- m.train.dt[-valIdx,]



formulaPred <- diff ~ U + V + Temp + QITOT + QICN + QI + Q + OMEGA + IWC + 
  CNV_CVW + BYNCY + ZLFC + ZLCL + ZCBL + SDMZ + Q600 + LWI + IWP + INHB + 
  CN_PRCP + CNV_TOPP + CNV_BASEP + CLDTT + CAPE + lat + lon - 1

#formulaPred <- diff ~ CAPE + CNV_CVW + LWI + ZLCL + Q600 + CN_PRCP  - 1

trainX <- build.x(formulaPred,m.train.dt,contrasts = F)
trainY <- build.y(formulaPred,m.train.dt)

normParam.X <- preProcess(trainX)
trainX <- predict(normParam.X,trainX)

testX <- predict(normParam.X,build.x(formulaPred,m.test.dt,contrasts = F))
testY <- build.y(formulaPred,m.test.dt)


valX <- predict(normParam.X,build.x(formulaPred,m.val.dt,contrasts = F))
valY <- build.y(formulaPred,m.val.dt)

###
# XGBoost Regression
###
dtrain <- xgb.DMatrix(data = trainX, label=trainY)
dtest <- xgb.DMatrix(data = valX, label=valY)
watchlist <- list(train=dtrain, test=dtest)


param <- list(objective = "multi:softmax",
              num_class = 3,
              max_depth = 14, eta = 0.4)
bst <- xgb.train(params=param,
                 data=dtrain, nrounds=1000,
                 watchlist=watchlist,early_stopping_rounds=25)


savDir <- '/pic/projects/deepclimate/christophOutput/lightning/Routput/'
savFN <- paste(savDir,'allData1Hot_modelOnly.Rdata',sep='')

save(list=c('param','bst','m.test.dt','testX','testY'),file=savFN)


print('yay!')

