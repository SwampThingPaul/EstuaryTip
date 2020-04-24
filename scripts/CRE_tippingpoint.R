## 
## LOSOM
## Caloosahatchee River Estuary Tipping Analysis
##
## Starting analysis by focusing on CRE
##
## Code was compiled by Paul Julian
## contact info: paul.julian@floridadep.gov
## contact info: pauljulianphd@gmail.com

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

# Standard Libraries
library(AnalystHelper);#devtools::install_github("SwampThingPaul/AnalystHelper")
library(plyr)
library(reshape)

# GIS Libraries
library(tmap)
library(rgdal)
library(rgeos)
library(raster)

# Analysis Libraries
library(segmented)
library(gvlma)

#Paths
wd="D:/_GitHub/EstuaryTip"

paths=paste0(wd,c("/Plots/","/Export/","/Data/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

# helper variables/definitions

utm17=CRS("+proj=utm +zone=17 +datum=WGS84 +units=m")
wgs84=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

N.mw=14.0067
P.mw=30.973762
C.mw=12.0107

# GIS Data ----------------------------------------------------------------
gen.GIS="D:/_GISData"

wmd.mon=spTransform(readOGR(paste0(gen.GIS,"/SFWMD_Monitoring_20190909"),"Environmental_Monitoring_Stations"),utm17)
wq.wmd.mon=subset(wmd.mon,ACTIVITY_T=="Chemistry"&ACTIVITY_S=="Surface Water Grab")
#plot(subset(wq.wmd.mon,STATION%in%c("S79",paste0("CES0",2:11))))

basins.all=spTransform(readOGR(paste0(gen.GIS,"/AHED_release/AHED_20171102.gdb"),"WATERSHED"),utm17)
#tmap_mode("view")
#tm_shape(basins.all)+tm_polygons()
#plot(subset(basins.all,NAME%in%c("TIDAL SOUTH","TIDAL NORTH","TELEGRAPH SWAMP","CALOOSAHATCHEE ESTUARY","EAST CALOOSAHATCHEE","WEST CALOOSAHATCHEE")))

roads.all=spTransform(readOGR(paste0(gen.GIS,"/FDOT"),"FDOT_Roads"),utm17)
#roads=spTransform(readOGR(paste0(gen.GIS,"/FDOT"),"FDOT_Roads_SFWMDClip"),utm17)
ogrListLayers(paste0(gen.GIS,"/SFER_GIS_Geodatabase.gdb"))
canals=spTransform(readOGR(paste0(gen.GIS,"/SFER_GIS_Geodatabase.gdb"),"SFWMD_Canals"),utm17)
shoreline=spTransform(readOGR(paste0(gen.GIS,"/FWC"),"FWC_Shoreline"),utm17)
lakes=spTransform(readOGR(paste0(gen.GIS,"/NHD"),"NHD100_Waterbody"),utm17)
# lakes2=subset(lakes,FTYPE%in%c("390","436"))
# lakes2=gSimplify(lakes2,100)
wetland=subset(lakes,FTYPE%in%c("466"))
# wetland=gSimplify(wetland,100)
shoreline2=gSimplify(shoreline,500)

wq.wmd.mon=subset(wmd.mon,ACTIVITY_T=="Chemistry"&ACTIVITY_S=="Surface Water Grab")
subset(wq.wmd.mon,substring(SITE,1,3)%in%c("CES","CAL"))@data

wq.sites=data.frame(SITE=c("S79","S77",paste0("CES0",c(2:9))),
                    REGION=c(rep("CRE",10)),
                    ALIAS=c("S79","S77",paste0("CES0",c(2:9))))

CRE.basins=subset(basins.all,NAME%in%c("TIDAL SOUTH","TIDAL NORTH","TELEGRAPH SWAMP","CALOOSAHATCHEE ESTUARY","EAST CALOOSAHATCHEE","WEST CALOOSAHATCHEE"))
#tiff(filename=paste0(plot.path,"CRE_map.tiff"),width=6.5,height=3.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"png/CRE_map.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5));
layout(matrix(c(1,1,2,3),2,2,byrow=F),widths=c(1,0.5))

bbox.lims=bbox(gBuffer(subset(wq.wmd.mon,STATION%in%c("S79",paste0("CES0",2:11))),width=500))
bbox.poly=as(raster::extent(bbox.lims),"SpatialPolygons")#makes the polygon
proj4string(bbox.poly)=utm17#projects the polygon

plot(shoreline,col="cornsilk",border="grey50",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(lakes,add=T,border=NA,col=adjustcolor("skyblue",0.5))
plot(wetland,add=T,border=NA,col=adjustcolor("palegreen3",0.5))
plot(canals,add=T,col="dodgerblue1",lwd=2)
plot(canals,add=T,col="lightblue",lwd=1)
plot(roads.all,add=T,col="grey",lwd=0.75,lty=1)
plot(basins.all,add=T,border="grey50",lwd=1.25)
points(subset(wq.wmd.mon,STATION%in%c("S79",paste0("CES0",2:11))),pch=21,bg="indianred1",lwd=0.1,cex=1.5)
text(subset(wq.wmd.mon,STATION%in%paste0("CES0",8:9)),subset(wq.wmd.mon,STATION%in%paste0("CES0",8:9))@data$STATION,halo=T,pos=1,cex=0.75)
text(subset(wq.wmd.mon,STATION%in%paste0("CES0",3:7)),subset(wq.wmd.mon,STATION%in%paste0("CES0",3:7))@data$STATION,halo=T,pos=2,cex=0.75)
text(subset(wq.wmd.mon,STATION=="CES02"),subset(wq.wmd.mon,STATION=="CES02")@data$STATION,halo=T,pos=3,cex=0.75)
text(subset(wq.wmd.mon,STATION=="S79"),subset(wq.wmd.mon,STATION=="S79")@data$STATION,halo=T,pos=1,cex=0.75)
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4);
box(lwd=1)

par(xpd=F)
bbox.lims=bbox(basins.all)
plot(shoreline2,col="cornsilk",border="grey50",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(basins.all,border="grey",add=T,lwd=0.8)
plot(bbox.poly,add=T,lty=1,border="red",col="indianred1")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
leg.text=c("SFWMD Basins","SFWMD Canals","Major Roads","Monitoring Locations")
legend(0.5,0.5,legend=leg.text,
       pch=c(22,NA,NA,21),
       lty=c(NA,1,2,NA),
       lwd=c(1,2,2,0.1),
       col=c("grey50","dodgerblue1","grey","black"),
       pt.bg=c(NA,NA,NA,"indianred1"),
       pt.cex=c(2,NA,NA,1.5),ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

# -------------------------------------------------------------------------
dates=as.Date(c("1998-05-01","2019-04-30"))

# CRE S79 Q ---------------------------------------------------------------
#q.dbkeys=data.frame(SITE=c("S79","S77","S80"),DBKEY=c("00865","15635","DJ238"))
q.dbkeys=data.frame(SITE=c("S79","S77"),DBKEY=c("00865","15635"))
q.dat=data.frame()
# pb <- txtProgressBar(min = 1, max =2, style = 3)
for(i in 1:nrow(q.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],q.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(q.dbkeys$DBKEY[i])
  q.dat=rbind(q.dat,tmp)
  # setTxtProgressBar(pb,i)
  # print(i)
}
q.dat=merge(q.dat,q.dbkeys,"DBKEY")
#write.csv(q.dat,paste0(data.path,"S79S80_discharge.csv"),row.names=F)

q.dat$month=format(q.dat$Date,"%m")
q.dat$CY=format(q.dat$Date,"%Y")
q.dat$monCY=with(q.dat,date_fun(paste(CY,month,"01",sep="-")))
q.dat$Date.EST=date_fun(q.dat$Date)
q.dat$WY=WY(q.dat$Date.EST)
q.dat$Data.Value=with(q.dat,ifelse(Data.Value<0,NA,Data.Value))

unique(q.dat$SITE)
q.dat.mon=ddply(q.dat,c("SITE","monCY"),summarise,TFlow.cfs=sum(ifelse(Data.Value<0,NA,Data.Value),na.rm=T))
dev.off()
plot(TFlow.cfs~monCY,subset(q.dat.mon,SITE=="S79"),type="l")
with(subset(q.dat.mon,SITE=="S77"),lines(monCY,TFlow.cfs,col="red"))
# Water quality -----------------------------------------------------------
params=data.frame(Test.Number=c(18,21,80,20,25,23,61,179,100,98,9,7,13,10,27,12),param=c("NOx","TKN","TN","NH4","TP","SRP","Chla","Chla","TOC","Sal","SPC","Temp","Color","PAR","Si","Turb"))
wq.sites

wq.dat=data.frame()
pb <- txtProgressBar(min = 1, max =nrow(wq.sites), style = 3)
for(i in 1:nrow(wq.sites)){
  wq.tmp=DBHYDRO_WQ(dates[1],dates[2],as.character(wq.sites$SITE[i]),params$Test.Number)
  wq.dat=rbind(wq.dat,wq.tmp)
  setTxtProgressBar(pb,i)
  #print(i)
}
unique(wq.dat$Station.ID)
wq.dat=merge(wq.dat,wq.sites,by.x="Station.ID",by.y="SITE")
wq.dat=merge(wq.dat,params,"Test.Number")
unique(wq.dat$Collection.Method)

wq.dat=subset(wq.dat,Collection.Method=="G")
wq.dat.xtab=cast(wq.dat,Station.ID+ALIAS+REGION+Date.EST~param,value="HalfMDL",mean)
wq.dat.xtab$DIN=with(wq.dat.xtab,NH4+NOx)
wq.dat.xtab$TN=with(wq.dat.xtab, TN_Combine(NOx,TKN,TN))
wq.dat.xtab$TON=with(wq.dat.xtab, ifelse(TKN-NH4<0|TKN==NH4,pmin(TKN,NH4),TKN-NH4))

# Reversal Evaluation
wq.dat.xtab$TPReversal=with(wq.dat.xtab,ifelse(is.na(SRP)==T|is.na(TP)==T,0,ifelse(SRP>(TP*1.3),1,0)));# Reversals identified as 1 reversals consistent with TP rule evaluation
wq.dat.xtab$TNReversal=with(wq.dat.xtab,ifelse(is.na(DIN)==T|is.na(TN)==T,0,ifelse(DIN>(TN*1.3),1,0)));

sum(wq.dat.xtab$TNReversal,na.rm=T)
subset(wq.dat.xtab,TNReversal==T)
sum(wq.dat.xtab$TPReversal,na.rm=T)
subset(wq.dat.xtab,TPReversal==T)

par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:2,1,2,byrow=F))
plot(TN~DIN,wq.dat.xtab,ylab="TN (mg L\u207B\u00b9)",xlab="DIN (mg L\u207B\u00b9)",pch=21,bg=ifelse(TNReversal==1,"dodgerblue1",NA),col=adjustcolor("grey",0.8));abline(0,1,col="dodgerblue1")
plot(TP~SRP,wq.dat.xtab,ylab="TP (mg L\u207B\u00b9)",xlab="SRP (mg L\u207B\u00b9)",pch=21,bg=ifelse(TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

dev.off()
wq.dat.xtab$TP=with(wq.dat.xtab,ifelse(TPReversal==1,NA,TP*1000))
wq.dat.xtab$TN=with(wq.dat.xtab,ifelse(TNReversal==1,NA,TN))
wq.dat.xtab$SRP=with(wq.dat.xtab,ifelse(TPReversal==1,NA,SRP*1000))
wq.dat.xtab$DIN=with(wq.dat.xtab,ifelse(TNReversal==1,NA,DIN))
wq.dat.xtab$TON=with(wq.dat.xtab,ifelse(TNReversal==1,NA,TON))
wq.dat.xtab$month=as.numeric(format(wq.dat.xtab$Date.EST,"%m"))
wq.dat.xtab$CY=format(wq.dat.xtab$Date.EST,"%Y")
wq.dat.xtab$monCY.Date=with(wq.dat.xtab,date_fun(paste(CY,month,"01",sep="-")))
wq.dat.xtab$WY=WY(wq.dat.xtab$Date.EST)
range(wq.dat.xtab$WY)

wq.dat.xtab$TP_mM=with(wq.dat.xtab,(TP/1000)/P.mw)
wq.dat.xtab$SRP_mM=with(wq.dat.xtab,(SRP/1000)/P.mw)
wq.dat.xtab$TN_mM=with(wq.dat.xtab,TN/N.mw)
wq.dat.xtab$DIN_mM=with(wq.dat.xtab,DIN/N.mw)
wq.dat.xtab$TON_mM=with(wq.dat.xtab,TON/N.mw)
wq.dat.xtab$TOC_mM=with(wq.dat.xtab,TOC/C.mw)

wq.dat.xtab$TN_TP=with(wq.dat.xtab,TN_mM/TP_mM)
wq.dat.xtab$DIN_SRP=with(wq.dat.xtab,DIN_mM/SRP_mM)
wq.dat.xtab$TOC_TON=with(wq.dat.xtab,TOC_mM/TON_mM)
wq.dat.xtab$TP_TON=with(wq.dat.xtab,TP_mM/TON_mM)


boxplot(TN_TP~ALIAS,wq.dat.xtab,outline=F)
boxplot(DIN_SRP~ALIAS,wq.dat.xtab,outline=F)
boxplot(TOC_TON~ALIAS,wq.dat.xtab,outline=F)
boxplot(TP_TON~ALIAS,wq.dat.xtab,outline=F)


# Data Explore ------------------------------------------------------------
dev.off()
boxplot(Sal~ALIAS,wq.dat.xtab)

boxplot(Chla~ALIAS,wq.dat.xtab,outline=F)
boxplot(PAR~ALIAS,wq.dat.xtab,outline=F)
boxplot(Color~ALIAS,wq.dat.xtab,outline=F)
boxplot(Turb~ALIAS,wq.dat.xtab,outline=F,log="y")
boxplot(PAR~ALIAS,wq.dat.xtab,outline=F)

boxplot(TP~ALIAS,wq.dat.xtab,outline=F)
boxplot(SRP~ALIAS,wq.dat.xtab,outline=F)

boxplot(TN~ALIAS,wq.dat.xtab,outline=F)
boxplot(TON~ALIAS,subset(wq.dat.xtab,WY%in%seq(2005,2019,1)),outline=F)
boxplot(DIN~ALIAS,wq.dat.xtab,outline=F)
boxplot(Si~ALIAS,wq.dat.xtab,outline=F)
boxplot(TOC~ALIAS,wq.dat.xtab,outline=F)


idvars=c("Station.ID", "ALIAS", "REGION", "Date.EST","WY")
paramvars=c("Sal","Chla", "Color","PAR","Turb","TP","SRP","TKN","TN","DIN", "TON","Si", "TOC")
wq.dat.xtab.melt=melt(data.frame(wq.dat.xtab[,c(idvars,paramvars)]),id.vars=idvars,variable_name = "parameter")
wq.dat.xtab.melt=subset(wq.dat.xtab.melt,is.na(value)==F)
wq.dat.xtab.melt$ALIAS=factor(wq.dat.xtab.melt$ALIAS,levels=c("S77","S79",paste0("CES0",2:9)))

## 
wq.dat.xtab.melt2=subset(wq.dat.xtab.melt,WY%in%seq(2005,2019,1))

ALIAS.param.sum=ddply(wq.dat.xtab.melt2,c("ALIAS","parameter"),summarise,N.val=N(value),min.val=min(value),max.val=max(value),q1=quantile(value,probs=0.25),q3=quantile(value,probs=0.75),IQR=q3-q1,up.ext=min(max(value),q3+(1.5*IQR)),low.ext=max(min(value),q1-(1.5*IQR)))
param.sum=ddply(ALIAS.param.sum,"parameter",summarise,min.val=min(low.ext),max.val=max(up.ext))
param.sum

paramvars=c("Sal","Chla", "Color","Turb","PAR","TP","SRP","TN","DIN", "TON")
param.lab=c("Salinity (PSU)","Chl-a (\u03BCg L\u207B\u00B9)","Color (PCU)","Turb (NTU)","PAR (m\u207B\u00B9)","TP (\u03BCg L\u207B\u00B9)","SRP (\u03BCg L\u207B\u00B9)","TN (mg L\u207B\u00B9)","DIN (mg L\u207B\u00B9)","TON (mg L\u207B\u00B9)")

x.lab=c("S77","S79",paste0("CES0",2:9))
#tiff(filename=paste0(plot.path,"CRE_WQ.tiff"),width=6.5,height=5.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"png/CRE_WQ.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,4,0.25,0.1),oma=c(3,1,1,0.75));
layout(matrix(1:10,5,2,byrow=F))

max.ylimval=c(45,45,275,32,9,300,200,2.5,0.8,2)
min.ylimval=c(0,0,0,0,6,0,0,0,0,0)
by.ylimval=c(20,20,100,10,1,100,100,1,0.2,1)
for(i in 1:10){
#ylim.val=with(subset(param.sum,parameter==paramvars[i]),c(floor(min.val),ceiling(max.val+max.val*0.1)))
ylim.val=c(min.ylimval[i],max.ylimval[i])
by.y=by.ylimval[i]#diff(ylim.val)/
ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

boxplot(value~ALIAS,subset(wq.dat.xtab.melt2,parameter==paramvars[i]),outline=F,ylim=ylim.val,axes=F,xlab=NA,ylab=NA,col=adjustcolor("dodgerblue1",0.5),boxlwd=0.5,boxwex=0.75,whisklwd=0.5,staplelwd=0.5,medlwd=1)
text(1:10,rep(ylim.val[2],10),subset(ALIAS.param.sum,parameter==paramvars[i])$N.val,cex=0.7,xpd=NA)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,1:10,1:10,NA,cex=0.9)
if(i==5){text(x=1:10,y=rep(ylim.val[1]-0.5,10),x.lab,srt=90,xpd=NA,adj=1)}
if(i==10){text(x=1:10,y=rep(ylim.val[1]-0.35,10),x.lab,srt=90,xpd=NA,adj=1)}
box(lwd=1)
mtext(side=2,line=2.5,param.lab[i],cex=0.8)
if(i==1){mtext(side=3,adj=0,"WY2005 - WY2019",cex=0.75)}
}
dev.off()


# Flow, Loads and FWM -----------------------------------------------------
library(Hmisc)
head(q.dat,3L)
head(wq.dat.xtab,3L)
vars=c("ALIAS","Date.EST","TP","TN","Color","Chla")
vars.q=c("SITE","Date.EST","WY","Data.Value")
q.wq.dat=merge(q.dat[,vars.q],subset(wq.dat.xtab,ALIAS%in%c("S79","S77"))[,vars],by.x=c("SITE","Date.EST"),by.y=c("ALIAS","Date.EST"),all.x=T)

WY.Q.FWM=ddply(q.wq.dat,c("SITE","WY"),summarise,
               Q.km3yr=sum(cfs.to.km3d(Data.Value),na.rm=T),
               TP.FWM=wtd.mean(TP,Data.Value),TP.FWSD=sqrt(wtd.var(TP,Data.Value)),
               TN.FWM=wtd.mean(TN,Data.Value),TN.FWSD=sqrt(wtd.var(TN,Data.Value)),
               Color.FWM=wtd.mean(Color,Data.Value),Color.FWSD=sqrt(wtd.var(Color,Data.Value)),
               Chla.FWM=wtd.mean(Chla,Data.Value),Chla.FWSD=sqrt(wtd.var(Chla,Data.Value)))

#tiff(filename=paste0(plot.path,"CRE_FWMs.tiff"),width=5,height=6,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"png/CRE_FWMs.png"),width=5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.25,0.1),oma=c(3,4,1,0.75));
layout(matrix(1:10,5,2))
sites.val=c("S77","S79")
xlim.val=c(1999,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
for(i in 1:2){
  ylim.val=c(0,5);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(Q.km3yr~WY,subset(WY.Q.FWM,SITE==sites.val[i]),axes=F,ylim=ylim.val,xlim=xlim.val,ylab=NA,xlab=NA,type="n",yaxs="i")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(WY.Q.FWM,SITE==sites.val[i]),shaded.range(WY,rep(0,N(WY)),Q.km3yr,"dodgerblue1",lty=1))
  with(subset(WY.Q.FWM,SITE==sites.val[i]),points(WY,Q.km3yr,pch=21,bg="dodgerblue1",lwd=0.01))
  axis_fun(1,xmaj,xmin,NA);box(lwd=1)
  if(i==1){axis_fun(2,ymaj,ymin,ymaj);
  mtext(side=2,line=2.5,"Discharge\n(km\u00B3 Yr\u207B\u00B9)",cex=0.75)
  mtext(side=3,sites.val[i])}else{
    axis_fun(2,ymaj,ymin,NA)
    mtext(side=3,sites.val[i])}

  ylim.val=c(0,450);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(TP.FWM~WY,subset(WY.Q.FWM,SITE==sites.val[i]),axes=F,ylim=ylim.val,xlim=xlim.val,ylab=NA,xlab=NA,type="n",yaxs="i")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(WY.Q.FWM,SITE==sites.val[i]),pt_line_error(WY,TP.FWM,TP.FWSD,2,"grey50",1,21,"indianred1",length=0.01,cex=1.25))
  axis_fun(1,xmaj,xmin,NA);box(lwd=1)
  if(i==1){axis_fun(2,ymaj,ymin,ymaj);
  mtext(side=2,line=2.5,"TP FWM\n(\u03BCg L\u207B\u00B9)",cex=0.75)}else{
    axis_fun(2,ymaj,ymin,NA);
  }
  
  ylim.val=c(0,4);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(TN.FWM~WY,subset(WY.Q.FWM,SITE==sites.val[i]),axes=F,ylim=ylim.val,xlim=xlim.val,ylab=NA,xlab=NA,type="n",yaxs="i")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(WY.Q.FWM,SITE==sites.val[i]),pt_line_error(WY,TN.FWM,TN.FWSD,2,"grey50",1,21,"indianred1",length=0.01,cex=1.25))
  axis_fun(1,xmaj,xmin,NA);box(lwd=1)
  if(i==1){axis_fun(2,ymaj,ymin,ymaj);
  mtext(side=2,line=2.5,"TN FWM\n(mg L\u207B\u00B9)",cex=0.75)}else{
    axis_fun(2,ymaj,ymin,NA);
  }
  
  ylim.val=c(0,300);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(Color.FWM~WY,subset(WY.Q.FWM,SITE==sites.val[i]),axes=F,ylim=ylim.val,xlim=xlim.val,ylab=NA,xlab=NA,type="n",yaxs="i")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(WY.Q.FWM,SITE==sites.val[i]),pt_line_error(WY,Color.FWM,Color.FWSD,2,"grey50",1,21,"indianred1",length=0.01,cex=1.25))
  axis_fun(1,xmaj,xmin,NA);box(lwd=1)
  if(i==1){axis_fun(2,ymaj,ymin,ymaj);
  mtext(side=2,line=2.5,"Color FWM\n(PCU)",cex=0.75)}else{
    axis_fun(2,ymaj,ymin,NA);
  }
  
  ylim.val=c(0,50);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(Chla.FWM~WY,subset(WY.Q.FWM,SITE==sites.val[i]),axes=F,ylim=ylim.val,xlim=xlim.val,ylab=NA,xlab=NA,type="n",yaxs="i")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(WY.Q.FWM,SITE==sites.val[i]),pt_line_error(WY,Chla.FWM,Chla.FWSD,2,"grey50",1,21,"indianred1",length=0.01,cex=1.25))
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5);box(lwd=1)
  if(i==1){axis_fun(2,ymaj,ymin,ymaj);
  mtext(side=2,line=2.5,"Chl-a FWM\n(\u03BCg L\u207B\u00B9)",cex=0.75)}else{
    axis_fun(2,ymaj,ymin,NA);
  }
  mtext(side=1,line=1.75,"Water Year")
}
dev.off()

# q.wq.dat$TP.inter=with(q.wq.dat,ave(TP,SITE,FUN=function(x)dat.interp(x)))
# q.wq.dat$TN.inter=with(q.wq.dat,ave(TN,SITE,FUN=function(x)dat.interp(x)))
# q.wq.dat$TP.load=with(q.wq.dat,Load.Calc.kg(Data.Value,TP.inter))
# q.wq.dat$TN.load=with(q.wq.dat,Load.Calc.kg(Data.Value,TN.inter))
# WY.Q.Load=ddply(q.wq.dat,c("SITE","WY"),summarise,TFlow.acft=sum(cfs.to.acftd(Data.Value),na.rm=T),TPLoad.kg=sum(TP.load,na.rm=T),TNLoad.kg=sum(TN.load,na.rm=T))
# WY.Q.Load$TP.FWM.ugL=with(WY.Q.Load,(TPLoad.kg*1e6)/((TFlow.acft*1233.48)*1000))
# WY.Q.Load$TN.FWM.mgL=with(WY.Q.Load,(TNLoad.kg*1e6)/((TFlow.acft*1233.48)*1000))

# WY.Q=cast(q.wq.dat,WY~SITE,value="Data.Value",fun.aggregate = function(x)sum(cfs.to.acftd(x),na.rm=T))
# colnames(WY.Q)=c("WY","S77.Q","S79.Q")
# WY.Q$C43.Q=with(WY.Q,S79.Q-S77.Q)
# WY.TPLoad=cast(q.wq.dat,WY~SITE,value="TP.load",fun.aggregate = function(x)sum(x,na.rm=T))
# colnames(WY.TPLoad)=c("WY","S77.TP","S79.TP")
# WY.TPLoad$C43.TP=with(WY.TPLoad,S79.TP-S77.TP)
# WY.TNLoad=cast(q.wq.dat,WY~SITE,value="TN.load",fun.aggregate = function(x)sum(x,na.rm=T))
# colnames(WY.TNLoad)=c("WY","S77.TN","S79.TN")
# WY.TNLoad$C43.TN=with(WY.TNLoad,S79.TN-S77.TN)
# 
# WY.Q.Load=merge(merge(WY.Q,WY.TPLoad,"WY"),WY.TNLoad,"WY")
# WY.Q.Load.melt=melt(WY.Q.Load,id.vars = "WY")
# spl=strsplit(as.character(WY.Q.Load.melt$variable),'[.]')
# WY.Q.Load.melt=cbind(WY.Q.Load.melt,data.frame(SITE=sapply(spl,"[",1),Param=sapply(spl,"[",2)))
# WY.Q.Load.xtab=cast(WY.Q.Load.melt,WY+SITE~Param,value="value",mean)

# Chlorophyll analysis ----------------------------------------------------
head(wq.dat.xtab)
wq.dat.xtab$hydro=FL.Hydroseason(wq.dat.xtab$Date.EST)
names(wq.dat.xtab)
tsamp.chla=cast(wq.dat.xtab,ALIAS+WY~hydro,value="Chla",fun.aggregate = function(x)N(x,na.rm=T))
tsamp.chla$total=rowSums(tsamp.chla[,c("A_Wet","B_Dry")])
tsamp.chla$screen=with(tsamp.chla,ifelse(total>4&A_Wet>=1&B_Dry>=1,1,0))
subset(tsamp.chla,screen==0)

chla.freq=ddply(wq.dat.xtab,c("ALIAS","WY"),summarise,GMChla=exp(mean(log(Chla),na.rm=T)),NSamp=N(Chla),Chla.f5=sum(Chla>5,na.rm=T),Chla.f10=sum(Chla>10,na.rm=T),Chla.f20=sum(Chla>10,na.rm=T),Chla.f40=sum(Chla>10,na.rm=T))
chla.freq=merge(chla.freq,tsamp.chla[,c("ALIAS","WY","screen")],c("ALIAS","WY"),all.x=T)
mean.chla=ddply(subset(wq.dat.xtab,month%in%5:8),c("ALIAS","WY"),summarise,Su.GMChla=exp(mean(log(Chla),na.rm=T)))
chla.freq=merge(chla.freq,mean.chla,c("ALIAS","WY"),all.x=T)
chla.freq[chla.freq$screen==0,3:7]=NA
chla.freq[,c("Chla.per5","Chla.per10","Chla.per20","Chla.per40")]=chla.freq[,5:8]/chla.freq$NSamp
head(chla.freq)
subset(chla.freq,ALIAS=="CES02")
# chla.freq2=aggregate(Chla~WY+ALIAS,wq.dat.xtab,FUN=function(x)table(cut(x,c(-Inf,5,10,20,40,Inf))))
# chla.freq2=data.frame(cbind(chla.freq2[,1:2],unlist(chla.freq2$Chla)))
# colnames(chla.freq2)=c("WY","ALIAS","Freq.0.5","Freq.5.10","Freq.10.20","Freq.20.40","Freq.GT40")
# chla.freq2=subset(chla.freq2,WY>2004)
# fill.date=seq(2005,2019,1)
# sites.val=c("S77","S78",paste0("CES0",2:9))
# fill.sites=sort(rep(sites.val,length(fill.date)))
# fill.dat=data.frame(WY=rep(fill.date,length(sites.val)),ALIAS=fill.sites,fill=1)
# chla.freq2=merge(chla.freq2,fill.dat,all.y=T,c("ALIAS","WY"))
# chla.freq2=merge(chla.freq2,ddply(wq.dat.xtab,c("ALIAS","WY"),summarise,Nsamp=N(Chla,na.rm=T)),c("ALIAS","WY"))
# chla.freq2[,c("Chla.per5","Chla.per10","Chla.per20","Chla.per40","Chla.per40_")]=chla.freq2[,3:7]/chla.freq2$Nsamp


chla.freq=subset(chla.freq,WY%in%seq(2005,2019,1))
fill.date=seq(2005,2019,1)
sites.val=paste0("CES0",2:5)
fill.sites=sort(rep(sites.val,length(fill.date)))
fill.dat=data.frame(WY=rep(fill.date,length(sites.val)),ALIAS=fill.sites,fill=1)
chla.freq=merge(chla.freq,fill.dat,all.y=T,c("ALIAS","WY"))
chla.freq$screen=with(chla.freq,ifelse(is.na(screen),0,screen))

est.sites.val=paste0("CES0",2:5)
xlim.val=c(2005,2019);by.x=3;xlab=seq(xlim.val[1],xlim.val[2],by.x)
#tiff(filename=paste0(plot.path,"CRE_EstuaryChla10.tiff"),width=4,height=5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"png/CRE_EstuaryChla10.png"),width=4,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.75,2.75,1,3.75),oma=c(2,2,0.75,0.5));
layout(matrix(1:5,5,1,byrow=F),heights=c(rep(1,4),0.5))

for(i in 1:4){
  tmp.dat=subset(chla.freq,ALIAS==est.sites.val[i])
  ylim.val=c(0,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  x=barplot(tmp.dat$Chla.per10,ylim=ylim.val,axes=F,space=0,col="grey80")
  points(x,rep(ylim.val[1]+by.y/7,N(x)),pch=ifelse(tmp.dat$screen==0,8,NA),cex=0.8)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==4){axis_fun(1,line=-0.5,x[seq(1,length(x),by.x)],x,xlab)}else{axis_fun(1,line=-0.5,x[seq(1,length(x),by.x)],x,NA)}
  if(i==3){text(x[1]-3.25,ylim.val[2]+0.1,"Proportion of Chloropyll Values > 10 \u03BCg L\u207B\u00b9",xpd=NA,srt=90,cex=1.4)}
  
  ylim.val=c(0,50);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  par(new=T);x=barplot(tmp.dat$GMChla,ylim = ylim.val,axes=F,space=0,border=NA,col=NA)
  with(tmp.dat,pt_line(x,Su.GMChla,1,"olivedrab4",2,21,"olivedrab4",pt.lwd=0.01,cex=1.5))
  with(tmp.dat,pt_line(x,GMChla,1,"black",1,23,"darkseagreen3",pt.lwd=0.01,cex=1.5))
  axis_fun(4,ymaj,ymin,format(ymaj));box(lwd=1)
  mtext(side=3,line=-1.25,est.sites.val[i],cex=0.8)
  if(i==3){text(x[length(x)]+3,ylim.val[2]+5,srt=90,"Geomean Chlorophyll-a Concentration (\u03BCg L\u207B\u00b9)",cex=1.4,xpd=NA)}
}
mtext(side=1,line=2,"Water Year")
plot(0:1,0:1,axes=F,ylab=NA,xlab=NA,type="n")
legend.text=c("Proportion of Samples","May-Sept Geomean Concentration", "Annual Geomean Concentration","Insufficient Data")
pt.col=c("grey","olivedrab4","darkseagreen3",NA)
legend(0.5,0.5,legend=legend.text,pch=c(NA,NA,NA,NA),pt.bg=pt.col,col=c("black","olivedrab4","black"),
       lty=c(0,1,1,0),lwd=1.5,pt.cex=1.5,ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,text.col="white")
legend(0.5,0.5,legend=legend.text,pch=c(22,21,23,8),pt.bg=pt.col,col="black",lty=0,lwd=0.5,pt.cex=1.5,ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)

dev.off()

#tiff(filename=paste0(plot.path,"CRE_EstuaryChlaecdf.tiff"),width=9,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"png/CRE_EstuaryChlaecdf.png"),width=9,height=4,units="in",res=200,type="windows",bg="white")

par(family="serif",mar=c(0.75,0.5,0.25,0.5),oma=c(3,3.25,1,0.75));
layout(matrix(1:60,4,15,byrow=T))
xlim.val=c(0.5,200);xmaj=c(1,100);xmin=c(1,10,100)#xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")#by.x=0.2;xmaj=c(0,seq(xlim.val[1],xlim.val[2],by.x));xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
tsamp.chla.scn=subset(tsamp.chla,screen==1)

for(j in 1:length(est.sites.val)){
for(i in 1:length(fill.date)){
  scn.val=subset(tsamp.chla.scn,ALIAS==est.sites.val[j]&WY==fill.date[i])
  tmp.dat=subset(wq.dat.xtab,ALIAS==est.sites.val[j]&WY==fill.date[i])
  
  if(nrow(scn.val)==0){
    plot(1:2,1:2,ylim=ylim.val,xlim=xlim.val,axes=F,ylab=NA,xlab=NA,type="n",log="x")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    if(j==4){axis_fun(1,xmaj,xmin,xmaj,line=-0.9,maj.tcl=-0.3,cex=0.8)}else{axis_fun(1,xmaj,xmin,NA,maj.tcl=-0.3)}
    if(i==1){axis_fun(2,ymaj,ymin,format(ymaj))}else{axis_fun(2,ymaj,ymin,NA)}
    if(j==1){mtext(side=3,fill.date[i],cex=0.8)}
    box(lwd=1)
  }else{
    tmp.dat.ecdf=ecdf_fun(tmp.dat$Chla)
  
    plot(proportion~value, tmp.dat.ecdf,ylim=ylim.val,xlim=xlim.val,axes=F,ylab=NA,xlab=NA,type="n",log="x")
    abline(h=ymaj,v=xmaj,lty=3,col="grey")
    with(tmp.dat.ecdf,shaded.range(value,lwr.CI,upr.CI,"grey30",lty=0))
    with(tmp.dat.ecdf,lines(value,proportion,col="indianred1",lwd=1.5))
    #with(tmp.dat.ecdf,lines(value,lwr.CI,col="indianred1",lwd=1,lty=2))
    #with(tmp.dat.ecdf,lines(value,upr.CI,col="indianred1",lwd=1,lty=2))
    with(subset(tmp.dat.ecdf,value>10),segments(min(value),-1,min(value),min(proportion),col="black",lty=2))
    with(subset(tmp.dat.ecdf,value>10),segments(0.1,min(proportion),min(value),min(proportion),col="black",lty=2))
    #with(subset(tmp.dat.ecdf,value>20),segments(20,-1,20,min(proportion),col="black"))
    #with(subset(tmp.dat.ecdf,value>20),segments(0.1,min(proportion),20,min(proportion),col="black"))
    if(j==4){axis_fun(1,xmaj,xmin,xmaj,line=-0.9,maj.tcl=-0.3,cex=0.8)}else{axis_fun(1,xmaj,xmin,NA,line=-1,maj.tcl=-0.3)}
    if(i==1){axis_fun(2,ymaj,ymin,format(ymaj))}else{axis_fun(2,ymaj,ymin,NA)}
    if(j==1){mtext(side=3,fill.date[i],cex=0.8)}
    if(i==15){mtext(side=4,cex=0.8,est.sites.val[j]) }
    box(lwd=1)
  }
}
  
}
mtext(side=2,"Proportion",outer=T,line=1.75)
mtext(side=1,"Chlorophyll-a Concentration (\u03BCg L\u207B\u00b9)",outer=T,line=1)
dev.off()

# Segemented Regression ---------------------------------------------------
head(chla.freq)
head(WY.Q.FWM)

chla.FWM=merge(chla.freq,subset(WY.Q.FWM,SITE=="S79"),"WY",all.x=T)
head(chla.FWM)

mod.results=data.frame()
cols=wesanderson::wes_palette("Zissou1",4,"continuous")

#tiff(filename=paste0(plot.path,"CRE_EstuarySeg.tiff"),width=6.5,height=5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"png/CRE_EstuarySeg.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,1,1),oma=c(1,2,0.5,0.5));
layout(matrix(1:4,2,2,byrow=F))

xlim.val=c(0,4);by.x=1;xmaj=c(0,seq(xlim.val[1],xlim.val[2],by.x));xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Chla.per10~Q.km3yr,chla.FWM,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
for(i in 1:length(est.sites.val)){
tmp.dat=subset(chla.FWM,ALIAS==est.sites.val[i])
with(tmp.dat,points(Q.km3yr,Chla.per10,pch=21,bg=adjustcolor(cols[i],0.25),col=adjustcolor(cols[i],0.5),lwd=0.1,cex=1.25))
mod=lm(Chla.per10~Q.km3yr,tmp.dat)
#gvlma::gvlma(mod)
mod.seg=segmented(mod,seg.Z=~Q.km3yr)
xval=seq(0,max(tmp.dat$Q.km3yr),length.out=500)
mod.seg.pred=predict(mod.seg,data.frame(Q.km3yr=xval),interval="confidence")
lines(xval,mod.seg.pred[,1],col=cols[i],lwd=2)

BK.pt=if(N(mod.seg$psi[2])==0){NA}else{mod.seg$psi[2]}
BK.pt.SE=if(N(mod.seg$psi[3])==0){NA}else{mod.seg$psi[3]}
RSE=if(is.na(BK.pt)==T){NA}else{summary(mod.seg)$sigma}
R2=if(is.na(BK.pt)==T){NA}else{summary(mod.seg)$r.squared}

mod.results=rbind(mod.results,
                  data.frame(Variable="S79 Discharge",SITE=est.sites.val[i],BK.pt=BK.pt,BK.pt.SE=BK.pt.SE,RSE=RSE,R2=R2))
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=2,"S-79 Discharge (km\u00B3 Yr\u207B\u00B9)")

xlim.val=c(100,300);by.x=50;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Chla.per10~Q.km3yr,chla.FWM,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
for(i in 1:length(est.sites.val)){
  tmp.dat=subset(chla.FWM,ALIAS==est.sites.val[i])
  with(tmp.dat,points(TP.FWM,Chla.per10,pch=21,bg=adjustcolor(cols[i],0.25),col=adjustcolor(cols[i],0.5),lwd=0.1,cex=1.25))
  mod=lm(Chla.per10~TP.FWM,tmp.dat)
  #gvlma::gvlma(mod)
  mod.seg=segmented(mod,seg.Z=~TP.FWM)
  xval=seq(min(tmp.dat$TP.FWM),max(tmp.dat$TP.FWM),length.out=500)
  mod.seg.pred=predict(mod.seg,data.frame(TP.FWM=xval),interval="confidence")
  lines(xval,mod.seg.pred[,1],col=cols[i],lwd=2)
  
  #Model results
  BK.pt=if(N(mod.seg$psi[2])==0){NA}else{mod.seg$psi[2]}
  BK.pt.SE=if(N(mod.seg$psi[3])==0){NA}else{mod.seg$psi[3]}
  RSE=if(is.na(BK.pt)==T){NA}else{summary(mod.seg)$sigma}
  R2=if(is.na(BK.pt)==T){NA}else{summary(mod.seg)$r.squared}
  resvar=sum(mod.seg$residuals^2)/mod.seg$df.residual
  mss= if (attr(mod.seg$terms, "intercept")){sum((mod.seg$fitted.values - mean(mod.seg$fitted.values))^2)}else{sum(mod.seg$fitted.values^2)}
  p <- mod.seg$rank
  df.int<-if (attr(mod.seg$terms, "intercept")){1L}else{0L}
  
  # resvar=sum(mod.seg$residuals^2)/mod.seg$df.residual
  # mss= if (attr(mod.seg$terms, "intercept")){sum((mod.seg$fitted.values - mean(mod.seg$fitted.values))^2)}else{sum(mod.seg$fitted.values^2)}
  # p <- mod.seg$rank
  # df.int<-if (attr(mod.seg$terms, "intercept")){1L}else{0L}
  # 
  # Fval=(mss/(p - df.int))/resvar;#F-value
  # numdf=p - df.int;#numdf
  # denf=mod.seg$df.residual;#denf
  # pval=pf((mss/(p - df.int))/resvar,p - df.int,mod$df.residual,lower.tail = F);#pvalue
  
  mod.results=rbind(mod.results,
                    data.frame(Variable="S79 TP FWM",SITE=est.sites.val[i],
                               BK.pt=BK.pt,BK.pt.SE=BK.pt.SE,RSE=RSE,R2=R2))
                               #Fval=Fval,DF=paste(numdf,denf,sep=", "),pval=pval))
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=2,"S-79 TP FWM (\u03BCg L\u207B\u00B9)")

xlim.val=c(1.2,1.7);by.x=0.1;xmaj=c(0,seq(xlim.val[1],xlim.val[2],by.x));xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Chla.per10~Q.km3yr,chla.FWM,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
for(i in 1:length(est.sites.val)){
  tmp.dat=subset(chla.FWM,ALIAS==est.sites.val[i])
  with(tmp.dat,points(TN.FWM,Chla.per10,pch=21,bg=adjustcolor(cols[i],0.25),col=adjustcolor(cols[i],0.5),lwd=0.1,cex=1.25))
  mod=lm(Chla.per10~TN.FWM,tmp.dat)
  gvlma::gvlma(mod)
  mod.seg=segmented(mod,seg.Z=~TN.FWM)
  xval=seq(min(tmp.dat$TN.FWM),max(tmp.dat$TN.FWM),length.out=500)
  mod.seg.pred=predict(mod.seg,data.frame(TN.FWM=xval),interval="confidence")
  lines(xval,mod.seg.pred[,1],col=cols[i],lwd=2)
  
  BK.pt=if(N(mod.seg$psi[2])==0){NA}else{mod.seg$psi[2]}
  BK.pt.SE=if(N(mod.seg$psi[3])==0){NA}else{mod.seg$psi[3]}
  RSE=if(is.na(BK.pt)==T){NA}else{summary(mod.seg)$sigma}
  R2=if(is.na(BK.pt)==T){NA}else{summary(mod.seg)$r.squared}
  
  mod.results=rbind(mod.results,
                    data.frame(Variable="S79 TN FWM",SITE=est.sites.val[i],BK.pt=BK.pt,BK.pt.SE=BK.pt.SE,RSE=RSE,R2=R2))
}
axis_fun(1,xmaj,xmin,xmaj);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=2,"S-79 TN FWM (mg L\u207B\u00B9)")
mtext(side=2,line=0.5,outer=T,"Proportion of Chloropyll Values > 10 \u03BCg L\u207B\u00b9")

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
leg.text=est.sites.val
leg.x=0.5;leg.y=0.5;leg.cex=1
legend(leg.x,leg.y,legend=leg.text,
       pch=c(NA,NA,NA),
       lty=c(1),lwd=c(2),
       col=c(cols),pt.bg=c(NA,NA,NA),
       pt.cex=2,ncol=1,cex=leg.cex,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,text.col = "white")
legend(leg.x,leg.y,legend=leg.text,
       pch=c(21,21,21),
       lty=c(NA),lwd=c(0.1),
       col=adjustcolor(cols,0.5),pt.bg=adjustcolor(cols,0.25),
       pt.cex=2,ncol=1,cex=leg.cex,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()
write.csv(mod.results,paste0(export.path,"annualsegmented.csv"),row.names=F)
