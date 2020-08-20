########################################################################################################
#' @title Discharge

#' @author Bobby Hensley email: hensley@battelleecology.org

#' @description R script which calculates continuous discharge

######################################################################################################
library(neonUtilities)
library(lubridate)
library(plotly)
surfElv<-loadByProduct(dpID="DP1.20016.001", site="COMO", startdate="2018-01", enddate="2020-06", 
                   package="expanded", check.size = F)
for(i in 1:length(surfElv)) {assign(names(surfElv)[i], surfElv[[i]])}
surfElv<-data.frame(EOS_5_min)
elv102<-surfElv[(surfElv$horizontalPosition=="102"),]
dscGagings<-loadByProduct(dpID="DP1.20048.001", site="COMO", startdate="2018-01", enddate="2020-06", 
                       package="expanded", check.size = F)
for(i in 1:length(dscGagings)) {assign(names(dscGagings)[i], dscGagings[[i]])}
dscGagings<-dsc_fieldData
######################################################################################################

#################################### Regression of troll vs stage ####################################
elv102$startDateTime<-as.POSIXct(elv102$startDateTime)
dscGagingsRounded<-dscGagings
dscGagingsRounded$startDate<-as.POSIXct(lubridate::round_date(dscGagings$startDate,"5 minutes"))
pairedMeas<-merge(elv102,dscGagingsRounded,by.x="startDateTime",by.y="startDate",all.x=F,all.y=F)
pairedMeas<-pairedMeas[c("startDateTime","surfacewaterElevMean","streamStage")]
pairedMeas<-na.omit(pairedMeas)

figure<-plot_ly(data=pairedMeas, x=~surfacewaterElevMean,y=~streamStage,type="scatter",mode="markers",
                marker=list(color="red",width=1))
fit<-lm(streamStage~surfacewaterElevMean, data=pairedMeas)
cf<-round(coef(fit),digits=8)
eq<-paste0("Gage Height = ",cf[2]," Troll Elv + ",cf[1])
figure<-figure%>%add_lines(x=(~surfacewaterElevMean),y=~fitted(fit),name="trendline",type="scatter",
                           mode="lines",line=list(color="black",width=1,dash="dash"))
figure<-figure%>%layout(title = "Stage Regression", xaxis = list(title = "Troll Elev (m)"),
                        yaxis = list (title = "Gage Height (m)"),annotations=list(text=eq,x=3022.8,y=0.5,showarrow=FALSE))
figure
#' Applies regression to all troll data to get gauge heights
elv102$stage=cf[2]*elv102$surfacewaterElevMean+cf[1]
######################################################################################################

################################### Creates rating curve ##################################
figure<-plot_ly(data=dscGagings, x=~streamStage,y=~totalDischarge,type="scatter",mode="markers",
                marker=list(color="blue",width=1))
fit<-nls(totalDischarge~c*streamStage^n, data=dscGagings,start=list(c=10,n=3))
cf<-round(coef(fit),digits=8)
eq<-paste0("Q = ",cf[1]," H ^ ",cf[2])
figure<-figure%>%add_lines(x=~streamStage,y=~fitted(fit),name="trendline",type="scatter",
                           mode="lines",line=list(color="black",width=1,dash="dash"))
figure<-figure%>%layout(title = "Rating Curve", xaxis = list(title = "Stage (m)"),
                        yaxis = list (title = "Discharge (L/s)"),annotations=list(text=eq,x=0.4,y=250,showarrow=FALSE))
figure
#' Applies regression to all estimated gauge heights to get discharge
elv102$estQ=cf[1]*elv102$stage^cf[2]
######################################################################################################

################################### Creates discharge regressions ##################################
pairedDsc<-merge(elv102,dscGagingsRounded,by.x="startDateTime",by.y="startDate",all.x=F,all.y=F)
pairedDsc<-pairedDsc[c("startDateTime","estQ","totalDischarge")]
pairedDsc<-na.omit(pairedDsc)

figure<-plot_ly(data=pairedDsc, x=~estQ,y=~totalDischarge,type="scatter",mode="markers",
                marker=list(color="green",width=1))
fit<-lm(totalDischarge~estQ, data=pairedDsc)
cf<-round(coef(fit),digits=8)
eq<-paste0("Gage Height = ",cf[2]," Troll Elv + ",cf[1])
figure<-figure%>%add_lines(x=(~estQ),y=~fitted(fit),name="trendline",type="scatter",
                           mode="lines",line=list(color="black",width=1,dash="dash"))
figure<-figure%>%layout(title = "Discharge Regression", xaxis = list(title = "Est Q (L/s)"),
                        yaxis = list (title = "Measured Q (L/s)"),annotations=list(text=eq,x=10,y=10,showarrow=FALSE))
figure
figure<-plot_ly(data=elv102, x=~startDateTime,y=~estQ,name="est_Q",type="scatter",mode="markers",
                marker=list(color="blue",width=1))
figure<-figure%>%add_trace(data=dscGagings, x=~startDate,y=~totalDischarge,name="meas_Q",type="scatter",mode="markers",
                           marker=list(color="red",width=1))
figure<-figure%>%layout(title = "Estimated and Measured Discharge", xaxis = list(title = "Date"),
                        yaxis = list (title = "Discharge (L/s)",type="log"))
figure
######################################################################################################
