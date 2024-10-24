#Set DSC XS dataframe for D04-per available
#GUIL DSC survey 

GUILXS<- readr::read_csv((base::url("https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge-NCC189/refs/heads/main/shiny-openFlow/src/data/D04_GUIL_surveyPts_20200312.csv")))



#Create dataframe of only points associated with mapCode 'Transect_DSC'
GUIL_dsc_pts_xs<-subset(GUILXS, mapCode=="Transect_DSC")

write.csv(GUIL_dsc_pts_xs, "C:/Users/mviggiano/Documents/Github/NEON-stream-discharge-NCC189/shiny-openFlow/src/data/D04_GUIL_Transect_DSC.csv")

#Create dataframe for staff gauge points
GUIL_SP_pts=subset(GUILXS, GUILXS$mapCode=="Gauge")
GUIL_SP_pts<-GUIL_SP_pts[order(GUIL_SP_pts$E),]
rownames(GUIL_SP_pts<-seq(length=nrow(GUIL_SP_pts))
         
         
#manually select NorthStart and EastStat coordinates
unique(GUIL_dsc_pts_xs$name)
GUIL_dsc_pts_xs_NorthStart<-GUIL_dsc_pts_xs$N[GUIL_dsc_pts_xs$name=="DSC_LFW"]
GUIL_dsc_pts_xs_EastStart<-GUIL_dsc_pts_xs$E[GUIL_dsc_pts_xs$name=="DSC_LFW"]


#Assigns a raw Distance value to each point relative to the NorthStart and EastStart coordinates.
for(i in 1:(length(GUIL_dsc_pts_xs$name))){
  GUIL_XS1PointN<-GUIL_dsc_pts_xs$N[i]
  GUIL_XS1PointE<-GUIL_dsc_pts_xs$E[i]
  GUIL_dsc_pts_xs$DistanceRaw[i]<-sqrt(((GUIL_XS1PointN-GUIL_dsc_pts_xs_NorthStart)^2)+((GUIL_XS1PointE-GUIL_dsc_pts_xs_EastStart)^2))
}

#Manually select Reference Distance
GUIL_dsc_pts_xs_RefDistance<- GUIL_dsc_pts_xs$DistanceRaw[GUIL_dsc_pts_xs$name=="DSC_LFW"]


#Sets Horizontal adjustment value based on reference point coordinate
GUIL_xs_HorizontalAdjust<-0-GUIL_dsc_pts_xs_RefDistance

#Transform raw distance to adjusted distance based on reference distance point
for( r in 1:(length(GUIL_dsc_pts_xs$name))){
  GUIL_dsc_pts_xs$DistanceAdj[r]<-GUIL_dsc_pts_xs$DistanceRaw[r]+GUIL_xs_HorizontalAdjust
}

#Create datafame of bankfull shots mapped in XS
GUIL_BFshot<- data.frame(matrix(nrow=2, ncol=3))
names(GUIL_BFshot)=c("name","DistanceAdj", "H")
GUIL_BFshot[1,1]<-GUIL_dsc_pts_xs$name[grepl("LBF", GUIL_dsc_pts_xs$name)]
GUIL_BFshot[1,2]<-GUIL_dsc_pts_xs$DistanceAdj[grepl("LBF", GUIL_dsc_pts_xs$name)]
GUIL_BFshot[1,3]<-GUIL_dsc_pts_xs$H[grepl("LBF", GUIL_dsc_pts_xs$name)]
GUIL_BFshot[2,1]<-GUIL_dsc_pts_xs$name[grepl("RBF", GUIL_dsc_pts_xs$name)]
GUIL_BFshot[2,2]<-GUIL_dsc_pts_xs$DistanceAdj[grepl("RBF", GUIL_dsc_pts_xs$name)]
GUIL_BFshot[2,3]<-GUIL_dsc_pts_xs$H[grepl("RBF", GUIL_dsc_pts_xs$name)]

#create csv for bankfull shots
write.csv(GUIL_BFshot, "C:/Users/mviggiano/Documents/Github/NEON-stream-discharge-NCC189/shiny-openFlow/src/data/D04_GUIL_BF_shots.csv")


####plot DSCXS    #######################
#Sets plot1 settings.  
xAxisTitle1<-list(title="Distance (m)",zeroline=FALSE, range=c(-20,50))
yAxisTitle1<-list(title="H (m)",zeroline=FALSE)
font<-list(size=12,color='black')

(GUIL_XS.plot<-plot_ly(data=GUIL_dsc_pts_xs, x=~DistanceAdj, y=~H, name= 'Distance vs H', type='scatter',
                       mode='markers+lines', text=~name) %>%
    add_trace(data=GUIL_BFshot, x=~DistanceAdj, y=~H, name='Bankfull Field Calls', type='scatter', mode='markers', text=~name, marker=list(size=10,line=list(color='black',width=.5)))%>%
    layout(title =paste("GUIL_20200312"), xaxis=xAxisTitle1, yaxis=yAxisTitle1))

print(GUIL_XS.plot)

###Adjusting H for gauge height ######

#Set meter mark where the staff gauge was shot in and name of the staff gauge point:
GUIL_staffGaugeMeterMark<- as.numeric(gsub("M","",gsub(".*\\_","",GUIL_SP_pts$name)))
GUILstaffGauge1Elevation<-GUIL_SP_pts$H

#Convert discharge XS transect point elevation to gauge height (rounded 2 digits)
GUIL_dsc_pts_xs$gaugeHeight<-GUIL_dsc_pts_xs$H -(GUILstaffGauge1Elevation - GUIL_staffGaugeMeterMark)
GUIL_dsc_pts_xs$gaugeHeight<-round(GUIL_dsc_pts_xs$gaugeHeight, digits=2)

#Converts bankfull elevations to gauge height (rounded to 2 digits)
GUIL_BFshot$gaugeHeight<-GUIL_BFshot$H-(GUILstaffGauge1Elevation - GUIL_staffGaugeMeterMark)
GUIL_BFshot$gaugeHeight<-round(GUIL_BFshot$gaugeHeight, digits=2)

#assign unique id for each point shot for viewing purposes
GUIL_dsc_pts_xs$ID<-c(1:length(GUIL_dsc_pts_xs$name))
GUIL_dsc_pts_xs<-GUIL_dsc_pts_xs[order(GUIL_dsc_pts_xs$DistanceAdj),]

#Sets plot2 settings.  
xAxisTitle2<-list(title="Distance (m)",zeroline=FALSE, range=c(-70,50))
yAxisTitle2<-list(title="Gauge Height  (m)",zeroline=FALSE)
font<-list(size=12,color='black')

#Plot the cross section by distance and gauge height. 
(QXS2_G<-plot_ly(data = GUIL_dsc_pts_xs, x=~DistanceAdj, y=~gaugeHeight, name='Distance vs Gauge Height', type = 'scatter', mode='markers+lines', text=~name)%>%
    add_trace(data=GUIL_BFshot, x=~DistanceAdj, y=~gaugeHeight, name=~'Bankfull Field Calls', type='scatter', mode='markers', text=~name, marker=list(size=10,line=list(color='black', width=.5))) %>%
    layout(title=paste("GUIL_20200312"), xaxis=xAxisTitle2, yaxis=yAxisTitle2))
print(QXS2_G)

write.csv(GUIL_dsc_pts_xs, "C:/Users/mviggiano/Documents/Github/NEON-stream-discharge-NCC189/shiny-openFlow/src/data/D04_GUIL_Transect_DSC.csv")




  