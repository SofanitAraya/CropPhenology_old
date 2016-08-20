

#' Phenologic metrics from time series vegetation index data
#' 
#' @author  Sofanit Araya
#' @return  OnsetV - NDVI value at the start of continuous positive slope between successive NDVI values above the user defined percentage threshold. It represent early gorth srages (seedling)
#' @return  OnsetT - MODIS acquisition time when OnsetV is derived. 
#' @return  MaxV - the annual maximum NDVI, represents full canopy coverage during Anthesis stage
#' @return  MaxT - the time when the maximum NDVI occurs, represents anthesis/flowering stage
#' @return  OffsetV - NDVI value measured at the lowest slope below a user defined percentage threshold. 
#' @return  OffsetT - the time when the offsetV derived, represents time when crop has ripened 
#' @return  LengthGS- the length of the growing season between Onset and Offset
#' @return  BeforeMaxT - The duration between the OnsetT and MaxT
#' @return  AfterMaxT - The duration between the MaxT and OffsetT
#' @return  GreenUpSlope- The rate at which NDVI increases from the OnsetV to MaxV over the time of the difference between MaxT and OnsetT
#' @return  BrownDownSlope - The rate at which NDVI decreases from MaxV to OffsetV over the difference between OffsetT and MaxT
#' @return  TINDVI - The sum of NDVI values attained at each image date within the growing season measured by the area under the NDVI curve.
#' @return  TINDVIBeforeMax - the integral area under the curve between Onset and Maximum NDVI, indicates the pre-anthesis crop growth.
#' @return  TINDVIAfterMax - the integral area under the curve between Maximum NDVI and Offset, indicates the post-anthesis growth. 
#' @return  Asymmetry - the difference between AreaBeforeMax and AreaAfterMax. It measures which part of the growing season attain relatively higher accumulated NDVI values
#'
#' @keywords Phenology, remote sensing, satellite image, Time-series
#' @seealso MultiPointsPlot (Path, N,Id1, Id2...IdN)
#' @description This function extracts 15 phenologic metrics from Moderate Resolution Imaging Spectroradiometer (MODIS)  time series vegetaion index data, as raster and Ascii files. The function takes path of the vegetation index data and the boolean Value for BolAOI (True- if there is AOI polygon, FALSE- if the parameters are calculated for the whole region).
#' @param Path - Text value - the path where the time series images saved 
#' @param BolAOI - Logical value - if there is any area of intererst or not
#' @param Percentage - Optional Numeric Vlaue - percentage of minimum NDVI value at which the Onset and Offset is defined. The 'Percentage' paramenter is optional; if not provided, a Default value of 10 will be taken.
#' 
#' @export
#' @details PhenoMetrics function provides the user with 15 phenological based metrics which build upon those available from previous software TIMESAT(Eklundh and Jönsson, 2015)  and PhenoSat  (Rodrigues et al., 2011) and include new metrics suggested by the remote sensing-agricultural based literature.  Furthermore, we have provided the theoretical biological inferences of how these metrics can be used for crop management to reduce the criticisms of the satellite imagery approach. The output allow easy interpretation as it is available in raster image format which is easy to visualize and swimmingly integrate with other data such as precision agriculture dataset, for further processing, such as analysis of regional yield estimate with the pattern of phenologic parameters.
#'
#' @references Eklundh, L., Jönsson, P., 2015. TIMESAT: A Software Package for Time-Series Processing and Assessment of Vegetation Dynamics, in: Kuenzer, C., Dech, S., Wagner, W. (Eds.), Remote Sensing Time Series: Revealing Land Surface Dynamics. Springer International Publishing, Cham, pp. 141-158.
#' @references Rodrigues, A., Marcal, A.R.S., Cunha, M., 2011. PhenoSat : A tool for vegetation temporal analysis from satellite image data, Analysis of Multi-temporal Remote Sensing Images (Multi-Temp), 2011 6th International Workshop on the, pp. 45-48.
#'
#' 
#'@examples
#' # EXAMPLE - 1
#' 
#' PhenoMetrics(system.file("extdata/data1", package="CropPhenology"), FALSE)
#'  
#' # EXAMPLE - 2
#'  
#' PhenoMetrics(system.file("extdata/data2", package="CropPhenology"), TRUE, 15)
#' 
#' 
PhenoMetrics<- function (Path, BolAOI, Percentage){
  
  
  #  require('shapefiles')
  #  require("raster")
  #  require("maptools")
  #  require("rgdal")
  require("xlsx")
  require("rgeos")
  require("grid")
  
  setwd(Path)
  raDir=dir(path=Path, pattern = c(".img$|.tif$"))
  FileLen=length(raDir)
  q=1
  qon=1
  qoff=1
  qmax=1
  par(mfrow=c(1,1))
  par(mar=c(3.5, 2.5, 2.5, 5.5))
  s=1
  Enter=FALSE
  if (BolAOI == TRUE){
    BolAOI=dir(pattern="*.shp$")
    shp=readShapePoly(BolAOI)
  }
  
  if (BolAOI == FALSE){
    ra=raster(raDir[1])    
    Points=rasterToPoints(ra)
    shp=rasterToPolygons((ra*0), dissolve=TRUE)
  }
  
  if (missing(Percentage)) {
    print ("The default value, 10%, will be applied")
    Percentage=20
  }
  
  if (!is.numeric(Percentage)){
    stop("Percentage value for Onset and Offset should be numeric")
  }
  if (Percentage<0){
    stop("Negative Onset-Offset percentage specified")
  }
  if (Percentage==0){
    stop("Onset-Offset percentage should be greated than 0")
  }
  
  i=1
  try=0
  
  if (FileLen==0){ stop ('No image file obtained in the path mensioned - Check your file type')}
  #  if (FileLen<23){ stop ('The number of images not complete cover the season - check your image files')}
  
  while (i<(FileLen+1)) {
    ras=raster(raDir[i])
    try[i]=extract (ras,shp, cellnumbers=TRUE)
    i=i+1
  }
  
  try
  
  
  cor=xyFromCell(ras,try[[1]][,"cell"])
  com=cbind(cor,try[[1]])
  
  Onset_Value=com
  Onset_Time=com
  Offset_Value=com
  Offset_Time=com
  Max_Value=com
  Max_Time=com
  Area_Total=com
  Area_Before=com
  Area_After=com
  Asymmetry=com
  GreenUpSlope=com
  BrownDownSlope=com
  LengthGS=com
  BeforeMaxT=com
  AfterMaxT=com
  Amplitude=com
  
  
  r=length(try[[1]][,"value"])
  Hd=list ("X-Cord"," Y_Cord","T1", "T2", "T3" ,"T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22", "T23")  
  AllP=data.frame()
  while(s>0 & s<(r+1)){ #iterate through the each pixel
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Plot the time series curve of the year for the sth pixel
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    AnnualTS=as.matrix(0)
    q=1 #reset qon for the next pixel comparision
    #===================== Iterate throught the files for s-th pixels to get the curve
    
    while (q>0 & q<FileLen+1){
      GRD_CD=(try[[q]][,"value"][s])/10000
      if (is.na(GRD_CD)){
        GRD_CD=(try[[q-1]][,"value"][s])/10000
      }
      AnnualTS[q]=GRD_CD
      q=q+1
    }
    
    print (AnnualTS)
    cordinate=0
    cordinate[1]=cor[s,1]
    cordinate[2]=cor[s,2]
    AP=append(cordinate,AnnualTS)
    AllP=rbind(AllP, AP)
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #                                                  Maximum
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    qmax=1
    max=AnnualTS[qmax]
    Max_T=qmax
    
    while (qmax>0 & qmax<FileLen){
      if ((AnnualTS[qmax]) > max){
        max=AnnualTS[qmax]
        Max_T=qmax
      }
      qmax=qmax+1
    }
    Max_TF=Max_T
    Max_Value[,"value"][s]=max
    Max_Time[,"value"][s]=Max_TF    
    
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #                                                  Onset 
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    onsetT=0
    onsetV=0
    # successive slops b/n  points
    j=Max_T
    slopon=(AnnualTS[j]-AnnualTS[j-1])
    slopon=as.matrix(slopon)
    f=2

    
    while(j >2){
      slopon[f]=(AnnualTS[j-1]-AnnualTS[j-2])
      j=j-1
      f=f+1
    }
    ratio=Percentage/100
    min1=min (AnnualTS[1:Max_T])
    min2=min(AnnualTS[Max_T:(FileLen-1)])
    range1=min1+(ratio*min1) #to get 10% of the min before Max
    range2=min2+(ratio*min2)#to get 10% of the min after Max
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
    #                            Last -ve slope- onset
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
    len=length(slopon)
    last=slopon[1]
    i=1
    #i=len
    ls=0
    while((i<(len+1)) & (i>0)){
      if (last<0.001){
        #print(last)
        if (last< 0.001){
          #print(last)
          ls=i
          break
        }
        quick=i
      }
      i=i+1
      last=slopon[i]
    }
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    print (slopon)
#    print(ls)
#    print (Max_T)
#    print(max)
#    print(range1)
 #   print(range2)
    
    
    
    if (ls==0){ #if only the growing season is presented and only increasing
      k=1
      Checked= FALSE
      while (k<Max_T){
        if (AnnualTS[k]< range1){
          onsetT=k
          onsetV=AnnualTS[k]
          Checked=TRUE
        }
        k=k+1
      }
      if (Checked==FALSE){
        onsetT=1
        onsetV=AnnualTS[1]
      }
    }
    
    
    if (ls>0){
      ko=Max_T-ls
      if (AnnualTS[ko]<range1){
        onsetT=ko
        onsetV=AnnualTS[ko]
      }
      if (AnnualTS[ko]>range1){
        p=ls
        Enter=FALSE
        while (p<(length(slopon)+1)){
          if (AnnualTS[Max_T-p]<range1){
            onsetT=Max_T-p
            onsetV=AnnualTS[Max_T-p]
            Enter=TRUE
            break
          }
          p=p+1    
        }
      }
    }
  
      


if (Enter==FALSE){
  p=Max_T-ls
  while (p<Max_T){
    if (AnnualTS[p]<range1){
      onsetT=p
      onsetV=AnnualTS[p]
    }
    p=p+1
  }
}

onsetTF=onsetT
#print (onsetV)
#print(onsetT)


Onset_Value[,"value"][s]=onsetV
Onset_Time[,"value"][s]=onsetTF
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                  Offset
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
offsetT=0
offsetV=0
crp=TRUE
z=Max_T+1
slopof=(AnnualTS[Max_T+1]-AnnualTS[Max_T])
slopof=as.matrix(slopof)
y=2

while (z<(length(AnnualTS))){
  slopof[y]=(AnnualTS[z+1]-AnnualTS[z])
  z=z+1
  y=y+1
}

print (slopof)
print(range2)

lenof=length(slopof)
lastof=slopof[lenof]

i=1
#i=len
lsof=0


while(i<lenof+1){
  if (lastof>(-0.01)){
    #print(last)
    if (lastof> (-0.01)){
      #print(last)
      lsof=i
      break
    }
quick=i
  }
i=i+1
lastof=slopof[i]
}



if (lsof==0){ #if only the growing season is presented and only increasing
  k=Max_T+1
  Checked= FALSE
  while (k<(length(AnnualTS)+1)){
    if (AnnualTS[k]< range2){
      offsetT=k
      offsetV=AnnualTS[k]
      Checked=TRUE
    }
    k=k+1
  }
  if (Checked==FALSE){
    offsetT=length(AnnualTS)
    offsetV=AnnualTS[offsetT]
  }
}

kof=(Max_T+lsof-1)
if (lsof>0){
  if (AnnualTS[kof]<range2){
    offsetT=kof
    offsetV=AnnualTS[kof]
  }
  if (AnnualTS[kof]>range2){
    p=lsof
    Enter=FALSE
    while (p<length(slopof)){
      if ((slopof[p]>(-0.01)) & (AnnualTS[Max_T+p-1]<range2)){
        offsetT=Max_T+p-1
        offsetV=AnnualTS[Max_T+p-1]
        Enter=TRUE
        break
      }
      p=p+1    
    }
  }
  if (Enter==FALSE){
    p=Max_T+lsof-1
    while (p<(length(AnnualTS)+1)){
      if (AnnualTS[p]<range2){
        offsetT=p
        offsetV=AnnualTS[p]
      }
      p=p+1
    }
  }
}

if ((max-offsetV)==0) {
  crp=FALSE
  offsetT=length(AnnualTS)
  offsetV=AnnualTS[offsetT]
}

print (lsof)

print (offsetV)
print(offsetT)
offsetTF=offsetT

Offset_Value[,"value"][s]=offsetV
Offset_Time[,"value"][s]= offsetTF


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                Area
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


St=abs(round (onsetT))
Ed=abs(round(offsetT))
Area=0
start=St
end=Ed+1
mx=Max_T

if (St<=0) {
  start=9
  start1=9
  start2=9
}

if (crp==FALSE){
  Area=0
  Area1=0
  Area2=0
}
while (start<end){
  Area=Area+AnnualTS[start]
  start=start+1
}
#print (Area)

Area_Total[,"value"][s]=Area


start1=St
Area1=AnnualTS[start1]/2
start1=start1+1
while (start1<mx){
  Area1=Area1+AnnualTS[start1]
  start1=start1+1
}
Area1=Area1+(AnnualTS[mx]/2)

print(onsetT)
print (Area1)
if (onsetT==0){ Area1=0}
if (Area==0){ Area1=0}
Area_Before[,"value"][s]=Area1

Area2=AnnualTS[mx]/2
start2=mx+1
while (start2<(end)){
  Area2=Area2+AnnualTS[start2]
  start2=start2+1
}
Area2=Area2+AnnualTS[Ed]/2
#print (Area2)
if (Area==0){ Area2=0}
Area_After[,"value"][s]=Area2

Asy=Area1-Area2
Asymmetry[,"value"][s]=Asy

s=s+1

  }

dir.create("Metrics")
setwd(paste(getwd(), "Metrics", sep="/"))


write.table(Area_Total, "TINDVI.txt")
write.table(Area_After, "TINDVIAfterMax.txt")
write.table(Area_Before, "TINDVIBeforeMax.txt")

write.table(Max_Value, "Max_V.txt")
write.table(Max_Time, "Max_T.txt")

write.table(Offset_Value, "Offset_V.txt")
write.table(Offset_Time, "Offset_T.txt")

write.table(Onset_Value, "Onset_V.txt")
write.table(Onset_Time, "Onset_T.txt")

write.table(Asymmetry, "Asymmetry.txt")
###===================================================================================================
#Defining secondary metrics

BeforeMaxT[,"value"]=Max_Time[,"value"]-Onset_Time[,"value"]
write.table(BeforeMaxT, "BeforeMaxT.txt")

AfterMaxT[,"value"]=Offset_Time[,"value"]-Max_Time[,"value"]
write.table(AfterMaxT, "AfterMaxT.txt")

#BrownDownSlope=Max_Time
BrownDownSlope[,"value"]=(Max_Value[,"value"]-Offset_Value[,"value"])/(Offset_Time[,"value"]-Max_Time[,"value"])
write.table(BrownDownSlope, "BrownDownSlope.txt")

#GreenUpSlope=Max_Time
GreenUpSlope[,"value"]=(Max_Value[,"value"]-Onset_Value[,"value"])/(Max_Time[,"value"]-Onset_Time[,"value"])
write.table(GreenUpSlope, "GreenUpSlope.txt")


#LengthGS=Max_Time
LengthGS[,"value"]=(Offset_Time[,"value"]-Onset_Time[,"value"])
write.table(LengthGS, "LengthGS.txt")

#LengthGS=Max_Time
Amplitude[,"value"]=(Max_Value[,"value"]-((Onset_Value[,"value"]+Offset_Value[,"value"])/2))
write.table(Amplitude, "Amplitude.txt")

###===================================================================================================
par(mfrow=c(2,2))

names(AllP)=Hd
write.csv(AllP, "AllPixels.txt")


OT=rasterFromXYZ(Onset_Time)
crs(OT)<-crs(ras)
brk=seq(2,16, by=0.01)
nbrk=length(brk)
plot(OT$value, main="OnsetT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(2,16,by=2), labels=seq(2,16,by=2)), zlim=c(2,16))
writeRaster(OT$value, "OnsetT.img", overwrite=TRUE)
OV=rasterFromXYZ(Onset_Value)
brk=seq(0.1,0.6, by=0.001)
nbrk=length(brk)
crs(OV)<-crs(ras)
plot(OV$value, main="OnsetV", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0.1,0.6,by=0.2), labels=seq(0.1,0.6,by=0.2)), zlim=c(0.1,0.6))
writeRaster(OV$value, "OnsetV.img", overwrite=TRUE)

MT=rasterFromXYZ(Max_Time)
crs(MT)<-crs(ras)
brk=seq(8,19, by=1)
nbrk=length(brk)
lblbrk=brk
plot(MT$value, main="MaxT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(8,19,by=2), labels=seq(8,19,by=2)),zlim=c(8,19))
writeRaster(MT$value, "MaxT.img", overwrite=TRUE)


MV=rasterFromXYZ(Max_Value)
crs(MV)<-crs(ras)
brk=seq(0.2,1, by=0.001)
nbrk=length(brk)
plot(MV$value, main="MaxV", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0.2,1,by=0.2), labels=seq(0.2,1,by=0.2)), zlim=c(0.2,1))
writeRaster(MV$value, "MaxV.img", overwrite=TRUE)

OFT=rasterFromXYZ(Offset_Time)
crs(OFT)<-crs(ras)
brk=seq(16,23, by=0.01)
nbrk=length(brk)
plot(OFT$value, main="OffsetT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(16,23,by=2), labels=seq(16,23,by=2)), zlim=c(16,23))
writeRaster(OFT$value, "OffsetT.img", overwrite=TRUE)

OFV=rasterFromXYZ(Offset_Value)
crs(OFV)<-crs(ras)
brk=seq(0,0.4, by=0.001)
nbrk=length(brk)
plot(OFV$value, main="OffsetV", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,0.4,by=0.1), labels=seq(0,0.4,by=0.1)), zlim=c(0,0.4))
writeRaster(OFV$value, "OffsetV.img", overwrite=TRUE)

GUS=rasterFromXYZ(GreenUpSlope)
crs(GUS)<-crs(ras)
brk=seq(0,0.25, by=0.00001)
nbrk=length(brk)
plot(GUS$value, main="GreenUpSlope", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,0.25,by=0.1), labels=seq(0,0.25,by=0.1)),zlim=c(0,0.25))
writeRaster(GUS$value, "GreenUpSlope.img", overwrite=TRUE)

BDS=rasterFromXYZ(BrownDownSlope)
crs(BDS)<-crs(ras)
brk=seq(0,0.25, by=0.00001)
nbrk=length(brk)
plot(BDS$value, main="BrownDownSlope", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,0.25,by=0.1), labels=seq(0,0.25,by=0.1)),zlim=c(0,0.25))
writeRaster(BDS$value, "BrownDownSlope.img", overwrite=TRUE)

BefMaxT=rasterFromXYZ(BeforeMaxT)
crs(BefMaxT)<-crs(ras)
brk=seq(0,12, by=0.01)
nbrk=length(brk)
plot(BefMaxT$value, main="BeforeMaxT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,12,by=2), labels=seq(0,12,by=2)), zlim=c(0,12))
writeRaster(BefMaxT$value, "BeforeMaxT.img", overwrite=TRUE)

AftMaxT=rasterFromXYZ(AfterMaxT)
crs(AftMaxT)<-crs(ras)
brk=seq(0,12, by=0.01)
nbrk=length(brk)
plot(AftMaxT$value, main="AfterMaxT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,12,by=2), labels=seq(0,12,by=2)), zlim=c(0,12))
writeRaster(AftMaxT$value, "AfterMaxT.img", overwrite=TRUE)

Len=rasterFromXYZ(LengthGS)
crs(Len)<-crs(ras)
brk=seq(6,17, by=0.1)
nbrk=length(brk)
writeRaster(Len$value, "LengthGS.img", overwrite=TRUE)
plot(Len$value, main="LengthGS", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(6,17,by=2), labels=seq(6,17,by=2)), zlim=c(6,17))

AA=rasterFromXYZ(Area_After)
crs(AA)<-crs(ras)
brk=seq(0,6, by=0.0001)
nbrk=length(brk)
plot(AA$value, main="TINDVIAfterMax", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,6,by=2), labels=seq(0,6,by=2)), zlim=c(0,6))
writeRaster(AA$value, "TINDVIAfterMax.img", overwrite=TRUE)

AB=rasterFromXYZ(Area_Before)
crs(AB)<-crs(ras)
brk=seq(0,6, by=0.0001)
nbrk=length(brk)
plot(AB$value, main="TINDVIBeforeMax", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,6,by=2), labels=seq(0,6,by=2)), zlim=c(0,6))
writeRaster(AB$value, "TINDVIBeforeMax.img", overwrite=TRUE)


AT=rasterFromXYZ(Area_Total)
crs(AT)<-crs(ras)
brk=seq(0,8, by=0.001)
nbrk=length(brk)
plot(AT$value, main="TINDVI", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,8,by=2), labels=seq(0,8,by=2)), zlim=c(0,8))
writeRaster(AT$value, "TINDVI.img", overwrite=TRUE)


As=rasterFromXYZ(Asymmetry)
crs(As)<-crs(ras)
brk=seq(-6,6, by=0.0001)
nbrk=length(brk)
plot(As$value, main="Asymmetry", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(-6.0,6.0,by=3), labels=seq(-6.0,6.0,by=3)), zlim=c(-6,6))
writeRaster(As$value, "Asymmetry.img", overwrite=TRUE)


##########################====================================##########################

return("*********************Output file saved at working directory*************************")

##########################====================================##########################
}  
#' @export
#' @return Multiple time series curves together at the plot panel
#' @param path - the path whee AllPixel.txt saved
#' @param N - number of intersted points
#' @param Id1 -  ID number for point 1
#' @param Id2 -  Id number for point 2
#' @param Id3 -  ID number for point 3
#' @param Id4 -  ID number for point 4
#' @param Id5 -  ID number for point 5
#' @title Time series curves for Multiple points in the Region of Interest
#' @description MultiPointsPlot function takes the ID for the pixels within the region of interst and returns, the timeseries curves from these points, ploted together. The Id numbers can be obtained from the txt file (AllPixels.txt) outputs.
#' @keywords Curve from multiple points 
#' @keywords time-series curves
#' @author Sofanit Araya
#' 
#' @details This function allows plotting time series curves from multiple points together in a single plot which helps understanding the growth variability across the field.This inforaiton allow observation of the spatial and temporal crop growth variability across the growth seasons, which provide important information about the environmental factors influencing crop growth and thus potential opportunities for influencing crop management (eg . Araya et al., 2016)
#' @details The maximum number of pixeles allowed plotting togther are 5 points.
#' 
#' @examples
#' MultiPointsPlot(system.file("extdata/data3", package="CropPhenology"),3,11,114,125)
#' 
#' @references Araya, S., Lyle, G., Lewis, M., Ostendorf, B., 2016. Phenologic metrics derived from MODIS NDVI as indicators for Plant Available Water-holding Capacity. Ecological Indicators 60, 1263-1272.
#' 
#' 
#' @seealso PhenoMetrics()
#' 
MultiPointsPlot<- function (path, N,Id1,Id2,Id3,Id4,Id5){
  #AP=read.table("Allpixels.txt")
  setwd(path)
  AP=read.table("AllPixels.txt", header=TRUE, sep=",", strip.white = TRUE)
  APP=as.matrix(AP[Id1,])
  #print (APP)
  par(mfrow=c(1,1))
  
  if (N>5){
    warning ('The maximum No of pixel to plot is 5')
    
    
    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) || (is.numeric(Id3)==FALSE) || (is.numeric(Id4)==FALSE) || (is.numeric(Id5)==FALSE) ){
      stop ('ID should be numeric')
    }
    
    
    if (missing (Id1) | missing(Id2) | missing (Id3) | missing (Id4) | missing (Id5)){
      stop('Id missed')
    }
    ts.plot((ts(as.matrix(AP[Id1,])[4:length(APP)])), (ts(as.matrix(AP[Id2,])[4:length(APP)])), (ts(as.matrix(AP[Id3,])[4:length(APP)])), (ts(as.matrix(AP[Id4,])[4:length(APP)])), (ts(as.matrix(AP[Id5,])[4:length(APP)])),  ylim=c(0,1), , col=1:5)
    axis(2, at=seq(0,1,by=0.1))
    
  }
  
  
  if (N==1){
    warning('only one pixel ploted')
    
    
    if ((is.numeric(Id1)==FALSE) ){
      stop ('ID should be numeric')
    }
    
    
    
    if ((Id1>length(AP$T1))){
      stop ('Id out of range')
    }
    
    ts.plot ((ts(as.matrix(AP[Id1,])[4:length(APP)])), ylim=c(0,1))
    axis(2, at=seq(0,1,by=0.1))
  }
  
  if (N==2){
    if (missing (Id1) || missing(Id2)){
      stop('Id missed')
    }
    
    
    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) ){
      stop ('ID should be numeric')
    }
    
    
    if ((Id1>length(AP$T1)) || (Id2>length(AP$T1))){
      stop ('Id out of range')
    }
    
    ts.plot ((ts(as.matrix(AP[Id1,])[4:length(APP)])), (ts(as.matrix(AP[Id2,])[4:length(APP)])), ylim=c(0,1), col=1:2)
    axis(2,  at=seq(0,1,by=0.1))
  }
  if (N==3){
    if ((missing (Id1)) || (missing(Id2)) || (missing (Id3))){
      stop ("Id missed")
    }
    
    
    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) || (is.numeric(Id3)==FALSE) ){
      stop ('ID should be numeric')
    }
    
    
    if ((Id1>length(AP$T1)) || (Id2>length(AP$T1)) || (Id3>length(AP$T1))){
      stop ('Id out of range')
    }
    
    ts.plot ((ts(as.matrix(AP[Id1,])[4:length(APP)])), (ts(as.matrix(AP[Id2,])[4:length(APP)])), (ts(as.matrix(AP[Id3,])[4:length(APP)])), ylim=c(0,1), col=1:3)
    axis(2,  at=seq(0,1,by=0.1))
  }
  
  if (N==4){
    if (missing (Id1) || missing(Id2) || missing (Id3) || missing (Id4)){
      stop('Id missed')
    }
    
    
    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) || (is.numeric(Id3)==FALSE) || (is.numeric(Id4)==FALSE) ){
      stop ('ID should be numeric')
    }
    
    
    if ((Id1>length(AP$T1)) || (Id2>length(AP$T1)) || (Id3>length(AP$T1)) || (Id4>length(AP$T1))){
      stop ('Id out of range')
    }
    
    ts.plot ((ts(as.matrix(AP[Id1,])[4:length(APP)])), (ts(as.matrix(AP[Id2,])[4:length(APP)])), (ts(as.matrix(AP[Id3,])[4:length(APP)])), (ts(as.matrix(AP[Id4,])[4:length(APP)])),  ylim=c(0,1), col=1:4)
    axis(2, at=seq(0,1,by=0.1))
  }
  if (N==5){
    if (missing (Id1) || missing(Id2) || missing (Id3) || missing (Id4) || missing (Id5)){
      stop('Id missed')
    }
    
    
    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) || (is.numeric(Id3)==FALSE) || (is.numeric(Id4)==FALSE) || (is.numeric(Id5)==FALSE) ){
      stop ('ID should be numeric')
    }
    
    
    if ((Id1>length(AP$T1)) || (Id2>length(AP$T1)) || (Id3>length(AP$T1)) || (Id4>length(AP$T1)) || (Id5>length(AP$T1)) ){
      stop ('Id out of range')
    }
    
    ts.plot ((ts(as.matrix(AP[Id1,])[4:length(APP)])), (ts(as.matrix(AP[Id2,])[4:length(APP)])), (ts(as.matrix(AP[Id3,])[4:length(APP)])), (ts(as.matrix(AP[Id4,])[4:length(APP)])), (ts(as.matrix(AP[Id5,])[4:length(APP)])), ylim=c(0,1),  col=1:5)
    axis(2, at=seq(0,1,by=0.1))
  }
  return ("..........Curves ploted............................")
}