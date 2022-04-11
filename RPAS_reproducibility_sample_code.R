# R code for paper titled "Evaluating the reproducibility of RPAS surveys to monitor submerged seagrass habitats"

#STEPS:
#   1) Supplementing training data *
#   2) Kernel density plots including supplemented training data *
#   3) Supervised image classification of orthoimage *
#   4) Measure proportion of quadrat coverage *
#   5) Compare drone vs snorkel assessment of habitat cover 
#   6) Zero inflated models of difference in eelgrass estimates - spearate models for drone and snorkel data
#   7) Caplin Cove altitude test
#   8) Quadrat altitude test 
#    

# *Example code, using the North Harbour dataset. Same methods were repeated for the other sites/seasons


# ~~~ Rasters are too large to upload on GitHub. Code is still uncluded but will not run.  ~~~~


####################################
# 1) Supplementing training data ###
####################################

# Install pakcages

install.packages("ks")
library(ks)
library(caret)


#Import Gopro data for reference curve
setwd("D:/OneDrive - Memorial University of Newfoundland/GitHub/RPAS_repeatability/Files")
train2<-read.csv("NH_valpoints_Summer_only_pixels_exposed_sw.csv")


#Import data to test - Code is for eelgrass data 

setwd("D:/OneDrive - Memorial University of Newfoundland/GitHub/RPAS_repeatability/Files")
dfeel<-read.csv("NH_summer_2020_made_kde_valpoints_eelgrass.csv")


class_name=unique(dfeel$Classification)

#8 is blue, 9 is green, 10 is red
bands<-c(8,9,10)


#Subset training data to only include eelgrass
samp_t<-subset(train2, Classification=="Eelgrass")
samp_t$Classification<-as.factor(as.character(samp_t$Classification))

head(samp_t)
unique(samp_t$Classification)

samp_v<-subset(dfeel, Classification==class_name)
samp_v$Classification<-as.factor(as.character(samp_v$Classification))
samp_v<-samp_v[complete.cases(samp_v), ] #Remove NAs

kde <- ks::kde(x = samp_t[,bands], eval.points = samp_v[,bands])

samp_tsw<-subset(train2, Classification=="Seaweed")
samp_tsw<-samp_tsw[complete.cases(samp_tsw), ]

kde_sw <- ks::kde(x = samp_tsw[,bands], eval.points = samp_v[,bands])

samp_ts<-subset(train2, Classification=="Substrate")
samp_ts<-samp_ts[complete.cases(samp_ts), ]

kde_s <- ks::kde(x = samp_ts[,bands], eval.points = samp_v[,bands])

samp_texp<-subset(train2, Classification=="Exposed seaweed")
samp_texp<-samp_texp[complete.cases(samp_texp), ]

kde_exp <- ks::kde(x = samp_texp[,bands], eval.points = samp_v[,bands])

samp_dw<-subset(train2, Classification=="Deep water")
samp_dw<-samp_dw[complete.cases(samp_dw), ]

kde_dw <- ks::kde(x = samp_dw[,bands], eval.points = samp_v[,bands])

new_df<-data.frame(eelgrass=kde$estimate, sw=kde_sw$estimate, substrate=kde_s$estimate, exposed=kde_exp$estimate, dw=kde_dw$estimate)

new_df$class<-as.factor(apply(new_df,1,FUN=which.max))
true_levels<-levels(new_df$class)
possible_levels<-c(1,2,3,4,5)
possible_levels_true<-possible_levels[!(possible_levels %in% new_df$class)]

levels(new_df$class)<-c(true_levels, possible_levels_true) #ensures that all levels are there for each run

new_df$class<-plyr::revalue(new_df$class, c("1"="eelgrass", "2"="sw", "3"="substrate",
                                            
                                            "4"="exposed_sw", "5"="deep_water"))
#tells you how many worked
freq_class<-data.frame(rbind(table(new_df$class)))
freq_class

#Save this as output for excel
new_df

dfeel2<-dfeel
dfeel2$newClass<-new_df$class

head(dfeel2)
tail(dfeel2)

#remove false classified values
df3<-dfeel2
names(df3)

df3$newClass <- gsub('eelgrass', 'Eelgrass', df3$newClass)
df3$newClass <- gsub('substrate', 'Substrate', df3$newClass)
df3$newClass <- gsub('exposed_sw', 'Exposed seaweed', df3$newClass)
df3$newClass <- gsub('sw', 'Seaweed', df3$newClass)
df3$newClass <- gsub('deep_water', 'Deep water', df3$newClass)
head(df3)

output<-matrix(nrow=length(df3$Classification), ncol=1)
for(i in 1:length(df3$Classification)){
  j<-which(df3$newClass[i] == df3$Classification[i]) # check if there is a match
  output[i,1]<-ifelse(length(j) > 0, 1, NA)
}

df3$match<-output
match<-df3[complete.cases(df3), ]  #Remove NAs

head(match)
length(match$ID)


#export
#setwd("D:/OneDrive - Memorial University of Newfoundland/PhD/Chapter 1 - drone analysis/Drafts/GitHub_Code/Files")
#write.csv(match,"New_deep_water_v2_NH_summer2020_small_AOI.csv")


# dataframe contains data that can be used to supplement GoPro training data




###################################################################################
# 2) Kernel density plots of training/validation data collected using the GoPro ###
###################################################################################

setwd("D:/OneDrive - Memorial University of Newfoundland/GitHub/RPAS_repeatability/Files")
dat<-read.csv("NH_valpoints_Summer_with_made_pixels_v4_small_AOI_less_seaweed.csv")

unique(dat$Classification)

#samp_E
dat_E<-subset(dat, Classification=="Eelgrass")
dat_E<-dat_E[complete.cases(dat_E), ]
dat_E$Classification<-as.factor(as.character(dat_E$Classification))

#samp_S
dat_S<-subset(dat, Classification=="Substrate")
dat_S<-dat_S[complete.cases(dat_S), ]
dat_S$Classification<-as.factor(as.character(dat_S$Classification))


#samp_sw
dat_SW<-subset(dat, Classification=="Seaweed")
dat_SW<-dat_SW[complete.cases(dat_SW), ]
dat_SW$Classification<-as.factor(as.character(dat_SW$Classification))

#samp_exp
dat_Exp<-subset(dat, Classification=="Exposed seaweed")
dat_Exp<-dat_Exp[complete.cases(dat_Exp), ]
dat_Exp$Classification<-as.factor(as.character(dat_Exp$Classification))

#samp_dw
dat_dw<-subset(dat, Classification=="Deep water")
dat_dw<-dat_dw[complete.cases(dat_dw), ]
dat_dw$Classification<-as.factor(as.character(dat_dw$Classification))

# Saving output

#setwd("D:/OneDrive - Memorial University of Newfoundland/GitHub/RPAS_repeatability/Files")
#jpeg("Pixel_density_plots_NH_summer_final.jpg", units="in", res=400, 
#     width = 8, height = 8)


par(mar=c(3,5,3,3))

m<-matrix(c(1,2,3,4), nrow=4,ncol=1)
layout(mat=m, heights=c(0.25,0.25,0.25,0.2))

#par(mfrow=c(3,1))
par(xpd=FALSE)
plot(kde <- ks::kde(x = dat_S$Red), xlim=c(0,250), col="red", main="Red" ,ylab="", yaxt="n", cex.lab = 1.5, cex.main=1.5, lwd=2) #ylim=c(0,0.2),
par(new=TRUE)
plot(kde <- ks::kde(x = dat_E$Red), xlim=c(0,250), col="green", ylab="", yaxt="n", cex.lab = 1.5,lwd=2) #ylim=c(0,0.2),  
par(new=TRUE)
plot(kde <- ks::kde(x = dat_SW$Red), xlim=c(0,250), ylab="", yaxt="n", lwd=2)  #ylim=c(0,0.2),
par(new=TRUE)
plot(kde <- ks::kde(x = dat_Exp$Red), xlim=c(0,250), col="orange", ylab="", yaxt="n", cex.lab = 1.5,lwd=2)
par(new=TRUE)
plot(kde <- ks::kde(x = dat_dw$Red), xlim=c(0,250), col="blue", ylab="",yaxt="n", cex.lab = 1.5,lwd=2)


plot(kde <- ks::kde(x = dat_S$Blue), xlim=c(0,220), col="red", main="Blue", yaxt="n",cex.lab = 1.5, cex.main=1.5, lwd=2)
par(new=TRUE) #, ylim=c(0,0.055)
plot(kde <- ks::kde(x = dat_E$Blue), xlim=c(0,220), col="green", yaxt="n", cex.lab = 1.5, lwd=2)  
par(new=TRUE)
plot(kde <- ks::kde(x = dat_SW$Blue), xlim=c(0,220), yaxt="n", cex.lab = 1.5, lwd=2)  
par(new=TRUE)
plot(kde <- ks::kde(x = dat_Exp$Blue), xlim=c(0,220), col="orange", yaxt="n", cex.lab = 1.5, lwd=2) 
par(new=TRUE)
plot(kde <- ks::kde(x = dat_dw$Blue), xlim=c(0,220), col="blue",yaxt="n", cex.lab = 1.5,lwd=2) 



plot(kde <- ks::kde(x = dat_S$Green), xlim=c(0,220), col="red", main="Green"
     ,ylab="", yaxt="n", cex.lab = 1.5, cex.main=1.5, lwd=2) #, ylim=c(0,0.15)
par(new=TRUE)
plot(kde <- ks::kde(x = dat_E$Green), xlim=c(0,220), col="green" 
     ,ylab="", yaxt="n", cex.lab = 1.5,lwd=2)  
par(new=TRUE)
plot(kde <- ks::kde(x = dat_SW$Green), xlim=c(0,220)
     ,ylab="", yaxt="n", cex.lab = 1.5, lwd=2) 
par(new=TRUE)
plot(kde <- ks::kde(x = dat_Exp$Green), xlim=c(0,220), col="orange" ,ylab="", yaxt="n", cex.lab = 1.5, lwd=2)
par(new=TRUE)
plot(kde <- ks::kde(x = dat_dw$Green), xlim=c(0,220), col="blue" ,ylab="",yaxt="n", cex.lab = 1.5,lwd=2)

plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend("top",legend=c("Substrate", "Eelgrass", 
                      "Seaweed",
                      "Exposed seaweed", "Deep water"),
       col=c("red", "green", "black","orange", "blue"), 
       inset = 0, bty = "n",
       lty=1:1, cex=1.5,
       box.lty=0, ncol=2)

#dev.off()




######################################################
# 3) Supervised image classification of orthoimage ###
######################################################

# This code will:

#1. Use the mask function to crop out the terrestrial protion of the orthomosaic 
#   (aka raster) using a polygon made in ArcMap
#2. Convert validation points to the same coordinate reference system as the raster
#3. Execute image classification using RStoolbox supervised classification and 
#   validation points collected in the field


# packages 

library(rgdal)
library(raster)
library(sp)
library(RStoolbox)
library(rasterVis)
library(sp)
library(rgeos)
library(caret)

removeTmpFiles(h=24) #remove temp files if run once alrady to open computer memory 



# A) Clip raster to remove terrestrial features 


#Import un-altered raster created and imported from Agisoft  ~~~ This will not run because rasters cannot be uploaded onto GitHub

setwd("D:/Agisoft/North_Harbour/Summer_2020")
DEM <- raster("Summer_2020_withGCP_final_1m2pixelres.tif") #10cm resolution 

#visualize un-altered raster
plot(DEM, col = rev(terrain.colors(50)))  

#Import polygon with raster area
setwd("D:/ArcMap/North_Harbour/GIS_output_forNH/NH_AOI")
crop_extent <- readOGR("NH_AOI_no_land_resized.shp")

#Check polygon over map to make sure they line up correctly
plot(crop_extent, add = TRUE)

#Clip raster to only include what falls within the polygon boundaries
obj<-mask(DEM, crop_extent)

#Visualize cropped raster
plot(obj)




# 2) Import validation points for image classification 



#Import validation points which have been converted to decimal degrees to match the corrdinate system of the raster file

# to alter coordinates that are not in decimal degrees, csv columns need to look
# like this: 47 52 42.04

setwd("D:/OneDrive - Memorial University of Newfoundland/GitHub/RPAS_repeatability/Files")
val_NH<-read.csv("NH_valpoints_Summer_with_made_pixels_v4_small_AOI_less_seaweed2.csv")
head(val_NH)

names(val_NH)

#clean CSV (remove NAs and remove rows with data collected in the fall)
val_NH2<-val_NH
val2<-val_NH2[complete.cases(val_NH2[,5:6]),] #remove rows with NAs
val2
table(val2$Classification)

head(val2)
tail(val2)

#make validation points
valpoints<-SpatialPoints(data.frame(val2$Wcoor_DD,val2$Ncoor_DD), proj4string = CRS("+proj=longlat +datum=WGS84"))   

#assign class to validation points you just made
valpoints_df<-SpatialPointsDataFrame(valpoints,data.frame(Class=val2$Classification)) 



# make final shapefile for validation points with coordinate reference system
# the same as the raster
valpoints_df<-spTransform(valpoints_df, crs(obj))
plot(valpoints_df, col=valpoints_df$Class, add=TRUE)

#can also try col <- terrain.colors(5) for other colours: plot(valpoints_df, col=col)

#create buffer around the val points, change width size for the size of the buffer in m
valpoints_df_buff<-gBuffer(valpoints_df, width=0.10, byid=TRUE)

#clip valpoints that are outside the raster
obj_val<-intersect(valpoints_df_buff, crop_extent)


#remove debris from the validation data
obj_val_no_debris<-subset(obj_val, Class!="Debris")
obj_val_no_debris$Class<-as.factor(as.character(obj_val_no_debris$Class))


#check 
unique(obj_val_no_debris$Class)
table(obj_val_no_debris$Class)




# 3) Run image classification using random forest model and validation points 


set.seed(4)

#Fit classification (splitting the validation points into 80% training data and
# 20% check points). ResponseCol referes to the different classes of features we
# want to identify. RF referes to the use of random forest for image classification

## Split training data in training and validation set (80%-20%)
splitIn   <- createDataPartition(obj_val_no_debris$Class, p = .8)[[1]]
train <- obj_val_no_debris[splitIn,]
val   <- obj_val_no_debris[-splitIn,]

#Classify map
SC_NH_summer<- superClass(obj, train, valData = NULL, responseCol = "Class",
                          nSamples = 1000, polygonBasedCV = TRUE, trainPartition = NULL,  #Make polygonBasedCV = TRUE for next step
                          model = "rf", tuneLength = 3, kfold = 5, minDist = 2,
                          mode = "classification", predict = TRUE, predType = "raw",
                          filename = NULL, overwrite = TRUE)


## Polish map with majority filter. Matrix is (1,3,3) because there are three classes
polishedMap <- focal(SC_NH_summer$map, matrix(1,3,3), fun = modal) 

#Validate
val1 <- validateMap(polishedMap, valData = val, responseCol = "Class",
                    classMapping = SC_NH_summer$classMapping)
val1




# 4) Plot map and save it 

#set a theme
#myTheme <- BTCTheme()
#myTheme$panel.background$col = 'white' #set background to grey
#classes<-SC_NH_summer$map@data@attributes[[1]]$value

#setwd("D:/OneDrive - Memorial University of Newfoundland/PhD/Chapter 1 - drone analysis/R_code/Plots_NH_Summer_2020")
#jpeg("NH_Summer_Large_AOI_res10cm_10cmbuff_same_training_dat_as_smallAOI.jpeg", units="in", res=400, height=8, width=8)

colors5<-c("#08519c","#018571","#b35806","#d95f0e","#dfc27d")
levelplot(polishedMap, xlab="Westings (m)", ylab="Northings (m)",
          at=c(0,1,2,3,4,5), #where the legend splits
          colorkey=list(height=0.5, at=c(0.5,1.5,2.5,3.5,4.5,5.5),
                        labels=list(at=c(1,2,3,4,5),
                                    labels=c("Deep water", "Eelgrass", "Exposed macroalgae", "Macroalgae", "Unvegetated"))),
          col.regions=colors5, margin=F)

#dev.off()




# 5) saving model output so you don't have to re-run all the funtions 

#saving model output
#setwd("D:/OneDrive - Memorial University of Newfoundland/PhD/Chapter 1 - drone analysis/R_code/Model_outputs/Summer_NH")

#make named list, can inlcude multiple things
#my_model_NH<-list(
#  NH_small_AOI_val1=val1,   #What I am naming it = What it is called originally
#  NH_small_AOI_polishedMap=polishedMap,    #What I am naming it = What it is called originally
#  NH_small_AOI_model=SC_NH_summer
#)

#actually saves it as an RData file
#save(my_model_NH, file="NH_summer_2020_outputs_10cmres_small_AOI_less_seaweed.RData")


#Saving raster files
#writeRaster(polishedMap, "NH_LARGE_AOI_less_seaweed_polishedMap.grd")


#5) reload outputs saved 

#setwd("D:/OneDrive - Memorial University of Newfoundland/PhD/Chapter 1 - drone analysis/R_code/Model_outputs/Summer_NH")

#loads files back into your environment
#load("NH_summer_2020_outputs_10cmres_small_AOI_less_seaweed.RData")
#my_model_NH 

#Open each objects that were saved 
#val1_NH<-my_model_NH$NH_small_AOI_val1

#polishedmap_NH<-raster("NH_LARGE_AOI_less_seaweed_polishedMap.grd")
#PLOT POLISHED MAP

#colors5<-c("#08519c","#018571","#b35806","#d95f0e","#dfc27d") # for with deep water
#levelplot(polishedmap_NH, xlab="Westings (m)", ylab="Northings (m)",
#               at=c(0,1,2,3,4,5), #where the legend splits
#               colorkey=list(height=0.5, at=c(0.5,1.5,2.5,3.5,4.5,5.5),
#                             labels=list(at=c(1,2,3,4,5),
#                                         labels=c("Deep water", "Eelgrass", "Exposed macroalgae", "Macroalgae", "Unvegetated"))),
#               col.regions=colors5, margin=F)




##############################################
# 4) Measure proportion of quadrat coverage ##
##############################################

# Example for North Harbour summer eelgrass quadrats only

#Data will not load beucase rasters are too large to upload



# A) Load packages

library(rgdal)
library(raster)
library(sp)
library(RStoolbox)
library(rasterVis)
library(sp)
library(rgeos)
library(caret)
library(ks)


# B) Import data 

#Import polygon with quadrat area (all quadrats for the site)
setwd("D:/OneDrive - Memorial University of Newfoundland/GitHub/RPAS_repeatability/Files")
Q_extent <- readOGR("NH_All_Quadrats.shp")
plot(Q_extent)

#Load model
setwd("D:/OneDrive - Memorial University of Newfoundland/PhD/Chapter 1 - drone analysis/R_code/Model_outputs/Summer_NH")

#loads files back into your environment
load("NH_summer_2020_outputs_10cmres_LARGE_AOI_less_seaweed.RData")
my_model_NH 

#Import model
setwd("D:/OneDrive - Memorial University of Newfoundland/PhD/Chapter 1 - drone analysis/R_code/Model_outputs/Summer_NH")
polishedMap_m<-raster("NH_LARGE_AOI_less_seaweed_polishedMap.grd") 
par(mar=c(2,2,2,2))
plot(polishedMap_m)



# C) Run classification with deep water 

#Make new dataframe
new_df<-data.frame(Transect_no=NA, Quadrat_no=NA, site=NA, Season=NA, Location = NA, Deep_water=NA, Eelgrass=NA, Exposed_seaweed=NA, Seaweed=NA, Substrate=NA)


#Loop for all quadrats at once! 
for(i in 1:length(Q_extent)){
  
  x<-crop(polishedMap_m, Q_extent[i,])  #Q_extent[1,] to look at individual polygons. i = different polygons it will look at
  x<-mask(x, Q_extent[i,])
  class_prop<-freq(x, useNA="no")        # caluculates frequency of each class and removes the NAs 
  class_prop_df<-as.data.frame(class_prop)    # turns it into a dataframe 
  
  #To make file
  class_prop_df$value<-as.factor(class_prop_df$value)   #turn the values into a factor
  
  true_levels<-levels(class_prop_df$value)
  possible_levels<-c(1,2,3,4,5)
  possible_levels_true<-possible_levels[!(possible_levels %in% class_prop_df$value)]
  levels(class_prop_df$value)<-c(true_levels, possible_levels_true) #ensures that all levels are there for each run
  
  # Name and in alphabetical order (same order as random forest model)!
  class_prop_df$value<-plyr::revalue(class_prop_df$value, c("1"="Deep water", "2"="Eelgrass", "3"="Exposed seaweed",
                                                            "4"="Seaweed", "5"="Substrate"))
  
  if(!("Eelgrass" %in% class_prop_df$value)){class_prop_df<-rbind(class_prop_df, data.frame(value=factor("Eelgrass"), count=0))}
  if(!("Deep water" %in% class_prop_df$value)){class_prop_df<-rbind(class_prop_df, data.frame(value=factor("Deep water"), count=0))}
  if(!("Seaweed" %in% class_prop_df$value)){class_prop_df<-rbind(class_prop_df, data.frame(value=factor("Seaweed"), count=0))}
  if(!("Substrate" %in% class_prop_df$value)){class_prop_df<-rbind(class_prop_df, data.frame(value=factor("Substrate"), count=0))}
  if(!("Exposed seaweed" %in% class_prop_df$value)){class_prop_df<-rbind(class_prop_df, data.frame(value=factor("Exposed seaweed"), count=0))}
  
  clean_df<-class_prop_df[order(as.character(class_prop_df$value)),] #sort so clean_df starts in alphabetical order
  clean_df$count<-as.character(as.factor(clean_df$count))
  
  clean_df<-rbind(data.frame(value="Location", count=Q_extent[i,]@data$Location), clean_df)
  clean_df<-rbind(data.frame(value="Season", count=Q_extent[i,]@data$Season), clean_df)  #rbind combines columns with the same name
  clean_df<-rbind(data.frame(value="Site", count=Q_extent[i,]@data$Site), clean_df)
  clean_df<-rbind(data.frame(value="Quadrat", count=Q_extent[i,]@data$Quadrat), clean_df) 
  clean_df<-rbind(data.frame(value="Transect", count=Q_extent[i,]@data$Transect), clean_df)
  #automatically adds the data from the attributes table in ArcMap 
  
  #move second row (row with results) into new dataframe
  # Do not need to change anything! Automatically adds other quadrats into dataframe. DO NOT CHANGE THE 2 IN THE FUNCTION!
  
  new_df[i,]<-t(as.matrix(clean_df))[2,]
}

new_df

#save dataframe
#setwd("D:/OneDrive - Memorial University of Newfoundland/PhD/Chapter 1 - drone analysis/R_code/Plots_NH_Summer_2020/Final_drone_vs_quad_v1")
#write.csv(new_df, "NH_Summer_2020_largeAOI.csv")


# 4 Check    

#Import polygon with quadrat area
setwd("D:/OneDrive - Memorial University of Newfoundland/PhD/Chapter 1 - drone analysis/Drafts/GitHub_Code/Files")
Q_extent <- readOGR("NH_T3_Q4.shp")
plot(Q_extent)


#NH_T1_Q1<-mask(SC_NH_summer$map, Q_extent)
Quadrat<-mask(polishedMap_m, Q_extent)
Quadrat_crop<-crop(Quadrat, Q_extent)

#Visualize cropped quadrat raster
plot(Quadrat_crop)

#make map
myTheme <- BTCTheme()
myTheme$panel.background$col = 'white' #set background to grey

#setwd("D:/OneDrive - Memorial University of Newfoundland/PhD/Chapter 1 - drone analysis/R_code/Plots_NH_Summer_2020/Quadrats/Large_AOI")
#jpeg("NH_T3_Q4.jpeg", units="in", res=400, height=8, width=8)

colors5<-c("#1f78b4","#018571","#d95f0e","#80cdc1","#dfc27d")
levelplot(Quadrat_crop, xlab="Westings (m)", ylab="Northings (m)",
          at=c(0,1,2,3,4,5), #where the legend splits
          colorkey=list(height=0.5, at=c(0.5,1.5,2.5,3.5,4.5,5.5),
                        labels=list(at=c(1,2,3,4,5),
                                    labels=c("Deep water", "Eelgrass", "Exposed Seaweed", "Seaweed", "Substrate"))),
          col.regions=colors5, margin=F)

#dev.off()

freq(Quadrat_crop)





#############################################################
# 5) Compare drone vs snorkel assessment of habitat cover ###
#############################################################


# Load Packages 

library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggpubr)
library(grid)
library(dplyr)
library(pscl)

setwd("D:/OneDrive - Memorial University of Newfoundland/GitHub/RPAS_repeatability/Files")
dat<-read.csv("Longformat_all_sites_all_seasons.csv")
#head(dat)

dat$ID<- paste(dat$site, dat$Transect_no, sep="_")
#head(dat)

#remove unwanted columns
dat_clean<-dat %>% dplyr::select(-Transect_no)%>% dplyr::select(-X.1)%>%dplyr::select(-X.2)
#head(dat_clean)
#tail(dat_clean)

#unique(dat_clean$Cover_Type)

#ChangeCover_type names to be the same for both methods
dat_clean$Cover_Type <- gsub('SQ_Eelgrass', 'Eelgrass', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('All_eelgrass', 'Eelgrass', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('SQ_Seaweed', 'Seaweed', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('All_seaweed', 'Seaweed', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('SQ_Sediment', 'Substrate', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('Deep_water', 'Deep water', dat_clean$Cover_Type)

dat_clean$site <- gsub('BL', "Baie de l'Eau", dat_clean$site)
dat_clean$site <- gsub('NH', 'North Harbour', dat_clean$site)
dat_clean$site <- gsub('SC', 'Swift Current', dat_clean$site)

#unique(dat_clean$Cover_Type)



# Plot data for correlation

#Make wide format
dat_wide <- spread(data = dat_clean, 
                   key =Method, 
                   value = Coverage_count)
#head(dat_wide)
#tail(dat_wide)

dat_wide[is.na(dat_wide)] <- 0

head(dat_wide)
#tail(dat_wide)

dat_wide$Difference<-rowSums(cbind(abs(dat_wide$Drone-dat_wide$Snorkel)), na.rm=TRUE)


dat_wide_summer<-subset(dat_wide, Season=="Summer")
p1<- ggplot(dat_wide_summer, aes(Cover_Type, Difference, fill=as.factor(Location)))+geom_boxplot()+
  theme_classic()+
  facet_wrap(~site)+
  ylab("")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ggtitle("Summer")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        legend.title = element_blank())+
  scale_fill_manual(values=c("#E69F00", "#31a354", "#2c7fb8"))+
  theme(legend.position = "none")

dat_wide_fall<-subset(dat_wide, Season=="Fall")
p2<- ggplot(dat_wide_fall, aes(Cover_Type, Difference, fill=as.factor(Location)))+geom_boxplot()+
  theme_classic()+
  facet_wrap(~site)+
  ylab("")+
  xlab("Habitat")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ggtitle("Fall")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 12))+
  scale_fill_manual(values=c("#E69F00", "#31a354", "#2c7fb8"))+
  theme(legend.position = "bottom")

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(p2)


p3<-grid.arrange(p1, p2+theme(legend.position = "none"), legend, nrow=3, heights=c(3, 3, 0.3),
                 left = textGrob("Difference (%)", rot = 90, vjust = 1, gp = gpar(cex = 1)))

#ggsave("Figure4-rename_xaxis2.jpeg", plot= p3,
#       path = "D:/OneDrive - Memorial University of Newfoundland/PhD/Chapter 1 - drone analysis/Drafts/Figures/Version_3",
#       width = 7,
#       height = 7,
#       units = "in",
#       dpi = 400)



##############################################################################################################
# 6) Zero inflated models of difference in eelgrass estimates - spearate models for drone and snorkel data ###
##############################################################################################################


setwd("D:/OneDrive - Memorial University of Newfoundland/GitHub/RPAS_repeatability/Files")
dat<-read.csv("Longformat_all_sites_all_seasons.csv")

dat$ID<- paste(dat$site, dat$Transect_no, sep="_")

#remove unwanted columns
dat_clean<-dat %>% dplyr::select(-Transect_no)%>% dplyr::select(-X.1)%>%dplyr::select(-X.2)

#unique(dat_clean$Cover_Type)

#ChangeCover_type names to be the same for both methods
dat_clean$Cover_Type <- gsub('SQ_Eelgrass', 'Eelgrass', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('All_eelgrass', 'Eelgrass', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('SQ_Seaweed', 'Seaweed', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('All_seaweed', 'Seaweed', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('SQ_Sediment', 'Substrate', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('Deep_water', 'Deep water', dat_clean$Cover_Type)

dat_clean$site <- gsub('BL', "Baie de l'Eau", dat_clean$site)
dat_clean$site <- gsub('NH', 'North Harbour', dat_clean$site)
dat_clean$site <- gsub('SC', 'Swift Current', dat_clean$site)


beta_dat<-dat_clean


# EELGRASS- Drone only

beta_eel<-subset(beta_dat, Cover_Type=="Eelgrass")
beta_drone<-subset(beta_eel, Method=="Drone")

hist(beta_drone$Coverage_count)

#don't need interactions
m3 <- zeroinfl(Coverage_count~Season+site+Location, data=beta_drone, na.action=na.omit)
summary(m3)

hist(resid(m3))
plot(resid(m3))

# EELGRASS- Snorkel only

beta_eel<-subset(beta_dat, Cover_Type=="Eelgrass")
beta_snorkel<-subset(beta_eel, Method=="Snorkel")
beta_snorkel_noout<-subset(beta_snorkel, Location!="Outside") #removed becausae they are all zero and model doesn't work with it

m4 <- zeroinfl(Coverage_count~Season+site+Location, data=beta_snorkel_noout, na.action=na.omit)
summary(m4)

hist(resid(m4))
plot(resid(m4))

#################################
# 7) Caplin Cove altitude test ##
#################################

# Same methods as full site models 

setwd("D:/OneDrive - Memorial University of Newfoundland/GitHub/RPAS_repeatability/Files")
dat<-read.csv("Longformat_CaplinCove_NH_final_v2.csv")
#head(dat)

dat$ID<- paste(dat$site, dat$Transect_no, dat$Quadrat_no, sep="_")
#head(dat)

#remove unwanted columns
dat_clean<-dat %>% dplyr::select(-Transect_no)%>% dplyr::select(-X.1)%>% dplyr::select(-X.2)
dat_clean<-filter(dat_clean, Cover_Type!="Deep_water")
#head(dat_clean)
#tail(dat_clean)

unique(dat_clean$Cover_Type)

#ChangeCover_type names to be the same for both methods
dat_clean$Cover_Type <- gsub('SQ_Eelgrass', 'Eelgrass', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('All_eelgrass', 'Eelgrass', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('SQ_Seaweed', 'Macroalgae', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('All_seaweed', 'Macroalgae', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('SQ_Sediment', 'Unvegetated', dat_clean$Cover_Type)
dat_clean$Cover_Type <- gsub('Substrate', 'Unvegetated', dat_clean$Cover_Type)

dat_clean$site <- gsub('Caplin_cove_100m', '100', dat_clean$site)
dat_clean$site <- gsub('Caplin_cove_50m', '50', dat_clean$site)
dat_clean$site <- gsub('NH', '115', dat_clean$site)


#unique(dat_clean$Cover_Type)

#remove deep water

#dat_clean<-filter(dat_clean, Cover_Type!="Deep_water")

#unique(dat_clean$Cover_Type)

# Plot data for correlation

#Make wide format
dat_wide <- spread(data = dat_clean, 
                   key =Method, 
                   value = Coverage_count)
#head(dat_wide)
#tail(dat_wide)

dat_wide$Difference<-rowSums(cbind(abs(dat_wide$Drone-dat_wide$Snorkel)), na.rm=TRUE)
dat_wide[is.na(dat_wide)] <- 0

dat_wide$site<-factor(dat_wide$site, levels =c("50","100","115"))


fig<-ggplot(dat_wide, aes(site, Difference, fill=as.factor(Location)))+geom_boxplot()+
  theme_classic()+
  xlab("Altitude (m)")+
  ylab("Difference (%)")+
  facet_wrap(~Cover_Type)+
  scale_fill_manual(values=c("#E69F00", "#31a354", "#2c7fb8"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.title = element_blank())


fig



#############################
# 8) Quadrat altitude test ##
#############################

# Uses unsupervised image classification unlike full site and Caplin Cove analyses

# This code will:

#1. Use the mask function to crop out the terrestrial protion of the orthomosaic 
#   (aka raster) using a polygon made in ArcMap
#2. Convert validation points to the same coordinate reference system as the raster
#3. Execute image classification using RStoolbox supervised classification and 
#   validation points collected in the field



# packages 

library(rgdal)
library(raster)
library(sp)
library(RStoolbox)
library(rasterVis)
library(sp)
library(rgeos)
library(caret)
install.packages("plotKML")
library(plotKML)

removeTmpFiles(h=24) #remove temp files


# A) Clip raster to remove terrestrial features

#Import un-altered raster created and imported from Agisoft
setwd("D:/OneDrive - Memorial University of Newfoundland/GitHub/RPAS_repeatability/Files") 

#Import image at different altitudes (provided onle one picture in this example)
DEM<-raster("Pic_010.tif")  #  will not load because the file is too big to upload onto GitHub

#visualize un-altered raster
plot(DEM, col = rev(terrain.colors(50)))

#Import polygon with raster area
setwd("D:/ArcMap/Altitude_test")
crop_extent <- readOGR("Small_Quadrat.shp")

#Check polygon over map to make sure they line up correctly
plot(crop_extent, add = TRUE)

#Clip raster to only include what falls within the polygon boundaries
obj<-mask(DEM, crop_extent)

#Visualize cropped raster
plot(obj)



# B) Run unsupervised clssification 


set.seed(24)

SC_NH_summer<- unsuperClass(obj, nSamples = 500,nClasses = 3,nStarts = 25)

polishedMap <- focal(SC_NH_summer$map, matrix(1,3,3), fun = modal)
Quadrat<-mask(polishedMap, crop_extent)
Quadrat_crop<-crop(Quadrat, crop_extent)


no_na<-Quadrat_crop@data@values[!is.na(Quadrat_crop@data@values)]
sort(c((freq(Quadrat_crop, useNA="no")/length(no_na))[,2]))


#make map
myTheme <- BTCTheme()
myTheme$panel.background$col = 'white' #set background to grey


freq(Quadrat_crop) # record frequencies in csv


#setwd("D:/OneDrive - Memorial University of Newfoundland/PhD/Chapter 1 - drone analysis/R_code/Plot_Quadrat_altitude/Small_quadrat/Raw")
#jpeg("Pic_039_unsupervised.jpeg", units="in", res=400, height=8, width=8)

colors3<-c("#d95f0e","#dfc27d","#018571")
levelplot(Quadrat_crop, xlab="Westings (m)", ylab="Northings (m)",
          at=c(0,1,2,3), #where the legend splits
          colorkey=list(height=0.5, at=c(0.5,1.5,2.5,3.5),
                        labels=list(at=c(1,2,3),
                                    labels=c("Eelgrass","Substrate","Seaweed"))),
          col.regions=colors3, margin=FALSE)

#dev.off()


no_na<-Quadrat_crop@data@values[!is.na(Quadrat_crop@data@values)]
freq(Quadrat_crop, useNA="no")/length(no_na)

# Get levels 
levelplot(Quadrat_crop, xlab="Westings (m)", ylab="Northings (m)",
          at=c(0,1,2,3), #where the legend splits
          colorkey=list(height=0.5, at=c(0.5,1.5,2.5,3.5),
                        labels=list(at=c(1,2,3))),
          col.regions=colors3, margin=FALSE)

freq(Quadrat_crop, useNA="no")
c(((freq(Quadrat_crop, useNA="no")/length(no_na))*100)[,2])

no_na<-Quadrat_crop@data@values[!is.na(Quadrat_crop@data@values)]
sort(c((freq(Quadrat_crop, useNA="no")/length(no_na))[,2]))


# C) Plot

setwd("D:/OneDrive - Memorial University of Newfoundland/GitHub/RPAS_repeatability/Files")
diff<-read.csv("Quadrat_frequencies_Raw_pixel_res.csv")

#remove 0 m from calculations
diff$Altitude..m.<-as.numeric(diff$Altitude..m.)
str(diff)
diff2<-subset(diff,Altitude..m.>27)
head(diff2)

#head(diff)

#setwd("D:/OneDrive - Memorial University of Newfoundland/PhD/Chapter 1 - drone analysis/Drafts/Figures")
#jpeg("Figure 6.jpeg", units="in", res=400, height=5, width=8)


par(mfrow=c(1,3))
linemod<-summary(lm(abs(diff2$Diff_eelgrass)~diff2$Altitude))
plot((as.numeric(abs(diff2$Diff_eelgrass))~diff2$Altitude),ylab= "Differences (sUAV-quadrat) (%)", 
     xlab = "Altitude (m)", main="Eelgrass", pch=19, las = 1, ylim=c(5,40), frame.plot = FALSE) 
#points((as.numeric(as.character(diff$Diff_eelgrass[1]))~diff$Altitude[1]), col="red", pch=19)
abline(lm((as.numeric(abs(diff2$Diff_eelgrass))~diff2$Altitude)))
mylabel=bquote(italic(R)^2==. (format(linemod$adj.r.squared,digits = 2)))
text(x=95, y=39, labels = mylabel)
mylabel2=bquote(italic(p)==. (format(linemod$coefficients[2,4],digits = 2)))
text(x=94, y=37, labels = mylabel2)
text(x=97, y=35, labels = "y=0.03x + 21.5")  # values taken from code linemod$coefficients

linemod<-summary(lm(as.numeric(abs(diff2$Diff_Seaweed))~diff2$Altitude))
plot((as.numeric(abs(diff2$Diff_Seaweed))~diff2$Altitude),ylab= "Differences (sUAV-quadrat) (%)", 
     xlab = "Altitude (m)", main = "Seaweed", pch=19, las = 1 , frame.plot = FALSE)
#points((as.numeric(as.character(diff$Diff_Seaweed[1]))~diff$Altitude[1]), col="red", pch=19)
abline(lm((as.numeric(abs(diff2$Diff_Seaweed))~diff2$Altitude)))
mylabel=bquote(italic(R)^2==. (format(linemod$adj.r.squared,digits = 2)))
text(x=96, y=24, labels = mylabel)
mylabel2=bquote(italic(p)==. (format(linemod$coefficients[2,4],digits = 2)))
text(x=96, y=22, labels = mylabel2)
text(x=97, y=20, labels = "y=-0.08x + 14.6")

linemod<-summary(lm(as.numeric(as.character(abs(diff2$Diff_substrate)))~diff2$Altitude))
plot((as.numeric(abs(diff2$Diff_substrate))~diff2$Altitude),ylab= "Differences (sUAV-quadrat) (%)", 
     xlab = "Altitude (m)",main = "Substrate", pch=19, las = 1, frame.plot = FALSE) 
abline(lm((as.numeric(abs(diff2$Diff_substrate))~diff2$Altitude)))
mylabel=bquote(italic(R)^2==. (format(linemod$adj.r.squared,digits = 2)))
text(x=95, y=40, labels = mylabel)
mylabel2=bquote(italic(p)==. (format(linemod$coefficients[2,4],digits = 2)))
text(x=94, y=39, labels = mylabel2)
text(x=97, y=38, labels = "y=0.04x +36.1")

#dev.off()

#With GGPLOT
# use above baseplot code to determing R2, pval and slope

#make long format
#remove notes colum
diff2$Notes<-NULL
head(diff2)

#calculate slope equation
lm(abs(diff2$Diff_eelgrass)~diff2$Altitude)

diff2$title<-"Eelgrass"
p61<-ggplot(diff2, aes(Altitude..m., abs(Diff_eelgrass)))+geom_point()+
  theme_classic()+xlim(25,130)+
  xlab("")+
  ylab("")+
  geom_smooth(method ="lm", se=FALSE, colour="black")+
  annotate("text", x=100, y= 34, label = expression(paste(italic(R^2), "= 0.20")), size =3.5)+
  annotate("text", x=100, y= 32.7, label = expression(paste(italic(p), "= 0.007")), size =3.5)+
  annotate("text", x=100, y= 31.7, label = expression(paste("y=0.09x+16.6")), size =3.5)+
  # annotate("text", x=36, y= 34, label = expression(paste(bold("(A)"))), size =5)+
  annotate("text", x=36, y= 34, label = "(A)", size =5)+
  facet_grid(.~title)+
  theme(strip.text.x = element_text(size = 11),
        axis.text=element_text(size=11))

#calcualte slope equation
lm(abs(diff2$Diff_Seaweed)~diff2$Altitude)

diff2$title<-"Macroalgae"
p62<-ggplot(diff2, aes(Altitude..m., abs(Diff_Seaweed)))+geom_point()+
  theme_classic()+xlim(25,130)+
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = pretty(diff2$Diff_Seaweed, n = 10))+
  geom_smooth(method ="lm", se=FALSE, colour="black")+
  annotate("text", x=100, y= 26, label = expression(paste(italic(R^2), "= 0.29")), size =3.5)+
  annotate("text", x=100, y= 24.7, label = expression(paste(italic(p), "= 0.001")), size =3.5)+
  annotate("text", x=100, y= 23.7, label = expression(paste("y=-1.0x+17.8")), size =3.5)+
  #annotate("text", x=36, y= 26, label = expression(paste(bold("(B)"))), size =5)+
  annotate("text", x=36, y= 26, label = "(B)", size =5)+
  facet_grid(.~title)+
  theme(strip.text.x = element_text(size = 11),
        axis.text=element_text(size=11))

#calcualte slope equation
lm(abs(diff2$Diff_substrate)~diff2$Altitude)

diff2$title<-"Unvegetated"
p63<-ggplot(diff2, aes(Altitude..m., abs(Diff_substrate)))+geom_point()+
  theme_classic()+xlim(25,130)+
  xlab("")+
  ylab("")+
  geom_smooth(method ="lm", se=FALSE, colour="black")+
  annotate("text", x=108, y= 40, label = expression(paste(italic(R^2), "= 0.027")), size =3.5)+
  annotate("text", x=108, y= 39, label = expression(paste(italic(p), "= 0.19")), size =3.5)+
  annotate("text", x=108, y= 38.2, label = expression(paste("y=-0.04x+35.8")), size =3.5)+
  #annotate("text", x=36, y= 40, label = expression(paste(bold("(C)"))), size =5)+
  annotate("text", x=36, y= 40, label = "(C)", size =5)+
  facet_grid(.~title)+
  theme(strip.text.x = element_text(size = 11),
        axis.text=element_text(size=11))


p6<-grid.arrange(p61, p62, p63, ncol=3,
                 left = textGrob("Difference (%)", rot = 90, vjust = 1, gp = gpar(cex = 1.2)),
                 bottom = textGrob("Altitude (m)", gp = gpar(cex = 1.2)))








