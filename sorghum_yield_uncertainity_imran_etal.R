
# title         : crop_yield_uncertainity_imran_etal_Rcode.R
# purpose       : R code for the paper "crop_yield_uncertainity_imran_etal";
# reference     : 
# producer      : Muhammad Imran, PhD Researcher, Dept Geoinformation Processing, Faculty of Geo-Information Science and Earth Observation (ITC), University of Twente. Postal: Hengelosestraat 99 PO Box 217, 7500 AE Enschede, The Netherlands 
# Email         : imran19632@itc.nl
# inputs        : 201 samples of sorghum crop yield in Burkina Faso (kg ha), along variables, principal component of SPOT NDVI coverig the growing period (NDVI.PC1), and market access (markd) ;
# outputs       : Sorghum prediction and prediction uncertainty maps;

# ------------------------------------------------------------
# Initial settings:
# ------------------------------------------------------------

setwd('D:/labs/secondpaper')


options(prompt="> ", continue="+ ", digits=3, width=70, show.signif.stars=T)
setInternet2(use=TRUE)  
rm(list=ls()) 

# load necessary packages:

sessionInfo()
download.file("http://plotkml.r-forge.r-project.org/plotKML_0.2-4.tar.gz", "plotKML_0.2-4.tar.gz")
system("R CMD INSTALL plotKML_0.2-4.tar.gz") 

library(proj4)
library(maps)
library(spatial)
library(mapproj)
library(rgdal)
library(lattice)
library(akima)
library(RSAGA)
library(geoR)
library(geoRglm)
library(raster)
library(maps)         ## Projections
library(maptools)     ## Data management
library(sp)           ## Data management
library(spdep)        ## Spatial autocorrelation
library(gstat)        ## Geostatistics
library(splancs)      ## Kernel Density
library(spatstat)     ## Geostatistics
library(pgirmess)     ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(spgwr)        ## GWR
library(MSBVAR)
library(moments)
library(nortest)
library(SeleMix)
library(relaimpo)
library(MASS)
library(GeoXp)
library(plotKML)
# load the data:

setwd('D:/labs/secondpaper')

setwd('D:/labs/thirdpaper')

bf_departments <- readShapePoly("bf_departments.shp")
prj <- "+proj=longlat +ellps=clrk66 +no_defs"
proj4string(bf_departments) <- CRS(prj)

sorghum <- readShapePoints("sorghum.shp")
prj.sor <- "+proj=tmerc +lat_0=0 +lon_0=0 +k=1 +x_0=500000 +y_0=0 +a=6378249.145 +b=6356514.96582849 +units=m +no_defs"
# prj.sor <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
projection(sorghum) <- CRS(prj)

writeOGR(sorghum, ".", "sorghum_prj", "ESRI Shapefile")

plot(sorghum)
str(sorghum)

?projection

proj4string(sorghum) <- CRS(prj.sor)
sorghum <-spTransform(sorghum, CRS(prj))

str(sorghum)

q8<- quantile(sorghum$y2009)

X11()
bubble(sorghum, zcol="y2009", col="black", maxsize=2.5, key.entries=q8,
       sp.layout=list("sp.polygons", bf_departments), main="Sorghum yield (kg ha-1) data")


sorghum.poly <- readShapePoly("bf_departments.shp")

TPOV_PT125<- raster('TPOV_PT125.asc')
projection(TPOV_PT125) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
TPOV_PT125 <- projectRaster(TPOV_PT125, raster("bf_boundary_r.tif"))
mask(TPOV_PT125, raster("bf_boundary_r.tif"), format="GTiff", 
     filename="D:/labs/ThirdPaper/BF_TPOV_PT125.tif", overwrite=TRUE)

PHCR <- readGDAL('BF_TPOV_PT125.tif')
PHCR<- data.frame(PHCR)
coordinates(PHCR) <- ~x+y
proj4string(eberg_grid) <- CRS(prj)

projection(sorghum)


PHCR <- spTransform(PHCR, CRS("+proj=longlat +ellps=GRS80"))

?projfstring
proj4string(eberg_grid) <- CRS(prj)
X11()
r <- spTransform(raster("BF_TPOV_PT125.tif"), sorghum.poly)
spplot(r, sp.layout=list("sp.polygons", sorghum.poly))


SPOT01 <- readGDAL('SPOT_VGT_2009_BurkinaFaso_01_01_tif.tif')


sorghum_rast <- raster('sorghum_rast.tif')

bf_department <- raster('bf_department')
bf_sorghum <- resample(sorghum_rast, bf_department)

writeRaster(bf_sorghum, filename="D:/labs/SECOND_PAPER_REVIZED/bf_sorghum.tif", format="GTiff", overwrite=TRUE)



setwd('D:/labs/secondpaper')
setwd('D:/labs/SECOND_PAPER_REVIZED/SPOTNDVI')

prj = "+proj=tmerc +lat_0=0 +lon_0=18 +k=0.9999 +x_0=6500000 +y_0=0 +ellps=bessel +units=m 
  +towgs84=550.499,164.116,475.142,5.80967,2.07902,-11.62386,0.99999445824"
      
SPOT_VGT_2009_BurkinaFaso_01_01_tif

eberg_grid<- readGDAL('SPOT_VGT_2009_BurkinaFaso_01_01_tif.tif')
eberg_grid<- data.frame(eberg_grid)
coordinates(eberg_grid) <- ~x+y
proj4string(eberg_grid) <- CRS(prj)

str(eberg_grid)


rfe2009_01_dk1 <- projectRaster(rfe2009_01_dk1, raster("SPOT_VGT_2009.tif"))


eberg_grid <- readGDAL('pc1_ndvi.tif')

eberg_grid <- spTransform(eberg_grid, prj)
?spTransform

data(barxyz)

str(barxyz)

gridded(eberg_grid) <- ~x+y
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
data(SAGA_pal)

str(SAGA_pal)

str(eberg_grid)
plotKML(eberg_grid["band1"], colour_scale = SAGA_pal[[1]])
## categorical data:
eberg_grid$LNCCOR6 <- as.factor(paste(eberg_grid$LNCCOR6))
levels(eberg_grid$LNCCOR6)
data(worldgrids_pal)
# attr(worldgrids_pal["corine2k"][[1]], "names")
pal = as.character(worldgrids_pal["corine2k"][[1]][c(1,11,13,14,16,17,18)])
plotKML(eberg_grid["LNCCOR6"], colour_scale=pal)



imagename = "pc1_ndvi.tif"
x1 <- getWikiMedia.ImageInfo(imagename)
sm <- spPhoto(filename = x1$url$url, exif.info = x1$metadata)
# str(sm)
plotKML(sm)

proj4string(sorghum) <- sorghum.grid@proj4string

proj4string(sorghum) <- CRS("+init=epsg:31467")
bubble(sorghum["y2009"])
plotKML(sorghum["y2009"])

?plotKML

lgp<- raster('LGP_AVG.asc')
projection(lgp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
lgp<- projectRaster(lgp, raster("bf_boundary_r.tif"))
mask(lgp, raster("bf_boundary_r.tif"), format="GTiff", 
     filename="D:/labs/ThirdPaper/lgp.tif", overwrite=TRUE)
plot(raster("lgp.tif"))


# get help for some package:
#help(package="gstat")
# ------------------------------------------------------------
# Import of data to R and data analysis
# ------------------------------------------------------------

#----------------------------------------------

# loading external covariate data

#----------------------------------------------
?resample

# hswdbf <- setExtent(raster("hswdbf.tif"), extent(raster("pc1_ndvi.tif")))
# hswdbf
#hswdbf_resample <- resample(hswdbf, raster("pc1_ndvi.tif"), method="ngb", filename="D:/labs/SecondPaper/hswdbf_resample.tif")
# writeRaster(hswdbf, filename='D:/labs/SecondPaper/hswdbf_e.tif', format="GTiff")

grid.list <- c("PRE_AVG_BF.tif", "pc1_ndvi.tif", "TPOV_PT125_BF.tif", "PD_RU05_BF.tif", "markdist.tif", "pc2_ndvi.tif", "elev_eros.tif", "slope.tif", "hswdbf_e.tif")
s <- stack(grid.list)
sorghum.grid <- as(s,"SpatialPointsDataFrame")
str(sorghum.grid)
sorghum.grid$hswdbf_e


# Longterm average annual rainfall (mm)

names(sorghum.grid)[1]
names(sorghum.grid)[1] <- "PERC"
mean(sorghum.grid$PERC)
mean(sorghum.grid$PERC,na.rm=TRUE)
sorghum.grid$PERC <- ifelse ( is.na(sorghum.grid$PERC)=="TRUE", mean(sorghum.grid$PERC,na.rm=TRUE),sorghum.grid$PERC)
mean(sorghum.grid$PERC)
median(sorghum.grid$PERC)
max(sorghum.grid$PERC)
min(sorghum.grid$PERC)
sqrt(var(sorghum.grid$PERC))

#replace NA values with mean
sorghum.grid$pc1_ndvi <- ifelse ( is.na(sorghum.grid$pc1_ndvi)=="TRUE", mean(sorghum.grid$pc1_ndvi,na.rm=TRUE),sorghum.grid$pc1_ndvi)
# names(sorghum.grid)[2]
# names(sorghum.grid)[2] <- "NDVI.PC1"
# mean(sorghum.grid$NDVI.PC1)
# median(sorghum.grid$NDVI.PC1)
# max(sorghum.grid$NDVI.PC1)
# min(sorghum.grid$NDVI.PC1)
# sqrt(var(sorghum.grid$NDVI.PC1))
# skewness(sorghum.grid$NDVI.PC1)

#Data\Poverty headcount ratio at below '05 PPP $1.25 and $2 per day (percent) (2005)

str(sorghum.grid)
names(sorghum.grid)[3]
names(sorghum.grid)[3] <- "PHCR"
sorghum.grid$PHCR <- ifelse ( is.na(sorghum.grid$PHCR)=="TRUE", mean(sorghum.grid$PHCR,na.rm=TRUE),sorghum.grid$PHCR)
mean(sorghum.grid$PHCR)
median(sorghum.grid$PHCR)
max(sorghum.grid$PHCR)
min(sorghum.grid$PHCR)
sqrt(var(sorghum.grid$PHCR))

# RURAL POPULATION DENSITY (Sq Km)
names(sorghum.grid)[4]
names(sorghum.grid)[4] <- "RURPD"
sorghum.grid$RURPD <- ifelse ( is.na(sorghum.grid$RURPD)=="TRUE", mean(sorghum.grid$RURPD,na.rm=TRUE),sorghum.grid$RURPD)
mean(sorghum.grid$RURPD)
median(sorghum.grid$RURPD)
max(sorghum.grid$RURPD)
min(sorghum.grid$RURPD)
sqrt(var(sorghum.grid$RURPD))

# transforming distnace to KM
names(sorghum.grid)[5]
# names(sorghum.grid)[5] <- "MARKD"
# sorghum.grid$markd <- sorghum.grid$markd/1000 
# mean(sorghum.grid$MARKD)
# median(sorghum.grid$MARKD)
# max(sorghum.grid$MARKD)
# min(sorghum.grid$MARKD)
# sqrt(var(sorghum.grid$MARKD))

#replace NA values with mean
names(sorghum.grid)[6]
names(sorghum.grid)[6] <- "NDVI.PC2"
sorghum.grid$NDVI.PC2 <- ifelse ( is.na(sorghum.grid$NDVI.PC2)=="TRUE", mean(sorghum.grid$NDVI.PC2,na.rm=TRUE),sorghum.grid$NDVI.PC2)
mean(sorghum.grid$NDVI.PC2)
median(sorghum.grid$NDVI.PC2)
max(sorghum.grid$NDVI.PC2)
min(sorghum.grid$NDVI.PC2)
sqrt(var(sorghum.grid$NDVI.PC2))
skewness(sorghum.grid$NDVI.PC2)

#replace NA values with mean
names(sorghum.grid)[7]
names(sorghum.grid)[7] <- "ELEV"
sorghum.grid$ELEV <- ifelse ( is.na(sorghum.grid$ELEV)=="TRUE", mean(sorghum.grid$ELEV,na.rm=TRUE),sorghum.grid$ELEV)
mean(sorghum.grid$ELEV)
median(sorghum.grid$ELEV)
max(sorghum.grid$ELEV)
min(sorghum.grid$ELEV)
sqrt(var(sorghum.grid$ELEV))
skewness(sorghum.grid$ELEV)

#replace NA values with mean
names(sorghum.grid)[8]
names(sorghum.grid)[8] <- "SLOPE"
sorghum.grid$SLOPE <- ifelse ( is.na(sorghum.grid$SLOPE)=="TRUE", mean(sorghum.grid$SLOPE,na.rm=TRUE),sorghum.grid$SLOPE)
mean(sorghum.grid$SLOPE)
median(sorghum.grid$SLOPE)
max(sorghum.grid$SLOPE)
min(sorghum.grid$SLOPE)
sqrt(var(sorghum.grid$SLOPE))
skewness(sorghum.grid$SLOPE)

#replace NA values with mean
names(sorghum.grid)[9]
names(sorghum.grid)[9] <- "SOIL"
sorghum.grid$SOIL <- ifelse ( is.na(sorghum.grid$SOIL)=="TRUE", 0 ,sorghum.grid$SOIL)
skewness(sorghum.grid$SOIL)
sorghum.grid$SOIL
str(sorghum.grid@coords)

# gridded(sorghum.grid) <- TRUE
# sorghum.grid <- as(sorghum.grid, "SpatialGridDataFrame")
# proj4string(sorghum.grid.mlr) <- sorghum.mlr@proj4string
# 
# p1 <- spplot(sorghum.grid, "NDVI.PC1", asp=1, col.regions=grey(0:256/256), cuts=256)
# p2 <- spplot(sorghum.grid, "PERC", asp=1, col.regions=grey(0:256/256), cuts=256)
# p3 <- spplot(sorghum.grid, "ELEV", asp=1, col.regions=grey(0:256/256), cuts=256)
# 
# windows(5, 7)
# par(mfrow=c(3,1))
# plot(p1, split=c(1,1,1,3), more=T)
# plot(p2, split=c(1,2,1,3), more=T)
# plot(p3, split=c(1,3,1,3), more=F)
# 
# p1 <- spplot(sorghum.grid, "PHCR", asp=1, col.regions=grey(0:256/256), cuts=256)
# p2 <- spplot(sorghum.grid, "RURPD", asp=1, col.regions=grey(0:256/256), cuts=256)
# p3 <- spplot(sorghum.grid, "MARKD", asp=1, col.regions=grey(0:256/256), cuts=256)
# 
# windows(5, 7)
# par(mfrow=c(3,1))
# plot(p1, split=c(1,1,1,3), more=T)
# plot(p2, split=c(1,2,1,3), more=T)
# plot(p3, split=c(1,3,1,3), more=F)


#---------------------------------------

##---LODAING SORGHUM OBSERVATIONS


#----------------------------------------

# load the data:

sorghum <- read.csv("sorghum.csv")

sorghum <- readShapePoints("sorghum.shp")

state.ll83 <- spTransform(states, CRS("+proj=longlat +ellps=GRS80"))

str(sorghum)
# convert to a "SpatialPointsDataFrame":
coordinates(sorghum) <- ~x+y

prj = "+proj=tmerc +lat_0=0 +lon_0=18 +k=0.9999 +x_0=6500000 +y_0=0 
  +ellps=bessel +units=m 
  +towgs84=550.499,164.116,475.142,5.80967,2.07902,-11.62386,0.99999445824"




data(eberg)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
eberg <- eberg[runif(nrow(eberg))<.2,]
bubble(eberg["CLYMHT_A"])

which in plotKML looks like exactly the same:
  
plotKML(eberg["CLYMHT_A"])

min(sorghum$y2009)
max(sorghum$y2009)
mean(sorghum$y2009)
median(sorghum$y2009)
quantile(sorghum$y2009)


# #writePointsShape(sorghum, "sorghum_observed.shp")
# # 10 years average (2000-2009)
# 
# avg_yield_2000_2009 <- (829.5+927.8+925.2+960.4+973.0+1091.9+1139.3+937.4+985.9+920.4)/10
# avg_yield_2000_2009
# 
# average<-sum(sorghum$y2009)/210
# average

# Study area

sorghum.poly <- readShapePoly("departements.shp")
sorghum.poly <- readShapePoly("PROVINCES.shp")
proj4string(sorghum.poly) <- sorghum.grid@proj4string

(q8 <- quantile(sorghum$y2009, seq(0, 1, length = 9)))
sorghum.c <- cut(sorghum$y2009, q8, include.lowest = T, labels = F)
sort(unique(sorghum.c))
q8
###
windows(6,6)
x <- sorghum$y2009
hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency",
probability = TRUE, main = "Gaussian kernel",
border = "black")
lines(density(x, width = 350), lwd = 3)
rug(x)

hist(sorghum$y2009, xlab = "Sorghum yield", ylab = "Frequency",
     probability = TRUE, main = "Gaussian kernel",
     border = "gray")
lines(density(sorghum$y2009, width = 12), lwd = 2)
# plot sorghum samples in the study area
X11()
bubble(sorghum, zcol="y2009", col="black", maxsize=2.5, key.entries=q8,
       sp.layout=list("sp.polygons", sorghum.poly), main="Sorghum yield (kg ha-1) data")
X11()
spplot(sorghum, zcol="y2009", cuts=8, cex=1.6*sorghum$y2009/max(sorghum$y2009), col.regions=topo.colors(256)(8), main="sorghum yield (kg ha-1)", 
       sub="Symbol size proportional to yield", key.space="right", sp.layout=list("sp.polygons", sorghum.poly))
warnings()

#spatial patterns

ts1 <- lm(sorghum$y2009 ~ coordinates(sorghum))
summary(ts1)
# 56% of sorghum yield is explained by the coordinates
# There is a trend; but are there hotspots?

plot(density(sorghum$y2009), col = "darkblue", main = "sorghum yield",
       lwd = 1.5)

#test logit transformation

plot(density(log1p(sorghum$y2009)), col = "darkblue", main = "sorghum yield",
     lwd = 1.5)

rug(sorghum$y2009, col = "darkgreen")

hist(sorghum$y2009,main="Histogram of observed data")

fitdistr(sorghum$y2009,"normal")

x.norm<-rnorm(n=210,m=961.3,sd=234.7)
plot(density(x.norm), col = "darkblue", main = "sorghum yield",
     lwd = 1.5)

hist(x.norm,main="Histogram of normal data")
qqplot(sorghum$y2009,x.norm) ## QQ-plot

windows(6,6)
qqplot(sorghum$y2009, x.norm, plot.it = TRUE, xlab = deparse(substitute(sorghum$y2009)),
       ylab = deparse(substitute(x.norm)))
abline(0,1) ## a 45-degree reference line is plotted

#descriptive analysis (normality tests) sorghum crop yield
m <- mean(sorghum$y2009)
sd <- sd(sorghum$y2009)

#Standard error of skewness 
se.sk <- sqrt(6/length(sorghum$y2009))
1.96*se.sk
skewness(sorghum$y2009)
ks.test(sorghum$y2009, "pnorm", m, sd)

ks.test(x.norm, sorghum$y2009)


x<- sorghum$y2009
#MAE

sum(x -mean(x))/length(x)

#MSE

sqrt(sum((x -mean(x))^2)/length(x))
windows(5,5)

h<-hist(x, breaks=11, col="red", xlab="Sorghum yield kg ha-1",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=60)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 

#------------------------------------------------

#---- OVERLAY

#--------------------------------------------------

str(sorghum.grid)
#overlay with sorghum data to get the values of the grids at the regression points (sampling points)

sorghum.ov <- data.frame(sorghum,extract(s,sorghum))
str(sorghum.ov)
names(sorghum.ov)[6] <- "MARKD"
names(sorghum.ov)[7] <- "NDVI.PC1"
names(sorghum.ov)[10] <- "PERC"
# names(sorghum.ov)[11] <- "NDVI.PC1"
names(sorghum.ov)[12] <- "PHCR"
names(sorghum.ov)[13] <- "RURPD"
names(sorghum.ov)[15] <- "NDVI.PC2"
names(sorghum.ov)[16] <- "ELEV"
names(sorghum.ov)[17] <- "SLOPE"
coordinates(sorghum.ov) <- ~x+y
proj4string(sorghum.ov) <- sorghum.grid@proj4string
str(sorghum.ov)

names(sorghum.grid)[2]
names(sorghum.grid)[2] <- "NDVI.PC1"
str(sorghum.grid)

names(sorghum.grid)[5]
names(sorghum.grid)[5] <- "MARKD"
sorghum.grid$MARKD <- sorghum.grid$MARKD/1000 


writePointsShape(sorghum.ov, "sorghum_overlay.shp")
write.csv(sorghum.ov, "sorghum_overlay.csv")
  
mean(sorghum.ov$y2009)
median(sorghum.ov$y2009)
max(sorghum.ov$y2009)
min(sorghum.ov$y2009)
sqrt(var(sorghum.ov$y2009))

#writePointsShape(sorghum.ov, "sorghum_overlay.shp")

#------------------------------------------------------

#----------- EXPLANATORY ANALYSIS

#------------------------------------------------------


# Point pattern statistics:
str(sorghum.grid)
sorghum.grid <- as(sorghum.grid, "SpatialPixelsDataFrame")
mg_owin <- as.owin(sorghum.grid["NDVI.PC1"])
sorghum.ppp <- ppp(x=coordinates(sorghum)[,1], y=coordinates(sorghum)[,2], marks=sorghum$y2009, window=mg_owin)

# plot(sorghum.ppp)
dist.points <- nndist(sorghum.ppp)
summary(dist.points)

# Complete Spatial Randomness:
env.sorghum <- envelope(sorghum.ppp, fun=Gest, global=TRUE)
windows(w = 8, h = 6)
plot(env.sorghum, lwd=list(3,1,1,1), main="CRS test (sorghum yield)", col="black", cex.axis = 1.5, cex.lab=1.5)
?plot
?envelope
str(env.sorghum)
max(env.sorghum$hi)
min(env.sorghum$lo)
env.sorghum$lo
?ppp
# predictors analysis

# hist(sorghum.grid$PERC, breaks=15, xlab="PC1 NDVI", col="grey", main="Histogram")
# # descriptive analysis NDVI.PC1
# m <- mean(sorghum.ov$PERC)
# sd <- sd(sorghum.ov$PERC)
# #Standard error of skewness 
# se.sk <- sqrt(6/length(sorghum.grid$PERC))
# 1.96*se.sk
# skewness(sorghum.grid$PERC)
# cor(sorghum.ov$PERC, sorghum.ov$y2009, use="complete.obs") # avearge rainfall
# 
# 
# hist(sorghum.grid$NDVI.PC1, breaks=15, xlab="PC1 NDVI", col="grey", main="Histogram")
# # descriptive analysis NDVI.PC1
# m <- mean(sorghum.ov$NDVI.PC1)
# sd <- sd(sorghum.ov$NDVI.PC1)
# #Standard error of skewness 
# se.sk <- sqrt(6/length(sorghum.grid$NDVI.PC1))
# 1.96*se.sk
# skewness(sorghum.grid$NDVI.PC1)
cor(sorghum.ov$NDVI.PC1, sorghum.ov$y2009, use="complete.obs", method = "pearson")
# ?cor
# # Correlations with significance levels
# library(Hmisc)
# rcorr(as.matrix(sorghum.ov$NDVI.PC1), type="pearson") # type can be pearson or spearma
# 
# hist(sorghum.grid$PHCR, breaks=15, xlab="PHCR", col="grey", main="Histogram")
# # descriptive analysis PHCR
# m <- mean(sorghum.grid$PHCR)
# sd <- sd(sorghum.grid$PHCR)
# #Standard error of skewness 
# se.sk <- sqrt(6/length(sorghum.grid$PHCR))
# 1.96*se.sk
# skewness(sorghum.grid$PHCR)
cor(sorghum.ov$PHCR, sorghum.ov$y2009, use="complete.obs")
# 
# hist(sorghum.grid$RURPD, breaks=15, xlab="RURPD", col="grey", main="Histogram")
# # descriptive analysis PHCR
# m <- mean(sorghum.grid$RURPD)
# sd <- sd(sorghum.grid$RURPD)
# #Standard error of skewness 
# se.sk <- sqrt(6/length(sorghum.grid$RURPD))
# 1.96*se.sk
# skewness(sorghum.grid$RURPD)
cor(sorghum.ov$RURPD, sorghum.ov$y2009, use="complete.obs")
# 
# # descriptive analysis markd
# m <- mean(sorghum.grid$markd)
# sd <- sd(sorghum.grid$markd)
# #Standard error of skewness 
# se.sk <- sqrt(6/length(sorghum.grid$markd))
# 1.96*se.sk
# skewness(sorghum.ov$markd)
cor(sorghum.ov$markd, sorghum.ov$y2009, use="complete.obs")
# 
# # descriptive analysis markd
# m <- mean(sorghum.grid$NDVI.PC2)
# sd <- sd(sorghum.grid$NDVI.PC2)
# #Standard error of skewness 
# se.sk <- sqrt(6/length(sorghum.grid$NDVI.PC2))
# 1.96*se.sk
# skewness(sorghum.ov$NDVI.PC2)
cor(sorghum.ov$y2009, sorghum.ov$NDVI.PC2, use="complete.obs")
# 
# # descriptive analysis markd
# m <- mean(sorghum.grid$ELEV)
# sd <- sd(sorghum.grid$ELEV)
# #Standard error of skewness 
# se.sk <- sqrt(6/length(sorghum.grid$ELEV))
# 1.96*se.sk
# skewness(sorghum.ov$ELEV)
cor(sorghum.ov$y2009, sorghum.ov$ELEV, use="complete.obs")
# 
# # descriptive analysis markd
# m <- mean(sorghum.grid$SLOPE)
# sd <- sd(sorghum.grid$SLOPE)
# #Standard error of skewness 
# se.sk <- sqrt(6/length(sorghum.grid$SLOPE))
# 1.96*se.sk
# skewness(sorghum.ov$SLOPE)
cor(sorghum.ov$y2009, sorghum.ov$SLOPE, use="complete.obs")
# 

# ----------------------------------------------------------------------------
### PART II - Regression Kriging with OLS/GLM (RK) 
# ---------------------------------------------------------------------------

# PART (a) - Predicting using GLM

str(sorghum.grid)
# OLS regression

vif(lm(y2009~NDVI.PC1+PERC+ELEV+PHCR+RURPD+MARKD, data=sorghum.ov))

str(sorghum.ov)
lm.sorghum = lm(y2009~NDVI.PC1+PERC+ELEV+PHCR+RURPD+MARKD, data=sorghum.ov)
summary(lm.sorghum)
anova(lm.sorghum)

?moranplotmap
glm.sorghum <- glm(y2009~NDVI.PC1+PERC+ELEV+PHCR+RURPD+MARKD, data=sorghum.ov)
summary(glm.sorghum)
anova(glm.sorghum)
str(glm.sorghum)

# Estimate of the relative importance of the independent variables for the 0.01 degree OLS regression, including 95 percent confidence limits.
# method LMG
windows(6,6)
lmg <- calc.relimp(lm.sorghum, type="lmg", rela = TRUE, main = "Relative importance of orghum predictors", col = "black")
plot(lmg)

# all mrthods
crlm <- calc.relimp(step.lm.sorghum,  
                    type = c("lmg", "last", "first", "betasq", "pratt", "genizi", "car"), rela = TRUE )
plot(crlm)


#Assumptions, required for linear regression

# 1- Homoscedasticity - the variance of the error term must be constant for each estimat  
# value (thier should not be obvious pattern between estimated crop yield and the
# residuals)
# To check this the residuals were plotted against y.
#A linear model must satisfy several assumptions, among which are:
#1. no relation between predicted values and residuals (i.e., errors are independent);
#2. normal distribution of residuals;
#3. homoscedascity, i.e,. variance of residuals does not depend on the fitted value.
windows(5,5)
scatter.smooth(lm.sorghum$fitted.values, step.lm.sorghum$residuals, span=18/19, col="black", xlab="OLS fitted values", ylab="OLS Residuals")
cor(lm.sorghum$fitted.values, step.lm.sorghum$residuals)

windows(10,4)
par(mfrow = c(1, 3))
plot(step.lm.sorghum, which = c(1, 2, 5))
par(mfrow = c(1, 1))


# 2- Independence of error terms - successive residuals should not be spatiaaly correlated.

## Residual Autocorrelation

## Contiguity Neighbors
coords <- coordinates(sorghum.ov)
col.rel.nb<- graph2nb(relativeneigh(coords), sym=TRUE)
W_cont_el_mat <- nb2listw(col.rel.nb, style="W", zero.policy=TRUE)

## autocorelation of samples

moran.test(sorghum.ov$y2009, listw=W_cont_el_mat, zero.policy=T)

## autocorelation of GLS residuals

moran.test(step.lm.sorghum$residuals, listw=W_cont_el_mat, zero.policy=T)

# normality of residuals
m <- mean(step.lm.sorghum$residuals)
sd <- sd(step.lm.sorghum$residuals)
ks.test(sorghum$y2009, "pnorm", m, sd)

# Normality of residuals - residuals should be normally distributed
hist(lm.sorghum$residuals, breaks=15, xlab="RURPD", col="grey", main="Histogram")
windows(5,5)
plot(density(lm.sorghum$residuals), col = "black", lwd = 2)


# 3- No multicollinearity - no strong correlation should be observed among the 
# independent variables. Bivariate collinearity was checrk for with scatter plots and 
# correlations between each pair of independent variables, and was assessed with a 
# variance inflation factor test.


# Remopve Outliers Function
# remove_outliers <- function(x, na.rm = TRUE, ...) {
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
#   H <- 1.5 * IQR(x, na.rm = na.rm)
#   y <- x
#   y[x < (qnt[1] - H)] <- NA
#   y[x > (qnt[2] + H)] <- NA
#   y
# }

# observe outliers

boxplot(sorghum.ov$y2009)
boxplot(sorghum.ov$NDVI.PC1)
boxplot(sorghum.ov$PERC)
boxplot(sorghum.ov$PHCR)
boxplot(sorghum.ov$RURPD)
boxplot(sorghum.ov$MARKD)
boxplot(sorghum.ov$ELEV)

  
set.seed(1)
sorghum.ov$RURPD2 <- remove_outliers(sorghum.ov$RURPD)
boxplot(sorghum.ov$RURPD2)

set.seed(1)
sorghum.ov$MARKD2 <- remove_outliers(sorghum.ov$MARKD)
boxplot(sorghum.ov$MARKD2)
## dev.off()



windows(6,6)
pairs(y2009~NDVI.PC1+PERC+ELEV+PHCR+RURPD2+MARKD2, sorghum.ov)


panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

names(sorghum.ov)[2] 
names(sorghum.ov)[2]<- "YIELD"

windows(6,6)
pairs(YIELD~NDVI.PC1+PERC+ELEV+PHCR+RURPD+MARKD, data=sorghum.ov,
      lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1.15, 
      pch=20, main="Sorghum Scatterplot Matrix")

names(sorghum.ov)[2]<- "y2009"
vif(lm(y2009~NDVI.PC1+ELEV+PHCR+RURPD+MARKD, data=sorghum.ov))



sorghum.ov$ELEV
?pairs
# 4 - Linearity - there should be a linear relationship between each independent 
# variable and the dependent variable. This can be assessed with a scatterplot matrix 
# for all variables. Non-linearity does not # invalidate the OLS model but it does 
# mean that the beta coefficients cannot fully capture the relationship. 


windows(width = 6, height = 6)
par.ori <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
scatter.smooth(sorghum.ov$NDVI.PC1, sorghum.ov$y2009, span=18/19, col="black", xlab="PC1 NDVI", ylab="Sorghum yield (kg ha-1)")
scatter.smooth(sqrt(sorghum.ov$PERC), sorghum.ov$y2009, span=18/19, col="black", xlab="Percipitation", ylab="Sorghum yield (kg ha-1)")
scatter.smooth(sorghum.ov$RURPD, sorghum.ov$y2009, span=18/19, col="black", xlab="Rural population density", ylab="Sorghum yield (kg ha-1)")
scatter.smooth(sorghum.ov$ELEV, sorghum.ov$y2009, span=18/19, col="black", xlab="Rural population density", ylab="Sorghum yield (kg ha-1)")


#------------------------------

# Predict using LM

#------------------------------
sorghum.grid.mlr <- as(sorghum.grid, "SpatialPointsDataFrame")

str(sorghum.grid.mlr)
lm.sorghum

# predict at all locations:

lm.fit <- predict(lm.sorghum, newdata=sorghum.grid.mlr, se.fit=TRUE) 

str(lm.fit)

windows(6,6)
plot(density(lm.fit$fit), col = "darkblue", main = "sorghum yield",
     lwd = 1.5)

min(lm.fit$fit)
max(lm.fit$fit)
mean(lm.fit$fit)
median(lm.fit$fit)
quantile(lm.fit$fit)
sd(lm.fit$fit)

min(lm.fit$se.fit)
max(lm.fit$se.fit)
mean(lm.fit$se.fit)
median(lm.fit$se.fit)
sd(lm.fit$se.fit)

mean(sorghum.ov$y2009)

(mean(sorghum.ov$y2009)-mean(lm.fit$fit))

# attach spatial reference:
sorghum.grid.mlr$pred.mlr <- lm.fit$fit
sorghum.grid.mlr$var.mlr <- lm.fit$se.fit

#MAE
x<- lm.fit$fit

sum(x -mean(x))/length(x)

#MSE

sqrt(sum((x -mean(x))^2)/length(x))


gridded(sorghum.grid.mlr) <- TRUE
#sorghum.gls <- as(sorghum.gls, "SpatialGridDataFrame")
proj4string(sorghum.grid.mlr) <- sorghum.grid@proj4string


#----------------------------------------------------------------

### GWR Regression AND PREDICTION

#-------------------------------------------------------------------

#GWR regression

str(sorghum.grid)
sorghum.grid.gwr <- as(sorghum.grid, "SpatialPointsDataFrame")
str(sorghum.grid.gwr)
str(sorghum.ov)

#gwr bandwidth 
?gwr.sel
#NO DIFFERENCE BETWEEN LM AND GLM HERE
bwG <- gwr.sel(lm.sorghum, data=sorghum.ov, adapt=TRUE, method = "CV", verbose=T)
bwG 
?gwr
mod.gwr.r <- gwr(lm.sorghum, data=sorghum.ov, adapt= bwG, bandwidth=bwG, hatmatrix=T)
mod.gwr.r
anova(mod.gwr.r)
BFC02.gwr.test(mod.gwr.r)
moran.test(mod.gwr.r$SDF$gwr.e, listw=W_cont_el_mat, zero.policy=T)


mod.gwr.r$SDF$gwr.e

windows(6,6)
scatter.smooth(mod.gwr.r$SDF$pred,  mod.gwr.r$SDF$gwr.e, span=18/19, col="black", xlab="OLS fitted values", ylab="OLS Residuals")

cor(mod.gwr.r$SDF$pred,  mod.gwr.r$SDF$gwr.e)

# plot residuals

windows(6,6)
plot(density( mod.gwr.r$SDF$gwr.e), col = "darkblue", main = "GWR residuals",
     lwd = 1.5)

se.sk <- sqrt(6/length(mod.gwr.r$SDF$pred))
.05*mean(mod.gwr.r$SDF$pred)
1.96*se.sk
skewness(mod.gwr.r$SDF$gwr.e)

##GWR prediction


mod.gwr <- gwr(formula=lm.sorghum, data = sorghum.ov, bandwidth=bwG, adapt=bwG,
               fit.points = sorghum.grid.gwr, predictions=TRUE, se.fit.CCT=TRUE, se.fit=TRUE, hatmatrix = TRUE, 
               fittedGWRobject=mod.gwr.r)


str(mod.gwr)
names(mod.gwr)
names(mod.gwr.r$SDF)

windows(6,6)
plot(density(mod.gwr$SDF$pred), col = "darkblue", main = "sorghum yield",
     lwd = 1.5)

windows(5,5)
plot(density(mod.gwr.r$SDF$gwr.e), col = "black", lwd = 2)



str(mod.gwr$SDF$pred)
min(mod.gwr$SDF$pred)
max(mod.gwr$SDF$pred)
mean(mod.gwr$SDF$pred)
median(mod.gwr$SDF$pred)
sd(mod.gwr$SDF$pred)

#MAE
x<- mod.gwr$SDF$pred

sum(x -mean(x))/length(x)

#MSE

sqrt(sum((x -mean(x))^2)/length(x))


#-------------- prediction maps

#rm('mod.gwr')
windows(width = 10, height = 8)
brks <- round(fivenum(mod.gwr$SDF$pred), digits=2)
cols <- rev(grey.colors(4))
plot(mod.gwr$SDF, col=cols[findInterval(mod.gwr$SDF$pred, brks,all.inside=TRUE)])
legend("bottom", legend = leglabs(brks,"<", ">="), 
       fill = cols,bty = "n", cex = 0.9, y.intersp = 0.9, title="Predicted yield")

windows(width = 10, height = 8)
brks <- round(fivenum(mod.gwr$SDF$pred.se), digits=2)
cols <- rev(grey.colors(4))
plot(mod.gwr$SDF, col=cols[findInterval(mod.gwr$SDF$pred.se, brks,all.inside=TRUE)])
legend("bottom", legend = leglabs(brks,"<", ">="), 
       fill = cols,bty = "n", cex = 0.9, y.intersp = 0.9, title="standard error")


#-------------Statistics

str(mod.gwr$SDF$pred)
min(mod.gwr$SDF$pred)
max(mod.gwr$SDF$pred)
mean(mod.gwr$SDF$pred)
median(mod.gwr$SDF$pred)
quantile(mod.gwr$SDF$pred)
sd(mod.gwr$SDF$pred)

str(mod.gwr$SDF$pred.se)
min(mod.gwr$SDF$pred.se)
max(mod.gwr$SDF$pred.se)
mean(mod.gwr$SDF$pred.se)
median(mod.gwr$SDF$pred.se)
quantile(mod.gwr$SDF$pred.se)
sd(mod.gwr$SDF$pred.se)

# prediction variance

mean(sqrt(var(mod.gwr$SDF$pred.se)))
sd(mod.gwr$SDF$pred.se)

# return back to grid and attach spatial reference:
sorghum.grid.gwr$pred.gwr <- mod.gwr$SDF$pred
sorghum.grid.gwr$var.gwr <- mod.gwr$SDF$pred.se
gridded(sorghum.grid.gwr) <- TRUE
sorghum.grid.gwr <- as(sorghum.grid.gwr, "SpatialGridDataFrame")
proj4string(sorghum.grid.gwr) <- sorghum.grid@proj4string
str(sorghum.grid.gwr)


#Sorghum yield predictions map (GWR)
windows(w = 8, h = 6)
spplot(sorghum.grid.gwr["sorghum_yield"], col.regions=topo.colors(256)(), scales = list(draw = T), 
       main = "Sorghum yield map (GWR)")

savePlot(filename = "Sor_GWR.tif", type = "tiff")
savePlot(filename = "Sor_GWR.eps", type = "eps")

#Sorghum yield prediction variance map (GWR)

windows(w = 8, h = 6)
spplot(sorghum.grid.gwr.grid["sorghum_yield_var"], col.regions=topo.colors(256)(),scales = list(draw = T),
       main = "Uncertainity map (GWR)", sp.layout=list("sp.points",sorghum,pch=1))

savePlot(filename = "Sor_GWR_sd.tif", type = "tiff")
savePlot(filename = "Sor_GWR_sd.eps", type = "eps")



## Analyzing GWR Coefficients

## Create blue-state red-state palette

br.palette <- colorRampPalette(c("blue", "red"), space = "rgb")
br.palette(5)
pal <- br.palette(n=5)

windows(height=8, width=8)
par(mfrow=c(1,1))
pairs(as(mod.gwr$SDF, "data.frame")[,2:8], pch=".")

windows(height=8, width=8)
par(mfrow=c(1,1))
sel.pairs(as(sorghum$y2009, "data.frame"), outl = rep(0, nrow(x)), 
          sel = rep(0,nrow(x)), labs = NULL, log = TRUE)

#Intercept

col_intercept = mod.gwr$SDF[[2]]
head(col_intercept)
hist(col_intercept)
#classes_fx <- classIntervals(col_intercept, n=5, style="fixed", fixedBreaks=c(-2000,-1800,-1200,-1000,-800,-600,-400,-200,-100,100,200,400,600,800,1000,1200,1400,1600,1800,2000,1100,1200,1300), rtimes = 1)
#cols <- findColours(classes_fx,pal)

brks <- round(fivenum(col_intercept), digits=2)
cols <- rev(heat.colors(4))

X11()
par(mfrow=c(1,1))
#plot(mod.gwr$SDF, col=cols)
plot(mod.gwr$SDF, col=cols[findInterval(col_intercept, brks,all.inside=TRUE)])
title(main="Map of Intercept")
legend("bottom", legend = leglabs(brks,"<", ">="),
       fill = cols,bty = "n", cex = 0.9, y.intersp = 0.9, title="Intercept")

#PC1
mod.gwr$SDF

col_NDVI.PC1 = mod.gwr$SDF[[3]]
head(col_NDVI.PC1)
hist(col_NDVI.PC1)

brks <- round(fivenum(col_NDVI.PC1), digits=2)
brks
cols <- rev(heat.colors(4))

X11()
par(mfrow=c(1,1))
#plot(mod.gwr$SDF, col=cols)
plot(mod.gwr$SDF, col=cols[findInterval(col_NDVI.PC1, brks,all.inside=TRUE)])
title(main="Map of PC1")
legend("bottom", legend = leglabs(brks,"<", ">="),
       fill = cols,bty = "n", cex = 0.9, y.intersp = 0.9, title="PC1")
dev.off()

#PC2
mod.gwr$SDF

col_NDVI.PC1 = mod.gwr$SDF[[4]]
head(col_NDVI.PC1)
hist(col_NDVI.PC1)

brks <- round(fivenum(col_NDVI.PC1), digits=2)
brks
cols <- rev(heat.colors(4))

X11()
par(mfrow=c(1,1))
#plot(mod.gwr$SDF, col=cols)
plot(mod.gwr$SDF, col=cols[findInterval(col_NDVI.PC1, brks,all.inside=TRUE)])
title(main="Map of PC2")
legend("bottom", legend = leglabs(brks,"<", ">="),
       fill = cols,bty = "n", cex = 0.9, y.intersp = 0.9, title="PC2")
dev.off()


#------------------------------------------

# Prediction maps OLS and GWR

#-------------------------------------

sorghum.grid.gwr <- as(sorghum.grid.gwr, "SpatialPointsDataFrame")
sorghum.grid.mlr <- as(sorghum.grid.mlr, "SpatialPointsDataFrame")

quantile(sorghum.grid.mlr$pred.mlr)
quantile(sorghum.grid.gwr$pred.gwr)

zmax <- round(max(sorghum.grid.gwr$pred.gwr, sorghum.grid.mlr$pred.mlr), 0) + 0
zmax
zmin <- round(min(sorghum.grid.gwr$pred.gwr, sorghum.grid.mlr$pred.mlr), 0) - 0
zmin
ramp <- seq(from=zmin, to=zmax, by=(zmax-zmin)/8)
ramp

gridded(sorghum.grid.gwr) <- TRUE
sorghum.grid.gwr <- as(sorghum.grid.gwr, "SpatialGridDataFrame")
proj4string(sorghum.grid.gwr) <- sorghum.grid@proj4string

gridded(sorghum.grid.mlr) <- TRUE
sorghum.grid.mlr <- as(sorghum.grid.mlr, "SpatialGridDataFrame")
proj4string(sorghum.grid.mlr) <- sorghum.grid@proj4string


p1 <- spplot(sorghum.grid.mlr, "pred.mlr", asp=0, col.regions=bpy.colors(10)[10:1], axes=TRUE, at=ramp)
p2 <- spplot(sorghum.grid.gwr, "pred.gwr", asp=0, col.regions=bpy.colors(10)[10:1], axes=TRUE, at=ramp)

windows(5, 6.666)
par(mfrow=c(2,1))
plot(p1, split=c(1,1,1,2), more=T)
plot(p2, split=c(1,2,1,2), more=F)

?spplot


#---- prediction variance of error

sorghum.grid.gwr <- as(sorghum.grid.gwr, "SpatialPointsDataFrame")
sorghum.grid.mlr <- as(sorghum.grid.mlr, "SpatialPointsDataFrame")


max(sorghum.grid.gwr$var.gwr)
sd(sorghum.grid.gwr$var.gwr)
max(sorghum.grid.mlr$var.mlr)
min(sorghum.grid.gwr$var.gwr)
min(sorghum.grid.mlr$var.mlr)
sd(sorghum.grid.mlr$var.mlr)

zmax <- round(max(sorghum.grid.gwr$var.gwr, sorghum.grid.mlr$var.mlr), 0) + 0
zmax
zmin <- round(min(sorghum.grid.gwr$var.gwr, sorghum.grid.mlr$var.mlr), 0) - 0
zmin
ramp <- seq(from=zmin, to=zmax, by=(zmax-zmin)/8)
ramp

gridded(sorghum.grid.gwr) <- TRUE
sorghum.grid.gwr <- as(sorghum.grid.gwr, "SpatialGridDataFrame")
proj4string(sorghum.grid.gwr) <- sorghum.grid@proj4string

gridded(sorghum.grid.mlr) <- TRUE
sorghum.grid.mlr <- as(sorghum.grid.mlr, "SpatialGridDataFrame")
proj4string(sorghum.grid.mlr) <- sorghum.grid@proj4string


p1 <- spplot(sorghum.grid.mlr, "var.mlr", asp=0, col.regions=bpy.colors(10)[10:1], axes=TRUE, at=ramp)
p2 <- spplot(sorghum.grid.gwr, "var.gwr", asp=0, col.regions=bpy.colors(10)[10:1], axes=TRUE, at=ramp)

windows(5, 6.666)
par(mfrow=c(2,1))
plot(p1, split=c(1,1,1,2), more=T)
plot(p2, split=c(1,2,1,2), more=F)


# ----------------------------------------------------------------------------

### PART 1 - Variogram Modelling (OK) 

# ---------------------------------------------------------------------------

gsoryield <- gstat(formula = y2009~1, data = sorghum.ov)
sorghum.svar <- variogram(gsoryield)

#sorghum.svar <- variogram(gsoryield, width=100)
plot(sorghum.svar)
# compute experimental variogram and fit function: (gstat automatic fitting)
#show.vgms()
sorghum.ivgm <- vgm(nugget=0, model="Sph", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + diff(sorghum.ov@bbox["y",])^2)/4, psill=var(sorghum.ov$y2009))
sorghum.ivgm # experimental variogram for CV and OK
print(plot(sorghum.svar, pl = T, model = sorghum.ivgm))
sorghum.vgm <- fit.variogram(sorghum.svar, model=sorghum.ivgm) 
sorghum.vgm # experimental variogram for CV and OK
# print(plot(sorghum.vgm, pl = T, model = sorghum.ivgm))
plot(sorghum.svar, sorghum.vgm)
#from arcgis
# sorghum.vgm[1,2] <- 0
# sorghum.vgm[2,2] <- 33250.838662171926
# sorghum.vgm[2,3] <- 117084.00871083647
plot(sorghum.svar, sorghum.vgm)
sorghum.vgm.plt <- plot(sorghum.svar, sorghum.vgm, pch="+", pl=TRUE,  col="black", main="Sorghum yield")
windows(5,6)
plot(sorghum.vgm.plt)
#---------------------------------------------------------------------
# cross valiation for oridinary kriging (OK)
#-----------------------------------------------------------------------
sorghum.ok.xv <- krige.cv(y2009~1, sorghum.ov, sorghum.vgm)
names(sorghum.ok.xv)
str(sorghum.ok.xv)

windows(width = 4, height = 4)
hist(sorghum.ok.xv$var1.pred)

mean(sorghum.ok.xv$zscore)
sd(sorghum.ok.xv$zscore)

head(sorghum.ok.xv$residual)
tail(sorghum.ok.xv$residual)

windows(width = 8, height = 6)
bubble(sorghum.ok.xv, zcol="residual", scales=list(draw=T), col="grey", maxsize=1.5,
       key.entries=c(-200,-150,-100,-75,-50,-25,0,25,50,75,100,125,150,200), 
       sp.layout=list("sp.polygons", sorghum.poly), main = "cross-validation residuals, OK")

names(sorghum.ok.xv)

# amount of variation explained by the OK model:
1-var(sorghum.ok.xv$residual, na.rm=T)/var(sorghum$y2009)

# Calculate the error, rmse and mean error

#error

sorghum.ok.xv$residual

# Mean error

sum(sorghum.ok.xv$residual)/length(sorghum.ok.xv$residual)

#rmse

sqrt(sum(sorghum.ok.xv$residual^2)/length(sorghum.ok.xv$residual))
#-------------------------------------------------------------------
### Ordinary krigging (OK)
#-------------------------------------------------------------------

sorghum.grid.ok <- as(sorghum.grid, "SpatialPointsDataFrame")

# block kriging
#sorghum.ok <- krige(y2009~1, sorghum, block = c(1000,1000), newdata = output.grid, sorghum.vgm)
# point kriging
sorghum.ok <- krige(y2009~1, sorghum.ov, newdata = sorghum.grid.ok, sorghum.vgm)
names(sorghum.ok)

str(sorghum.ok$var1.pred)
min(sorghum.ok$var1.pred)
max(sorghum.ok$var1.pred)
mean(sorghum.ok$var1.pred)
median(sorghum.ok$var1.pred)
quantile(sorghum.ok$var1.pred)

sd(sorghum.ok$var1.pred)
sqrt(var(sorghum.ok$var1.pred))

#mean of OK prediction variance
mean(sqrt(sorghum.ok$var1.var))
#assigning the spatial data frame and projecton system again

#MAE
x<- sorghum.ok$var1.pred

sum(x -mean(x))/length(x)

#MSE

sqrt(sum((x -mean(x))^2)/length(x))

gridded(sorghum.ok) <- TRUE
sorghum.ok <- as(sorghum.ok, "SpatialGridDataFrame")
proj4string(sorghum.ok) <- sorghum.grid@proj4string
names(sorghum.ok)
str(sorghum.ok)


# plot and save kriging maps:
## Sorghum yield predictions map
windows(w = 8, h = 6)
spplot(sorghum.ok["var1.pred"], col.regions=topo.colors(256),scales = list(draw = T),
       main = "Sorghum yield map (OK)")

savePlot(filename = "Sor_okpred.tif", type = "tiff")
savePlot(filename = "Sor_okpred.eps", type = "eps")

# 
# rainbow(n, s = 1, v = 1, start = 0, end = max(1,n - 1)/n, alpha = 1)
# heat.colors(n, alpha = 1)
# terrain.colors(n, alpha = 1)
# topo.colors(n, alpha = 1)
# cm.colors(n, alpha = 1)
# standard deviatian of prediction = sqrt of prediction variance = prediction error


#standard deviations or uncertainity map
sorghum.ok$var1.sd <- sqrt(sorghum.ok$var1.var)

str(sorghum.ok$var1.sd)
min(sorghum.ok$var1.sd)
max(sorghum.ok$var1.sd)
mean(sorghum.ok$var1.sd)
median(sorghum.ok$var1.sd)
sd(sorghum.ok$var1.sd)

windows(w = 5, h = 5)
plot(density(sqrt(sorghum.ok$var1.sd)), col = "black", main = "sorghum OK SD",
     lwd = 2)

windows(w = 8, h = 6)
spplot(sorghum.ok["var1.sd"], col.regions=topo.colors(256),scales = list(draw = T),
       main = "Uncertainity map (OK)", sp.layout=list("sp.points",sorghum,pch=1))

savePlot(filename = "Sor_oksd.tif", type = "tiff")
savePlot(filename = "Sor_oksd.eps", type = "eps")


# ###EXTRACT VALUES BY DEPARTMETS
# sorghum.ok.pred <- as(sorghum.ok["var1.pred"], "SpatialGridDataFrame")
# proj4string(sorghum.ok.pred) <- sorghum.grid@proj4string
# sorghum.ok.pred <- raster(sorghum.ok.pred)
# 
# deps <- Sp(sorghum.poly,over(sorghum.poly, sorghum.ov))
# ok.pred.mean <- SpatialPointsDataFrame(sorghum.poly, extract(sorghum.ok.pred, sorghum.poly, df=TRUE, fun="mean", na.rm=TRUE))
# proj4string(ok.pred.mean) <- sorghum.grid@proj4string
# class(ok.pred.mean)
# str(ok.pred.mean)
# names(ok.pred.mean)[2] <- "ok.pred.mean"
# str(ok.pred.mean)
# ok.pred.mean
# sorghum.ov
# 
# x <- over(ok.pred.mean, sorghum.o)
# 
# class(x)
# x
# 
# ok.pred.mean.poly <- SpatialPolygons(sorghum.poly, overlay(ok.pred.mean, sorghum.poly))
# 
# ?overlay
# sorghum.ov < data.frame(sorghum.ov)
# 
# sorghum.ov <- ov(sorghum.ov, ok.pred.mean)
# 
# str(sorghum.o)
# 
# coordinates(sorghum.ov) <- ~x+y
# proj4string(sorghum) <- sorghum.grid@proj4string
# str(sorghum)
# 
# 
# 
# sorghum.ok.pred.var <- as(sorghum.ok["var1.sd"], "SpatialGridDataFrame")
# proj4string(sorghum.ok.pred.var) <- sorghum.grid@proj4string
# sorghum.ok.pred.var <- raster(sorghum.ok.pred.var)
# ok.pred.meansd <- extract(sorghum.ok.pred.var, sorghum.poly, df=TRUE, fun="mean", na.rm=TRUE)
# 
# str(deps)



#-----------------------------------------------------

# Variogram of OLS residuals for MLRK and KED

#--------------------------------------------------------

names(lm.sorghum$coefficients)
# add residuals to sorghum object
plot(density(lm.sorghum$residuals), col = "darkblue", main = "sorghum yield",
     lwd = 1.5)
sorghum.ov$resid = lm.sorghum$residuals
sorghum.ov$resid
str(sorghum.ov)

## Residual Autocorrelation

moran.test(sorghum.ov$resid, listw=W_cont_el_mat, zero.policy=T)

# histogram of the residuals
plot(density(step.lm.sorghum$residuals), col = "darkblue", main = "sorghum yield",
     lwd = 1.5)

# calculate sample and experimental variograms of OLS residuals

sorghum.rsvar = variogram(resid~1, data=sorghum.ov)
show.vgms()
sorghum.ivgm <- vgm(nugget=0, model="Sph", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + 
  diff(sorghum.ov@bbox["y",])^2)/4, psill=var(residuals(step.lm.sorghum)))
sorghum.rvgm <- fit.variogram(sorghum.rsvar, model=sorghum.ivgm)
sorghum.rvgm # experimental variogram of OLS for CV and OK
#plot(sorghum.rsvar, sorghum.rvgm)
sorghum.rvgm.plt <- plot(sorghum.rsvar, sorghum.rvgm, pc="+", pl=T, col="black", main="Residuals")

windows(w = 5, h = 6)
print(sorghum.rvgm.plt, split=c(1,1,1,1), more=T)

# synchronize the two plots and interpret results:
sorghum.rvgm.plt$x.limits <- sorghum.vgm.plt$x.limits
sorghum.rvgm.plt$y.limits <- sorghum.vgm.plt$y.limits
windows(w = 8, h = 6)
print(sorghum.vgm.plt, split=c(1,1,2,1), more=TRUE)
print(sorghum.rvgm.plt, split=c(2,1,2,1), more=FALSE)

sum(sorghum.rvgm$psill)
sum(sorghum.vgm$psill)
sum(sorghum.rvgm$psill)/sum(sorghum.vgm$psill)

dev.off()


#TESTING GLS for KED

# fit a variogram for residuals:
sorghum.kvgm <- vgm(nugget=0, model="Sph", range=sqrt(diff(sorghum@bbox["x",])^2 + diff(sorghum@bbox["y",])^2)/4, psill=var(residuals(glm.sorghum)))
sorghum.krvgm <- fit.variogram(variogram(residuals(glm.sorghum)~1, sorghum.ov), model=sorghum.kvgm)
sorghum.krvgm

#gives same results as 

sorghum.rvgm

#------------------------------------------------------------

# Cross validation for KED

#-------------------------------------------------------------

str(sorghum.ov)
sorghum.ked.xv = krige.cv(formula = y2009~NDVI.PC1+PERC+RURPD+ELEV+PHCR+MARKD, sorghum.ov, sorghum.rvgm)

summary(sorghum.ked.xv)

windows(width = 4, height = 4)
hist(sorghum.ked.xv$var1.pred)

mean(sorghum.ked.xv$zscore)
sd(sorghum.ked.xv$zscore)

windows(width = 4, height = 4)
hist(sorghum.ked.xv$zscore)

windows(width = 8, height = 6)
bubble(sorghum.ked.xv, zcol="residual", scales=list(draw=T), col="blue", maxsize=1.5,
       key.entries=c(-200,-150,-100,-75,-50,50,75,100,125,150,200), 
       sp.layout=list("sp.polygons", sorghum.poly), main = "cross-validation residuals, trend removed")


names(sorghum.ked.xv)

# amount of variation explained by the OK model:
1-var(sorghum.ked.xv$residual, na.rm=T)/var(sorghum.ov$y2009)

# Calculate the error, rmse and mean error

#error

sorghum.ked.xv$residual

# Mean error

sum(sorghum.ked.xv$residual)/length(sorghum.ked.xv$residual)

#rmse

sqrt(sum(sorghum.ked.xv$residual^2)/length(sorghum.ked.xv$residual))


#------------------------------------------------------------

# Cross validation for RK

#-------------------------------------------------------------



str(sorghum.ov)
sorghum.rk.xv = krige.cv(resid~1, sorghum.ov, sorghum.rvgm)

summary(sorghum.rk.xv)

windows(width = 4, height = 4)
hist(sorghum.rk.xv$var1.pred)

mean(sorghum.rk.xv$zscore)
sd(sorghum.rk.xv$zscore)

windows(width = 4, height = 4)
hist(sorghum.rk.xv$zscore)

windows(width = 8, height = 6)
bubble(sorghum.rk.xv, zcol="residual", scales=list(draw=T), col="blue", maxsize=1.5,
       key.entries=c(-200,-150,-100,-75,-50,50,75,100,125,150,200), 
       sp.layout=list("sp.polygons", sorghum.poly), main = "cross-validation residuals, trend removed")


names(sorghum.rk.xv)

# amount of variation explained by the OK model:
1-var(sorghum.rk.xv$residual, na.rm=T)/var(sorghum.ov$y2009)

# Calculate the error, rmse and mean error

#error

sorghum.rk.xv$residual

# Mean error

sum(sorghum.rk.xv$residual)/length(sorghum.rk.xv$residual)

#rmse

sqrt(sum(sorghum.rk.xv$residual^2)/length(sorghum.rk.xv$residual))

#---------------------------------------------------------------------

# Regression Kriging (implicit) - (KED) 

#--------------------------------------------------------------------
sorghum.grid.ked <- as(sorghum.grid, "SpatialPointsDataFrame")
str(sorghum.grid.ked)

sorghum.pred.ked <- krige(formula = y2009~NDVI.PC1+PERC+RURPD+ELEV+PHCR+MARKD, sorghum.ov, newdata = sorghum.grid.ked, sorghum.rvgm)
str(sorghum.pred.ked)
#Mean UK prediction variance
#calculateMukv(sorghum.ov, sorghum.grid, sorghum.rvgm, y2009~NDVI.PC1+markd)
#library(lattice)
#levelplot(var1.var, sorghum.rk, aspect = "iso", main = "kriging variance")

windows(6,6)
plot(density(sorghum.pred.ked$var1.pred), col = "darkblue", main = "sorghum yield",
     lwd = 1.5)

min(sorghum.pred.ked$var1.pred)
max(sorghum.pred.ked$var1.pred)
mean(sorghum.pred.ked$var1.pred)
median(sorghum.pred.ked$var1.pred)
quantile(sorghum.pred.ked$var1.pred)
sd(sorghum.pred.ked$var1.pred)


x<- sorghum.pred.ked$var1.pred
#MAE

sum(x -mean(x))/length(x)

#MSE

sqrt(sum((x -mean(x))^2)/length(x))

#mean of RK prediction variance

mean(sqrt(sorghum.pred.ked$var1.var))

## SD
sorghum.pred.ked$var1.var
sorghum.pred.ked$var1.sd <- sqrt(sorghum.pred.ked$var1.var)

min(sorghum.pred.ked$var1.sd)
max(sorghum.pred.ked$var1.sd)
mean(sorghum.pred.ked$var1.sd)
median(sorghum.pred.ked$var1.sd)
sd(sorghum.pred.ked$var1.sd)

#assigning the spatial data frame and projecton system again

gridded(sorghum.pred.ked) <- TRUE
sorghum.rk <- as(sorghum.pred.ked, "SpatialGridDataFrame")
proj4string(sorghum.pred.ked) <- sorghum.grid@proj4string
names(sorghum.pred.ked)

# plot and save kriging maps:
## Sorghum yield predictions map
windows(w = 8, h = 6)
spplot(sorghum.rk["var1.pred"], col.regions=topo.colors(256)(),scales = list(draw = T),
       main = "Sorghum yield map (OLS-RK)")

savePlot(filename = "Sor_rkpred.tif", type = "tiff")
savePlot(filename = "Sor_rkpred.eps", type = "eps")

#standard deviation or uncertainity map
mean(sorghum.pred.ked$var1.sd)
windows(w = 8, h = 6)
spplot(sorghum.pred.ked["var1.sd"], col.regions=topo.colors(256)(),scales = list(draw = T),
       main = "Uncertainity map (OLS-RK)", sp.layout=list("sp.points",sorghum,pch=1))

savePlot(filename = "Sor_rksd.tif", type = "tiff")
savePlot(filename = "Sor_rksd.eps", type = "eps")


#------------------------------------------- -----------------

# Regression Kriging (explicit) MLRK

#---------------------------------------------------------

sorghum.grid.rk <- as(sorghum.grid, "SpatialPointsDataFrame")
str(sorghum.grid.rk)
# interpolate GLS residuals:
sorghum.mlr.rk = krige(resid~1, sorghum.ov, sorghum.grid.rk, sorghum.rvgm)
names(sorghum.mlr.rk)
str(lm.fit$fit)

# adding GLS predicted trend and predicted residuals on each cell:
sorghum.mlr.rk <- as(sorghum.mlr.rk, "SpatialPointsDataFrame")

sorghum.mlr.rk$pred.mlrk <- (lm.fit$fit+sorghum.mlr.rk$var1.pred)



windows(6,6)
plot(density(sorghum.mlr.rk$pred.mlrk), col = "darkblue", main = "sorghum yield",
     lwd = 1.5)

str(sorghum.mlr.rk$pred.mlrk)
min(sorghum.mlr.rk$pred.mlrk)
max(sorghum.mlr.rk$pred.mlrk)
mean(sorghum.mlr.rk$pred.mlrk)
median(sorghum.mlr.rk$pred.mlrk)
quantile(sorghum.mlr.rk$pred.mlrk)
sd(sorghum.mlr.rk$pred.mlrk)


x<- sorghum.mlr.rk$pred.mlrk
#MAE

sum(x -mean(x))/length(x)

#MSE

sqrt(sum((x -mean(x))^2)/length(x))
#mean of RK prediction variance

#assigning the spatial data frame and projecton system again

gridded(sorghum.mlr.rk) <- TRUE
sorghum.mlr.rk <- as(sorghum.mlr.rk, "SpatialGridDataFrame")
proj4string(sorghum.mlr.rk) <- sorghum.grid@proj4string
names(sorghum.mlr.rk)

#Sorghum yield predictions map GWR-RK
windows(w = 8, h = 6)
spplot(sorghum.mlr.rk["var1.rk"], col.regions=topo.colors(256)(), scales = list(draw = T), 
       main = "Sorghum yield map (GLS-RK)")

savePlot(filename = "Sor_GLSrk.tif", type = "tiff")
savePlot(filename = "Sor_GLSrk.eps", type = "eps")

write.asciigrid(sorghum.mlr.rk["var1.rk"], "sorghum.mlr.rk.asc", na.value=-1)


sqrt(sorghum.mlr.rk$var1.var)
max(p.gls$se.fit)
min(p.gls$se.fit)
max(sqrt(sorghum.mlr.rk$var1.var))
min(sqrt(sorghum.mlr.rk$var1.var))
sqrt(sorghum.glsvgm[2,2])

# sorghum.mlr.rk$var1.sd <- p.gls$se.fit+sqrt(sorghum.mlr.rk$var1.var)-sqrt(sorghum.glsvgm[2,2])
# max(sorghum.mlr.rk$var1.sd)
# min(sorghum.mlr.rk$var1.sd)
# mean(sorghum.mlr.rk$var1.sd)

sorghum.mlr.rk$var1.var
sorghum.mlr.rk$var1.sd <- sqrt((lm.fit$se.fit*lm.fit$se.fit)+sorghum.mlr.rk$var1.var)

min(sorghum.mlr.rk$var1.sd)
max(sorghum.mlr.rk$var1.sd)
mean(sorghum.mlr.rk$var1.sd)
median(sorghum.mlr.rk$var1.sd)
sd(sorghum.mlr.rk$var1.sd)

windows(w = 5, h = 5)
plot(density(sqrt(sorghum.mlr.rk$var1.sd)), col = "black", main = "sorghum OK SD",
     lwd = 2)


###EXTRACT VALUES BY DEPARTMETS
sorghum.mlr.rk.pred <- as(sorghum.mlr.rk["var1.rk"], "SpatialGridDataFrame")
proj4string(sorghum.mlr.rk.pred) <- sorghum.grid@proj4string
mlr.rk <- raster(sorghum.mlr.rk.pred)
mlr.rk.pred.departments.mean <- extract(mlr.rk, sorghum.poly, df=TRUE, fun=mean)
mlr.rk.pred.departments.mean

sorghum.mlr.rk.var <- as(sorghum.mlr.rk["var1.sd"], "SpatialGridDataFrame")
proj4string(sorghum.mlr.rk.var)<- sorghum.grid@proj4string
mlr.rk.pred.var <- raster(sorghum.mlr.rk.var)
mlr.rk.pred.departments.sd <- extract(mlr.rk.pred.var, sorghum.poly, df=TRUE, fun=mean)
mlr.rk.pred.departments.sd





#----------------------------------------------------------------------------------------------------------------

#Sorghum yield prediction variance map GWRK

#---------------------------------------------------------------------------------------------------------------

#------------------------------------------
### variogram modelling of residuals of GWR
#------------------------------------------

??spgwr
#add residuals 
gwrres <- mod.gwr.r$SDF$gwr.e
sorghum.ov$gwrresid = mod.gwr.r$SDF$gwr.e
str(sorghum.ov)

# More normal distribution

brks <- round(fivenum(gwrres), digits=2)
brks
cols <- rev(heat.colors(4))

#GWR Residuals

X11()
par(mfrow=c(1,1))
#plot(mod.gwr$SDF, col=cols)
plot(mod.gwr$SDF, col=cols[findInterval(gwrres, brks,all.inside=TRUE)])
title(main="Map of Residuals from GWR Model")
legend("bottom", legend = leglabs(brks,"<", ">="),
fill = cols,bty = "n", cex = 0.9, y.intersp = 0.9, title="Residuals")

## Residual Autocorrelation

moran.test(sorghum.ov$gwrresid, listw=W_cont_el_mat, zero.policy=T)

#### GWR residuals variogram modelling

sorghum.gwrrsvar = variogram(gwrres~1, data=sorghum.ov)
sorghum.gwrivgm <- vgm(nugget=0, model="Sph", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + diff(sorghum.ov@bbox["y",])^2)/4, psill=var(gwrres))

sorghum.gwrrvgm <- fit.variogram(sorghum.gwrrsvar, model=sorghum.gwrivgm)
sorghum.gwrrvgm # experimental variogram of GWR for CV and OK
plot(sorghum.gwrrsvar, sorghum.gwrrvgm)
sorghum.gwrrvgm.plt <- plot(sorghum.gwrrsvar, sorghum.gwrrvgm, pc="+", pl=T, col="black", main="Residuals")

# synchronize the two plots and interpret results:
sorghum.gwrrvgm.plt$x.limits <- sorghum.vgm.plt$x.limits
sorghum.gwrrvgm.plt$y.limits <- sorghum.vgm.plt$y.limits
windows(w = 5, h = 6)
print(sorghum.vgm.plt, split=c(1,1,2,1), more=TRUE)
print(sorghum.gwrrvgm.plt, split=c(2,1,2,1), more=FALSE)

windows(w = 5, h = 6)
print(sorghum.gwrrvgm.plt, split=c(1,1,1,1), more=T)

sum(sorghum.gwrrvgm$psill) # psill of the GWR residuals variogram
sum(sorghum.rvgm$psill) # psill of the OLS residuals variogram
sum(sorghum.vgm$psill) # # psill of the OK residuals 
sum(sorghum.gwrrvgm$psill)/sum(sorghum.vgm$psill)

### Sill Comparisons
windows(w = 10, h = 6)
par(mfrow=c(1,3))
print(sorghum.vgm.plt, split=c(1,1,2,1), more=TRUE)
print(sorghum.rvgm.plt, split=c(2,1,2,1), more=TRUE)
print(sorghum.gwrrvgm.plt, split=c(2,1,2,1), more=FALSE)

### Sill Comparisons

### Sill Comparisons
windows(w = 5, h = 6)
print(sorghum.vgm.plt, split=c(1,1,1,1), more=TRUE)

sorghum.vgm.plt <- plot(sorghum.svar, sorghum.vgm, pch="+", pl=F,  col="black", main="Sorghum yield")

windows(w = 5, h = 6)
print(sorghum.vgm.plt, split=c(1,1,1,1), more=TRUE)
print(sorghum.rvgm.plt, split=c(1,1,1,1), more=TRUE)
print(sorghum.gwrrvgm.plt, split=c(1,1,1,1), more=FALSE)

savePlot(filename = "sill_comaparison.tif", type = "tiff")
dev.off()

#----------------------------------------------------------------
## Cross validation for regression kriging (GWR)
#-----------------------------------------------------------------

str(sorghum.ov)
sorghum.gwr.rk.xv = krige.cv(gwrresid~1, sorghum.ov, sorghum.gwrrvgm)

windows(width = 4, height = 4)
hist(sorghum.gwr.rk.xv$var1.pred)

mean(sorghum.gwr.rk.xv$zscore)
sd(sorghum.gwr.rk.xv$zscore)

windows(width = 4, height = 4)
hist(sorghum.gwr.rk.xv$zscore)

windows(width = 8, height = 6)
bubble(sorghum.gwr.rk.xv, zcol="residual", scales=list(draw=T), col="blue", maxsize=1.5,
key.entries=c(-200,-150,-100,-75,-50,50,75,100,125,150,200), 
sp.layout=list("sp.polygons", sorghum.poly), main = "cross-validation residuals, trend removed")

# amount of variation explained by the OK model:
1-var(sorghum.gwr.rk.xv$residual, na.rm=T)/var(sorghum.ov$y2009)

# Calculate the error, rmse and mean error

#error

sorghum.gwr.rk.xv$residual

# Mean error

sum(sorghum.gwr.rk.xv$residual)/length(sorghum.gwr.rk.xv$residual)

#rmse

sqrt(sum(sorghum.gwr.rk.xv$residual^2)/length(sorghum.gwr.rk.xv$residual))

# mean and standard deviation of the CV tells us that the these
# are more close to the normal distributions for GWR

#----------------------------------------------------------------------------------
### GWR Rgressin Krigging (GWR-RK)  
#---------------------------------------------------------------------------------

str(sorghum.ov)
str(sorghum.grid.gwr)
sorghum.grid.gwr <- as(sorghum.grid.gwr, "SpatialPointsDataFrame")
str(sorghum.grid.gwr)

# interpolate residuals:
sorghum.gwr.rk = krige(gwrresid~1, sorghum.ov, sorghum.grid.gwr, sorghum.gwrrvgm)
sorghum.gwrrvgm
#GWR resiual prediction each grid cell
names(sorghum.gwr.rk)

#predicted GWR trend (from running GWR on each grid cell)
str(mod.gwr$SDF$pred)
str(sorghum.gwr.rk$var1.pred)
min(mod.gwr$SDF$pred)
max(mod.gwr$SDF$pred)
mean(mod.gwr$SDF$pred)
median(mod.gwr$SDF$pred)
sd(mod.gwr$SDF$pred)

#mean of GWR prediction variance 
mean(sqrt(var(mod.gwr$SDF$pred.se)))

# adding GWR predicted trend and predicted residuals on each cell:
sorghum.gwr.rk$pred.gwrk <- (mod.gwr$SDF$pred+sorghum.gwr.rk$var1.pred)
names(sorghum.gwr.rk)

windows(6,6)
plot(density(sorghum.gwr.rk$pred.gwrk), col = "darkblue", main = "sorghum yield",
     lwd = 1.5)
min(sorghum.gwr.rk$pred.gwrk)
max(sorghum.gwr.rk$pred.gwrk)
mean(sorghum.gwr.rk$pred.gwrk)
median(sorghum.gwr.rk$pred.gwrk)
quantile(sorghum.gwr.rk$pred.gwrk)
sd(sorghum.gwr.rk$pred.gwrk)

x<- sorghum.gwr.rk$pred.gwrk
#MAE

sum(x -mean(x))/length(x)

#MSE

sqrt(sum((x -mean(x))^2)/length(x))

#mean of RK prediction variance

mean(sqrt(sorghum.pred.ked$var1.var))

# standard deviatian of prediction = sqrt of prediction variance = prediction error
# variance = nuggut + partial sill
# sqrt of (nuggut + partial sill) 

#mean of standard deviatian of prediction of GWR residuals
mean(sqrt(sorghum.gwr.rk$pred.gwrk))
str(sorghum.gwr.rk)
# standard deviatian of prediction GWR-K

plot(density(sqrt(sorghum.gwr.rk$var1.var)), col = "darkblue", main = "GWR residuals",
     lwd = 1.5)

plot(density(mod.gwr$SDF$pred.se), col = "darkblue", main = "GWR residuals",
     lwd = 1.5)

max(sqrt(sorghum.gwr.rk$var1.var))
max(mod.gwr$SDF$pred.se)
sqrt(sorghum.gwrrvgm[2,2])

sd_GWRRK <- mod.gwr$SDF$pred.se + sqrt(sorghum.gwr.rk$var1.var) - sqrt(sorghum.gwrrvgm[2,2])
min(sd_GWRRK)
max(sd_GWRRK)
mean(sd_GWRRK)
sd(sd_GWRRK)

sd_GWRRK <-sqrt((mod.gwr$SDF$pred.se)^2 + sorghum.gwr.rk$var1.var)
min(sd_GWRRK)
max(sd_GWRRK)
mean(sd_GWRRK)
median(sd_GWRRK)
sd(sd_GWRRK)


#prediction error of GWR-K
sorghum.gwr.rk$var1.sd <- sd_GWRRK
sd(sorghum.gwr.rk$var1.sd)

plot(density(sorghum.gwr.rk$var1.sd), col = "black", main = "GWRK local prediction error variance",
     lwd = 2)



#assigning the spatial data frame and projecton system again
#sorghum.gwr.rk <- as(sorghum.gwr.rk, "SpatialPointsDataFrame")
gridded(sorghum.gwr.rk) <- TRUE
sorghum.gwr.rk <- as(sorghum.gwr.rk, "SpatialGridDataFrame")
proj4string(sorghum.gwr.rk) <- sorghum.grid@proj4string
names(sorghum.gwr.rk)

#Sorghum yield predictions map GWR-RK
windows(w = 8, h = 6)
spplot(sorghum.gwr.rk["pred.gwrk"], col.regions=topo.colors(256)(), scales = list(draw = T), 
main = "Sorghum yield map (GWR-RK)")

savePlot(filename = "Sor_GWRrk.tif", type = "tiff")
savePlot(filename = "Sor_GWRrk.eps", type = "eps")

write.asciigrid(sorghum.gwr.rk["pred.gwrk"], "sorghum.gwrk.asc", as.image = TRUE,na.value=-1)
?write
#Sorghum yield prediction variance map GWR-RK

windows(w = 8, h = 6)
spplot(sorghum.gwr.rk["var1.sd"], col.regions=topo.colors(256)(),scales = list(draw = T),
main = "Uncertainity map (GWR-RK)", sp.layout=list("sp.points",sorghum.ov,pch=1))

savePlot(filename = "Sor_GWRrksd.tif", type = "tiff")
savePlot(filename = "Sor_GWRrksd.eps", type = "eps")

write.asciigrid(sorghum.gwr.rk["var1.sd"], "sorghum.gwr.sd.asc", na.value=-1)

# prediction accuracy:

sorghum.ov.gwr.rk <- overlay(sorghum.gwr.rk, sorghum.OV)
str(sorghum.ov.gwr.rk)
sorghum.ov.gwr.rk@data <- cbind(sorghum.ov.gwr.rk@data, sorghum[c("y2009", "popden")]@data)
str(sorghum.ov.gwr.rk)

#library(mda) 
#library(vcd) # kappa statistics
#Kappa(confusion(sorghum.ov.gwr.rk$y2009, as.factor(round(sorghum.ov.gwr.rk$var1.rk, 0))))


#----------------------------------------------------------------------------------
#comparing predictiosn from three models
#----------------------------------------------------------------------------------

sorghum.gwr.rk <- as(sorghum.gwr.rk, "SpatialPointsDataFrame")
sorghum.pred.ked <- as(sorghum.pred.ked, "SpatialPointsDataFrame")
sorghum.mlr.rk <- as(sorghum.mlr.rk, "SpatialPointsDataFrame")
sorghum.ok <- as(sorghum.ok, "SpatialPointsDataFrame")

quantile(sorghum.ok$var1.pred)
quantile(sorghum.pred.ked$var1.pred)
quantile(sorghum.mlr.rk$pred.mlrk)
quantile(sorghum.gwr.rk$pred.gwrk)

zmax <- round(max(sorghum.ok$var1.pred,sorghum.pred.ked$var1.pred,sorghum.mlr.rk$pred.mlrk,sorghum.gwr.rk$pred.gwrk), 0) + 0
zmax
zmin <- round(min(sorghum.ok$var1.pred,sorghum.pred.ked$var1.pred,sorghum.mlr.rk$pred.mlrk,sorghum.gwr.rk$pred.gwrk), 0) - 0
zmin
ramp <- seq(from=zmin, to=zmax, by=200)
ramp

gridded(sorghum.ok) <- TRUE
sorghum.ok <- as(sorghum.ok, "SpatialGridDataFrame")
proj4string(sorghum.ok) <- sorghum.grid@proj4string

gridded(sorghum.pred.ked) <- TRUE
sorghum.pred.ked <- as(sorghum.pred.ked, "SpatialGridDataFrame")
proj4string(sorghum.pred.ked) <- sorghum.grid@proj4string

gridded(sorghum.mlr.rk) <- TRUE
sorghum.mlr.rk <- as(sorghum.mlr.rk, "SpatialGridDataFrame")
proj4string(sorghum.mlr.rk) <- sorghum.grid@proj4string

gridded(sorghum.gwr.rk) <- TRUE
sorghum.gwr.rk <- as(sorghum.gwr.rk, "SpatialGridDataFrame")
proj4string(sorghum.gwr.rk) <- sorghum.grid@proj4string

p1 <- spplot(sorghum.ok, "var1.pred", asp=0, col.regions=bpy.colors(10)[10:1], axes=TRUE, at=ramp)
p2 <- spplot(sorghum.pred.ked, "var1.pred", asp=0, col.regions=bpy.colors(10)[10:1], axes=TRUE, at=ramp)
p3 <- spplot(sorghum.mlr.rk, "pred.mlrk", asp=0, col.regions=bpy.colors(10)[10:1], axes=TRUE, at=ramp)
p4 <- spplot(sorghum.gwr.rk, "pred.gwrk", asp=0, col.regions=bpy.colors(10)[10:1], axes=TRUE, at=ramp)

windows(5, 12)
par(mfrow=c(4,1))
plot(p1, split=c(1,1,1,4), more=T)
plot(p2, split=c(1,2,1,4), more=T)
plot(p3, split=c(1,3,1,4), more=T)
plot(p4, split=c(1,4,1,4), more=F)

?spplot
?split
windows(10, 4)
par(mfrow=c(1,4))
plot(density(sorghum.ok$var1.pred), col = "black", main = "GWR residuals", breaks=ramp,
     lwd = 2)
plot(density(sorghum.pred.ked$var1.pred), col = "black", main = "GWR residuals",
     lwd = 2)
plot(density(sorghum.mlr.rk$pred.mlrk), col = "black", main = "GWR residuals",
     lwd = 2)
plot(density(sorghum.gwr.rk$pred.gwrk), col = "black", main = "GWR residuals",
     lwd = 2)
y <- cbind(sorghum.ok$var1.pred, sorghum.mlr.rk$pred.mlrk, sorghum.gwr.rk$pred.gwrk)

sm.density(y)
sm.density.compare(sorghum.ok$var1.pred, sorghum.mlr.rk$pred.mlrk)
# Add a legend (the color numbers start from 2 and go up)
legend("topright", levels(data$cond), fill=2+(0:nlevels(data$cond)))
?sm.density.compare


#------------------------------------------------------------------------------------
#comparing prediction variance from three models
#------------------------------------------------------------------------------------
sorghum.ok <- as(sorghum.ok, "SpatialPointsDataFrame")
sorghum.pred.ked <- as(sorghum.pred.ked, "SpatialPointsDataFrame")
sorghum.mlr.rk <- as(sorghum.mlr.rk, "SpatialPointsDataFrame")
sorghum.gwr.rk <- as(sorghum.gwr.rk, "SpatialPointsDataFrame")

quantile(sorghum.ok$var1.sd)
quantile(sorghum.pred.ked$var1.sd)
quantile(sorghum.mlr.rk$var1.sd)
quantile(sorghum.gwr.rk$var1.sd)


zmax <- round(max(sorghum.ok$var1.sd,sorghum.pred.ked$var1.sd,sorghum.mlr.rk$var1.sd, sorghum.gwr.rk$var1.sd), 0) + 0
zmax
zmin <- round(min(sorghum.ok$var1.sd,sorghum.pred.ked$var1.sd,sorghum.mlr.rk$var1.sd, sorghum.gwr.rk$var1.sd), 0) - 0
zmin
ramp <- seq(from=zmin, to=zmax, by=(zmax-zmin)/8)
ramp

gridded(sorghum.ok) <- TRUE
sorghum.ok <- as(sorghum.ok, "SpatialGridDataFrame")
proj4string(sorghum.ok) <- sorghum.grid@proj4string

gridded(sorghum.pred.ked) <- TRUE
sorghum.pred.ked <- as(sorghum.pred.ked, "SpatialGridDataFrame")
proj4string(sorghum.pred.ked) <- sorghum.grid@proj4string

gridded(sorghum.mlr.rk) <- TRUE
sorghum.lm.rk <- as(sorghum.mlr.rk, "SpatialGridDataFrame")
proj4string(sorghum.mlr.rk) <- sorghum.grid@proj4string

gridded(sorghum.gwr.rk) <- TRUE
sorghum.gwr.rk <- as(sorghum.gwr.rk, "SpatialGridDataFrame")
proj4string(sorghum.gwr.rk) <- sorghum.grid@proj4string

p1 <- spplot(sorghum.ok, "var1.sd", asp=0, col.regions=bpy.colors(10)[10:1], axes=TRUE, at=ramp)
p2 <- spplot(sorghum.pred.ked, "var1.sd", asp=0, col.regions=bpy.colors(10)[10:1], axes=TRUE, at=ramp)
p3 <- spplot(sorghum.mlr.rk, "var1.sd", asp=0, col.regions=bpy.colors(10)[10:1], axes=TRUE, at=ramp)
p4 <- spplot(sorghum.gwr.rk, "var1.sd", asp=0, col.regions=bpy.colors(10)[10:1], axes=TRUE, at=ramp)

windows(5, 12)
par(mfrow=c(4,1))
plot(p1, split=c(1,1,1,4), more=T)
plot(p2, split=c(1,2,1,4), more=T)
plot(p3, split=c(1,3,1,4), more=T)
plot(p4, split=c(1,4,1,4), more=F)


sorghum.gwr.rk$var1.sd

#density maps
quantile(sorghum.ok$var1.sd)
quantile(sorghum.pred.ked$var1.sd)
quantile(sorghum.mlr.rk$var1.sd)
quantile(sorghum.gwr.rk$var1.sd)

plot(density(sorghum.ok$var1.sd), col = "darkblue", main = "GWR residuals",
     lwd = 1.5)
plot(density(sorghum.pred.ked$var1.sd), col = "darkblue", main = "GWR residuals",
     lwd = 1.5)
plot(density(sorghum.mlr.rk$var1.sd), col = "darkblue", main = "GWR residuals",
     lwd = 1.5)
plot(density(sorghum.gwr.rk$var1.sd), col = "darkblue", main = "GWR residuals",
     lwd = 1.5)
?spplot

#---------------------------------------------------------------------

#### extracting variograms from prediction maps (Figure 5)

#------------------------------------------------------------------------


#OK

gridded(sorghum.ok) <- TRUE
sorghum.ok <- as(sorghum.ok, "SpatialGridDataFrame")
proj4string(sorghum.ok) <- sorghum.grid@proj4string

str(sorghum.mlr.rk)

sorghum.ov.ok <- overlay(sorghum.ok, sorghum.ov)
str(sorghum.ov.ok)
# sorghum.ov.gwr.rk@data <- cbind(sorghum.ov.mlrk@data, sorghum[c("y2009", "x", "y")]@data)
# str(sorghum.ov.gwr.rk)

predokyield <- gstat(formula = var1.pred~1, data = sorghum.ov.ok)
sorghum.predokvar <- variogram(predokyield)

#sorghum.svar <- variogram(gsoryield, width=100)
plot(sorghum.svar)
# compute experimental variogram and fit function: (gstat automatic fitting)
#show.vgms()
sorghum.predok.ivgm <- vgm(nugget=0, model="Sph", range=sqrt(diff(sorghum.ov.ok@bbox["x",])^2 + diff(sorghum.ov.ok@bbox["y",])^2)/4, psill=var(sorghum.ov.ok$var1.pred))
sorghum.predok.ivgm # experimental variogram for CV and OK
print(plot(sorghum.predokvar, pl = T, model = sorghum.predok.ivgm))
sorghum.predok.vgm <- fit.variogram(sorghum.predokvar, model= sorghum.predok.ivgm) 
sorghum.predok.vgm # experimental variogram for CV and OK
print(plot(sorghum.predokvar, pl = T, model = sorghum.predok.vgm))
sorghum.predok.vgm.plt <- plot(sorghum.predokvar, sorghum.predok.vgm, pch="+", pl=TRUE,  col="black")
windows(5,5)
plot(sorghum.predmlrk.vgm.plt)

#KED

gridded(sorghum.pred.ked) <- TRUE
sorghum.pred.ked <- as(sorghum.pred.ked, "SpatialGridDataFrame")
proj4string(sorghum.pred.ked) <- sorghum.grid@proj4string

str(sorghum.pred.ked)
str(sorghum.ov)

sorghum.ov.ked <- overlay(sorghum.pred.ked, sorghum.ov)
str(sorghum.ov.ked)
# sorghum.ov.gwr.rk@data <- cbind(sorghum.ov.mlrk@data, sorghum[c("y2009", "x", "y")]@data)
# str(sorghum.ov.gwr.rk)

predkedyield <- gstat(formula = var1.pred~1, data = sorghum.ov.ked)
sorghum.predkedvar <- variogram(predkedyield)

#sorghum.svar <- variogram(gsoryield, width=100)
plot(sorghum.predkedvar)
# compute experimental variogram and fit function: (gstat automatic fitting)
#show.vgms()
sorghum.predked.ivgm <- vgm(nugget=0, model="Sph", range=sqrt(diff(sorghum.ov.ked@bbox["x",])^2 + diff(sorghum.ov.ked@bbox["y",])^2)/4, psill=var(sorghum.ov.ked$var1.pred))
sorghum.predked.ivgm # experimental variogram for CV and OK
print(plot(sorghum.predkedvar, pl = T, model = sorghum.predked.ivgm))
sorghum.predked.vgm <- fit.variogram(sorghum.predkedvar, model= sorghum.predked.ivgm) 
sorghum.predked.vgm # experimental variogram for CV and OK
print(plot(sorghum.predkedvar, pl = T, model = sorghum.predked.vgm))
plot(sorghum.svar, sorghum.vgm)
sorghum.predked.vgm.plt <- plot(sorghum.predkedvar, sorghum.predked.vgm, pch="+", pl=TRUE,  col="black")
windows(5,6)
plot(sorghum.predked.vgm.plt)


#MLRK

gridded(sorghum.mlr.rk) <- TRUE
sorghum.mlr.rk <- as(sorghum.mlr.rk, "SpatialGridDataFrame")
proj4string(sorghum.mlr.rk) <- sorghum.grid@proj4string

str(sorghum.mlr.rk)

sorghum.ov.mlrk <- overlay(sorghum.mlr.rk, sorghum.ov)
str(sorghum.ov.mlrk)
# sorghum.ov.gwr.rk@data <- cbind(sorghum.ov.mlrk@data, sorghum[c("y2009", "x", "y")]@data)
# str(sorghum.ov.gwr.rk)

predmlryield <- gstat(formula = pred.mlrk~1, data = sorghum.ov.mlrk)
sorghum.predmlrvar <- variogram(predmlryield)

#sorghum.svar <- variogram(gsoryield, width=100)
plot(sorghum.svar)
# compute experimental variogram and fit function: (gstat automatic fitting)
#show.vgms()
sorghum.predmlr.ivgm <- vgm(nugget=0, model="Sph", range=sqrt(diff(sorghum.ov.mlrk@bbox["x",])^2 + diff(sorghum.ov.mlrk@bbox["y",])^2)/4, psill=var(sorghum.ov.mlrk$pred.mlrk))
sorghum.predmlr.ivgm # experimental variogram for CV and OK
print(plot(sorghum.predmlrvar, pl = T, model = sorghum.predmlr.ivgm))
sorghum.predmlr.vgm <- fit.variogram(sorghum.predmlrvar, model= sorghum.predmlr.ivgm) 
sorghum.predmlr.vgm # experimental variogram for CV and OK
print(plot(sorghum.predmlrvar, pl = T, model = sorghum.predmlr.vgm))
plot(sorghum.svar, sorghum.vgm)
sorghum.predmlrk.vgm.plt <- plot(sorghum.predmlrvar, sorghum.predmlr.vgm, pch="+", pl=TRUE,  col="black")
windows(5,6)
plot(sorghum.predmlrk.vgm.plt)

#GWRK

gridded(sorghum.gwr.rk) <- TRUE
sorghum.gwr.rk <- as(sorghum.gwr.rk, "SpatialGridDataFrame")
proj4string(sorghum.gwr.rk) <- sorghum.grid@proj4string

str(sorghum.gwr.rk)

sorghum.ov.gwrk <- overlay(sorghum.gwr.rk, sorghum.ov)
str(sorghum.ov.gwrk)
# sorghum.ov.gwr.rk@data <- cbind(sorghum.ov.gwrk@data, sorghum[c("y2009", "x", "y")]@data)
# str(sorghum.ov.gwr.rk)

predgwrkyield <- gstat(formula = pred.gwrk~1, data = sorghum.ov.gwrk)
sorghum.predgwrkvar <- variogram(predgwrkyield)

#sorghum.svar <- variogram(gsoryield, width=100)
# compute experimental variogram and fit function: (gstat automatic fitting)
#show.vgms()
sorghum.predgwrk.ivgm <- vgm(nugget=0, model="Sph", range=sqrt(diff(sorghum.ov.gwrk@bbox["x",])^2 + diff(sorghum.ov.gwrk@bbox["y",])^2)/4, psill=var(sorghum.ov.gwrk$pred.gwrk))
sorghum.predgwrk.ivgm # experimental variogram for CV and OK
print(plot(sorghum.predgwrkvar, pl = T, model = sorghum.predgwrk.ivgm))
sorghum.predgwrk.vgm <- fit.variogram(sorghum.predgwrkvar, model= sorghum.predgwrk.ivgm) 
sorghum.predgwrk.vgm # experimental variogram for CV and OK
print(plot(sorghum.predgwrkvar, pl = T, model = sorghum.predgwrk.vgm))
plot(sorghum.predgwrkvar, model = sorghum.predgwrk.vgm)
sorghum.predgwrk.vgm.plt <- plot(sorghum.predgwrkvar, sorghum.predgwrk.vgm, pch="+", pl=TRUE,  col="black")
windows(5,4)
plot(sorghum.predgwrk.vgm.plt)


p1 <- sorghum.predok.vgm.plt
p2 <- sorghum.predked.vgm.plt
p3 <- sorghum.predmlrk.vgm.plt
p4 <- sorghum.predgwrk.vgm.plt

windows(6, 14)
par(mfrow=c(4,1))
plot(p1, split=c(1,1,1,4), more=T)
plot(p2, split=c(1,2,1,4), more=T)
plot(p3, split=c(1,3,1,4), more=T)
plot(p4, split=c(1,4,1,4), more=F)

#density maps
quantile(sorghum.ok$var1.sd)
mean(sorghum.ok$var1.sd)
sd(sorghum.ok$var1.sd)

quantile(sorghum.pred.ked$var1.sd)
mean(sorghum.pred.ked$var1.sd)
sd(sorghum.pred.ked$var1.sd)

quantile(sorghum.mlr.rk$var1.sd)
mean(sorghum.mlr.rk$var1.sd)
sd(sorghum.mlr.rk$var1.sd)

quantile(sorghum.gwr.rk$var1.sd)
mean(sorghum.gwr.rk$var1.sd)
sd(sorghum.gwr.rk$var1.sd)


windows(5,5)
plot(density(sorghum.ok$var1.sd), col = "black", main = "OK local prediction error variance",
     lwd = )
windows(5,5)
plot(density(sorghum.pred.ked$var1.sd), col = "black", main = "KED local prediction error variance",
     lwd = 2)
windows(5,5)
plot(density(sorghum.mlr.rk$var1.sd), col = "black", main = "MLR local prediction error variance",
     lwd = 2)
windows(5,5)
plot(density(sorghum.gwr.rk$var1.sd), col = "black", main = "GWRK local prediction error variance",
     lwd = 2)
?spplot

#-------------------------------------------------------------

#----- Sequentional Guassian simulation (sGs)

#------------------------------------------------------------


#################### sGs (with simple kriging)


nsim=100
library(geoR)

?boxcox

sy.bc<-boxcox(y2009~NDVI.PC1+markd, lambda = seq(-4, 4, .1), plotit = TRUE, eps = 1/50,
              data = sorghum.ov) 

tala.bc = cbind(tala, was.bc)

coordinates(tala.bc) <- ~ x + y  
coordinates(tala.grid) <-~ x + y

str(sorghum.grid)

sorghum.rksim <- krige(y2009~NDVI.PC1+PERC+RURPD+ELEV+PHCR+MARKD, sorghum.ov, sorghum.grid, 
                       sorghum.rvgm, nsim = nsim, nmax=5, debug.level = -1)
str(sorghum.rksim)

cs <- sorghum.rksim@data[,c(1:nsim)]

str(cs)

# mean of the simulation
sorghum.rksim$mean <- apply(cs, 1, mean)

# histogram:
windows( 5, 5)
hist(sorghum.rksim$mean, col="grey",  xlab="Mean simulated (cGs) sorghum yield (kg ha-1)", main="Histogram")

windows(w = 5, h = 5)
plot(density(sqrt(sorghum.rksim$mean)), col = "black", main = "sorghum OK SD",
     lwd = 2)


# summary statistics
min(sorghum.rksim$mean)
max(sorghum.rksim$mean)
mean(sorghum.rksim$mean)
median(sorghum.rksim$mean)
sd(sorghum.rksim$mean)



# standard deviation of simulation

sorghum.rksim$sd <- apply(cs, 1, sd)

min(sorghum.rksim$sd)
max(sorghum.rksim$sd)
mean(sorghum.rksim$sd)
median(sorghum.rksim$sd)
sd(sorghum.rksim$sd)



#comparing simulation and priction

# correlation mean of simulated and predicted, ideally close to 1
#OK
cor(sorghum.rksim$mean, sorghum.ok$var1.pred)
#GWR
cor(sorghum.rksim$mean, mod.gwr$SDF$pred)
#GWR-RK
cor(sorghum.rksim$mean, sorghum.gwr.rk$pred.gwr)
#UK
cor(sorghum.rksim$mean, sorghum.rk$var1.pred)

# constructing variogram and performing cross validation
str(sorghum.grid)
sorghum.grid$sim_rk_mean <- sorghum.rksim$mean
sorghum.grid$sim_rk_sd <- sorghum.rksim$sd 
str(sorghum.grid)

sim_rk.grid <- sorghum.grid  
gridded(sim_rk.grid) <- TRUE
sim_rk.grid <- as(sim_rk.grid, "SpatialGridDataFrame")
proj4string(sim_rk.grid) <- sorghum.grid@proj4string
sorghum.ov1 <- overlay(sim_rk.grid, sorghum)
sorghum.ov$sim_rk_mean <- sorghum.ov1$sim_rk_mean 
str(sorghum.ov)

gsimyield <- gstat(formula = sim_rk_mean~1, data = sorghum.ov)
sim_rk.svar <- variogram(gsimyield)
plot(sim_rk.svar)
plot(sorghum.svar)

show.vgms()
sim_rk.ivgm <- vgm(nugget=0, model="Ste", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + diff(sorghum.ov@bbox["y",])^2)/4, psill=var(sorghum.ov$sim_rk_mean))
sim_rk.vgm <- fit.variogram(sim_rk.svar, model=sim_rk.ivgm) 
plot(sim_rk.svar, sim_rk.vgm)

sim_rk.vgm.plt <- plot(sim_rk.svar, sim_rk.vgm, pc="+", pl=FALSE, col="black", main="cGs")
# synchronize the two plots and interpret results:
sim_rk.vgm.plt$x.limits <- sorghum.vgm.plt$x.limits
sim_rk.vgm.plt$y.limits <- sorghum.vgm.plt$y.limits
windows(w = 8, h = 6)
print(sorghum.vgm.plt, split=c(1,1,2,1), more=TRUE)
print(sim_rk.vgm.plt, split=c(2,1,2,1), more=FALSE)


############cross validation

sim_rk.xv <- krige.cv(sim_rk_mean~1, sorghum.ov, sim_rk.vgm)

windows(width = 4, height = 4)
hist(sim_rk.xv$var1.pred)

mean(sim_rk.xv$zscore)
sd(sim_rk.xv$zscore)

# amount of variation explained by the simulation mean:
1-var(sim_rk.xv$residual, na.rm=T)/var(sorghum$y2009)

# Calculate the error, rmse and mean error

#error

sim_rk.xv$residual

# Mean error

sum(sim_rk.xv$residual)/length(sim_rk.xv$residual)

#rmse

sqrt(sum(sim_rk.xv$residual^2)/length(sim_rk.xv$residual))


##### maps of mean and SD

str(sim_rk.grid)
str(sim_maps)
windows(w = 8, h = 6)
spplot(sim_rk.grid,zcol = "sim_rk_mean", col.regions = topo.colors(256), scales = list(draw = T),
       main = "Mean of simulated sorghum cGs")
savePlot(file = "TSC_cGs_mean.tif", type = "tiff")
savePlot(file = "TSC_cGs_mean.eps", type = "eps")

# calculate and plot standard deviation:

windows(w = 8, h = 6)
spplot(sim_rk.grid, zcol = "sim_rk_sd", col.regions=topo.colors(256),scales = list(draw = T),
       main = "Standard deviation of simulated sorghum", sp.layout=list("sp.points",sorghum.ov,pch=1))
savePlot(file = "TSC_CGS_sd.tif", type = "tiff")
savePlot(file = "TSC_CGS_sd.eps", type = "eps")

############### comparasons
### MEAN
sorghum.gwr.rk <- as(sorghum.gwr.rk, "SpatialPointsDataFrame")
sorghum.mlr.rk <- as(sorghum.mlr.rk, "SpatialPointsDataFrame")
sim_rk.grid <- as(sim_rk.grid, "SpatialPointsDataFrame")

max(sim_rk.grid$sim_rk_mean)
max(sorghum.mlr.rk$pred.mlrk)
max(sorghum.gwr.rk$pred.gwrk)

min(sim_rk.grid$sim_rk_mean)
min(sorghum.mlr.rk$pred.mlrk)
min(sorghum.gwr.rk$pred.gwrk)

zmax <- round(max(sim_rk.grid$sim_rk_mean,sorghum.mlr.rk$pred.mlrk,sorghum.gwr.rk$pred.gwrk), 0) + 0
zmax
zmin <- round(min(sim_rk.grid$sim_rk_mean,sorghum.mlr.rk$pred.mlrk,sorghum.gwr.rk$pred.gwrk), 0) - 0
zmin
ramp <- seq(from=zmin, to=zmax, by=200)
ramp

p1 <- spplot(sim_rk.grid, "sim_rk_mean", asp=1, col.regions=topo.colors(256),
             main="OK prediction", at=ramp)
p2 <- spplot(sorghum.mlr.rk, "pred.mlrk", asp=1, col.regions=topo.colors(256),
             main="OLS-RK prediction", at=ramp)
p3 <- spplot(sorghum.gwr.rk, "pred.gwrk", asp=1, col.regions=topo.colors(256),
             main="GWR-K prediction", at=ramp)
windows(10, 5)
plot(p1, split=c(1,1,3,1), more=T)
plot(p2, split=c(2,1,3,1), more=T)
plot(p3, split=c(3,1,3,1), more=F)


## SD


zmax <- round(max(sim_rk.grid$sim_rk_sd,sorghum.mlr.rk$var1.sd, sorghum.gwr.rk$var1.sd), 0) + 0
zmax
zmin <- round(min(sim_rk.grid$sim_rk_sd,sorghum.mlr.rk$var1.sd, sorghum.gwr.rk$var1.sd), 0) - 0
zmin
ramp <- seq(from=zmin, to=zmax, by=20)
ramp


p1 <- spplot(sim_rk.grid, "sim_rk_sd", asp=1, col.regions=topo.colors(256),
             at=ramp)
p2 <- spplot(sorghum.mlr.rk, "var1.sd", asp=1, col.regions=topo.colors(256),
             at=ramp)
p3 <- spplot(sorghum.gwr.rk, "var1.sd", asp=1, col.regions=topo.colors(256),
             at=ramp)

windows(5, 10)
par(mfrow=c(3,1))
plot(p1, split=c(1,1,1,3), more=T)
plot(p2, split=c(1,2,1,3), more=T)
plot(p3, split=c(1,3,1,3), more=F)


##with grids (convert into grids)
gridded(sim_rk.grid) <- TRUE
sim_rk.grid<- as(sim_rk.grid, "SpatialGridDataFrame")
proj4string(sim_rk.grid) <- sorghum.grid@proj4string

gridded(sorghum.mlr.rk) <- TRUE
sorghum.mlr.rk <- as(sorghum.mlr.rk, "SpatialGridDataFrame")
proj4string(sorghum.mlr.rk) <- sorghum.grid@proj4string

gridded(sorghum.gwr.rk) <- TRUE
sorghum.gwr.rk <- as(sorghum.gwr.rk, "SpatialGridDataFrame")
proj4string(sorghum.gwr.rk) <- sorghum.grid@proj4string




# ------------------------------------------------------------------------------------------

# Part V - Stochastic simulations: (with co-kriging)

# ------------------------------------------------------------------------------------------

sorghum.grid <- readShapePoly("burkinafaso_boundary.shp")
sorghum.grid <- spsample(sorghum.poly, cellsize = 1000, type = "regular")
gridded(sorghum.grid) <- TRUE

# check for duplicated coordinates:
zerodist(sorghum)

# remove records with duplicated coordinates:
#sorghum <-subset(sorghum, duplicated(coordinates(sorghum)) == "FALSE")
#sorghum$X <- NULL; sorghum$Y <- NULL

# check for removal duplicated coordinates:
zerodist(sorghum)

str(sorghum)

# some info on these data (Th only, can do the same for U and K):
hist(sorghum.ov$NDVI.PC1, main = "Histogram of sorghum measurements")
hist(sorghum.ov$markd, main = "Histogram of sorghum measurements")

# spatial plot of measurements:
windows(w = 8, h = 6)
border <- list("sp.polygons", sorghum.poly)
cuts <- seq(0,4,0.5)
spplot(sorghum.ov, zcol = "NDVI.PC1", col.regions = topo.colors(256)(), sp.layout=list(border),
       key.space = list(x = 0.05, y = 0.02, corner = c(0,0)),  scales = list(draw = T),
       main = "Sorghum crop yield (kg ha) measurements Burkina Faso", cuts = cuts)
savePlot(filename = "Th_points.png", type = "png")

# step I -  fit variograms
show.vgms()

str(sorghum.ov)
# direct variogarms
gstat_tsc_sor_k <- gstat(id = c("NDVI.PC1"), formula = NDVI.PC1 ~ 1,
                         data = sorghum.ov, beta = mean(sorghum.ov$NDVI.PC1))
# gstat_tsc_sor_k <- gstat(gstat_tsc_sor_k, id = c("PERC"), formula = PERC ~ 1,
#                          data = sorghum.ov, beta = mean(sorghum.ov$PERC))
# gstat_tsc_sor_k <- gstat(gstat_tsc_sor_k, id = c("PHCR"), formula = PHCR ~ 1,
#                          data = sorghum.ov, beta = mean(sorghum.ov$PHCR))
gstat_tsc_sor_k <- gstat(gstat_tsc_sor_k, id = c("ELEV"), formula = ELEV ~ 1,
                         data = sorghum.ov, beta = mean(sorghum.ov$ELEV))
gstat_tsc_sor_k <- gstat(gstat_tsc_sor_k, id = c("MARKD"), formula = MARKD ~ 1,
                         data = sorghum.ov, beta = mean(sorghum.ov$MARKD))
# gstat_tsc_sor_k <- gstat(gstat_tsc_sor_k, id = c("RURPD"), formula = RURPD ~ 1,
#                          data = sorghum.ov, beta = mean(sorghum.ov$RURPD))

gstat_tsc_sor_k <- gstat(gstat_tsc_sor_k, id = "NDVI.PC1", model = vgm(nugget = 1, model="Bes", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + diff(sorghum.ov@bbox["y",])^2)/4, 
                                                                       psill = 1))
# gstat_tsc_sor_k <- gstat(gstat_tsc_sor_k, id = "PERC", model = vgm(nugget = 1, model="Bes", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + diff(sorghum.ov@bbox["y",])^2)/4, 
#                                                                     psill = 1))
# gstat_tsc_sor_k <- gstat(gstat_tsc_sor_k, id = "PHCR", model = vgm(nugget = 1, model="Bes", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + diff(sorghum.ov@bbox["y",])^2)/4, 
#                                                                     psill = 1))
gstat_tsc_sor_k <- gstat(gstat_tsc_sor_k, id = "ELEV", model = vgm(nugget = 1, model="Bes", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + diff(sorghum.ov@bbox["y",])^2)/4, 
                                                                   psill = 1))
gstat_tsc_sor_k <- gstat(gstat_tsc_sor_k, id = "MARKD", model = vgm(nugget = 1, model="Bes", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + diff(sorghum.ov@bbox["y",])^2)/4, 
                                                                   psill = 1))
# gstat_tsc_sor_k <- gstat(gstat_tsc_sor_k, id = "RURPD", model = vgm(nugget = 1, model="Bes", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + diff(sorghum.ov@bbox["y",])^2)/4, 
#                                                                    psill = 1))

# cross variogram
gstat_tsc_sor_k <- gstat(gstat_tsc_sor_k, id = c("NDVI.PC1", "MARKD", "ELEV", "PERC"), model = vgm(nugget = 1, model="Bes", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + diff(sorghum.ov@bbox["y",])^2)/4, 
                                                                                   psill = 1), fill.all = TRUE)

# compute and plot experimental variogram:
gstat_tsc_sor_k.vg <- variogram(gstat_tsc_sor_k)
gstat_tsc_sor_k.vg
plot(gstat_tsc_sor_k.vg, plot.nu = FALSE)

str(gstat_tsc_sor_k.vg)

### linear model of coregionalization (LMC)

vgm_sor_k <- fit.lmc(gstat_tsc_sor_k.vg, gstat_tsc_sor_k)
vgm_sor_k
# FOR THREE VARIABLES
vgm_sor_k$model$NDVI.PC1
vgm_sor_k$model$NDVI.PC1[1,2] <- 0.01
vgm_sor_k$model$NDVI.PC1.PHCR[1,2] <- 0
vgm_sor_k$model$MARKD[1,2] <- 0
vgm_sor_k$model$markd

# FOR SIX VARIABLES
# vgm_sor_k$model$NDVI.PC1
vgm_sor_k$model$NDVI.PC1[1,2] <- 0
# vgm_sor_k$model$NDVI.PC1.PHCR[1,2] <- 0
vgm_sor_k$model$NDVI.PC1.ELEV[1,2]<- 0
# vgm_sor_k$model$NDVI.PC1.MARKD[1,2]<- 0
vgm_sor_k$model$NDVI.PC1.RURPD[1,2] <- 0
# vgm_sor_k$model$markd

windows(w = 8, h = 5)
plot(gstat_tsc_sor_k.vg, vgm_sor_k)
savePlot(filename = "TSC_sor_lmc_variograms.tif", type = "tiff")

###########
# kriging #
###########

# spatial prediction with ordinary point cokriging:
sor_tsc_krig <- predict.gstat(vgm_sor_k, newdata = sorghum.grid, BLUE = FALSE)
names(sor_tsc_krig)

gridded(sor_tsc_krig) <- TRUE
sor_tsc_krig <- as(sor_tsc_krig, "SpatialGridDataFrame")
proj4string(sor_tsc_krig) <- sorghum.grid@proj4string
names(sor_tsc_krig)

# plot and save kriging maps:
windows(w = 8, h = 6)
spplot(sor_tsc_krig["NDVI.PC1.pred"], col.regions=topo.colors(256)(),scales = list(draw = T),
       main = "PC1 NDVI kriging predictions for sorghum yield")
savePlot(filename = "PC1_NDVI_krigpred.tif", type = "tiff")

sor_tsc_krig$NDVI.PC1.sd <- sqrt(sor_tsc_krig$NDVI.PC1.var)

windows(w = 8, h = 6)
spplot(sor_tsc_krig["NDVI.PC1.sd"], col.regions=topo.colors(256)(),scales = list(draw = T),
       main = "PC1 NDVI kriging standard deviations for sorghum yield")
savePlot(filename = "PC1_NDVI_krigsd.tif", type = "tiff")

# set number of simulations:

nsim = 100

# cross validation of variogram

out = gstat.cv(vgm_sor_k, nmax = nmax, nfold = 5) 

# mean error, ideally 0:
mean(out$residual)

# MSPE, ideally small
mean(out$residual^2)

# correlation observed and predicted, ideally close to 1
cor(out$observed, out$observed - out$residual)

# correlation predicted and residual, ideally close to 0

cor(out$observed - out$residual, out$residual)


###### cGc  
str(sorghum.grid)

sor_tsc_sim <- predict.gstat(vgm_sor_k, newdata = sorghum.grid, BLUE = FALSE, nsim = 100, nmax=20,
                               debug.level = -1)
 names(sor_tsc_sim)

# step II - simulate regression coefficients and residual:

# fit a multiple linear model:
lmsor_k = lm(y2009~NDVI.PC1+markd, data = sorghum.ov)
summary(lmsor_k)
anova(lmsor_k)
summary(anova(lmsor_k))
vcov(lmsor_k)
summary(lmsor_k)$sigma

# to generate a mutivariate normal distribution from the mean and variance-covariance of the mutiple linear model
simcoef_sor_k <- rmultnorm(n = nsim, mu = lmsor_k$coefficients,
                               vmat = vcov(lmsor_k), tol = 1e-10)
simresidual_sor_k <- rnorm(n = nsim, mean = 0, sd = summary(lmsor_k)$sigma)

# combining both simulated parameter and input x-values
cs <- t(simcoef_sor_k[,1] + simcoef_sor_k[,2] * t(sor_tsc_sim@data[,c(1:nsim)]) +
  t(simcoef_sor_k[,3] * sor_tsc_sim@data[,c((nsim+1):(2*nsim))]) +
  simresidual_sor_k)

str(cs)
# analyzing output uncertainty with summary statistics

sim <- SpatialPixelsDataFrame(coordinates(sor_tsc_sim), as.data.frame(cs))

str(sim)

str(sorghum.ov$y2009)
min(sorghum.ov$y2009)
max(sorghum.ov$y2009)
mean(sorghum.ov$y2009)
median(sorghum.ov$y2009)
sd(sorghum.ov$y2009)

str(sim$NDVI.PC1.sim1)
min(sim$NDVI.PC1.sim1)
max(sim$NDVI.PC1.sim1)
mean(sim$NDVI.PC1.sim1)
median(sim$NDVI.PC1.sim1)
sd(sim$NDVI.PC1.sim1)
sqrt(var(sim$NDVI.PC1.sim1))

min(sim$NDVI.PC1.sim25)
max(sim$NDVI.PC1.sim25)
mean(sim$NDVI.PC1.sim25)
median(sim$NDVI.PC1.sim25)
sd(sim$NDVI.PC1.sim25)

min(sim$NDVI.PC1.sim75)
max(sim$NDVI.PC1.sim75)
mean(sim$NDVI.PC1.sim75)
median(sim$NDVI.PC1.sim75)
sd(sim$NDVI.PC1.sim75)

min(sim$NDVI.PC1.sim100)
max(sim$NDVI.PC1.sim100)
mean(sim$NDVI.PC1.sim100)
median(sim$NDVI.PC1.sim75)
sd(sim$NDVI.PC1.sim100)

windows(w = 8, h = 6)
spplot(sim, zcol = "NDVI.PC1.sim100", col.regions = topo.colors(256)(), scales = list(draw = T),
       main = "Mean of simulated sorghum")
savePlot(file = "NDVI.PC1.sim100.tif", type = "tiff")
savePlot(file = "NDVI.PC1.sim100.eps", type = "eps")

# Point pattern statistics:
#mg_owin <- as.owin(sim)
#sorghum.ppp <- ppp(x=coordinates(sorghum)[,1], y=coordinates(sorghum)[,2], marks=sorghum$y2009, window=mg_owin)

# plot(sorghum.ppp)
#dist.points <- nndist(sorghum.ppp)
#summary(dist.points)

# Complete Spatial Randomness:
#env.sorghum <- envelope(sorghum.ppp, fun=Gest)
#windows(w = 8, h = 6)
#$plot(env.sorghum, lwd=list(3,1,1,1), main="CRS test (sorghum yield)")

# calculate and plot prediction:
sim$mean <- apply(cs, 1, mean)

min(sim$mean)
max(sim$mean)
mean(sim$mean)
median(sim$mean)


windows(w = 8, h = 6)
spplot(sim, zcol = "mean", col.regions = topo.colors(256)(), scales = list(draw = T),
       main = "Mean of simulated sorghum")
savePlot(file = "TSC_sorghum_pred.tif", type = "tiff")
savePlot(file = "TSC_sorghum_pred.eps", type = "eps")

# calculate and plot standard deviation:
sim$sd <- apply(cs, 1, sd)

windows(w = 8, h = 6)
spplot(sim, zcol = "sd", col.regions=topo.colors(256)(),scales = list(draw = T),
       main = "Standard deviation of simulated sorghum", sp.layout=list("sp.points",sorghum.ov,pch=1))
savePlot(file = "TSC_sorghum_sd.tif", type = "tiff")
savePlot(file = "TSC_sorghum_sd.eps", type = "eps")

#comparing simulation and priction

# histogram:
windows( 5, 5)
hist(sim$mean, col="grey",  xlab="Mean simulated (cGc) sorghum yield (kg ha-1)", main="Histogram")

#

# correlation mean of simulated and predicted, ideally close to 1
#OK
cor(sim$mean, sorghum.ok$var1.pred)
#GWR
cor(sim$mean, mod.gwr$SDF$pred)
#GWR-RK
cor(sim$mean, sorghum.gwr.rk$pred.gwr)
#UK
cor(sim$mean, sorghum.rk$var1.pred)

# constructing variogram and performing cross validation
str(sorghum.grid)
sorghum.grid$sim_mean <- sim$mean
sorghum.grid$sim_sd <- sim$sd 
str(sorghum.grid)

sim.grid <- sorghum.grid  
gridded(sim.grid) <- TRUE
sim.grid <- as(sim.grid, "SpatialGridDataFrame")
proj4string(sim.grid) <- sorghum.grid@proj4string
sorghum.ov1 <- overlay(sim.grid, sorghum)
sorghum.ov$sim_mean <- sorghum.ov1$sim_mean 
sorghum.ov$sim_sd <- sorghum.ov1$sim_sd
str(sorghum.ov)

gsim <- gstat(formula = sim_mean~1, data = sorghum.ov)
sim.svar <- variogram(gsim)
plot(sim.svar)

sim.ivgm <- vgm(nugget=0, model="Bes", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + diff(sorghum.ov@bbox["y",])^2)/4, psill=var(sorghum.ov$sim_mean))
sim.vgm <- fit.variogram(sim.svar, model=sim.ivgm) 
plot(sim.svar, sim.vgm)

sim.vgm.plt <- plot(sim.svar, sim.vgm, pc="+", pl=FALSE, col="black", main="cGc")
# synchronize the two plots and interpret results:
sim.vgm.plt$x.limits <- sorghum.vgm.plt$x.limits
sim.vgm.plt$y.limits <- sorghum.vgm.plt$y.limits
windows(w = 8, h = 6)
print(sorghum.vgm.plt, split=c(1,1,2,1), more=TRUE)
print(sim.vgm.plt, split=c(2,1,2,1), more=FALSE)


############cross validation

sim.xv <- krige.cv(sim_mean~1, sorghum.ov, sim.vgm)

windows(width = 4, height = 4)
hist(sim.xv$var1.pred)

mean(sim.xv$zscore)
sd(sim.xv$zscore)

# amount of variation explained by the simulation mean:
1-var(sim.xv$residual, na.rm=T)/var(sorghum$y2009)

# Calculate the error, rmse and mean error

#error

sim.xv$residual

# Mean error

sum(sim.xv$residual)/length(sim.xv$residual)

#rmse

sqrt(sum(sim.xv$residual^2)/length(sim.xv$residual))


############# SILL comprasons of cGs, cGc and OK

### Sill Comparisons
windows(w = 10, h = 6)
par(mfrow=c(1,3))
print(sorghum.vgm.plt, split=c(1,1,2,1), more=TRUE)
print(sim.vgm.plt, split=c(2,1,2,1), more=TRUE)
print(sim_rk.vgm.plt, split=c(2,1,2,1), more=FALSE)

savePlot(filename = "sim_sill_comaparison.tif", type = "tiff")
dev.off()

######################TRASNFORAMTIONA

was.bc<-box.cox(tala$was, 0.47) 
tala.bc = cbind(tala, was.bc)

coordinates(tala.bc) <- ~ x + y  
coordinates(tala.grid) <-~ x + y 

v.ok.was<-variogram(was.bc~1,data=tala.bc)
plot(v.ok.was, pl=F, pch=20, cex=1, col="Black")
m.ok.was<-vgm(.06,"Bes",4000,0.02)
(m.ok.f.was<-fit.variogram(v.ok.was, m.ok.was))

# Simulation
#--------------
set.seed(130)
sim.was<-krige(was.bc~1,tala.bc,tala.grid,model=m.ok.f.was, nmax=50, 
               nsim = 1000)

# Back-transformation
#-----------------
for(i in 1:length(sim.was@data)){sim.was@data[[i]] <- 
  (((sim.was[[i]]*0.47+1)^k.was))}
sim.data.was<-as.data.frame(sim.was)
summary(sim.data.was)









#### USING PLOTKML

#str(sorghum.grid)
#names(sorghum.grid)[2] <- "markd"
#str(sorghum.ov)
#sorghum.grid.rk <- as(sorghum.grid, "SpatialPixelsDataFrame")

#str(sorghum.grid.rk)

#fit.gstatModel
# fit a model:
#sor.rk <- fit.gstatModel(observations=sorghum.ov, y2009~NDVI.PC1+markd+PERC+PHCR, covariates=sorghum.grid.rk)


#str(sor.rk)
#show(sor.rk@regModel)
#show(sor.rk@vgmModel)
#sor.rk@regModel$model

#sor.rk.sp <- SpatialPointsDataFrame(sor.rk@sp, data = sor.rk@regModel$model)
## Not run: 
# plot a variogram:
#plot(variogram(y2009~1, sor.rk.sp))
#------------------------------------------------------------------------------------

#---------- USING TOM HENGLE

#----------------------------------------------------------------------------------

## variogram modelling RK

names(step.sorghum)
# add residuals to sorghum object
sorghum.ov$resid = step.sorghum$residuals
sorghum.ov$resid

# histogram of the residuals
hist(sorghum.ov$resid)

# calculate sample and experimental variograms of OLS residuals

sorghum.rsvar = variogram(resid~1, data=sorghum.ov)
sorghum.ivgm <- vgm(nugget=0, model="Bes", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + 
  diff(sorghum.ov@bbox["y",])^2)/4, psill=var(residuals(step.sorghum)))
sorghum.rvgm <- fit.variogram(sorghum.rsvar, model=sorghum.ivgm)
sorghum.rvgm # experimental variogram of OLS for CV and OK
plot(sorghum.rsvar, sorghum.rvgm)
sorghum.rvgm.plt <- plot(sorghum.rsvar, sorghum.rvgm, pc="+", pl=FALSE, col="black", main="Residuals")
# synchronize the two plots and interpret results:
sorghum.rvgm.plt$x.limits <- sorghum.vgm.plt$x.limits
sorghum.rvgm.plt$y.limits <- sorghum.vgm.plt$y.limits
windows(w = 8, h = 6)
print(sorghum.vgm.plt, split=c(1,1,2,1), more=TRUE)
print(sorghum.rvgm.plt, split=c(2,1,2,1), more=FALSE)

sum(sorghum.rvgm$psill)
sum(sorghum.vgm$psill)
sum(sorghum.rvgm$psill)/sum(sorghum.vgm$psill)

dev.off()


# interpolate residuals (logits):
sorghum.rk <- krige(residuals(step.sorghum)~1, sorghum.ov, pc.comps, sorghum.rvgm)
# add predicted trend and residuals:
sorghum.rk$var1.rk <- sorghum.glm$sorghum + sorghum.rk$var1.pred
# back-transform logits to the original response scale (0,1):
#sorghum.rk$var1.rko <- exp(sorghum.rk$var1.rk)/(1+exp(sorghum.rk$var1.rk))

write.asciigrid(sorghum.rk["var1.rko"], "sorghum_rk.asc", na.value=-1)
spplot(sorghum.rk["var1.rk"], scales=list(draw=T), at=seq(0.05,1,0.05), col.regions=grey(rev(seq(0,0.95,0.05))), main="Liming requirements", sp.layout=list("sp.polygons", col="black", sorghum.ov))
# prediction accuracy:
sorghum.ov <- overlay(sorghum.rk, sorghum)
sorghum.ov@data <- cbind(sorghum.ov@data, sorghum["y2009"]@data)
library(mda) 
library(vcd) # kappa statistics
Kappa(confusion(sorghum.ov$y2009, as.factor(round(sorghum.ov$var1.rk, 0))))

##Regressin Krigging (RK)  _____________________________________________________________________________
str(sorghum.grid)
sorghum.rk <- krige(y2009~NDVI.PC1+markd, sorghum.ov, newdata = sorghum.grid, sorghum.rvgm)

#Mean UK prediction variance
#calculateMukv(sorghum.ov, sorghum.grid, sorghum.rvgm, y2009~NDVI.PC1+markd)
#library(lattice)
#levelplot(var1.var, sorghum.rk, aspect = "iso", main = "kriging variance")

str(sorghum.grid)
str(sorghum.rk)

str(sorghum.rk$var1.pred)
min(sorghum.rk$var1.pred)
max(sorghum.rk$var1.pred)
mean(sorghum.rk$var1.pred)
median(sorghum.rk$var1.pred)
sd(sorghum.rk$var1.pred)
#mean of RK prediction variance

mean(sqrt(sorghum.rk$var1.var))

#assigning the spatial data frame and projecton system again

gridded(sorghum.rk) <- TRUE
sorghum.rk <- as(sorghum.rk, "SpatialGridDataFrame")
proj4string(sorghum.rk) <- sorghum.grid@proj4string
names(sorghum.rk)

# plot and save kriging maps:
## Sorghum yield predictions map
windows(w = 8, h = 6)
spplot(sorghum.rk["var1.pred"], col.regions=topo.colors(256)(),scales = list(draw = T),
       main = "Sorghum yield map (OLS-RK)")

savePlot(filename = "Sor_rkpred.tif", type = "tiff")
savePlot(filename = "Sor_rkpred.eps", type = "eps")

#standard deviation or uncertainity map
sorghum.rk$var1.sd <- sqrt(sorghum.rk$var1.var)
mean(sorghum.rk$var1.sd)
windows(w = 8, h = 6)
spplot(sorghum.rk["var1.sd"], col.regions=topo.colors(256)(),scales = list(draw = T),
       main = "Uncertainity map (OLS-RK)", sp.layout=list("sp.points",sorghum,pch=1))

savePlot(filename = "Sor_rksd.tif", type = "tiff")
savePlot(filename = "Sor_rksd.eps", type = "eps")



### GWRK Regression__________________________________________________________________________________________

str(sorghum.ov)
str(pc.comps)
sorghum.grid.gwr <- as(pc.comps, "SpatialPixelsDataFrame")
str(sorghum.grid.gwr)
?gwr.sel
#gwr bandwidth 
str(sorghum.ov)
bwG <- gwr.sel(y2009~PC1+PC2+PC3+PC4+PC5, data=sorghum.ov, method = "CV",verbose=T)
bwG

mod.gwr.r <- gwr(y2009~PC1+PC2+PC3+PC4+PC5, data=sorghum.ov, bandwidth=bwG, hatmatrix=T)
mod.gwr.r
anova(mod.gwr.r)
str(mod.gwr.r$SDF)
BFC02.gwr.test(mod.gwr.r)
moran.test(mod.gwr.r$SDF$gwr.e, listw=W_cont_el_mat, zero.policy=T)


mod.gwr <- gwr(y2009~PC1+PC2+PC3+PC4+PC5, data = sorghum.ov, bandwidth=bwG, fit.points = sorghum.grid.gwr,
               predict=TRUE, se.fit=TRUE, fittedGWRobject=mod.gwr.r)
str(mod.gwr)
str(mod.gwr.r$SDF)

### variogram modelling of residuals of GWR __________________________________________________________________
??spgwr
#add residuals 
gwrres <- mod.gwr.r$SDF$gwr.e
sorghum.ov$gwrresid = mod.gwr.r$SDF$gwr.e
gwrres

# plot residuals

head(gwrres)
hist(gwrres)

# More normal distribution

brks <- round(fivenum(gwrres), digits=2)
brks
cols <- rev(heat.colors(4))

#GWR Residuals

X11()
par(mfrow=c(1,1))
#plot(mod.gwr$SDF, col=cols)
plot(mod.gwr$SDF, col=cols[findInterval(gwrres, brks,all.inside=TRUE)])
title(main="Map of Residuals from GWR Model")
legend("bottom", legend = leglabs(brks,"<", ">="),
       fill = cols,bty = "n", cex = 0.9, y.intersp = 0.9, title="Residuals")

## Residual Autocorrelation

moran.test(sorghum.ov$gwrresid, listw=W_cont_el_mat, zero.policy=T)

#### GWR residuals variogram modelling

sorghum.gwrrsvar = variogram(gwrres~1, data=sorghum.ov)
sorghum.gwrivgm <- vgm(nugget=0, model="Bes", range=sqrt(diff(sorghum.ov@bbox["x",])^2 + diff(sorghum.ov@bbox["y",])^2)/4, psill=var(gwrres))

sorghum.gwrrvgm <- fit.variogram(sorghum.gwrrsvar, model=sorghum.gwrivgm)
sorghum.gwrrvgm # experimental variogram of GWR for CV and OK
plot(sorghum.gwrrsvar, sorghum.gwrrvgm)
sorghum.gwrrvgm.plt <- plot(sorghum.gwrrsvar, sorghum.gwrrvgm, pc="+", pl=FALSE, col="black", main="Residuals")

# synchronize the two plots and interpret results:
sorghum.gwrrvgm.plt$x.limits <- sorghum.vgm.plt$x.limits
sorghum.gwrrvgm.plt$y.limits <- sorghum.vgm.plt$y.limits
windows(w = 8, h = 6)
print(sorghum.vgm.plt, split=c(1,1,2,1), more=TRUE)
print(sorghum.gwrrvgm.plt, split=c(2,1,2,1), more=FALSE)

sum(sorghum.gwrrvgm$psill) # psill of the GWR residuals variogram
sum(sorghum.rvgm$psill) # psill of the OLS residuals variogram
sum(sorghum.vgm$psill) # # psill of the OK residuals 
sum(sorghum.gwrrvgm$psill)/sum(sorghum.vgm$psill)

### Sill Comparisons
windows(w = 10, h = 6)
par(mfrow=c(1,3))
print(sorghum.vgm.plt, split=c(1,1,2,1), more=TRUE)
print(sorghum.rvgm.plt, split=c(2,1,2,1), more=TRUE)
print(sorghum.gwrrvgm.plt, split=c(2,1,2,1), more=FALSE)

### Sill Comparisons
windows(w = 8, h = 6)
print(sorghum.vgm.plt, split=c(1,1,1,1), more=TRUE)
print(sorghum.rvgm.plt, split=c(1,1,1,1), more=TRUE)
print(sorghum.gwrrvgm.plt, split=c(1,1,1,1), more=FALSE)

savePlot(filename = "sill_comaparison.tif", type = "tiff")
dev.off()

str(mod.gwr$SDF$pred)
min(mod.gwr$SDF$pred)
max(mod.gwr$SDF$pred)
mean(mod.gwr$SDF$pred)
median(mod.gwr$SDF$pred)

#prediction variance

mean(sqrt(var(mod.gwr$SDF$pred.se)))
sd(mod.gwr$SDF$pred.se)

# return back to grid and attach spatial reference:
sorghum.grid.gwr$sorghum_yield <- mod.gwr$SDF$pred
sorghum.grid.gwr$sorghum_yield_var <- mod.gwr$SDF$pred.se
gridded(sorghum.grid.gwr) <- TRUE
sorghum.grid.gwr <- as(sorghum.grid.gwr, "SpatialGridDataFrame")
proj4string(sorghum.grid.gwr) <- sorghum.grid@proj4string
str(sorghum.grid.gwr)


#Sorghum yield predictions map (GWR)
windows(w = 8, h = 6)
spplot(sorghum.grid.gwr["sorghum_yield"], col.regions=topo.colors(256)(), scales = list(draw = T), 
       main = "Sorghum yield map (GWR)")

savePlot(filename = "Sor_GWR.tif", type = "tiff")
savePlot(filename = "Sor_GWR.eps", type = "eps")

#Sorghum yield prediction variance map (GWR)

windows(w = 8, h = 6)
spplot(sorghum.grid.gwr["sorghum_yield_var"], col.regions=topo.colors(256)(),scales = list(draw = T),
       main = "Uncertainity map (GWR)", sp.layout=list("sp.points",sorghum,pch=1))

savePlot(filename = "Sor_GWR_sd.tif", type = "tiff")
savePlot(filename = "Sor_GWR_sd.eps", type = "eps")

### Rgressin Krigging (RK)  -  GWR  ________________________________________________________________________
str(sorghum.ov)

# interpolate residuals:
sorghum.gwr.rk = krige(gwrresid~1, sorghum.ov, sorghum.grid.gwr, sorghum.gwrrvgm)
sorghum.gwrrvgm
#GWR resiual prediction each grid cell
names(sorghum.gwr.rk)

#predicted GWR trend (from running GWR on each grid cell)
str(mod.gwr$SDF$pred)
min(mod.gwr$SDF$pred)
max(mod.gwr$SDF$pred)
mean(mod.gwr$SDF$pred)
median(mod.gwr$SDF$pred)

#mean of GWR prediction variance 
mean(sqrt(var(mod.gwr$SDF$pred.se)))

# adding GWR predicted trend and predicted residuals on each cell:

sorghum.gwr.rk$pred.gwr <- (mod.gwr$SDF$pred+sorghum.gwr.rk$var1.pred)
names(sorghum.gwr.rk)
str(sorghum.gwr.rk$pred.gwr)
min(sorghum.gwr.rk$pred.gwr)
max(sorghum.gwr.rk$pred.gwr)
mean(sorghum.gwr.rk$pred.gwr)
median(sorghum.gwr.rk$pred.gwr)

# standard deviatian of prediction = sqrt of prediction variance = prediction error
# variance = nuggut + partial sill
# sqrt of (nuggut + partial sill) 

#mean of standard deviatian of prediction of GWR residuals
mean(sqrt(sorghum.gwr.rk$var1.var))

# standard deviatian of prediction GWR-K

sd_GWRRK <- mod.gwr$SDF$pred.se + sqrt(sorghum.gwr.rk$var1.var) - sqrt(sorghum.gwrrvgm[2,2])
sd_GWRRK
mean(sd_GWRRK)

sd_GWRRK2 <- sqrt(var(mod.gwr$SDF$pred.se))+sqrt(sorghum.gwr.rk$var1.var)-sqrt(sorghum.gwrrvgm[2,2])
sd_GWRRK2
mean(sd_GWRRK2)

#prediction error of GWR residuals
sorghum.gwr.rk$var1.sd <- sqrt(sorghum.gwr.rk$var1.var)
mean(sorghum.gwr.rk$var1.sd)

#prediction error of GWR-K 
sorghum.gwr.rk$var1.sd <- sd_GWRRK
mean(sorghum.gwr.rk$var1.sd)

#assigning the spatial data frame and projecton system again

gridded(sorghum.gwr.rk) <- TRUE
sorghum.gwr.rk <- as(sorghum.gwr.rk, "SpatialGridDataFrame")
proj4string(sorghum.gwr.rk) <- sorghum.grid@proj4string
names(sorghum.gwr.rk)

#Sorghum yield predictions map GWR-RK
windows(w = 8, h = 6)
spplot(sorghum.gwr.rk["var1.rk"], col.regions=topo.colors(256)(), scales = list(draw = T), 
       main = "Sorghum yield map (GWR-RK)")

savePlot(filename = "Sor_GWRrk.tif", type = "tiff")
savePlot(filename = "Sor_GWRrk.eps", type = "eps")

write.asciigrid(sorghum.gwr.rk["var1.rk"], "sorghum.gwr.rk.asc", na.value=-1)

#Sorghum yield prediction variance map GWR-RK

windows(w = 8, h = 6)
spplot(sorghum.gwr.rk["var1.sd"], col.regions=topo.colors(256)(),scales = list(draw = T),
       main = "Uncertainity map (GWR-RK)", sp.layout=list("sp.points",sorghum.ov,pch=1))

savePlot(filename = "Sor_GWRrksd.tif", type = "tiff")
savePlot(filename = "Sor_GWRrksd.eps", type = "eps")

write.asciigrid(sorghum.gwr.rk["var1.sd"], "sorghum.gwr.sd.asc", na.value=-1)

# prediction accuracy:

sorghum.ov.gwr.rk <- overlay(sorghum.gwr.rk, sorghum.OV)
str(sorghum.ov.gwr.rk)
sorghum.ov.gwr.rk@data <- cbind(sorghum.ov.gwr.rk@data, sorghum[c("y2009", "popden")]@data)
str(sorghum.ov.gwr.rk)

#library(mda) 
#library(vcd) # kappa statistics
#Kappa(confusion(sorghum.ov.gwr.rk$y2009, as.factor(round(sorghum.ov.gwr.rk$var1.rk, 0))))





