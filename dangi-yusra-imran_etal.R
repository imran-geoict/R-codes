# title         : crop_yield_CAR_GWR_imran_etal_Rcode.R
# purpose       : R code for the paper "crop_yield_uncertainity_imran_etal";
# reference     : 
# producer      : Muhammad Imran, PhD Researcher, Dept Geoinformation Processing, Faculty of Geo-Information Science and Earth Observation (ITC), University of Twente. Postal: Hengelosestraat 99 PO Box 217, 7500 AE Enschede, The Netherlands 
# Email         : imran19632@itc.nl
# inputs        : 201 samples of crop crop yield in Burkina Faso (kg ha), along variables, principal component of SPOT NDVI coverig the growing period (NDVI.PC1), and market access (markd) ;
# outputs       : crop prediction and prediction uncertainty maps;

# ------------------------------------------------------------
# Initial settings:
# ------------------------------------------------------------

setwd('D:/RESEARCH_PAPERS_IN_PROGRESS/Dengue_Paper_Yasra')

options(prompt="> ", continue="+ ", digits=3, width=70, show.signif.stars=T)
setInternet2(use=TRUE)  
rm(list=ls()) 

# load necessary packages:

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
library(splancs)      ## Kernel Density
library(pgirmess)     ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(MSBVAR)
library(moments)
library(nortest)
library(SeleMix)
library(relaimpo)
library(MASS)
library(johnson)
library(ggplot2)
library(gplots)
# library(GeoXp)
library(DAAG)
library(lmtest)
library(frontier)
library(bbmle)
library(Johnson)

#install.pakages("gwrr")
library(ggplot2)

sessionInfo()

# get help for some package:
#help(package="gstat")
# ------------------------------------------------------------
# Import of data to R and data analysis
# ------------------------------------------------------------

#----------------------------------------------

# loading external covariate data

#----------------------------------------------


# load the data:

################ LOAD Dangi Observations***********
# dangi_semiarid <- read.csv("dangi_semiarid.csv")

dangi <- readShapePoints("RVpca.shp")
str(dangi)
plot(dangi)

#coordinates(dangi) <- ~coords.x1+coords.x2
proj4string(dangi) <- CRS("+init=epsg:32643")
str(dangi)

# data gung buksh study area


dgb.poly <- readShapePoly("Export_Output_2.shp")
proj4string(dgb.poly) <- CRS("+init=epsg:32643")
plot(dgb.poly)
str(dgb.poly)


dev.off()
plot(dgb.poly)

spplot(dangi, zcol="larave", sp.layout=list("sp.polygons", dgb.poly))

ras_pc1 <- raster("dangi_pc1_lhr.tif")
plot(ras_pc1)
plot(dgb.poly, bg="transparent", add=TRUE)

ras_pc2 <- raster("dangi_pc2_lhr.tif")
plot(ras_pc2)
plot(dgb.poly, bg="transparent", add=TRUE)

ras_pc3 <- raster("dangi_pc3_lhr.tif")
plot(ras_pc3)
plot(dgb.poly, bg="transparent", add=TRUE)

dangi_rainfal_lhr <- raster('dangi_rainfal_inter.tif')
dangi_rainfal_lhr <- projectRaster(dangi_rainfal_lhr, raster("dangi_pc2_lhr.tif"))
mask(dangi_rainfal_lhr, raster("dangi_pc2_lhr.tif"), format="GTiff", 
     filename="D:/labs/SecondPaper/dangi_rainfal_lhr.tif")
plot(raster("dangi_rainfal_lhr.tif"))

dangi_temp_int <- raster('dangi_temp_lhr.tif')
dangi_temp_int <- projectRaster(dangi_temp_int, raster("dangi_pc2_lhr.tif"))
mask(dangi_temp_int, raster("dangi_pc2_lhr.tif"), format="GTiff", 
     filename="D:/labs/SecondPaper/dangi_temp_int.tif")
plot(raster("dangi_temp_int.tif"))




# loading tifs


grid.list <- c("dangi_pc1_lhr.tif", "dangi_pc2_lhr.tif", "dangi_pc3_lhr.tif", "dangi_rainfal_lhr.tif", "dangi_temp_int.tif")
s <- stack(grid.list)
dangi.grid <- as(s,"SpatialPointsDataFrame")
str(dangi.grid)


names(dangi.grid)[1]
dangi.grid$dangi_pc1_lhr <- ifelse ( is.na(dangi.grid$dangi_pc1_lhr)=="TRUE", mean(dangi.grid$dangi_pc1_lhr,na.rm=TRUE),dangi.grid$dangi_pc1_lhr)
mean(dangi.grid$dangi_pc1_lhr)
median(dangi.grid$dangi_pc1_lhr)
max(dangi.grid$dangi_pc1_lhr)
min(dangi.grid$dangi_pc1_lhr)
sqrt(var(dangi.grid$dangi_pc1_lhr))
skewness(dangi.grid$dangi_pc1_lhr)

names(dangi.grid)[2]
dangi.grid$dangi_pc2_lhr <- ifelse ( is.na(dangi.grid$dangi_pc2_lhr)=="TRUE", mean(dangi.grid$dangi_pc2_lhr,na.rm=TRUE),dangi.grid$dangi_pc2_lhr)
mean(dangi.grid$dangi_pc2_lhr)
median(dangi.grid$dangi_pc2_lhr)
max(dangi.grid$dangi_pc2_lhr)
min(dangi.grid$dangi_pc2_lhr)
sqrt(var(dangi.grid$dangi_pc2_lhr))
skewness(dangi.grid$dangi_pc2_lhr)

names(dangi.grid)[3]
dangi.grid$dangi_pc3_lhr <- ifelse ( is.na(dangi.grid$dangi_pc3_lhr)=="TRUE", mean(dangi.grid$dangi_pc3_lhr,na.rm=TRUE),dangi.grid$dangi_pc3_lhr)
mean(dangi.grid$dangi_pc3_lhr)
median(dangi.grid$dangi_pc3_lhr)
max(dangi.grid$dangi_pc3_lhr)
min(dangi.grid$dangi_pc3_lhr)
sqrt(var(dangi.grid$dangi_pc3_lhr))
skewness(dangi.grid$dangi_pc3_lhr)

names(dangi.grid)[4]
dangi.grid$dangi_rainfal_lhr <- ifelse ( is.na(dangi.grid$dangi_rainfal_lhr)=="TRUE", mean(dangi.grid$dangi_rainfal_lhr,na.rm=TRUE),dangi.grid$dangi_rainfal_lhr)
mean(dangi.grid$dangi_rainfal_lhr)
median(dangi.grid$dangi_rainfal_lhr)
max(dangi.grid$dangi_rainfal_lhr)
min(dangi.grid$dangi_rainfal_lhr)
sqrt(var(dangi.grid$dangi_rainfal_lhr))
skewness(dangi.grid$dangi_rainfal_lhr)

names(dangi.grid)[5]
dangi.grid$dangi_temp_int <- ifelse ( is.na(dangi.grid$dangi_temp_int)=="TRUE", mean(dangi.grid$dangi_temp_int,na.rm=TRUE),dangi.grid$dangi_temp_int)
mean(dangi.grid$dangi_temp_int)
median(dangi.grid$dangi_temp_int)
max(dangi.grid$dangi_temp_int)
min(dangi.grid$dangi_temp_int)
sqrt(var(dangi.grid$dangi_temp_int))
skewness(dangi.grid$dangi_temp_int)



### Figures
tiff(filename = "d:\\Fig04.tiff", res = 300, width=22, height=20,  units = "cm", compression = "lzw")
pc.fig <- c("dangi_pc1_lhr.tif", "dangi_pc2_lhr.tif", "dangi_pc3_lhr.tif")
pc.s <- stack(pc.fig)
names(pc.s) <- c('PC1.NDVI', 'PC2.NDVI', 'PC3.NDVI')
spplot(pc.s, col.regions=rev(heat.colors(256)),  gridded=T)
dev.off()

?spplot
#plotting global logit (a) and gwr logit (b)


tiff(filename = "d:\\Fig03.tiff", res = 300, width=22, height=20,  units = "cm", compression = "lzw")
en.fig <- c("dangi_rainfal_lhr.tif", "dangi_temp_int.tif")
en.s <- stack(en.fig)
names(en.s) <- c('PERC', 'TEMP')
plot(en.s, col=rev(heat.colors(256)),  gridded=T)
#spplot(en.s, c("dangi_rainfal_lhr"), col.regions=heat.colors(256), scales = list(draw = T))
#spplot(en.s, c("dangi_temp_int"), col.regions=heat.colors(256), scales = list(draw = T))

dev.off()
?plot

##################### OVERLAY ####################################

tiff(filename = "d:\\Fig03.tiff", res = 600, width=12, height=8,  units = "cm", compression = "lzw")

dangi.ov <- data.frame(dangi,extract(s,dangi))
str(dangi.ov)
#coordinates(dangi.ov) <- ~coords.x2+coords.x1
coordinates(dangi.ov) <- ~lat__X_+long__Y_
proj4string(dangi.ov) <- dangi.grid@proj4string
str(dangi.ov)



names(dangi.ov)[17]
dangi.ov$dangi_pc1_lhr <- ifelse (is.na(dangi.ov$dangi_pc1_lhr)=="TRUE", mean(dangi.ov$dangi_pc1_lhr,na.rm=TRUE),dangi.ov$dangi_pc1_lhr)
mean(dangi.ov$dangi_pc1_lhr)
median(dangi.ov$dangi_pc1_lhr)
max(dangi.ov$dangi_pc1_lhr)
min(dangi.ov$dangi_pc1_lhr)
sqrt(var(dangi.ov$dangi_pc1_lhr))
skewness(dangi.ov$dangi_pc1_lhr)

names(dangi.ov)[18]
dangi.ov$dangi_pc2_lhr <- ifelse ( is.na(dangi.ov$dangi_pc2_lhr)=="TRUE", mean(dangi.ov$dangi_pc2_lhr,na.rm=TRUE),dangi.ov$dangi_pc2_lhr)
mean(dangi.ov$dangi_pc2_lhr)
median(dangi.ov$dangi_pc2_lhr)
max(dangi.ov$dangi_pc2_lhr)
min(dangi.ov$dangi_pc2_lhr)
sqrt(var(dangi.ov$dangi_pc2_lhr))
skewness(dangi.ov$dangi_pc2_lhr)

names(dangi.ov)[19]
dangi.ov$dangi_pc3_lhr <- ifelse ( is.na(dangi.ov$dangi_pc3_lhr)=="TRUE", mean(dangi.ov$dangi_pc3_lhr,na.rm=TRUE),dangi.ov$dangi_pc3_lhr)
mean(dangi.ov$dangi_pc3_lhr)
median(dangi.ov$dangi_pc3_lhr)
max(dangi.ov$dangi_pc3_lhr)
min(dangi.ov$dangi_pc3_lhr)
sqrt(var(dangi.ov$dangi_pc3_lhr))
skewness(dangi.ov$dangi_pc3_lhr)

names(dangi.ov)[20]
dangi.ov$dangi_rainfal_lhr <- ifelse ( is.na(dangi.ov$dangi_rainfal_lhr)=="TRUE", mean(dangi.ov$dangi_rainfal_lhr,na.rm=TRUE),dangi.ov$dangi_rainfal_lhr)
mean(dangi.ov$dangi_rainfal_lhr)
median(dangi.ov$dangi_rainfal_lhr)
max(dangi.ov$dangi_rainfal_lhr)
min(dangi.ov$dangi_rainfal_lhr)
sqrt(var(dangi.ov$dangi_rainfal_lhr))
skewness(dangi.ov$dangi_rainfal_lhr)

names(dangi.ov)[21]
dangi.ov$dangi_temp_int <- ifelse ( is.na(dangi.ov$dangi_temp_int)=="TRUE", mean(dangi.ov$dangi_temp_int,na.rm=TRUE),dangi.ov$dangi_temp_int)
mean(dangi.ov$dangi_temp_int)
median(dangi.ov$dangi_temp_int)
max(dangi.ov$dangi_temp_int)
min(dangi.ov$dangi_temp_int)
sqrt(var(dangi.ov$dangi_temp_int))
skewness(dangi.ov$dangi_temp_int)



###########SPLITING dataset into training and test DATA SETS #######

splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/4))
  trainset <- dataframe[-trainindex, ]
  testset <- dataframe[trainindex, ]
  list(trainset=trainset,testset=testset)
}

str(dangi.ov)

#apply the function
splits <- splitdf(dangi.ov, seed=808)
?split
#it returns a list - two data frames called trainset and testset
str(splits)

# there are 233 observations in training data set and 77 in test dataset
lapply(splits,nrow)

#view the first few columns in each data frame
lapply(splits,head)

# save the training and testing sets as data frames
dangi.ov.training <- splits$trainset
dangi.ov.test <- splits$testset

str(dangi.ov.training)
str(dangi.ov.test)
?writeOGR

writeOGR(obj=dangi.ov.training, dsn='D:/labs/SecondPaper', layer="dangi.ov.training", driver="ESRI Shapefile")
writeOGR(obj=dangi.ov.test, dsn='D:/labs/SecondPaper', layer="dangi.ov.test", driver="ESRI Shapefile")

df <- data.frame(dangi.ov.training)
write.csv(dangi.ov.training, file = "dangi.ov.training.csv")

########## CREATING TEMPRATURE SURFACE USING OK ########################

### PART 1 - Variogram Modelling (OK) 

mean(dangi.ov$dangi_temp_int)
median(dangi.ov$dangi_temp_int)
max(dangi.ov$dangi_temp_int)
min(dangi.ov$dangi_temp_int)
str(dangi)


#descriptive analysis (normality tests) sorghum crop yield
m_temp <- mean(dangi.ov$dangi_temp_int)
sd_temp <- sd(dangi.ov$dangi_temp_int)

#descrptive statistics Temp

plot(density(dangi$Temp), col = "darkblue", main = "Temprarture Observations",
     lwd = 1.5)
rug(dangi$Temp, col = "darkgreen")

hist((dangi$Temp), main="Histogram of observed data")

hist((dangi.ov$dangi_temp_int),main="Histogram of temprature data")

fitdistr(dangi.ov$dangi_temp_int,"normal")

m_temp
sd_temp

x.norm<-rnorm(n=310,m=32.6, sd=2.18)
plot(density(x.norm), col = "darkblue", main = "Temprarture Observations",
     lwd = 1.5)

hist(x.norm,main="Histogram of normal data")
qqplot(dangi$Temp,x.norm) ## QQ-plot

windows(6,6)
qqplot(dangi.ov$dangi_temp_int, x.norm, plot.it = TRUE, xlab = deparse(substitute(dangi$Temp)),
       ylab = deparse(substitute(x.norm)))
abline(0,1) ## a 45-degree reference line is plotted

#descrptive statistics Rainfal

# Rainfall 
mean(dangi.ov$dangi_rainfal_lhr)
median(dangi.ov$dangi_rainfal_lhr)
max(dangi.ov$dangi_rainfal_lhr)
min(dangi.ov$dangi_rainfal_lhr)

#Standard error of skewness 
se.sk <- sqrt(6/length(dangi$Rainfal))
1.96*se.sk
skewness(dangi$Rainfal)

# Rainfall

hist((dangi.ov$dangi_rainfal_lhr),main="Histogram of observed data")
qqnorm(dangi.ov$dangi_rainfal_lhr)
qqline(dangi.ov$dangi_rainfal_lhr, col = "red")
plot(density(dangi.ov$dangi_rainfal_lhr), col = "darkblue", main = "Rainfal Observations",
     lwd = 1.5)



# Log transformation

# A signed logarithm takes the logarithm of the absolute value of the
# variable and multiplies by the appropriate sign. 
# Values with absolute value less than one are mapped to zero.

signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}


qqnorm(signedlog10(dangi$Rainfal))
qqline(signedlog10(dangi$Rainfal), col = "red")
hist((signedlog10(dangi$Rainfal)),main="Histogram of observed rainfal data")


# Jonson Transformation   


rainfal_johnson <- RE.Johnson(dangi$Temp)

hist(rainfal_johnson$transformed, breaks = 25, main="Histogram of transformed rainfal data")
qqnorm(rainfal_johnson$transformed)
qqline(rainfal_johnson$transformed, col = "red")
plot(density(rainfal_johnson$transformed), col = "darkblue", main = "Rainfal transformed Observations",
     lwd = 1.5)



#Temp data



hist((dangi$Temp),main="Histogram of observed Temprature data")
qqnorm(dangi$Temp)
qqline(dangi$Temp, col = "red")
plot(density(dangi$Temp), col = "darkblue", main = "Temp Observations",
     lwd = 1.5)



# Jonson Transformation   


Temp_johnson <- RE.Johnson(dangi$Temp)

hist(Temp_johnson$transformed, breaks = 25, main="Histogram of transformed Temp data")
qqnorm(Temp_johnson$transformed)
qqline(Temp_johnson$transformed, col = "red")
plot(density(Temp_johnson$transformed), col = "darkblue", main = "Temp transformed Observations",
     lwd = 1.5)

# Variogram modeling raifall



dangi$rainfal_tr <- rainfal_johnson$transformed
dangi$temp_tr <- Temp_johnson$transformed

str(dangi)

################### PERFORMING OLS & STEP OLS#########################

#dangi.ov.training <-readShapePoints("dangi.ov.training.shp")
#proj4string(dangi.ov.training) <- CRS("+init=epsg:32643")

str(dangi.ov.training)

lm.dangi = lm(larave~dangi_temp_int+dangi_rainfal_lhr+dangi_pc1_lhr+dangi_pc2_lhr+dangi_pc3_lhr, data=dangi.ov.training) 
              
#step.lm.dangi = step.glm(larave~dangi_temp_int+dangi_rainfal_lhr+dangi_pc1_lhr+dangi_pc2_lhr+dangi_pc3_lhr, data=dangi.ov.training) 

                ##Pca1__RAST+Pca2_RASTE+pca3__RAST, data=dangi)
?step
step
summary(lm.dangi)

anova(lm.dangi)

AIC(lm.dangi)

cor(, dangi_temp_int)

# corelation analysis
?cor
cor(dangi.ov.training$larave, dangi.ov.training$dangi_pc1_lhr, use="complete.obs", method = "pearson")
cor.test(dangi.ov.training$larave, dangi.ov.training$dangi_pc1_lhr, use="complete.obs", method = "pearson", conf.level = 0.95)

cor(dangi.ov.training$larave, dangi.ov.training$dangi_temp_int, use="complete.obs", method = "pearson")
cor.test(dangi.ov.training$larave, dangi.ov.training$dangi_temp_int, use="complete.obs", method = "pearson", conf.level = 0.95)

cor(dangi.ov.training$larave, dangi.ov.training$dangi_rainfal_lhr, use="complete.obs", method = "pearson")
cor.test(dangi.ov.training$larave, dangi.ov.training$dangi_rainfal_lhr, use="complete.obs", method = "pearson", conf.level = 0.95)

cor(dangi.ov.training$larave, dangi.ov.training$dangi_pc3_lhr, use="complete.obs", method = "pearson")
cor.test(dangi.ov.training$larave, dangi.ov.training$dangi_pc3_lhr, use="complete.obs", method = "pearson", conf.level = 0.95)


cor(dangi.ov.training$larave, dangi.ov.training$dangi_pc2_lhr, use="complete.obs", method = "pearson")
cor.test(dangi.ov.training$larave, dangi.ov.training$dangi_pc2_lhr, use="complete.obs", method = "pearson", conf.level = 0.95)



#Figures

tiff(filename = "d:\\Fig04.tiff", res = 600, width=20, height=20,  units = "cm", compression = "lzw")
pairs(larave~Temp+Rainfal+Pca1__RAST+Pca2_RASTE+pca3__RAST, dangi.ov.training)
dev.off()

## Contiguity Neighbors
coords <- coordinates(dangi.ov.training)
col.rel.nb<- graph2nb(relativeneigh(coords), sym=TRUE)
W_cont_el_mat <- nb2listw(col.rel.nb, style="W", zero.policy=TRUE)
moran.test(lm.dangi$residuals, listw=W_cont_el_mat, zero.policy=T)


#----------------- OLS Tests

# multicoleanrity

vif(lm.dangi)

plot(density(lm.dangi$residuals), col = "darkblue", main = "GWR residuals",
     lwd = 1.5)




# 
#----------------------------------------------------------------

### Global and GWR Logistic Regression AND PREDICTION

#-------------------------------------------------------------------


##Global logistic regression



#dangi.ov$larave_log <- factor(dangi.ov$larave)


dangi_logit <- glm(larave ~ dangi_temp_int+dangi_rainfal_lhr+dangi_pc1_lhr+dangi_pc2_lhr+dangi_pc3_lhr, 
                   data=dangi.ov.training, 
                   family=binomial(link='logit'))
?glm
dangi_logit
summary(dangi_logit)

vif(dangi_logit)

#RSS Residual sum of sequar

sum(residuals(dangi_logit, type = "pearson")^2)


residuals(dangi_logit, type = "gof")

anova(dangi_logit)

coefficients(dangi_logit)

dangi_logit$aic
dangi_logit$df.residual

# calculating R2

library(pscl)

pR2(dangi_logit)

dangi_logit$fitted.values

fitted.values <- ifelse(dangi_logit$fitted.values > 0.5,1,0)

over(fitted.values, dangi.ov.test)

misClasificError <- mean(over(fitted.values, dangi.ov.test$larave) != dangi.ov.test$larave)
print(paste('Accuracy',1-misClasificError))



#In the steps above, we briefly evaluated the fitting of the model, 
#now we would like to see how the model is doing when predicting y 
#on a new set of data. By setting the parameter type='response', 
#R will output probabilities in the form of P(y=1|X).
# Our decision boundary will be 0.5. If P(y=1|X) > 0.5 then y = 1 
#otherwise y=0. Note that for some applications different 
#thresholds could be a better option.

#fitted.results <- predict(dangi_logit, newdata=dangi.ov.training, type='response')
#fitted.results


#fitted.results <- ifelse(fitted.results > 0.5,1,0)


#misClasificError <- mean(fitted.results != dangi.ov.test$larave)
#print(paste('Accuracy',1-misClasificError))


## autocorelation of OLS residuals

moran.test(dangi_logit$residuals, listw=W_cont_el_mat, zero.policy=T)

plot(density(dangi_logit$residuals), col = "darkblue", main = "Step forward logistic residuals",
     lwd = 1.5)


## Fitting over grid



dangi.logit.grid <- as(dangi.grid, "SpatialPointsDataFrame")
str(dangi.logit.grid)

dangi.logit.grid$fitted.results <- predict(dangi_logit, newdata=dangi.grid, type='response')

dangi.logit.grid$fitted.results <- ifelse(dangi.logit.grid$fitted.results > 0.5,1,0)

dangi.logit.grid$fitted.results

dangi.grid$pred.lr_tr <- dangi.logit.grid$fitted.results

dangi.grid$pred.lr_tr

gridded(dangi.logit.grid) <- TRUE
dangi.logit.grid <- as(dangi.logit.grid, "SpatialGridDataFrame")

dangi.logit.grid$fitted.results

# str(dangi.grid)

spplot(dangi.logit.grid, "fitted.results", asp=1, col.regions=grey(0:256/256), cuts=256, cex.color='black', cex=1.5, cex.labels=1.5, cex.axis=1.5)

spplot(dangi.logit.grid, "fitted.results", col.regions=topo.colors(256), scales = list(draw = T))

ramp <- seq(from=0, to=1, by=0.2)

spplot(dangi.logit.grid, "fitted.results", asp=0, col.regions=heat.colors(256), axes=TRUE)







?spplot

library(ggmap)
library(rgdal)

merc = CRS("+init=epsg:3857")
WGS84 = CRS("+init=epsg:4326")

dangi.ll = spTransform(dangi.grid, WGS84)
bgMap = get_map(as.vector(bbox(dangi.ll)), source = "google", zoom = 13) # useless without zoom level

par(mar = rep(0,4))
plot(spTransform(dangi.grid, merc), bgMap = bgMap, pch = 16, cex = .5)


spplot(spTransform(dangi.grid, merc), c("fitted.results"), colorkey = TRUE,
       sp.layout = list(panel.ggmap, bgMap, first = TRUE))

# 
misClasificError <- mean(fitted.results != dangi.ov.test$larave)
print(paste('Accuracy',1-misClasificError))




### STEP LOGISTIC 



# Generate a full model (using all the explanatory variables)

#fullGlm = glm(
 # larave~dangi_temp_int+dangi_rainfal_lhr+dangi_pc1_lhr+dangi_pc2_lhr+dangi_pc3_lhr,
 # data=dangi.ov.training,
  #family = binomial()
#)

#summary(fullGlm)

# Generate an empty model

#emptyGlm = glm(larave ~ 1, data = dangi.ov.training,family=binomial)

# Do forward stepwise lgistic regressionregression

#dangi.step.logit = step(
 # emptyGlm,
#  scope = list(lower=formula(emptyGlm),upper=formula(fullGlm)),
 # direction='forward'
#)

#summary(dangi.step.logit)

#anova(dangi.step.logit)

# calculating R2

#library(pscl)

#pR2(dangi.step.logit)


#In the steps above, we briefly evaluated the fitting of the model, 
#now we would like to see how the model is doing when predicting y 
#on a new set of data. By setting the parameter type='response', 
#R will output probabilities in the form of P(y=1|X).
# Our decision boundary will be 0.5. If P(y=1|X) > 0.5 then y = 1 
#otherwise y=0. Note that for some applications different 
#thresholds could be a better option.

#fitted.results <- predict(dangi.step.logit, newdata=dangi.ov.training, type='response')
#fitted.results
#fitted.results <- ifelse(fitted.results > 0.5,1,0)
#fitted.results


?extract


####################### LOGISTIC GWR ######################

# return back to grid and attach spatial reference:


gridded(dangi.grid) <- TRUE
dangi.grid.gwr <- as(dangi.grid, "SpatialGridDataFrame")
proj4string(dangi.grid.gwr) <- dangi.grid@proj4string

str(dangi.grid)
?GWmodel


#Estimating basic GWR Logistic model

bwG_tr <- gwr.sel(dangi_logit, 
                  data=dangi.ov.training, 
                  #family=binomial(link='logit'),
                  adapt=TRUE, method = "AIC", verbose=T)
bwG_tr

mod.gwr.r.basic <- gwr(dangi_logit, 
                       data=dangi.ov.training, 
                       adapt=0.146, 
                       gweight=gwr.Gauss,
                       #family=binomial(link='logit'),
                       predictions=TRUE, 
                       se.fit.CCT=TRUE, 
                       se.fit=TRUE, 
                       hatmatrix=T)

?gwr
mod.gwr.r.basic
summary(mod.gwr.r.basic)
mod.gwr.r.basic$results$AICc    # compare to global model
BFC99.gwr.test(mod.gwr.r.basic) 
BFC02.gwr.test(mod.gwr.r.basic) 

mod.gwr.r.basic$SDF$pred # Model prediction

mod.gwr.r.basic$SDF$gwr.e # Model error

mean(mod.gwr.r.basic$SDF$localR2) #local R2
str(mod.gwr.r.basic$SDF)

mod.gwr.r.basic
str(mod.gwr.r.basic$SDF)
anova(mod.gwr.r.basic, test="Chisq")
mod.gwr.r.basic$SDF$gwr.e

moran.test(mod.gwr.r.basic$SDF$gwr.e, listw=W_cont_el_mat, zero.policy=T)

mod.gwr.r.basic$SDF$pred

gwr.fitted.values <- ifelse(mod.gwr.r.basic$SDF$pred > 0.5,1,0)

misClasificError <- mean(gwr.fitted.values != dangi.ov.training$larave)
print(paste('Accuracy',1-misClasificError))

fitted.values

sigTest = abs(mod.gwr.r.basic$SDF$dangi_temp_int) -2*mod.gwr.r.basic$SDF$dangi_temp_int_se 

sigTest #must be less than zero to be statistically signifucant 

#### SAVING GWR COEFFICIENTS TO DISPLAY IN GEODA

dangi.ov.training$dangi_temp_int_gwr <- mod.gwr.r.basic$SDF$dangi_temp_int  

dangi.ov.training$dangi_rainfal_lhr_gwr <- mod.gwr.r.basic$SDF$dangi_rainfal_lhr

dangi.ov.training$dangi_pc1_lhr_gwr <- mod.gwr.r.basic$SDF$dangi_pc1_lhr

dangi.ov.training$dangi_pc2_lhr_gwr <- mod.gwr.r.basic$SDF$dangi_pc2_lhr

dangi.ov.training$dangi_pc3_lhr_gwr <- mod.gwr.r.basic$SDF$dangi_pc3_lhr

dangi.ov.training$dangi_localR2 <- mod.gwr.r.basic$SDF$localR2



str(dangi.ov.training)

writePointsShape(dangi.ov.training, "dangi_ov_training_gwr.shp")


writePointsShape(dangi.ov.test, "dangi_ov_test_gwr.shp")


write.csv(dangi.ov.training, "dangi_ov_training_gwr.csv")



### Ploting GWR Coeefcients

dgb.poly <- readShapePoly("Export_Output_2.shp")
dgb.polyline <- fortify(dgb.poly, region="Distt_name")

results <- as.data.frame(dangi.ov.training)

str(results)

library(rgeos)
library(gplots)
library(ggplot2)
library(Cairo)

colours <- c("dark blue", "blue", "red", "dark red") 

#Temprature
  
 p1<- ggplot(results, aes(x=x,y=y))+
  geom_point(aes(colour=results$dangi_temp_int_gwr))+
  scale_colour_gradient2(low = "red", mid = "yellow", high = "orange", 
  midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", 
  guide_legend(title="TEMP"))

?scale_colour_gradient2
plot(p1)

#rainfall

p2<- ggplot(results, aes(x=x,y=y))+
  geom_point(aes(colour=results$dangi_rainfal_lhr_gwr))+
  scale_colour_gradient2(low = "red", mid = "yellow", high = "orange", 
                         midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", 
                         guide_legend(title="PREC"))
p2

#pc1.ndvi

p3<- ggplot(results, aes(x=x,y=y))+
  geom_point(aes(colour=results$dangi_pc1_lhr_gwr))+
  scale_colour_gradient2(low = "red", mid = "yellow", high = "orange", 
                         midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", 
                         guide_legend(title="NDVI.PC1"))

p3
#pc2.ndvi

p4<- ggplot(results, aes(x=x,y=y))+
  geom_point(aes(colour=results$dangi_pc2_lhr_gwr))+
  scale_colour_gradient2(low = "red", mid = "yellow", high = "orange", 
                         midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", 
                         guide_legend(title="NDVI.PC2"))

p4
#pc3.ndvi

p5<- ggplot(results, aes(x=x,y=y))+
  geom_point(aes(colour=results$dangi_pc3_lhr_gwr))+
  scale_colour_gradient2(low = "red", mid = "yellow", high = "orange", 
                         midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", 
                         guide_legend(title="NDVI.PC3"))


p5

p6<- ggplot(results, aes(x=x,y=y))+
  geom_point(aes(colour=results$dangi_localR2))+
  scale_colour_gradient2(low = "red", mid = "yellow", high = "orange", 
                         midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", 
                         guide_legend(title="localR2"))

p6

Cairo(file="Fig06.png", type="png", units="in", width=12, height=08, pointsize=12, dpi=600)
set.seed(1337)

pushViewport(viewport(layout = grid.layout(2, 3)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(p3, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
print(p4, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p5, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(p6, vp = viewport(layout.pos.row = 2, layout.pos.col = 3))
dev.off()

?Cairo
###GWR Prediction

mod.gwr.r.basic.pred <- gwr(formula=dangi_logit, 
                  data = dangi.ov.training, 
                  adapt= 0.146, 
                  gweight=gwr.Gauss,
                  fit.points = dangi.grid, 
                  predictions=TRUE, 
                  se.fit.CCT=TRUE, 
                  se.fit=TRUE, 
                  hatmatrix = TRUE, 
                  fittedGWRobject=mod.gwr.r.basic)

str(mod.gwr.r.basic.pred)


dangi.grid$pred.gwr_tr <- mod.gwr.r.basic.pred$SDF$pred
dangi.grid$var.gwr_tr <- mod.gwr.r.basic.pred$SDF$pred.se

dangi.grid$pred.gwr_tr <- ifelse(dangi.grid$pred.gwr_tr> 0.5,1,0)

dangi.grid$pred.gwr_tr

#plotting global logit (a) and gwr logit (b)

tiff(filename = "d:\\Fig8a.tiff", res = 600, width=12, height=8,  units = "cm", compression = "lzw")
spplot(dangi.logit.grid, c("fitted.results"), col.regions= rev(heat.colors(256)), sp.layout=list("sp.polygons", dgb.poly), scales = list(draw = T))
dev.off()

tiff(filename = "d:\\Fig8b.tiff", res = 600, width=12, height=8,  units = "cm", compression = "lzw")
spplot(dangi.grid, c("pred.gwr_tr"), col.regions= rev(heat.colors(256)), sp.layout=list("sp.polygons", dgb.poly), scales = list(draw = T))
dev.off()

?heat.colors


######################## Accuracy Test##########################

str(dangi.grid)

gridded(dangi.grid) <- TRUE
dangi.grid <- as(dangi.grid, "SpatialGridDataFrame")
proj4string(dangi.grid) <- dangi.grid@proj4string


dangi.grid$pred.lr_tr

dangi.grid <- as(dangi.grid, "SpatialPointsDataFrame")

dangi.ov.test <- readShapePoints("dangi_ov_test.shp")
str(dangi.ov.test)
proj4string(dangi.ov.test) <- dangi.grid@proj4string

dangi.ov.logit.grid <- over( dangi.ov.test, dangi.logit.grid)
  
  str(dangi.logit.grid$fitted.results)

str(dangi.ov.logit.grid)


str(dangi.ov.logit.grid)
dangi.ov.logit.grid$fitted.results

pmlr_tr <- data.frame(dangi.ov.lr.gwlr$pred.lr_tr)

pmlr_ov
pmlr_tr

o<- data.frame(dangi.ov.test$larave)
o
# Numerical Goodness of Fit

g_mlr_ov <- gof(pmlr_ov,o, na.rm=TRUE)
g_mlr_tr <- gof(pmlr_tr,o, na.rm=TRUE)

g_mlr_ov
g_mlr_tr

# Sum of the Squared Residuals

ssq(pgwr_ov,o, na.rm=TRUE)

ssq(pgwr_tr,o, na.rm=TRUE)

#######################################################################

#Generalized logistic

#Selecting bandwidth


bwG <- ggwr.sel(formula=dangi_logit, 
                data=dangi.ov.training,
                coord= ~lat__X_+long__Y_,
                family=binomial(link='logit'), 
                longlat=TRUE, verbose=T)

?bw.ggwr


mod.gwr.r <- ggwr(formula=dangi_logit, 
                  data=dangi.ov.training, 
                  bandwidth=8.1, 
                  gweight=gwr.Gauss,
                  coord= ~lat__X_+long__Y_,
                  #coord=cbind(dangi.ov.training$coords.x1, dangi.ov.training$coords.x1),
                  family=binomial(link="logit"),              
                  longlat=FALSE, 
                  type=c("response"))





#### SAVING GWR COEFFICIENTS TO DISPLAY IN GEODA

dangi.ov.training$dangi_temp_int_gwlr <- mod.gwr.r$SDF$dangi_temp_int  
dangi.ov.training$dangi_temp_int_gwlr


dangi.ov.training$dangi_rainfal_lhr_gwlr <- mod.gwr.r$SDF$dangi_rainfal_lhr

dangi.ov.training$dangi_pc1_lhr_gwlr <- mod.gwr.r$SDF$dangi_pc1_lhr

dangi.ov.training$dangi_pc2_lhr_gwlr <- mod.gwr.r$SDF$dangi_pc2_lhr

dangi.ov.training$dangi_pc3_lhr_gwlr <- mod.gwr.r$SDF$dangi_pc3_lhr

dangi.ov.training$dangi_localR2_gwlr <- mod.gwr.r$SDF$localR2

# plot residuals

windows(6,6)
plot(density( mod.gwr.r$SDF$response_resids), col = "darkblue", main = "GWR residuals",
     lwd = 1.5)

### Ploting GWR Coeefcients

dgb.poly <- readShapePoly("Export_Output_2.shp")
dgb.polyline <- fortify(dgb.poly, region="Distt_name")

results <- as.data.frame(dangi.ov.training)

str(results)

library(rgeos)
library(gplots)
library(ggplot2)
library(Cairo)

colours <- c("dark blue", "blue", "red", "dark red") 

#Temprature

p_1<- ggplot(results, aes(x=x,y=y))+
  geom_point(aes(colour=results$dangi_temp_int_gwlr))+
  scale_colour_gradient2(low = "red", mid = "blue", high = "dark blue", 
                         midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", 
                         guide_legend(title="TEMP"))

plot(p_1)

#rainfall

p2<- ggplot(results, aes(x=x,y=y))+
  geom_point(aes(colour=results$dangi_rainfal_lhr_gwr))+
  scale_colour_gradient2(low = "red", mid = "blue", high = "dark blue", 
                         midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", 
                         guide_legend(title="PREC"))
p2

#pc1.ndvi

p3<- ggplot(results, aes(x=x,y=y))+
  geom_point(aes(colour=results$dangi_pc1_lhr_gwr))+
  scale_colour_gradient2(low = "red", mid = "blue", high = "dark blue", 
                         midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", 
                         guide_legend(title="NDVI.PC1"))

p3
#pc2.ndvi

p4<- ggplot(results, aes(x=x,y=y))+
  geom_point(aes(colour=results$dangi_pc2_lhr_gwr))+
  scale_colour_gradient2(low = "red", mid = "blue", high = "dark blue", 
                         midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", 
                         guide_legend(title="NDVI.PC2"))

p4
#pc3.ndvi

p5<- ggplot(results, aes(x=x,y=y))+
  geom_point(aes(colour=results$dangi_pc3_lhr_gwr))+
  scale_colour_gradient2(low = "red", mid = "blue", high = "dark blue", 
                         midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", 
                         guide_legend(title="NDVI.PC3"))


p5

p6<- ggplot(results, aes(x=x,y=y))+
  geom_point(aes(colour=results$dangi_localR2))+
  scale_colour_gradient2(low = "red", mid = "blue", high = "dark blue", 
                         midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", 
                         guide_legend(title="localR2"))

p6

Cairo(file="fig5gwlr.png", type="png", units="in", width=12, height=8, pointsize=12, dpi=300)
set.seed(1337)

pushViewport(viewport(layout = grid.layout(2, 3)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(p3, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
print(p4, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p5, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(p6, vp = viewport(layout.pos.row = 2, layout.pos.col = 3))
dev.off()





##GWR prediction

mod.gwr <- ggwr(formula=dangi_logit, 
                  data=dangi.ov.training, 
                  bandwidth=8.1, 
                  gweight=gwr.Gauss,
                   adapt=TRUE,
                  #coord= ~lat__X_+long__Y_,
                  #coord=cbind(dangi.grid$coords.x1, dangi.grid$coords.x1),
                  family=binomial(link="logit"),              
                  longlat=FALSE,
                  fit.points = dangi.grid,
                  type=c("response"),
                  #predictions=TRUE, 
                  #se.fit.CCT=TRUE, 
                  #se.fit=TRUE, 
                  #hatmatrix = TRUE, 
                  #fittedGWRobject=mod.gwr.r
                )
                

Both Poisson and Logistic GWR require model fitting using a 
technique known as iteratively reweighted least squares (IRLS). 
The analysis is carried out in much the same manner as 
previously described, but the computation of the Akaike
Information Criterion (AIC)  and AICc differs from the OLS
expressions.

?glm




                
                
