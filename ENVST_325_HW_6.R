install.packages("caret")
install.packages("randomForest")
install.packages("rgdal")
install.packages("sf")
install.packages("raster")

library(caret)
library(randomForest)
library(rgdal)
library(sf)
library(raster)
oct <- stack("/cloud/project/activity08/Oct_12.tif")
plot(oct)

drStack <- stack("/cloud/project/activity08/May_19.tif",
                 "/cloud/project/activity08/June_10.tif",
                 "/cloud/project/activity08/June_18.tif",
                 "/cloud/project/activity08/Oct_12.tif")

lc <- st_read("/cloud/project/activity08/land_pts.shp")
head(lc)

trainPt <- subset(lc, lc$train=="train", drop=FALSE)


train <- extract(drStack, trainPt)
trainTable <- st_drop_geometry(trainPt)
trainDF <- na.omit(cbind(y=as.factor(trainTable[,3]), train))

tc <- trainControl(method = "repeatedcv",
                   number = 10,
                   repeats = 10) 

nbands <- 20

rf.grid <- expand.grid(mtry=1:round(sqrt(nbands)))

set.seed(43)

rf_model <- caret::train(x = trainDF[,2:21], 
                         y = as.factor(trainDF[,1]), 
                         method = "rf",
                         metric="Accuracy", 
                         trainControl = tc,
                         tuneGrid = rf.grid)

rf_prediction <- raster::predict(drStack, rf_model)

validPts <- subset(lc, lc$train=="valid", drop=FALSE)
valid_Table <- st_drop_geometry(validPts)

valid_rf <- extract(rf_prediction, validPts)
validDF_rf <- data.frame(y=valid_Table[,3], rf=valid_rf)

rf_errorM = confusionMatrix(as.factor(validDF_rf$rf),as.factor(validDF_rf$y))
rf_errorM
colnames(rf_errorM$table) <- c("field","tree","path")
rownames(rf_errorM$table) <- c("field","tree","path")
rf_errorM$table

#--Q1-- Neural Network
# starting parameters for neural net
nnet.grid <- expand.grid(size = seq(from = 1, to = 10, by = 1), # number of neurons units in the hidden layer 
                         decay = seq(from = 0.001, to = 0.01, by = 0.001)) # regularization parameter to avoid over-fitting 

# train nnet
set.seed(18)
nnet_model <- caret::train(x = trainDF[,c(2:21)], y = as.factor(trainDF[,1]),
                           method = "nnet", metric="Accuracy", 
                           trainControl = tc, tuneGrid = nnet.grid,
                           trace=FALSE)
nnet_model

# predictions
nn_prediction <- raster::predict(drStack, nnet_model)

#Confusion Matrix
valid_nn <- extract(nn_prediction, validPts)
validDF_nn <- data.frame(y=valid_Table[,3], nn=valid_nn)

nn_errorM = confusionMatrix(as.factor(validDF_nn$nn),as.factor(validDF_nn$y))
# make LCID easier to interpret
colnames(nn_errorM$table) <- c("field","tree","path")
rownames(nn_errorM$table) <- c("field","tree","path")
nn_errorM


#Side by Side Predictions
par(mfrow=c(1,2))
plot(nn_prediction, col= hcl.colors(3, palette = "Harmonic"),
     legend=FALSE, axes=FALSE, main="Neural network", box=FALSE)
legend("bottomleft", c("field","tree","path"),
       fill=hcl.colors(3, palette = "Harmonic") ,bty="n")

plot(rf_prediction, col= hcl.colors(3, palette = "Harmonic"),
     legend=FALSE, axes=FALSE, main="Random forest", box=FALSE)
legend("bottomleft", c("field","tree","path"),
       fill=hcl.colors(3, palette = "Harmonic") ,bty="n")

#--Q2--
oct <- stack("/cloud/project/activity08/Oct_12.tif")
may <- stack("/cloud/project/activity08/May_19.tif")
jun_10 <- stack("/cloud/project/activity08/June_10.tif")
jun_18 <- stack("/cloud/project/activity08/June_18.tif")

#calculate NDVI pre from NIR (4th band) and Red (3rd band)
ndvi_oct <- (oct[[4]]-oct[[3]])/(oct[[4]]+oct[[3]])
ndvi_may <- (may[[4]]-may[[3]])/(may[[4]]+may[[3]])
ndvi_jun_10 <- (jun_10[[4]]-jun_10[[3]])/(jun_10[[4]]+jun_10[[3]])
ndvi_jun_18 <- (jun_18[[4]]-jun_18[[3]])/(jun_18[[4]]+jun_18[[3]])

par(mfrow=c(1,1))
plot(ndvi_oct)
plot(ndvi_may)
plot(ndvi_jun_10)
plot(ndvi_jun_18)

#--Q3--
nnet2.grid <- expand.grid(size = seq(from = 5, to = 15, by = 1), # number of neurons units in the hidden layer 
                         decay = seq(from = 0.001, to = 0.01, by = 0.001)) # regularization parameter to avoid over-fitting 

# train nnet
set.seed(18)
nnet2_model <- caret::train(x = trainDF[,c(2:21)], y = as.factor(trainDF[,1]),
                           method = "nnet", metric="Accuracy", 
                           trainControl = tc, tuneGrid = nnet2.grid,
                           trace=FALSE)
nnet2_model
nn2_prediction <- raster::predict(drStack, nnet2_model)
#Confusion Matrix
valid_nn2 <- extract(nn2_prediction, validPts)
validDF_nn2 <- data.frame(y=valid_Table[,3], nn=valid_nn2)

nn2_errorM = confusionMatrix(as.factor(validDF_nn2$nn),as.factor(validDF_nn2$y))
# make LCID easier to interpret
colnames(nn2_errorM$table) <- c("field","tree","path")
rownames(nn2_errorM$table) <- c("field","tree","path")
nn2_errorM