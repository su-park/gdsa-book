## Geographical Data Science and Spatial Data Analysis: An Introduction in R
## Lex Comber and Chris Brunsdon
## The code correct as of July 2020. Please let us know if it needs updating
## email: a.comber@leeds.ac.uk

## Chapter 7: Applications of Machine Learning to Spatial Data


library(sf)
library(tmap)
library(caret)
library(gbm)
library(rpart)
library(tidyverse)
library(gstat)
library(GGally)
library(visNetwork)
library(rgl)
library(cluster)
library(RColorBrewer)


library(sf)
library(tidyverse)
oa <- st_read("data.prep/liverpool_oa.shp", quiet = T) %>% `st_crs<-`(27700)
lsoa <- st_read("data.prep/liverpool_lsoa.shp", quiet = T) %>% `st_crs<-`(27700)

names(oa)
names(oa)[c(8,10:16,18:21)]
oa <- oa[,c(8,10:16,18:21)]
names(oa)

names(lsoa)
names(lsoa)[c(5,7:13, 15:17)]
lsoa <- lsoa[,c(5,7:13, 15:17)]
names(lsoa)
oa$gs_area = oa$gs_area*100
lsoa$gs_area = lsoa$gs_area*100


# check your current working directory
getwd()
download.file("http://archive.researchdata.leeds.ac.uk/741/1/ch7.Rdata",
"./ch7.RData", mode = "wb")


load("ch7.RData")
ls()


properties %>% st_drop_geometry() %>% select_if(is_logical) %>%
  colnames() %>% paste(" - ",.) %>% paste(collapse='\n') %>% cat()


# OA pairs plot
oa %>% st_drop_geometry() %>% select_if(is.numeric)  %>%
  ggpairs(lower = list(continuous =
                         wrap("points", alpha = 0.5, size=0.1))) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())


ggplot(properties,aes(x=Beds,y=Price, group=Beds)) + geom_boxplot()


tmap_mode('view')
tm_shape(properties) +
  tm_dots(col='Beds', size = 0.03, palette = "PuBu")
tmap_mode('plot')


set.seed(78910) # for reproducibility
tree_model <- rpart(Price~.,data=properties %>% 
                      st_drop_geometry() %>%
                      mutate_if(is_logical,as.character))


tree_model


library(visNetwork)
visTree(tree_model,legend=FALSE,collapse=TRUE,direction='LR')


library(png)
library(grid)
img <- readPNG("rtree.png")
grid.raster(img)


library(rgl)
df <- properties %>% st_drop_geometry() %>%
    select(Price, Beds, Lat) %>%
  mutate(Beds = as.numeric(as.character(Beds)))
plot3d(lm(Price~Beds+Lat, data = df), col="orange", type = 's', size=1,
       vars = df[,c("Price", "Beds", "Lat")])
# run the code below to see the plot in your RStudio session
# rglwidget(width = 600, height = 1100, reuse = FALSE)


library(dplyr)
df <- properties %>% st_drop_geometry() %>%
    select(Price, Beds, Lat) %>% 
  mutate(Beds = as.numeric(as.character(Beds)))

X <- df %>% scale()


head(df)
head(X)


apply(X, 2, mean)
apply(X, 2, sd)


library(caret)
props_pp = 
  properties %>% st_drop_geometry() %>% 
  mutate_if(is_logical, as.character) %>% 
  preProcess(method = 
               c("center", "scale", "YeoJohnson", "nzv", "pca"))


props_pp
props_pp$method


set.seed(1234) 
train.index = as.vector(createDataPartition(
  properties$Price, p = 0.7, list = F))


summary(properties$Price[train.index])
summary(properties$Price[-train.index])


train_data = 
  properties[train.index,] %>% st_drop_geometry() %>% 
  mutate_if(is_logical,as.character)
test_data = 
  properties[-train.index,] %>% st_drop_geometry() %>% 
  mutate_if(is_logical,as.character)


ctrl1 <- trainControl(method = "cv",
                      number = 10) # a 10-fold CV


# a 10-fold repeated CV, repeated 10 times
ctrl2 <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 10) 


# there are many settings
names(ctrl1)
# compare the ones that have been specified
c(ctrl1[1], ctrl1[2], ctrl1[3])
c(ctrl2[1], ctrl2[2], ctrl2[3])


modelLookup("knn")


set.seed(123) # for reproducibility
knnFit <- train(Price ~ ., data = train_data, method = "knn")


knnFit


names(knnFit)
help(train, package = "caret")


ggplot(knnFit)
ggplot(knnFit, metric = "MAE")


set.seed(123)
knnFit <- train(Price ~ ., data = train_data, method = "knn",
                tuneLength = 20)

knnFit$results %>% select(k, RMSE, MAE, Rsquared) %>% 
  pivot_longer(-k) %>%
  ggplot(aes(x = k, y = value)) +
  geom_point() +
  geom_line()+ ylab("Accuracy") +
  facet_wrap("name", scales = "free")


set.seed(123)
knnFit <- train(Price ~ ., data = train_data, method = "knn",
                tuneLength = 20, metric = "MAE")


ggplot(knnFit, metric = "MAE")


ctrl <- trainControl(method = "repeatedcv",
                      number = 10, repeats = 10)
knnFit <- train(Price ~ ., data = train_data, method = "knn",
                tuneLength = 20, metric = "MAE", trControl = ctrl)


knnFit


knnFit <- train(Price ~ ., data = train_data, method = "knn",
                tuneLength = 20, metric = "MAE", trControl = ctrl,
                preProcess = c("center","scale"))
knnFit



modelLookup("gbm")


set.seed(123)
ctrl <- trainControl(method="repeatedcv", repeats=10)
gbmFit <- train(Price ~ ., data = train_data, method = "gbm",
                trControl = ctrl, verbose = FALSE)
gbmFit


gbmFit
gbmFit$bestTune


gbmFit$results[which.min(gbmFit$results$RMSE),]
gbmFit$results[which.max(gbmFit$results$Rsquared),]
gbmFit$results[which.min(gbmFit$results$MAE),]


params <- expand.grid(n.trees = seq(50, 300, by = 50),
                      interaction.depth  = seq(1, 5, by = 1),
                      shrinkage = seq(0.1, 0.3, by = 0.1),
                      n.minobsinnode = seq(5,15,5))
dim(params)
head(params)
gbmFit <- train(Price ~ ., data = train_data, method = "gbm",
                trControl = ctrl,
                tuneGrid = params, verbose = FALSE)


gbmFit$bestTune


gbmFit


gbmFit$results[which.min(gbmFit$results$RMSE),]
gbmFit$results[which.max(gbmFit$results$Rsquared),]
gbmFit$results[which.min(gbmFit$results$MAE),]


params <- expand.grid(n.trees = seq(50, 400, by = 50),
                      interaction.depth  = seq(1, 5, by = 1),
                      shrinkage = seq(0.1, 0.5, by = 0.1),
                      n.minobsinnode = seq(10,50,10))
dim(params)
#head(params)
gbmFit <- train(Price ~ ., data = train_data, method = "gbm",
                trControl = ctrl,
                tuneGrid = params, verbose = FALSE)


# save("knnFit", file = "prepared.in/knnFit3.RData")
load("prepared.in/knnFit3.RData")


pred = predict(knnFit,newdata = test_data)
data.frame(Predicted = pred, Observed = test_data$Price) %>% 
  ggplot(aes(x = Observed, y = Predicted))+
  geom_point(size = 1, alpha = 0.5)+
  geom_smooth(method = "loess", col = "red")+
  geom_smooth(method = "lm")


postResample(pred = pred, obs = test_data$Price)


properties %>% st_transform(27700) %>% 
  # add Easting and Northing as variables
  mutate(Easting =  (properties %>% st_coordinates() %>% .[,1]) ) %>%
  mutate(Northing = (properties %>% st_coordinates() %>% .[,2]) ) %>%
  # intersect with OA data and drop unwanted variables
  st_intersection(oa) %>% 
  # drop Lat, Lon, OAC code, LSOA ID and sf geometry
  select(-Lon, -Lat, -OAC, -code) %>% 
  st_drop_geometry() %>% 
  # remove NAs and pipe to a data.frame
  drop_na() -> data_anal


set.seed(123)
tree_model_oa <- rpart(Price~.,data=data_anal %>%
                        mutate_if(is_logical,as.character))


visTree(tree_model_oa,legend=F,collapse=TRUE,direction='LR')


set.seed(1234)
train.index = 
  as.vector(createDataPartition(data_anal$Price, p = 0.7, list = F))
train_anal = data_anal[train.index,] 
test_anal = data_anal[-train.index,] 


train_z = 
  train_anal %>% select(-Price) %>% 
  mutate_if(is_logical,as.character) %>% 
  mutate_if(is_double,scale) %>%  data.frame()
test_z = 
  test_anal %>% select(-Price) %>% 
  mutate_if(is_logical,as.character) %>% 
  mutate_if(is_double,scale) %>%  data.frame()
# add unscaled price back
train_z$Price = train_anal$Price
test_z$Price = test_anal$Price


# the control parameters
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)


lmFit  = train(Price~.,data = train_z, method = "lm",
               trControl = ctrl, verbose = FALSE)
knnFit =  train(Price~.,data = train_z, method = "knn",
               trControl = ctrl, verbose = FALSE)
tbFit  =  train(Price~.,data = train_z, method = "treebag",
               trControl = ctrl, verbose = FALSE)
rfFit  =  train(Price~.,data = train_z, method = "rf",
               trControl = ctrl, verbose = FALSE)
gbmFit =  train(Price~.,data = train_z, method = "gbm",
               trControl = ctrl, verbose = FALSE)
svmFit =  train(Price~.,data = train_z,method = "svmLinear",
               trControl = ctrl, verbose = FALSE)


# the models
save(list = c("lmFit", "knnFit", "tbFit", "rfFit", "gbmFit", "svmFit"),
     file = "all_pred_fits.RData")



# generate the predictions for each model
pred.lm  = postResample(pred = predict(lmFit, newdata = test_z),
                        obs = test_z$Price)
pred.knn = postResample(pred = predict(knnFit, newdata = test_z),
                        obs = test_z$Price)
pred.tb  = postResample(pred = predict(tbFit, newdata = test_z),
                        obs = test_z$Price)
pred.rf  = postResample(pred = predict(rfFit, newdata = test_z),
                        obs = test_z$Price)
pred.gbm = postResample(pred = predict(gbmFit, newdata = test_z),
                        obs = test_z$Price)
pred.svm = postResample(pred = predict(svmFit, newdata = test_z),
                        obs = test_z$Price)
# Extract the model validations
df_tab = rbind(
      lmFit$results[which.min(lmFit$results$Rsquared),2:4],
      knnFit$results[which.min(knnFit$results$Rsquared),2:4],
      tbFit$results[which.min(tbFit$results$Rsquared),2:4],
      rfFit$results[which.min(rfFit$results$Rsquared),2:4],
      gbmFit$results[which.min(gbmFit$results$Rsquared),5:7],
      svmFit$results[which.min(svmFit$results$Rsquared),2:4])
colnames(df_tab) = paste0("Model ", colnames(df_tab))
# Extract the prediction validations
df_tab2 = t(cbind(pred.lm, pred.knn, pred.tb,
                  pred.rf, pred.gbm, pred.svm))
colnames(df_tab2) = paste0("Prediction ", colnames(df_tab2))
# Combine
df_tab = data.frame(df_tab, df_tab2)
rownames(df_tab) = c("SLR", "kNN","BRT", "RF", "GBM", "SVM")
# Print out
df_tab


X = data_anal %>% select(-Price, -u25) %>% 
  mutate_if(is_logical,as.character) %>% 
  mutate(Beds = as.numeric(Beds)) %>%
  mutate_if(is_double,scale) %>%  data.frame()
Y = data_anal["Price"]


library(car)
m = lm(Price~.,data=cbind(X,Y))
vif(m)[ vif(m)> 2]


ctrl= trainControl(method="repeatedcv",number=10,repeats=5)
set.seed(123)
lmFit  = train(Price~.,data = cbind(X,Y),method = "lm",
               trControl = ctrl)
knnFit = train(Price~.,data = cbind(X,Y),method = "knn",
               trControl = ctrl,trace = F)
tbFit  = train(Price~.,data = cbind(X,Y),method = "treebag",
               trControl = ctrl)
rfFit  = train(Price~.,data = cbind(X,Y),method = "rf",
               trControl = ctrl,verbose = F,importance = T)
gbmFit = train(Price~.,data = cbind(X,Y),method = "gbm",
               trControl = ctrl,verbose = F)
svmFit = train(Price~.,data = cbind(X,Y),method = "svmLinear",
               trControl = ctrl,verbose = F)


# define a print function 
# this was modified from here: 
# https://github.com/topepo/caret/blob/master/pkg/caret/R/print.varImp.train.R
print.varImp.10 <- function(x = vimp, top = 10) {
   printObj <- data.frame(as.matrix(sortImp(x, top)))
   printObj$name = rownames(printObj)
   printObj
}   
# use this to extract the top 10 variables to a data.frame - df
df = data.frame(print.varImp.10(varImp(lmFit)), 
                method = "SLR") 
df = rbind(df, data.frame(print.varImp.10(varImp(knnFit)), 
                          method = "kNN")) 
df = rbind(df, data.frame(print.varImp.10(varImp(tbFit)), 
                          method = "BRT")) 
df = rbind(df, data.frame(print.varImp.10(varImp(rfFit)), 
                          method = "RF")) 
df = rbind(df, data.frame(print.varImp.10(varImp(gbmFit)), 
                          method = "GBM")) 
df = rbind(df, data.frame(print.varImp.10(varImp(svmFit)), 
                          method = "SVM")) 
df %>% 
  ggplot(aes(reorder(name, Overall), Overall)) +
  geom_col(fill = "tomato") +
  facet_wrap( ~ method, ncol = 3, scales = "fixed") +
  coord_flip() + xlab("") + ylab("Variable Importance") +
  theme(axis.text.y = element_text(size = 6)) + theme_bw()


rm(list = ls())
load("ch7.RData")


# convert to factor
oa$OAC = as.factor(oa$OAC)
set.seed(1234)
# partition the data
train.index <-
  as.vector(createDataPartition(oa$OAC, p=0.75, list = FALSE))
# Create Training Data
train_data = oa[train.index,-1] %>% st_drop_geometry() %>%
  mutate_if(is_double,scale)
test_data = oa[-train.index,-1] %>% st_drop_geometry() %>%
  mutate_if(is_double,scale)
cbind(Train = summary(train_data$OAC), Test = summary(test_data$OAC))



# set control paramters
ctrl= trainControl(method="repeatedcv",number=10,repeats=10)
# create models
set.seed(123)
ldaMod = train(OAC ~ ., data=train_data, method="lda",
               trControl=ctrl, metric="Accuracy")
knnMod = train(OAC ~ ., data=train_data, method="knn",
               trControl=ctrl, metric="Accuracy")
tbMod  = train(OAC ~ ., data=train_data, method="treebag",
               trControl=ctrl, metric="Accuracy")
rfMod  = train(OAC ~ ., data=train_data, method="rf",
               trControl=ctrl, metric="Accuracy")
gbmMod = train(OAC ~ ., data=train_data, method="gbm",
               trControl=ctrl, metric="Accuracy", verbose = F)
svmMod = train(OAC ~ ., data=train_data, method="svmLinear",
               trControl=ctrl, metric="Accuracy")
# evaluate the models
round(rbind(
  LDA = ldaMod$results[which.min(ldaMod$results$Accuracy),
                       c("Accuracy", "Kappa")],
  kNN = knnMod$results[which.min(knnMod$results$Accuracy),
                       c("Accuracy", "Kappa")],
  BRT  = tbMod$results[which.min(tbMod$results$Accuracy),
                       c("Accuracy", "Kappa")],
  RF  = rfMod$results[which.min(rfMod$results$Accuracy),
                      c("Accuracy", "Kappa")],
  GBM = gbmMod$results[which.min(gbmMod$results$Accuracy),
                       c("Accuracy", "Kappa")],
  SVM = svmMod$results[which.min(svmMod$results$Accuracy),
                       c("Accuracy", "Kappa")]), 3)


# generate predictions
pred.lda = postResample(pred = predict(ldaMod,newdata = test_data),
                      obs = test_data$OAC)
pred.knn = postResample(pred = predict(knnMod,newdata = test_data),
                      obs = test_data$OAC)
pred.tb  = postResample(pred = predict(tbMod,newdata = test_data),
                     obs = test_data$OAC)
pred.rf  = postResample(pred = predict(rfMod,newdata = test_data),
                     obs = test_data$OAC)
pred.gbm = postResample(pred = predict(gbmMod,newdata = test_data),
                      obs = test_data$OAC)
pred.svm = postResample(pred = predict(svmMod,newdata = test_data),
                      obs = test_data$OAC)
# print out
round(rbind(LDA = pred.lda, kNN = pred.knn, BRT = pred.tb,
            RF = pred.rf, GBM = pred.gbm, SVM = pred.svm),3)


data.frame(varImp(svmMod)$importance, 
           var=rownames(varImp(svmMod)$importance)) %>%
  pivot_longer(-var) %>%
  ggplot(aes(reorder(var, value), value, fill = var)) +
  geom_col() + coord_flip() +
  scale_fill_brewer(palette="Dark2") +
  labs(x = "", y = "Variable Importance for SVM clusters", 
       fill = "Attribute")+
  facet_wrap(~name, ncol = 3, scales = "fixed") +
  theme(axis.text.y = element_text(size = 7), 
        legend.position = "none")


set.seed(1234) # reproducibility!
cls <- kmeans(st_drop_geometry(oa[,-c(1, 9)]), 
              centers = 10, iter.max = 100, nstart = 20)


names(cls)


oa$cls = cls$cluster
# map
tm_shape(oa) + 
  tm_fill("cls", title = "Class No.", style = "cat", 
          palette = "Set1") 
# remove the `cls` variable
oa = oa [, -11]


set.seed(1234)  # Reproducible outcome
smallest.clus <- wss <- rep(0, 100) # define 2 results vectors
for (i in 1:100) {
	clus <- kmeans(st_drop_geometry(oa[,-c(1, 9)]),
	               centers = i, iter.max = 100, nstart = 20)
  wss[i] <- clus$tot.withinss
  smallest.clus[i] <- min(clus$size)
  if (i%% 10 == 0) cat("progress:", i, "% \n")
}



par(mfrow = c(1,2)) # set plot paramters
# the WCSS scree plot
plot(1:100, wss[1:100], type = "h", xlab = "Number of Clusters",
    ylab = "Within Cluster Sum of Squares")
# the smallest cluster size
plot(1:100, smallest.clus[1:100], type = "h",
     xlab = "Number of Clusters",
     ylab = "Smallest Cluster Size")
par(mfrow = c(1,1)) # reset the plot parameters


set.seed(1234) # reproducibility!
cls <- kmeans(st_drop_geometry(oa[,-c(1, 9)]),
              centers = 8, iter.max = 100, nstart = 20)
oa$cls = cls$cluster
# map
tm_shape(oa) +
  tm_fill("cls", title = "Class No.", style = "cat",
          palette = "Set1")
# remove the `cls` variable
oa = oa [, -11]


library(cluster)
set.seed(1234)  # Reproducible outcome
smallest.clus <- sil <- rep(0, 99) # define 2 results vectors
for (i in 2:100) {
	clus <- kmeans(st_drop_geometry(oa[,-c(1, 9)]),
	               centers = i, iter.max = 100, nstart = 20)
  ss = silhouette(clus$cluster,
                  dist(st_drop_geometry(oa[,-c(1, 9)])))
	sil[i] = mean(ss[,3])
  smallest.clus[i] <- min(clus$size)
  if (i%% 10 == 0) cat("progress:", i, "% \n")
}
# the WCSS scree plot
plot(2:100, sil[1:99], type = "h",
     xlab = "Number of Clusters",
     ylab = "Average Silhouette")


gs <- clusGap(st_drop_geometry(oa[,-c(1, 9)]),
              kmeans, nstart = 25, K.max = 15, B = 50)
df = data.frame(gs$Tab)
plot(1:15, df$gap, type = "b",
     xlab = "Number of Clusters",
     ylab = "Gap Statistic")
abline(v = 5, col = "blue", lty = 2)


PCA <- princomp(st_drop_geometry(oa[,-c(1, 9)]), cor = T, scores = T)
cumsum(PCA$sdev^2/sum(PCA$sdev^2))


cls <- kmeans(PCA$scores[, 1:4], centers = 8, iter.max = 100, nstart = 20)


# 1. Agglomerative
# dissimilarity matrix
d <- dist(st_drop_geometry(oa[,-c(1, 9)]), method = "euclidean")
# hierarchical clustering
hc <- hclust(d, method = "complete" )
# plot
plot(hc, cex = 0.6, hang = -1, labels = F,
     xlab = "", main = "Dendrogram of Agglomerative")
# identify 8 clusters
rect.hclust(hc , k = 8, border = 2:12)
# 2. Divisive
# hierarchical clustering
hc <- diana(st_drop_geometry(oa[,-c(1, 9)]))
# the amount of clustering structure
hc$dc
# plot dendrogram
pltree(hc, cex = 0.6, hang = -1, labels = F,
       xlab = "", main = "Dendrogram of Divisive")
rect.hclust(hc , k = 8, border = 2:12)


# Part 1: numeric summaries over LSOAs
properties %>% st_transform(27700) %>% 
  # intersect with LSOA data and remove geometry
  st_intersection(lsoa) %>% st_drop_geometry() %>%
  # group by LSOAs  
  group_by(code) %>%
  # summarise beds and price over LSOAs (areas)
  # mutate beds to numeric
  mutate(Beds = as.numeric(Beds)) %>%
  summarise(BedsM = median(Beds), 
            BedsSpread = IQR(Beds),
            PriceM = median(Price),
            PriceSpread = IQR(Price)) -> df1
# Part 2: logical summaries over LSOAs
properties %>% st_transform(27700) %>% 
  # intersect with LSOA data 
  st_intersection(lsoa)  %>%
  group_by(code) %>%
  # summarise logical property values and remove geometry
  summarise_if(is_logical,mean) %>% st_drop_geometry() -> df2
# Part 3: combine with LSOA
lsoa %>% left_join(df1) %>% left_join(df2) %>% 
  st_drop_geometry() %>% dplyr::select(-code) -> class_data


index.na = which(apply(class_data, 1, function(x) any(is.na(x))))
class_data = class_data[-index.na,]


PCA <- princomp(class_data, cor = T, scores = T)
PCA$sdev^2/sum(PCA$sdev^2)


cumsum(PCA$sdev^2/sum(PCA$sdev^2))


set.seed(1234)  # Reproducible outcome
smallest.clus <- wss <- rep(0, 100) # define 2 variables
for (i in 1:100) {
	clus <- kmeans(PCA$scores[, 1:24],
	               centers = i, iter.max = 100, nstart = 20)
  wss[i] <- clus$tot.withinss
  smallest.clus[i] <- min(clus$size)
  if (i%% 10 == 0) cat("progress:", i, "% \n")
}


plot(1:100, wss[1:100], type = "h", main = "Cluster Scree Plot",
     xlab = "Number of Clusters",
     ylab = "Within Cluster Sum of Squares")
plot(1:100, smallest.clus[1:100], type = "h", main = "Smallest Cluster Plot",
     xlab = "Number of Clusters",
     ylab = "Smallest Cluster Size")


set.seed(1234)
clus <- kmeans(PCA$scores[, 1:24], 
               centers = 10, iter.max = 100, nstart = 20)
LSOAclusters = clus$cluster


# We need this for the 'ddply' function
library(plyr)
# Compute a data frame (one row per cluster) containing the means 
# of each variable in that cluster
mean_by_cluster <- 
  ddply(class_data, .(LSOAclusters), numcolwise(mean))[, -1]
# Compute the columnwise means for *all* observations
mean_by_col <- apply(class_data, 2, mean)
# Compute the columnwise *sample* sd's for *all* observations
sd_by_col <- apply(class_data, 2, sd)
# Create the z-scores via the 'scale' function
z_scores <- scale(mean_by_cluster, 
                  center = mean_by_col, scale = sd_by_col)


library(RColorBrewer)
heatmap(t(z_scores),
        scale = 'none',
        col=brewer.pal(6,'BrBG'),
        breaks=c(-1e10,-2,-1,0,1,2,+1e10),
        xlab='Cluster Number', cexRow = 0.6, 
        add.expr=abline(h=(0:49)+0.5,v=(0:10)+0.5,col='white'))

