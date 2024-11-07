library(MASS) ## a library of example datasets
library(class) ## a library with lots of classification tools
library(glmnet) ## One of the Lasso libraries
library(tree) ## library for tree methods
library(randomForest) ## library for random forests


#### ******* Forensic Glass ****** ####

data(fgl) ## loads the data into R; see help(fgl)
attach(fgl)

par(mfrow=c(2,3))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6),cex.lab=1.4)
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6),cex.lab=1.4)
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6),cex.lab=1.4)
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6),cex.lab=1.4)
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6),cex.lab=1.4)
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6),cex.lab=1.4)



## for illustration, consider the RIxAl plane
## use 200 training points to find nearest neighbors for 14
train <- sample(1:214,200)
x <- scale(fgl[,c(4,1)]) 
gtype <- fgl$type
ng <- length(gtype)


nearest1 <- knn(train=x[train,], test=x[-train,], cl=gtype[train], k=1)
nearest5 <- knn(train=x[train,], test=x[-train,], cl=gtype[train], k=5)
data.frame(gtype[-train],nearest1,nearest5)

## plot them to see how it worked
par(mfrow=c(1,1))
plot(x[train,], col=gtype[train], cex=.8, main="5-nearest neighbor",pch=1)
lixo = (1:ng)[-train]
points(x[172,1],x[172,2], pch=18, col=nearest5[(1:14)[lixo==172]], cex=2)
for(i in 1:14){
points(x[lixo[i],1],x[lixo[i],2], pch=18, col="orange", cex=2)
points(x[lixo[i],1],x[lixo[i],2], pch=18, col=nearest1[i], cex=2)
}


#######################
## Trees...
#######################
out_size = 30
n = 214
train <- sample(1:n,n-out_size)

treeGlass = tree(type~.,fgl,subset=train)
tree.pred=predict(treeGlass,fgl,type="class")
table(tree.pred[-train],type[-train])
1-sum(diag(table(tree.pred[-train],type[-train])))/out_size # compute classification error rate



par(mfrow=c(1,1))
plot(treeGlass)
text(treeGlass,pretty=0)


cv.glass = cv.tree(treeGlass,K=out_size)

par(mfrow=c(1,2))
plot(cv.glass$size,cv.glass$dev,type="b")
plot(cv.glass$k,cv.glass$dev,type="b")

cv.glass$k[which.min(cv.glass$dev)]

prune.glass = prune.tree(treeGlass,k=14)


par(mfrow=c(1,2))
plot(treeGlass)
text(treeGlass,pretty=0)
plot(prune.glass)
text(prune.glass,pretty=0)


tree.predNEW = predict(prune.glass,fgl,type="class")

1-sum(diag(table(tree.pred[-train],type[-train])))/out_size
1-sum(diag(table(tree.predNEW[-train],type[-train])))/out_size


# Run knn again in this new set of points...

x <- scale(fgl[,-10])
nearest1 <- knn(train=x[train,], test=x[-train,], cl=gtype[train], k=1)
nearest5 <- knn(train=x[train,], test=x[-train,], cl=gtype[train], k=5)

1-sum(diag(table(nearest1,type[-train])))/out_size
1-sum(diag(table(nearest5,type[-train])))/out_size



#################################################
###### *** California Housing Data *** ######
## median home values in various census tracts
## lat/long are centroids of the tract
## response value is log(medianhomeval)
ca <- read.csv("CAhousing.csv")
ca$AveBedrms <- ca$totalBedrooms/ca$households
ca$AveRooms <- ca$totalRooms/ca$households
ca$AveOccupancy <- ca$population/ca$households
logMedVal <- log(ca$medianHouseValue)
ca <- ca[,-c(4,5,9)] # lose lmedval and the room totals

## create a full matrix of interactions (only necessary for linear model)
## do the normalization only for main variables.
XXca <- model.matrix(~.*longitude*latitude, data=data.frame(scale(ca)))[,-1]

## what would a lasso linear model fit look like?
## it likes a pretty complicated model
par(mfrow=c(1,2))
plot(capen <- cv.glmnet(x=XXca, y=logMedVal))
plot(capen$glm,xv="lambda")
round(coef(capen),2)

## First, lets do it with CART
## no need for interactions; the tree finds them automatically
catree <- tree(logMedVal ~ ., data=ca) 
par(mfrow=c(1,1))
plot(catree, col=8, lwd=2)
text(catree)

## Next, with random forest (takes some time to run)
## limit the number of trees and the minimum tree size for speed
## add importance=TRUE so that we store the variable importance information
carf <- randomForest(logMedVal ~ ., data=ca, ntree=250, nodesize=25, importance=TRUE)
## variable importance plot. Add type=1 to plot % contribution to MSE
varImpPlot(carf,  type=1, pch=21, bg="navy", main='RF variable importance')

## Fitted values
yhatlasso <- predict(capen, XXca)
yhattree <- predict(catree, ca)
yhatrf <- predict(carf, ca) ## takes time!  drawback of RFs


## Plotting the fitted values on a map...
library(maps)
par(mfrow=c(1,2))
## preds
map('state', 'california') 
points(ca[,1:2], col=predmap(yhatlasso), pch=20, cex=.5)
mtext("lasso fitted")
map('state', 'california') 
points(ca[,1:2], col=predmap(yhatrf), pch=20, cex=.5)
mtext("rf fitted")
legend("topright", title="prediction", bty="n",
       fill=predcol[c(1,4,7,9)], legend=c("20k","100k","400k","1mil"))



## Out of sample prediction (takes a while since RF is slow)
MSE <- list(LASSO=NULL, CART=NULL, RF=NULL)
for(i in 1:10){
  train <- sample(1:nrow(ca), 5000)
  
  lin <- cv.glmnet(x=XXca[train,], y=logMedVal[train])
  yhat.lin <- predict(lin, XXca[-train,])
  
  MSE$LASSO <- c( MSE$LASSO, sqrt(mean((logMedVal[-train] - yhat.lin)^2)))

  rt <- tree(logMedVal[train] ~ ., data=ca[train,])
  yhat.rt <- predict(rt, newdata=ca[-train,])
  MSE$CART <- c( MSE$CART, sqrt(mean((logMedVal[-train] - yhat.rt)^2)))

  rf <- randomForest(logMedVal[train] ~ ., data=ca[train,], ntree=250, nodesize=25)
  yhat.rf <- predict(rf, newdata=ca[-train,])
  MSE$RF <- c( MSE$RF, sqrt(mean((logMedVal[-train] - yhat.rf)^2)))
 
  cat(i)
} 

par(mfrow=c(1,1))
boxplot(MSE, col="blue", xlab="model", ylab="MSE")
















