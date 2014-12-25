# Exploring the Wisconsin Breast Cancer Data
# Building Classifiers
# -------------------------------------------
# Suchana Seth
# December 2014
# For Women Who Code 
# Workshop : Intro to Data Science With R
# -------------------------------------------

# read data
WBCD_data <- read.csv("C:/Users/Suchana/Desktop/SuchanaWorkshops_WomenWhoCode_2014/Datasets/WBCD_data.csv", header=FALSE)
View(WBCD_data)

# copy to another data frame
bc <- WBCD_data
# sanity
str(bc)
#change names of first 2 columns
names(bc) <- c("id","label",names(bc)[3:32])

# eyeball the data
summary(bc)
# let's plot some variables against each other 
# base R - no ggplot2
plot(bc$V3,bc$V4)
plot(bc$V3,bc$V4,col=bc$label, pch = 20, main = "V3 vs V4", xlab="V3", ylab="V4")
legend("topright",legend=unique(bc$label),col=unique(bc$label),pch=20)
# are there obvious correlations?
# discuss multi-collinear data 
pairs(bc) # too large error
pairs(bc[,3:32])
pairs(bc[,3:10])
pairs(bc[,10:20])
pairs(bc[,20:32])
# quantify correlation
cor(bc)
cor(bc[,3:32])

# are there outliers?
# how is the data distributed?
boxplot(bc[,3:32])
# from above plot, talk about log scales and distributions
boxplot(log(bc[,3:32])
# log is natural log, log10 for base 10
boxplot(log10(bc[,3:32]))
# illustrate distributions with hist()
# explain boxplot
# is there any difference between the two classes?
boxplot(V3 ~ label, data = bc)
boxplot(V3 ~ label, data = bc)
boxplot(V4 ~ label, data = bc)
boxplot(V5 ~ label, data = bc)
boxplot(V6 ~ label, data = bc)
boxplot(V7 ~ label, data = bc)
boxplot(V32 ~ label, data = bc)
boxplot(V24 ~ label, data = bc)
# good place to talk about 
# functions
# for loops
# png() | pdf() | grid of plots

# dimensionality reduction - if there's time
# pca vs lda



# build a classifier

# split data into train, validation & test
# talk about why we do it
dim(bc)
summary(bc$label)
# create sampling vector
0.8*dim(bc)[] # vectorized by mistake :)
groups <- sample(bc$id, size=dim(bc)[1], replace=F)
# subset command to actually split 
bc.test <- subset(bc, bc$id %in% groups[530:569])
bc.cval <- subset(bc, bc$id %in% groups[456:529])
bc.train <- subset(bc, bc$id %in% groups[1:455])
# sanity
str(bc.train)
str(bc.cval)
str(bc.test)

# logistic regression
lrmodel1 <- glm(label ~ V3+V4+V5, data=bc.train, family="binomial")
summary(lrmodel1)
# type V3 to V32 ??? no way
paste("V",3:32,sep="",collapse="+")
# fun digression
# play with paste() and see
# not the only way - talk abt "label ~ ."
lrmodel2 <- glm(label ~ V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32, data=bc.train, family="binomial")
# explain this - correlated variables & matrix inversion - determinant going to zero
#Warning messages:
#1: glm.fit: algorithm did not converge 
#2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
summary(lrmodel2)
# model diagnostics from AIC
# call predict()
lrmodel2.p.cval <- predict(lrmodel2,bc.cval,type="response")
# from probability to labels
lrmodel2.p.cval.label <- rep("B", dim(bc.cval)[1])
lrmodel2.p.cval.label[lrmodel2.p.cval > 0.5] <- "M"
predicted <- lrmodel2.p.cval.label
actual <- as.character(bc.cval$label)

# print confusion matrix
confmat <- as.matrix(table(actual,predicted))
confmat
# "B" is "negative"
# "M" is "positive"
# predicted = actual : "true"
# predicted != actual : "false"
# tp : M predicted = M actual : confmat[2,2]
# fp : M predicted = B actual : confmat[1,2]
# tn : B predicted = B actual : confmat[1,1]
# fn : B predicted = M actual : confmat[2,1]
tp <- confmat[2,2]
fp <- confmat[1,2]
tn <- confmat[1,1]
fn <- confmat[2,1]
# precision <- "out of the positives you predicted, how many were correct"
precision <- tp/(tp+fp)
precision
# recall <- "out of the actual positives, how many could you identify"
recall <- tp/(tp+fn)
recall
# false discovery rate <- "how many false positives out of predicted positives"
# fdr = 1 - precision
fdr <- fp/(tp+fp)
fdr
precision + fdr
# think about why & where different accuracy measures might make sense
# precision recall trade off
# can't increase both simultaneously
# other accuracy measures
# f1 score
# area under ROC curve

# what could affect model performance?
# each time I sample differently results will change a bit
# if i use less data for training, my accuracy will drop
# if i use fewer variables as features, my accuracy will drop ...
# or will it ?
# talk about overfitting
# let's check the silly 3 variable model we built first - how does it perform?
lrmodel1.p.cval <- predict(lrmodel1,bc.cval,type="response")
# from probability to labels
lrmodel1.p.cval.label <- rep("B", dim(bc.cval)[1])
lrmodel1.p.cval.label[lrmodel1.p.cval > 0.5] <- "M"
predicted <- lrmodel1.p.cval.label
actual <- as.character(bc.cval$label)
# print confusion matrix
confmat <- as.matrix(table(actual,predicted))
confmat
tp <- confmat[2,2]
fp <- confmat[1,2]
tn <- confmat[1,1]
fn <- confmat[2,1]
# precision <- "out of the positives you predicted, how many were correct"
precision <- tp/(tp+fp)
precision
# recall <- "out of the actual positives, how many could you identify"
recall <- tp/(tp+fn)
recall
# false discovery rate <- "how many false positives out of predicted positives"
# fdr = 1 - precision
fdr <- fp/(tp+fp)
fdr
precision + fdr

# advanced
# class imbalance
# what if I had very bad data
# only 10 examples of M, and 400 examples of B
# try creating an artificial split like that in this data & check

# back to dimensionality reduction
# what is principal component analysis?
#
bc.pca <- prcomp(bc, scale=T)
bc.pca <- prcomp(bc[,3:32], scale=T)
str(bc.pca)
bc.pca$center
bc.pca$scale
bc.pca$rotation
# plot principal components
biplot(bc.pca, scale=0)
bc.pca$sdev^2
# compute variance captured by each PC
bc.pca.var <- bc.pca$sdev^2
# compute PROPORTION OF variance captured by each PC
bc.pca.pve <- bc.pca.var/sum(bc.pca.var)
bc.pca.var
bc.pca.pve
# plot proportion of variance captured
plot(bc.pca.pve, type="b", ylim=c(0,1), xlab="Principal Component", ylab="Proportion of Variance Explained")
# plot cumulative variance captured
plot(cumsum(bc.pca.pve), type="b", ylim=c(0,1), xlab="Principal Component", ylab="Cumulative Variance Explained")
# quicker way
summary(bc.pca)
plot(bc.pca)
