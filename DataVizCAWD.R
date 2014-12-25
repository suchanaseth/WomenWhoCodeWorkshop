# Exploring Crimes Against Women in India 2013 Data
# Data Visualization
# -------------------------------------------
# Suchana Seth
# December 2014
# For Women Who Code 
# Workshop : Intro to Data Science With R
# -------------------------------------------

# packages
install.packages("ggplot2")
# load packages
library("ggplot2")


# read the data
CAWI_data <- read.csv("C:/Users/Suchana/Desktop/SuchanaWorkshops_WomenWhoCode_2014/Datasets/CAWI_data.csv")
View(CAWI_data)
cw <- CAWI_data
str(cw)
summary(cw)
summary(cw$State.UTs)
summary(cw$Crime.Head)
head(cw)
tail(cw,2)

# starting with ggplot2
# scatterplots
ggplot(data=cw, aes(x=Cases.reported.during.the.year, y=Cases.sent.for.trial.during.the.year, color=Crime.Head)) + geom_point()
# try log of x
ggplot(data=cw, aes(x=log(Cases.reported.during.the.year), y=Cases.sent.for.trial.during.the.year, color=Crime.Head)) + geom_point()
# try log of y too
ggplot(data=cw, aes(x=log(Cases.reported.during.the.year), y=log(Cases.sent.for.trial.during.the.year), color=Crime.Head)) + geom_point()
# separate the data for each state
# store the plot from previous step
gp1 <- ggplot(data=cw, aes(x=log(Cases.reported.during.the.year), y=log(Cases.sent.for.trial.during.the.year), color=Crime.Head)) + geom_point()
# add facets for each state / union territory
gp1 + facet_wrap( ~ State.UTs, ncol=2)

# is there a better way to represent this ?
# bar plots
gp2 <- ggplot(data=cw, aes(x=State.UTs, y=Cases.reported.during.the.year)) + geom_bar(stat="identity")
gp2
gp3 <- ggplot(data=cw, aes(x=Crime.Head, y=Cases.reported.during.the.year)) + geom_bar(stat="identity")
gp3
# the facets trick again
gp3 + facet_wrap( ~ State.UTs, ncol=2)


# obviously we have too many states :D

# let's learn about subsetting data
# the first 2 variables are "factors"
# they have "levels"
crimes <- as.character(levels(cw$Crime.Head))
states <- as.character(levels(cw$State.UTs))
# the first 5 states
cw.5states <- subset(cw, cw$State.UTs %in% head(states))
str(cw.5states)
# factor levels NOT dropped by default
# repeat plot for these 5 states
gp4 <- ggplot(data=cw.5states, aes(x=Crime.Head, y=Cases.reported.during.the.year)) + geom_bar(stat="identity")
gp4
gp4 + facet_wrap( ~ State.UTs, ncol=2)

# better
# let's add color for crimes
gp5 <- ggplot(data=cw.5states, aes(x=Crime.Head, y=Cases.reported.during.the.year, fill=Crime.Head)) + geom_bar(stat="identity")
gp5
gp5 + facet_wrap( ~ State.UTs, ncol=2)

# such a happy rainbow of crimes !

# play time
# try other variables - names(cw)
# try removing the total crimes - that should help with the y axis scales & readability

# let's think of the kind of amorphous problem statements data scientists usually get
# "build a women's safety index" for states
# what's safe ?
# low incidence ? incidence = reported + unreported
# high conviction to acquittal ratio ?
# let's look at some ratios
cw.r <- as.data.frame(cbind(state=cw$State.UTs,crime=cw$Crime.Head,subToTrial=cw[,10]/cw[,13]))
head(cw.r)
# where did my factors go ?
cw.r <- as.data.frame(cbind(state=as.character(cw$State.UTs),crime=as.character(cw$Crime.Head),subToTrial=cw[,10]/cw[,13]))
head(cw.r)
# so why are some ratios greater than 1 ?
# interpretation is crucial to good data science
cw.r <- as.data.frame(cbind(state=as.character(cw$State.UTs),crime=as.character(cw$Crime.Head),subToTrial=cw[,10]/cw[,13],convictedToAcquitted=cw[,18]/cw[,19]))
str(cw.r)
head(cw.r)
summary(cw.r)
# can you spot issues ?
gp6 <- ggplot(data=cw.r, aes(x=crime, y=subToTrial, fill=crime)) + geom_bar(stat="identity")
gp6
# now ? why is the ratio high for Sati ?
# let's try with just crime==Rape
cw.r.rape <- subset(cw.r, crime=="Rape")
summary(cw.r.rape)
gp7 <- ggplot(data=cw.r.rape, aes(x=state, y=subToTrial, fill=state)) + geom_bar(stat="identity")
gp7
# what about convicted to acquitted ?
# problem with scale
gp9 <- ggplot(data=cw.r.rape, aes(x=state, y=log(as.numeric(convictedToAcquitted)), fill=state)) + geom_bar(stat="identity")
gp9















