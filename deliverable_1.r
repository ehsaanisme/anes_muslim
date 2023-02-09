rm(list=ls(all=TRUE))
options(scipen = 999)  # turns of scientific notations like 1e+40

#----------------------------------------------------------------

#Setting working directory
setwd("~/Documents/MyPol51")

#----------------------------------------------------------------

#Reading in the current version of the data
therms<-read.csv("anes2020v2.csv")

#----------------------------------------------------------------

#Summary statistics for survey items
summary(therms)

#----------------------------------------------------------------

#Variable names
colnames(therms)

#----------------------------------------------------------------

#Univariates
mean(therms$muslim_therm, na.rm=TRUE)
sd(therms$muslim_therm, na.rm=TRUE)
median(therms$muslim_therm, na.rm=TRUE)
IQR(therms$muslim_therm, na.rm=TRUE)


#----------------------------------------------------------------

#Step 5 Task: Univariate plots

#Percent histogram of Muslim thermometer
trumphist_info <- hist(therms$muslim_therm, plot = FALSE)         # Store output of hist function
trumphist_info$density <- trumphist_info$counts /    # Compute density values
  sum(trumphist_info$counts) * 100

plot(trumphist_info, freq = FALSE, 
     xlab="Thermometer ratings of Muslims",
     ylab="Percent of sample",
     main="Muslim thermometer ratings",
     col="cornflowerblue")

#Histogram of being american

nathist_info <- hist(therms$beingamerican, plot = FALSE)         # Store output of hist function
nathist_info$density <- nathist_info$counts /    # Compute density values
  sum(nathist_info$counts) * 100

plot(nathist_info, freq = FALSE, 
     xlab="Nativist definition of 'being american'",
     ylab="Percent of sample",
     main="Beliefs about being american",
     col="cornflowerblue")

#Barplot percent for follow politics
follow_table<-table(therms$follow)
follow_percent<-follow_table/sum(follow_table)*100
follow_percent

barplot(follow_percent, 
        main="How closely politics are followed", 
        xlab="How closely do you follow politics?", 
        ylab="Percent of Sample", 
        ylim=c(0,50),
        names.arg=c("Not at All", "Not Very Closely", "Fairly Closely", 
                    "Very Closely"),
        cex.lab=.85,
        cex.names=.85,
        col=c("cornflowerblue"))

#Barplot of Racial Identity
race_table<-table(therms$raceidentity)
race_percent<-race_table/sum(race_table)*100
race_percent

barplot(race_percent, 
        main="Race identity in 2022 ANES", 
        xlab="Which do you identify as?", 
        ylab="Percent of Sample", 
        ylim=c(0,80),
        names.arg=c("White", "Black", "Hispanic", 
                    "AAPI", "NAAN", "Multiple"),
        cex.lab=.85,
        cex.names=.85,
        col=c("cornflowerblue"))

#----------------------------------------------------------------

#Boxplots

therms$p7<-factor((therms$pid7),
                  levels = c(1,2,3,4,5,6,7),
                  labels = c("SD", "D", "DL", "I", "RL", "R", "SR")) 

therms$pid3<-factor(therms$pid7,
                    levels = c(1,2,3,4,5,6,7),
                    labels = c("Dem.", "Dem.", 
                               "Dem.", "Ind.", "Rep.", "Rep.", "Rep."))
#3 level plot
boxplot(therms$muslim_therm~therms$pid3,
        xlab="Party affiliation",
        ylab="Muslim feeling thermometer",
        main="Boxplot of ratings of Muslims by party",
        col=c("dodgerblue", "lavender", "red"))

#7 level plot
boxplot(therms$muslim_therm~therms$p7,
        xlab="Party affiliation",
        ylab="Muslim feeling thermometer",
        main="Boxplot of ratings of Muslims by party",
        col=c("dodgerblue", "lavender", "red"))

#----------------------------------------------------------------

#Scatterplots

reg<-lm(therms$beingamerican~therms$muslim_therm)

#Summarizing the object so we can visualize the output
summary(reg)


plot(jitter(therms$muslim_therm, factor=2), jitter(therms$beingamerican, factor=6),
     xlab="Beliefs about being American",
     ylab="Muslim thermometer rating",
     main="Muslim ratings are strongly related\n to endorsment of Nativist beliefs",
     col="cornflowerblue")


#This code will insert the regression line 
abline(reg, col="coral", lwd=4, lty=2)

#----------------------------------------------------------------

#Step 11: Create two-level variable and boxplot

table(therms$raceidentity)

therms$race_factor<-factor(therms$raceidentity,
                    levels = c(1,2,3,4,5,6),
                    labels = c("White", "Non-White", 
                               "Non-White", "Non-White", "Non-White", "Non-White"))

#made new variable to seperate race
table(therms$race_factor)

meansM<-mean(therms$muslim_therm[therms$race_factor=="White"], na.rm=TRUE)
meansF<-mean(therms$muslim_therm[therms$race_factor=="Non-White"], na.rm=TRUE)

boxplot(therms$muslim_therm~therms$race_factor,
        xlab="Race identification",
        ylab="Muslim thermometer",
        main="Boxplot of feelings about muslims by race",
        col=c("dodgerblue", "lavender", "red"))
points(c(meansM,meansF), col=c("black", "black"),pch=19)