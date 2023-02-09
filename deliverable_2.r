#PREPCODE-----------------------------------------------------

rm(list=ls(all=TRUE))
options(scipen = 999)

library(ggplot2)
library(sjPlot)
library(gridExtra)

setwd("~/Documents/MyPol51")

therms<-read.csv("anes2020v2.csv")

summary(therms)

#TASK 1---------------------------------------------------------- 
 
t.test(therms$muslim_therm, mu=50, alternative="less")
 
#TASK 2------------------------------------------------------

therms$race_factor<-factor(therms$raceidentity,
                           levels = c(1,2,3,4,5,6),
                           labels = c("White", "Non-White", 
                                      "Non-White", "Non-White", "Non-White", "Non-White"))

t.test(therms$muslim_therm~therms$race_factor)

#TASK 2 PART B------------------------------------------------

therms$rd<-factor(therms$pid7,
                  levels=c(1,2,3,5,6,7),
                  labels=c("Dem", "Dem", "Dem", "Rep", "Rep", "Rep"))

table(therms$rd)

t.test(therms$muslim_therm~therms$rd)

#TASK 2 PART C------------------------------------------------

t.test(therms$muslim_therm~therms$race_factor, subset=therms$pid7<=3)

t.test(therms$muslim_therm~therms$race_factor, subset=therms$pid7>=5)

#TASK 3---------------------------------------------------------

cor(therms$muslim_therm, therms$beingamerican, use="complete.obs")

#TASK 4 PART A----------------------------------------------------------------

model4a<-lm(muslim_therm~pid7, data=therms)

summary(model4a)

plot_model(model4a, type = "pred", terms = c("pid7"), ci.lvl = .95, 
           title="Muslim evaluations by party affiliation",
           axis.title=c("Party affiliation", "Muslim ratings"), axis.lim=c(-1, 100)) +
             theme_bw()

#TASK 4 PART B----------------------------------------------------------------

therms$p7<-factor((therms$pid7),
                  levels = c(1,2,3,4,5,6,7),
                  labels = c("SD", "D", "DL", "I", "RL", "R", "SR")) 

table(therms$p7)


model4b<-lm(muslim_therm~p7, data=therms)

summary(model4b)

plot_model(model4b, type = "pred", terms = c("p7"), ci.lvl = .95, 
           title="Muslim evaluations by party affiliation",
           axis.title=c("Party affiliation", "Muslim ratings"), 
           axis.lim=c(-1, 100)) + geom_line() + 
  theme_bw()

#TASK 4 PART C----------------------------------------------------------------

therms$p3<-factor(therms$pid7,
                    levels = c(1,2,3,4,5,6,7),
                    labels = c("Dem.", "Dem.", 
                               "Dem.", "Ind.", "Rep.", "Rep.", "Rep.")) 

table(therms$pid3)


model4c<-lm(muslim_therm~p3, data=therms)

summary(model4c)

plot_model(model4c, type = "pred", terms = c("p3"), ci.lvl = .95, 
           title="Muslim evaluations by party affiliation",
           axis.title=c("Party affiliation", "Muslim ratings"), 
           axis.lim=c(-1, 100)) + geom_line() + 
  theme_bw()

#TASK 5----------------------------------------------------------------

#TASK 6----------------------------------------------------------------

model5<-lm(muslim_therm~p7 + beingamerican, data=therms)

summary(model5)

library(stargazer)

stargazer(model5, type = "text")

plot_model(model5, type = "pred", terms = c("beingamerican", "p7"), ci.lvl = .95, 
           title="Muslim evaluations are strongly related to party affiliation \nand endorsement of nativist definitions of citizenship",
           axis.title=c("Endorsement of nativist definition of citizenship", "Muslim ratings"), 
           axis.lim=c(-8, 100), legend.title="Party", 
           colors=c("darkblue", "blue", "cornflowerblue", 
                    "purple", "indianred1", "firebrick3", "darkred")) + 
  theme_bw()

#TASK 7----------------------------------------------------------------

#1: Treating my additional x variable as a factor
therms$fp<-factor(therms$follow,
                   levels=c(1,2,3,4),
                   labels=c("Not at all", "Not very closely",
                    "Fairly closely", "Very closely"))


model6a<-lm(muslim_therm~p7 + beingamerican + follow, data=therms)

summary(model6a)

plot_model(model6a, type = "pred", terms = c("follow", "p7"), ci.lvl = .95, 
           title="Muslim evaluations are strongly related to party affiliation \nbut weakly related to how much one follows politics",
           axis.title=c("How closely do you follow politics?", "Muslim ratings"), 
           axis.lim=c(-8, 100), legend.title="Party", 
           colors=c("darkblue", "blue", "cornflowerblue", 
                    "purple", "indianred1", "firebrick3", "darkred")) + geom_line() +
  theme_bw()

#TASK 8----------------------------------------------------------------

therms$wnw<-factor(therms$raceidentity,
                   levels=c(1,2,3,4,5,6),
                   labels=c("White", "Nonwhite", 
                            "Nonwhite", "Nonwhite",
                            "Nonwhite","Nonwhite"))


white<-subset(therms, raceidentity==1)
table(white$raceidentity)


model7a<-lm(muslim_therm~p7 + beingamerican + fp, data=white)

summary(model7a)

plot_model(model7a, type = "pred", terms = c("p7"), ci.lvl = .95, 
           title="Muslim evaluations among whites are strongly related to party affiliation",
           axis.title=c("How closely do you follow politics?", "Muslim ratings"), 
           axis.lim=c(-8, 100), legend.title="Party", 
           colors=c("darkblue", "blue", "cornflowerblue", 
                    "purple", "indianred1", "firebrick3", "darkred")) + geom_line() +
  theme_bw()

whites

#Nonwhite
nonwhite<-subset(therms, raceidentity>1)
table(nonwhite$raceidentity)


model7b<-lm(muslim_therm~p7 + beingamerican + fp, data=nonwhite)

summary(model7b)

plot_model(model7b, type = "pred", terms = c("p7"), ci.lvl = .95, 
           title="Muslim evaluations among nonwhites are strongly related to party affiliation",
           axis.title=c("Party affiliation", "Muslim ratings"), 
           axis.lim=c(-8, 100), legend.title="Party", 
           colors=c("darkblue", "blue", "cornflowerblue", 
                    "purple", "indianred1", "firebrick3", "darkred")) + geom_line() +
  theme_bw()

#TASK 9-----------------------------------------------------------------------------------------------------------------

model8a<-lm(muslim_therm~p7*wnw + beingamerican + fp, data=therms)

summary(model8a)

plot_model(model8a, type = "pred", terms = c("p7", "wnw"), ci.lvl = .95, 
           title="Muslim evaluations are strongly related to party affiliation \nbut unrelated to white/nonwhite racial identification",
           axis.title=c("Party affiliation", "Muslim ratings"), 
           axis.lim=c(-8, 100), legend.title="Party", 
           colors=c("cornflowerblue", 
                    "coral"), dodge=.3) + geom_line() +
  theme_bw()

#Education as a two-level variable

therms$cnc<-factor(therms$education,
                   levels=c(1,2,3,4,5),
                   labels=c("No college", "No college", 
                            "No college", "College",
                            "College"))

model8b<-lm(muslim_therm~p7*cnc + beingamerican + fp, data=therms)

summary(model8b)

plot_model(model8b, type = "pred", terms = c("p7", "cnc"), ci.lvl = .95, 
           title="Muslim evaluations are strongly related to party affiliation \nand moderately related to education status",
           axis.title=c("Party affiliation", "Muslim ratings"), 
           axis.lim=c(-8, 100), legend.title="Party", 
           colors=c("cornflowerblue", 
                    "coral"), dodge=.3) + geom_line() +
  theme_bw()
