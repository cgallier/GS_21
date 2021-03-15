###########################################################################
### The Ratchet Effect in Social Dilemmas #################################
### Carlo Gallier & Bodo Sturm ############################################
### JEBO R&R 3 ############################################################

# This version: February, 2021

# Packages ----------------------------------------------------------------

library(AER)
library(ggplot2)
library(plyr)
library(wesanderson)
library(plotrix)
library(car)
library(stargazer)
library(gridExtra)
library(reshape)
library(lmtest)
library(sandwich)
library(multiwayvcov)
library(scales)

# Directory ---------------------------------------------------------------
getwd()  # Where is the current working Directory?
setwd("C:/Users/cgi/2020/BMBF InFairCom/02 WP3/StockGame/Paper/JEBO/Paper/Revision3/Analysis") # Set working directory

# Time stamp --------------------------------------------------------------------
st=format(Sys.time() , "%Y-%m-%d_%H-%M") # Set time stamp y-m-d-h-m

# Data & data preparation -------------------------------------------------

# Data 
data_long<- read.csv("GS_2020_JEBO.csv", sep = ";")
names(data_long)

# Variables & labels
data_long$group <- ifelse(data_long$subject <= 4, 1, ifelse(data_long$subject <= 8, 2, (ifelse(data_long$subject <= 12, 3, ifelse(data_long$subject <= 16, 4, ifelse(data_long$subject<= 20, 5, 6))))))
data_long$session_num <- ifelse(data_long$session == "S01", 1, 
                                ifelse(data_long$session == "S02", 2,
                                ifelse(data_long$session == "S03", 3,
                                ifelse(data_long$session == "S04", 4,
                                ifelse(data_long$session == "S05", 5,
                                ifelse(data_long$session == "S06", 6,
                                ifelse(data_long$session == "S07", 7,
                                ifelse(data_long$session == "S08", 8,
                                ifelse(data_long$session == "S09", 9,
                                ifelse(data_long$session == "S10", 10,
                                ifelse(data_long$session == "S11", 11,
                                ifelse(data_long$session == "S12", 12,
                                ifelse(data_long$session == "S13", 13,
                                ifelse(data_long$session == "S14", 14,
                                ifelse(data_long$session == "S15", 15,
                                ifelse(data_long$session == "S16", 16,
                                ifelse(data_long$session == "S17", 17,
                                ifelse(data_long$session == "S18", 18,
                                ifelse(data_long$session == "S19", 19,
                                ifelse(data_long$session == "S20", 20, 0))))))))))))))))))))

## Define additional variables: functions

# Cooperation rate: Transform contribution levels (in absolute terms) into cooperation rates (in percent)
cooprate <- function(x){
  (x-15)/(90-15)
}

# Transform contributions into cooperation rates
data_long$cooprate <- cooprate(data_long$q)
# Plausibility:
# Check range - contribution
summary(data_long$q)
# Check range - cooperation rate
summary(data_long$cooprate)

# Standard errors
stderr <- function(x){
  sqrt(var(x)/length(x))
}

# Groups
data_long$group_total <- as.numeric(paste(data_long$session_num, data_long$group, sep = ""))

# Aggregate variables

# Aggregate variables at the group, period, phase, game level
# Aggreagte data at tratment & group & period - Phase level by group
data_notshort <- ddply(data_long, .(treatment,group_total,period), summarize,  q=mean(q), cooprate=mean(cooprate))

# Aggregate data at treatment & subject & period - Phase level by subject
data_notsoshort <- ddply(data_long, .(treatment,group_total,period, subject), summarize,  q=mean(q), cooprate=mean(cooprate))

# Aggregate data at treatment & group - Game level by group
data_short <- ddply(data_long, .(treatment,group_total), summarize,  q=mean(q), cooprate=mean(cooprate))

#Aggregated data at treatment & group & phase - Phase level by group
data_phase <- ddply(data_long, .(treatment,group_total, phase), summarize,  q_mean=mean(q), cooprate=mean(cooprate), q_sum=sum(q))

# Aggregate data at treatment & group & phase & period - Period level by group
data_short_period <- ddply(data_long, .(treatment,group_total, phase, period), summarize,  q=mean(q), cooprate=mean(cooprate))

# Aggregate data at treatment & group & period - Period level by group
data_short_phase <- ddply(data_long, .(treatment,group_total, period), summarize,  q=mean(q), cooprate=mean(cooprate))

# Section 3 Results -----------------------------------------------------------

###
# Section 3.1 - The ratchet effect --------------------------------------------
###

###
# Figure 1: Cooperation rates per group by phase, period, and treatment 
###

# Data preparation 
# Periods
Round <- c(1,2,3,4,5)
# Means
# base
q_base_ph1 <- c(mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==1]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==2]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==3]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==4]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==5]))
q_base_ph2 <- c(mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==1]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==2]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==3]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==4]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==5]))
q_base_ph3 <- c(mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==1]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==2]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==3]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==4]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==5]))
q_base_ph4 <- c(mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==1]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==2]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==3]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==4]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==5]))
q_base_ph5 <- c(mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==1]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==2]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==3]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==4]),
                mean(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==5]))
# weakR
q_weakR_ph1 <- c(mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==1]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==2]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==3]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==4]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==5]))
q_weakR_ph2 <- c(mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==1]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==2]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==3]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==4]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==5]))
q_weakR_ph3 <- c(mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==1]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==2]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==3]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==4]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==5]))
q_weakR_ph4 <- c(mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==1]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==2]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==3]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==4]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==5]))
q_weakR_ph5 <- c(mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==1]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==2]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==3]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==4]),
                 mean(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==5]))
# strongR
q_strongR_ph1 <- c(mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==1]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==2]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==3]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==4]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==5]))
q_strongR_ph2 <- c(mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==1]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==2]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==3]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==4]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==5]))
q_strongR_ph3 <- c(mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==1]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==2]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==3]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==4]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==5]))
q_strongR_ph4 <- c(mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==1]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==2]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==3]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==4]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==5]))
q_strongR_ph5 <- c(mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==1]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==2]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==3]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==4]),
                 mean(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==5]))

# standard errors
# base
q_base_ph1_SE <- c((sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==1])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==2])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==3])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==4])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                  (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==5]/sqrt(length(data_short$treatment[data_short$treatment=="base"])))))
q_base_ph2_SE <- c((sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==1])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==2])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==3])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==4])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==5]/sqrt(length(data_short$treatment[data_short$treatment=="base"])))))
q_base_ph3_SE <- c((sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==1])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==2])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==3])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==4])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==5]/sqrt(length(data_short$treatment[data_short$treatment=="base"])))))
q_base_ph4_SE <- c((sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==1])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==2])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==3])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==4])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==5]/sqrt(length(data_short$treatment[data_short$treatment=="base"])))))
q_base_ph5_SE <- c((sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==1])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==2])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==3])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==4])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                   (sd(data_short_period$q[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==5]/sqrt(length(data_short$treatment[data_short$treatment=="base"])))))
# weakR
q_weakR_ph1_SE <- c((sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==1])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==2])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==3])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==4])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==5])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))))
q_weakR_ph2_SE <- c((sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==1])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==2])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==3])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==4])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==5])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))))
q_weakR_ph3_SE <- c((sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==1])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==2])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==3])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==4])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==5])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))))
q_weakR_ph4_SE <- c((sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==1])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==2])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==3])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==4])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==5])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))))
q_weakR_ph5_SE <- c((sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==1])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==2])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==3])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==4])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))),
                    (sd(data_short_period$q[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==5])/sqrt(length(data_short$treatment[data_short$treatment=="base"]))))
# strongR 
q_strongR_ph1_SE <- c(stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==1]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==2]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==3]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==4]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==5]))
q_strongR_ph2_SE <- c(stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==1]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==2]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==3]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==4]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==5]))
q_strongR_ph3_SE <- c(stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==1]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==2]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==3]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==4]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==5]))
q_strongR_ph4_SE <- c(stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==1]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==2]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==3]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==4]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==5]))
q_strongR_ph5_SE <- c(stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==1]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==2]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==3]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==4]),
                      stderr(data_short_period$q[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==5]))

# Confidence intervals

alpha <- 0.05 # significance level

# base
q_base_ph1_CI <- qt(1-(alpha/2), 
                    df = length(data_short$q[data_short$treatment=="base"])-1)*q_base_ph1_SE
q_base_ph2_CI <- qt(1-(alpha/2), 
                    df = length(data_short$q[data_short$treatment=="base"])-1)*q_base_ph2_SE
q_base_ph3_CI <- qt(1-(alpha/2), 
                    df = length(data_short$q[data_short$treatment=="base"])-1)*q_base_ph3_SE
q_base_ph4_CI <- qt(1-(alpha/2), 
                    df = length(data_short$q[data_short$treatment=="base"])-1)*q_base_ph4_SE
q_base_ph5_CI <- qt(1-(alpha/2), 
                    df = length(data_short$q[data_short$treatment=="base"])-1)*q_base_ph5_SE

# weakR
q_weakR_ph1_CI <- qt(1-(alpha/2), 
                    df = length(data_short$q[data_short$treatment=="weakR"])-1)*q_weakR_ph1_SE
q_weakR_ph2_CI <- qt(1-(alpha/2), 
                     df = length(data_short$q[data_short$treatment=="weakR"])-1)*q_weakR_ph2_SE
q_weakR_ph3_CI <- qt(1-(alpha/2), 
                     df = length(data_short$q[data_short$treatment=="weakR"])-1)*q_weakR_ph3_SE
q_weakR_ph4_CI <- qt(1-(alpha/2), 
                     df = length(data_short$q[data_short$treatment=="weakR"])-1)*q_weakR_ph4_SE
q_weakR_ph5_CI <- qt(1-(alpha/2), 
                     df = length(data_short$q[data_short$treatment=="weakR"])-1)*q_weakR_ph5_SE

# strongR
q_strongR_ph1_CI <- qt(1-(alpha/2), 
                    df = length(data_short$q[data_short$treatment=="strongR"])-1)*q_strongR_ph1_SE
q_strongR_ph2_CI <- qt(1-(alpha/2), 
                       df = length(data_short$q[data_short$treatment=="strongR"])-1)*q_strongR_ph2_SE
q_strongR_ph3_CI <- qt(1-(alpha/2), 
                       df = length(data_short$q[data_short$treatment=="strongR"])-1)*q_strongR_ph3_SE
q_strongR_ph4_CI <- qt(1-(alpha/2), 
                       df = length(data_short$q[data_short$treatment=="strongR"])-1)*q_strongR_ph4_SE
q_strongR_ph5_CI <- qt(1-(alpha/2), 
                       df = length(data_short$q[data_short$treatment=="strongR"])-1)*q_strongR_ph5_SE

## Data file 
data_plot <- data.frame(Round, 
                        q_base_ph1, q_base_ph1_CI, q_weakR_ph1, q_weakR_ph1_CI, q_strongR_ph1, q_strongR_ph1_CI,
                        q_base_ph2, q_base_ph2_CI, q_weakR_ph2, q_weakR_ph2_CI, q_strongR_ph2, q_strongR_ph2_CI,
                        q_base_ph3, q_base_ph3_CI, q_weakR_ph3, q_weakR_ph3_CI, q_strongR_ph3, q_strongR_ph3_CI,
                        q_base_ph4, q_base_ph4_CI, q_weakR_ph4, q_weakR_ph4_CI, q_strongR_ph4, q_strongR_ph4_CI,
                        q_base_ph5, q_base_ph5_CI, q_weakR_ph5, q_weakR_ph5_CI, q_strongR_ph5, q_strongR_ph5_CI)

## Transform contributions into cooperation rates
#base
#mean
data_plot$q_base_ph1C <- cooprate(q_base_ph1)
data_plot$q_base_ph2C <- cooprate(q_base_ph2)
data_plot$q_base_ph3C <- cooprate(q_base_ph3)
data_plot$q_base_ph4C <- cooprate(q_base_ph4)
data_plot$q_base_ph5C <- cooprate(q_base_ph5)
# CI 
data_plot$q_base_ph1_CIC <- cooprate(q_base_ph1 + q_base_ph1_CI) - cooprate(q_base_ph1)
data_plot$q_base_ph2_CIC <- cooprate(q_base_ph2 + q_base_ph2_CI) - cooprate(q_base_ph2)
data_plot$q_base_ph3_CIC <- cooprate(q_base_ph3 + q_base_ph3_CI) - cooprate(q_base_ph3)
data_plot$q_base_ph4_CIC <- cooprate(q_base_ph4 + q_base_ph4_CI) - cooprate(q_base_ph4)
data_plot$q_base_ph5_CIC <- cooprate(q_base_ph5 + q_base_ph5_CI) - cooprate(q_base_ph5)
# weakR
# mean
data_plot$q_weakR_ph1C <- cooprate(q_weakR_ph1)
data_plot$q_weakR_ph2C <- cooprate(q_weakR_ph2)
data_plot$q_weakR_ph3C <- cooprate(q_weakR_ph3)
data_plot$q_weakR_ph4C <- cooprate(q_weakR_ph4)
data_plot$q_weakR_ph5C <- cooprate(q_weakR_ph5)
# CI 
data_plot$q_weakR_ph1_CIC <- cooprate(q_weakR_ph1 + q_weakR_ph1_CI) - cooprate(q_weakR_ph1) 
data_plot$q_weakR_ph2_CIC <- cooprate(q_weakR_ph2 + q_weakR_ph2_CI) - cooprate(q_weakR_ph2) 
data_plot$q_weakR_ph3_CIC <- cooprate(q_weakR_ph3 + q_weakR_ph3_CI) - cooprate(q_weakR_ph3) 
data_plot$q_weakR_ph4_CIC <- cooprate(q_weakR_ph4 + q_weakR_ph4_CI) - cooprate(q_weakR_ph4) 
data_plot$q_weakR_ph5_CIC <- cooprate(q_weakR_ph5 + q_weakR_ph5_CI) - cooprate(q_weakR_ph5) 
# strongR
# mean
data_plot$q_strongR_ph1C <- cooprate(q_strongR_ph1)
data_plot$q_strongR_ph2C <- cooprate(q_strongR_ph2)
data_plot$q_strongR_ph3C <- cooprate(q_strongR_ph3)
data_plot$q_strongR_ph4C <- cooprate(q_strongR_ph4)
data_plot$q_strongR_ph5C <- cooprate(q_strongR_ph5)
# CI 
data_plot$q_strongR_ph1_CIC <- cooprate(q_strongR_ph1 + q_strongR_ph1_CI) - cooprate(q_strongR_ph1)
data_plot$q_strongR_ph2_CIC <- cooprate(q_strongR_ph2 + q_strongR_ph2_CI) - cooprate(q_strongR_ph2)
data_plot$q_strongR_ph3_CIC <- cooprate(q_strongR_ph3 + q_strongR_ph3_CI) - cooprate(q_strongR_ph3)
data_plot$q_strongR_ph4_CIC <- cooprate(q_strongR_ph4 + q_strongR_ph4_CI) - cooprate(q_strongR_ph4)
data_plot$q_strongR_ph5_CIC <- cooprate(q_strongR_ph5 + q_strongR_ph5_CI) - cooprate(q_strongR_ph5)

## Plot
# Fig.1
# out of space
pdf(file = paste("Fig1_", st, ".pdf",sep = ""),
    width = 11.00, 
    height = 7.00) 

# layout
layout.matrix <- matrix(c(1,2,3,4,5), nrow = 1, ncol= 5)
layout(mat = layout.matrix,
       widths = c(2.35,2,2,2,2.35))

# Colors
cols <- wes_palette("FantasticFox1",5)

# Phase I
par(mar = c(4,5,0,0.00))
Plot_ph1 <- plot(data_plot$Round, data_plot$q_base_ph1C, type = "b", 
                pch = 15, col = cols[1] ,lty=1, lwd = 2, cex = 2.0, font=1, bty="n",
                ylim = c(-0.1, 1.1), xlim = c(1, 5),
                ylab = "", yaxt="n", xlab ="", xaxt="n",
                axes = TRUE, cex.axis = 1.5, cex.lab = 1.0,
                main = "")
  text(3, 1.09, "Phase I", cex = 2.0, xpd=TRUE)
  axis(2, at=c(-0.10,0.0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00,1.10),
     labels=c(-10.00,0.00,10.00,20.00,30.00,40.00,50.00,60.00,70.00,80.00,90.00,100.00,110.00), las=2, lty=1, cex.axis=1.5)
  axis(1, at=c(1,2,3,4,5), 
     labels=c(1,2,3,4,5), las=1, lty=1, cex.axis=1.5)
  axis(4, at=c(-0.10,0.0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00,1.10),
     labels=c("","","","","","","","","","","","",""), las=2, lty=1, cex.axis=1.5)
  mtext("Cooperation rate (in %)", side=2, line=3.4, cex=1.25)
  arrows(data_plot$Round, data_plot$q_base_ph1C, data_plot$Round, data_plot$q_base_ph1C+data_plot$q_base_ph1_CIC, length = 0.05, angle = 90, col = cols[1])
  arrows(data_plot$Round, data_plot$q_base_ph1C, data_plot$Round, data_plot$q_base_ph1C-data_plot$q_base_ph1_CIC, length = 0.05, angle = 90, col = cols[1])
  lines(data_plot$Round, data_plot$q_weakR_ph1C, type = "b", pch = 19, cex = 2.0, col = cols[3], lwd = 2)
  arrows(data_plot$Round, data_plot$q_weakR_ph1C, data_plot$Round, data_plot$q_weakR_ph1C+data_plot$q_weakR_ph1_CIC, length = 0.05, angle = 90, col = cols[3])
  arrows(data_plot$Round, data_plot$q_weakR_ph1C, data_plot$Round, data_plot$q_weakR_ph1C-data_plot$q_weakR_ph1_CIC, length = 0.05, angle = 90, col = cols[3])
  lines(data_plot$Round, data_plot$q_strongR_ph1C , type = "b", pch = 17, cex = 2.0,  col = cols[5], lwd = 2)
  arrows(data_plot$Round, data_plot$q_strongR_ph1C, data_plot$Round, data_plot$q_strongR_ph1C+data_plot$q_strongR_ph1_CIC, length = 0.05, angle = 90,  col = cols[5])
  arrows(data_plot$Round, data_plot$q_strongR_ph1C, data_plot$Round, data_plot$q_strongR_ph1C-data_plot$q_strongR_ph1_CIC, length = 0.05, angle = 90,  col = cols[5])
  abline(h=c(0.00, 1.00), lty = 5, lwd = 1, col = "gray30", xpd=FALSE)
  abline(h=c(-0.10,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00), lty = 3, lwd = 1, col = "gray30", xpd=FALSE)

# Phase II
par(mar = c(4,1.60,0,1.00))
Plot_ph2 <- plot(data_plot$Round, data_plot$q_base_ph2C, type = "b", 
                pch = 15, col = cols[1], lty=1, lwd = 2, cex = 2.0, font=1, bty="n",
                ylim = c(-0.10, 1.10), xlim = c(1, 5),
                ylab = "", yaxt="n", xlab ="", xaxt="n",
                cex.axis = 1.5, cex.lab = 1.0,
                axes = TRUE, main = "")
  text(3, 1.09, "Phase II", cex = 2.0, xpd=TRUE)
  axis(2, at=c(-0.10,0.0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00,1.10),
     labels=c("","","","","","","","","","","","",""), las=2, lty=1, cex.axis=1)
  axis(1, at=c(1,2,3,4,5), 
     labels=c(1,2,3,4,5), las=1, lty=1, cex.axis=1.5)
  axis(4, at=c(-0.10,0.0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00,1.10),
     labels=c("","","","","","","","","","","","",""), las=2, lty=1, cex.axis=1)
  arrows(data_plot$Round, data_plot$q_base_ph2C, data_plot$Round, data_plot$q_base_ph2C+data_plot$q_base_ph2_CIC, length = 0.05, angle = 90, col = cols[1])
  arrows(data_plot$Round, data_plot$q_base_ph2C, data_plot$Round, data_plot$q_base_ph2C-data_plot$q_base_ph2_CIC, length = 0.05, angle = 90, col = cols[1])
  lines(data_plot$Round, data_plot$q_weakR_ph2C, type = "b", pch = 19, col = cols[3], cex = 2.0, lwd = 2)
  arrows(data_plot$Round, data_plot$q_weakR_ph2C, data_plot$Round, data_plot$q_weakR_ph2C+data_plot$q_weakR_ph2_CIC, length = 0.05, angle = 90, col = cols[3])
  arrows(data_plot$Round, data_plot$q_weakR_ph2C, data_plot$Round, data_plot$q_weakR_ph2C-data_plot$q_weakR_ph2_CIC, length = 0.05, angle = 90, col = cols[3])
  lines(data_plot$Round, data_plot$q_strongR_ph2C , type = "b", pch = 17, col = cols[5], cex = 2.0, lwd = 2)
  arrows(data_plot$Round, data_plot$q_strongR_ph2C, data_plot$Round, data_plot$q_strongR_ph2C+data_plot$q_strongR_ph1_CIC, length = 0.05, angle = 90, col = cols[5])
  arrows(data_plot$Round, data_plot$q_strongR_ph2C, data_plot$Round, data_plot$q_strongR_ph2C-data_plot$q_strongR_ph1_CIC, length = 0.05, angle = 90, col = cols[5])
  abline(h=c(0.00, 1.00), lty = 5, lwd = 1, col = "gray30", xpd=FALSE)
  abline(h=c(-0.10,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00), lty = 3, lwd = 1, col = "gray30", xpd=FALSE)

# Phase III
par(mar = c(4,0.90,0,0.90))
Plot_ph3 <- plot(data_plot$Round, data_plot$q_base_ph3C, type = "b", 
                pch = 15, col = cols[1],lty=1, lwd = 2, cex = 2.0, font=1, bty="n",
                ylim = c(-0.10, 1.10), xlim = c(1, 5),
                ylab = "", yaxt="n", xlab ="", xaxt="n",
                cex.axis = 1.5, cex.lab = 1.0,
                axes = TRUE, main = "")
  text(3, 1.09, "Phase III", cex = 2.0, xpd=TRUE)
  axis(2, at=c(-0.10,0.0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00,1.10),
     labels=c("","","","","","","","","","","","",""), las=2, lty=1, cex.axis=1)
  axis(1, at=c(1,2,3,4,5), 
     labels=c(1,2,3,4,5), las=1, lty=1, cex.axis=1.5)
  axis(4, at=c(-0.10,0.0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00,1.10),
     labels=c("","","","","","","","","","","","",""), las=2, lty=1, cex.axis=1)
  arrows(data_plot$Round, data_plot$q_base_ph3C, data_plot$Round, data_plot$q_base_ph3C+data_plot$q_base_ph3_CIC, length = 0.05, angle = 90, col = cols[1])
  arrows(data_plot$Round, data_plot$q_base_ph3C, data_plot$Round, data_plot$q_base_ph3C-data_plot$q_base_ph3_CIC, length = 0.05, angle = 90, col = cols[1])
  lines(data_plot$Round, data_plot$q_weakR_ph3C, type = "b", pch = 19, col = cols[3], cex = 2.0, lwd = 2)
  arrows(data_plot$Round, data_plot$q_weakR_ph3C, data_plot$Round, data_plot$q_weakR_ph3C+data_plot$q_weakR_ph3_CIC, length = 0.05, angle = 90, col = cols[3])
  arrows(data_plot$Round, data_plot$q_weakR_ph3C, data_plot$Round, data_plot$q_weakR_ph3C-data_plot$q_weakR_ph3_CIC, length = 0.05, angle = 90, col = cols[3])
  lines(data_plot$Round, data_plot$q_strongR_ph3C, type = "b", pch = 17, col = cols[5], cex = 2.0, lwd = 2)
  arrows(data_plot$Round, data_plot$q_strongR_ph3C, data_plot$Round, data_plot$q_strongR_ph3C+data_plot$q_strongR_ph3_CIC, length = 0.05, angle = 90, col = cols[5])
  arrows(data_plot$Round, data_plot$q_strongR_ph3C, data_plot$Round, data_plot$q_strongR_ph3C-data_plot$q_strongR_ph3_CIC, length = 0.05, angle = 90, col = cols[5])
  legend(0.90, 0.88,  pch = 15, col = cols[1], lty = 1, "base", cex = 1.25, lwd = 2, bty = "n", x.intersp = 0.15, text.font = 3)
  legend(0.90, 0.82,  pch = 19, col = cols[3], lty = 1, "weakR", cex = 1.25, lwd = 2, bty = "n", x.intersp = 0.15, text.font = 3)
  legend(0.90, 0.76,  pch = 17, col = cols[5], lty = 1, "strongR", cex = 1.25, lwd = 2, bty = "n", x.intersp = 0.15, text.font = 3)
  mtext("Periods", side=1, line=3.0, cex=1.5)
  text(3, -0.025, "Nash equilibrium", cex = 1.5, xpd=TRUE)
  text(3, 0.975, "Social optimum", cex = 1.5, xpd=TRUE)
  abline(h=c(0.00, 1.00), lty = 5, lwd = 1, col = "gray30", xpd=FALSE)
  abline(h=c(-0.10,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00), lty = 3, lwd = 1, col = "gray30", xpd=FALSE)

# Phase IV
par(mar = c(4,1.00,0,1.60))
Plot_ph4 <- plot(data_plot$Round, data_plot$q_base_ph4C, type = "b", 
                pch = 15, col = cols[1], lty=1, lwd = 2, cex = 2.0, font=1, bty="n",
                ylim = c(-0.10, 1.10), xlim = c(1, 5),
                ylab = "", yaxt="n", xlab ="", xaxt="n",
                cex.axis = 1.5, cex.lab = 1.0,
                axes = TRUE, main = "")
  text(3, 1.09, "Phase IV", cex = 2.0, xpd=TRUE)
  axis(2, at=c(-0.10,0.0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00,1.10),
     labels=c("","","","","","","","","","","","",""), las=2, lty=1, cex.axis=1)
  axis(1, at=c(1,2,3,4,5), 
     labels=c(1,2,3,4,5), las=1, lty=1, cex.axis=1.5)
  axis(4, at=c(-0.10,0.00,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00,1.10),
     labels=c("","","","","","","","","","","","",""), las=2, lty=1, cex.axis=1)
  arrows(data_plot$Round, data_plot$q_base_ph4C, data_plot$Round, data_plot$q_base_ph4C+data_plot$q_base_ph4_CIC, length = 0.05, angle = 90, col = cols[1])
  arrows(data_plot$Round, data_plot$q_base_ph4C, data_plot$Round, data_plot$q_base_ph4C-data_plot$q_base_ph4_CIC, length = 0.05, angle = 90, col = cols[1])
  lines(data_plot$Round, data_plot$q_weakR_ph4C, type = "b", pch = 19, col = cols[3], cex = 2.0, lwd = 2)
  arrows(data_plot$Round, data_plot$q_weakR_ph4C, data_plot$Round, data_plot$q_weakR_ph4C+data_plot$q_weakR_ph4_CIC, length = 0.05, angle = 90, col = cols[3])
  arrows(data_plot$Round, data_plot$q_weakR_ph4C, data_plot$Round, data_plot$q_weakR_ph4C-data_plot$q_weakR_ph4_CIC, length = 0.05, angle = 90, col = cols[3])
  lines(data_plot$Round, data_plot$q_strongR_ph4C, type = "b", pch = 17, col = cols[5], cex = 2.0, lwd = 2)
  arrows(data_plot$Round, data_plot$q_strongR_ph4C, data_plot$Round, data_plot$q_strongR_ph4C+data_plot$q_strongR_ph4_CIC, length = 0.05, angle = 90, col = cols[5])
  arrows(data_plot$Round, data_plot$q_strongR_ph4C, data_plot$Round, data_plot$q_strongR_ph4C-data_plot$q_strongR_ph4_CIC, length = 0.05, angle = 90, col = cols[5])
  abline(h=c(0.00, 1.00), lty = 5, lwd = 1, col = "gray30", xpd=FALSE)
  abline(h=c(-0.10,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00), lty = 3, lwd = 1, col = "gray30", xpd=FALSE)

# Phase V
par(mar = c(4,0.00,0,5))
Plot_ph5 <- plot(data_plot$Round, data_plot$q_base_ph5C, type = "b", 
                pch = 15, col = cols[1], lty=1, lwd = 2, cex = 2.0, font=1, bty="n",
                ylim = c(-0.10, 1.10), xlim = c(1, 5),
                ylab = "", yaxt="n", xlab ="", xaxt="n",
                cex.axis = 1.5, cex.lab = 1.0,
                axes = TRUE, main = "")
  text(3, 1.09, "Phase V", cex = 2.0, xpd=TRUE)
  axis(2, at=c(-0.10,0.00,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00,1.10),
     labels=c("","","","","","","","","","","","",""), las=2, lty=1, cex.axis=1.5)
  axis(1, at=c(1,2,3,4,5), 
     labels=c(1,2,3,4,5), las=1, lty=1, cex.axis=1.5)
  axis(4, at=c(-0.10,0.00,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00,1.10),
     labels=c(-10.00,0.00,10.00,20.00,30.00,40.00,50.00,60.00,70.00,80.00,90.00,100.00,110.00), las=2, lty=1, cex.axis=1.5)
  mtext("Cooperation rate (in %)", side=4, line=3.4, cex=1.25)
  arrows(data_plot$Round, data_plot$q_base_ph5C, data_plot$Round, data_plot$q_base_ph5C+data_plot$q_base_ph3_CIC, length = 0.05, angle = 90, col = cols[1])
  arrows(data_plot$Round, data_plot$q_base_ph5C, data_plot$Round, data_plot$q_base_ph5C-data_plot$q_base_ph3_CIC, length = 0.05, angle = 90, col = cols[1])
  lines(data_plot$Round, data_plot$q_weakR_ph5C, type = "b", pch = 19, col = cols[3], cex = 2.0, lwd = 2)
  arrows(data_plot$Round, data_plot$q_weakR_ph5C, data_plot$Round, data_plot$q_weakR_ph5C+data_plot$q_weakR_ph3_CIC, length = 0.05, angle = 90, col = cols[3])
  arrows(data_plot$Round, data_plot$q_weakR_ph5C, data_plot$Round, data_plot$q_weakR_ph5C-data_plot$q_weakR_ph3_CIC, length = 0.05, angle = 90, col = cols[3])
  lines(data_plot$Round, data_plot$q_strongR_ph5C, type = "b", pch = 17, col = cols[5], cex = 2.0, lwd = 2)
  arrows(data_plot$Round, data_plot$q_strongR_ph5C, data_plot$Round, data_plot$q_strongR_ph5C+data_plot$q_strongR_ph5_CIC, length = 0.05, angle = 90, col = cols[5])
  arrows(data_plot$Round, data_plot$q_strongR_ph5C, data_plot$Round, data_plot$q_strongR_ph5C-data_plot$q_strongR_ph5_CIC, length = 0.05, angle = 90, col = cols[5])
  abline(h=c(0.00, 1.00), lty = 5, lwd = 1, col = "gray30", xpd=FALSE)
  abline(h=c(-0.10,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00), lty = 3, lwd = 1, col = "gray30", xpd=FALSE)

dev.off()
  
## Summary statistics & tests

###
# Table 1. Average cooperation rates
###

# overall
# base
# all periods
summary(data_notshort$cooprate[data_notshort$treatment=="base"])*100
stderr(data_notshort$cooprate[data_notshort$treatment=="base"])*100
# first period
summary(data_notshort$cooprate[data_notshort$treatment=="base"&data_notshort$period==1])*100
stderr(data_notshort$cooprate[data_notshort$treatment=="base"&data_notshort$period==1])*100
# last period
summary(data_notshort$cooprate[data_notshort$treatment=="base"&data_notshort$period==5])*100
stderr(data_notshort$cooprate[data_notshort$treatment=="base"&data_notshort$period==5])*100
# weakR
# all periods
summary(data_notshort$cooprate[data_notshort$treatment=="weakR"])*100
stderr(data_notshort$cooprate[data_notshort$treatment=="weakR"])*100
# first period
summary(data_notshort$cooprate[data_notshort$treatment=="weakR"&data_notshort$period==1])*100
stderr(data_notshort$cooprate[data_notshort$treatment=="weakR"&data_notshort$period==1])*100
# last period
summary(data_notshort$cooprate[data_notshort$treatment=="weakR"&data_notshort$period==5])*100
stderr(data_notshort$cooprate[data_notshort$treatment=="weakR"&data_notshort$period==5])*100
# strongR
# all periods
summary(data_notshort$cooprate[data_notshort$treatment=="strongR"])*100
stderr(data_notshort$cooprate[data_notshort$treatment=="strongR"])*100
# first period
summary(data_notshort$cooprate[data_notshort$treatment=="strongR"&data_notshort$period==1])*100
stderr(data_notshort$cooprate[data_notshort$treatment=="strongR"&data_notshort$period==1])*100
# last period
summary(data_notshort$cooprate[data_notshort$treatment=="strongR"&data_notshort$period==5])*100
stderr(data_notshort$cooprate[data_notshort$treatment=="strongR"&data_notshort$period==5])*100

# Per phase
# Phase I
# base
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==1])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==1&data_short_period$period==5])*100
# weakR
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==1])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==1&data_short_period$period==5])*100
# strongR
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==1])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==1&data_short_period$period==5])*100
# Phase II
# base
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==2])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==2])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==2&data_short_period$period==5])*100
# weakR
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==2])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==2])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==2&data_short_period$period==5])*100
# strongR
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==2])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==2])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==2&data_short_period$period==5])*100
# Phase III
# base
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==3])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==3])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==3&data_short_period$period==5])*100
# weakR
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==3])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==3])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==3&data_short_period$period==5])*100
# strongR
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==3])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==3])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==3&data_short_period$period==5])*100
# Phase IV
# base
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==4])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==4])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==4&data_short_period$period==5])*100
# weakR
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==4])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==4])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==4&data_short_period$period==5])*100
# strongR
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==4])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==4])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==4&data_short_period$period==5])*100
# Phase V
# base
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==5])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="base"&data_short_period$phase==5&data_short_period$period==5])*100
# weakR
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==5])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="weakR"&data_short_period$phase==5&data_short_period$period==5])*100
# strongR
# all periods
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==5])*100
# first period
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==1])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==1])*100
# last period
summary(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==5])*100
stderr(data_short_period$cooprate[data_short_period$treatment=="strongR"&data_short_period$phase==5&data_short_period$period==5])*100

# Cooperation rates in base across periods
#summary(data_notshort$cooprate[data_notshort$treatment=="base"])
#summary(data_notshort$cooprate[data_notshort$treatment=="base"&data_notshort$period==1])
#summary(data_notshort$cooprate[data_notshort$treatment=="base"&data_notshort$period==2])
#summary(data_notshort$cooprate[data_notshort$treatment=="base"&data_notshort$period==3])
#summary(data_notshort$cooprate[data_notshort$treatment=="base"&data_notshort$period==4])
#summary(data_notshort$cooprate[data_notshort$treatment=="base"&data_notshort$period==5])
# Cooperation rates in weakR across periods
#summary(data_notshort$cooprate[data_notshort$treatment=="weakR"])
#summary(data_notshort$cooprate[data_notshort$treatment=="weakR"&data_notshort$period==1])
#summary(data_notshort$cooprate[data_notshort$treatment=="weakR"&data_notshort$period==2])
#summary(data_notshort$cooprate[data_notshort$treatment=="weakR"&data_notshort$period==3])
#summary(data_notshort$cooprate[data_notshort$treatment=="weakR"&data_notshort$period==4])
#summary(data_notshort$cooprate[data_notshort$treatment=="weakR"&data_notshort$period==5])
# Cooperation rates in strongR across periods
#summary(data_notshort$cooprate[data_notshort$treatment=="strongR"])
#summary(data_notshort$cooprate[data_notshort$treatment=="strongR"&data_notshort$period==1])
#summary(data_notshort$cooprate[data_notshort$treatment=="strongR"&data_notshort$period==2])
#summary(data_notshort$cooprate[data_notshort$treatment=="strongR"&data_notshort$period==3])
#summary(data_notshort$cooprate[data_notshort$treatment=="strongR"&data_notshort$period==4])
#summary(data_notshort$cooprate[data_notshort$treatment=="strongR"&data_notshort$period==5])

## Tests

###
# Differences across treatments at the beginning of each phase: First round 
###

## base vs. weakR
# first round
# t-test
BaseVsWeakR_T_r1 <- t.test(data_notshort$cooprate[data_notshort$treatment=="base"&data_notshort$period==1], 
                             data_notshort$cooprate[data_notshort$treatment=="weakR"&data_notshort$period ==1])
BaseVsWeakR_T_r1
# Wilcoxon rank sum test
BaseVsWeakR_W_r1 <- wilcox.test(data_notshort$cooprate[data_notshort$treatment=="base"&data_notshort$period==1], 
                                data_notshort$cooprate[data_notshort$treatment=="weakR"&data_notshort$period ==1])
BaseVsWeakR_W_r1

## base vs. strongR
# first round
# t-test
BaseVsStrongR_T_r1 <- t.test(data_notshort$cooprate[data_notshort$treatment=="base"&data_notshort$period==1], 
                             data_notshort$cooprate[data_notshort$treatment=="strongR"&data_notshort$period ==1])
BaseVsStrongR_T_r1
# Wilcoxon rank sum test
BaseVsStrongR_W_r1 <- wilcox.test(data_notshort$cooprate[data_notshort$treatment=="base"&data_notshort$period==1], 
                                  data_notshort$cooprate[data_notshort$treatment=="strongR"&data_notshort$period ==1])
BaseVsStrongR_W_r1

## weakR vs. strongR
# first round
# t-test
WeakVsStrongR_T_r1 <- t.test(data_notshort$cooprate[data_notshort$treatment=="weakR"&data_notshort$period==1], 
                             data_notshort$cooprate[data_notshort$treatment=="strongR"&data_notshort$period ==1])
WeakVsStrongR_T_r1
# Wilcoxon rank sum test
WeakVsStrongR_W_r1 <- wilcox.test(data_notshort$cooprate[data_notshort$treatment=="weakR"&data_notshort$period==1], 
                                  data_notshort$cooprate[data_notshort$treatment=="strongR"&data_notshort$period ==1])
WeakVsStrongR_W_r1

##
# Result 1: 
# At the beginning of each phase, cooperation rates in weakR and strongR are significantly lower
# than those in base. The difference in cooperation rates between weakR and strongR
# is statistically indistinguishable from zero
##

## Regressions

# Data preparation
data_short_phase$treatment.f <- factor(data_short_phase$treatment)
data_short_phase$period.f <- factor(data_short_phase$period)
data_short_phase$group.f <- factor(data_short_phase$group_total)

data_short$treatment.f <- factor(data_short$treatment)
data_short$group.f <- factor(data_short$group_total)

###
# Table 2: Ratchet effects
###

## Model 1 
# OLS
# with homoscedastic standard errors
reg_1 <- lm(cooprate*100 ~ treatment.f,
                 data = data_short)
summary(reg_1)
# with heteroskedasticity robust standard error
r1 <- coeftest(reg_1, vcov = vcovHC(reg_1, type = "HC0"))
# Robustness check: Tobit
#reg_all_1C_T <- tobit(cooprate*100 ~ treatment.f,
#                      left = -0.20*100, right = 1.13*100,
#                      data = data_short)
# with heteroskedasticity robust standard error
#r1_T <- coeftest(reg_all_1C_T, vcov = vcovHC(reg_all_1C, type = "HC0"))

## Model 2
# OLS
# with homoscedastic standard errors
reg_2 <- lm(cooprate*100 ~ treatment.f + period.f,
                 data = data_short_phase)
summary(reg_2)
# with heteroskedasticity robust standard error
r2 <- coeftest(reg_2, vcov = vcovHC(reg_2, type = "HC0"))
# with standard errors clustered at the group level
v <- cluster.vcov(reg_2, data_short_phase$group.f)
r3<- coeftest(reg_2, v)
# Tobit
#reg_all_2C_T <- tobit(cooprate*100 ~ treatment.f + period.f,
#                      left = -0.20*100, right = 1.13*100,
#                      data = data_short_phase)
# with heteroskedasticity robust standard error
#r2_T <- coeftest(reg_all_2C_T, vcov = vcovHC(reg_all_2C, type = "HC0"))
# with standard errors clustered at the group level
#r3_T <- coeftest(reg_all_2C_T, vcov. = vcovCL(reg_all_2C_T, 
#                                              cluster = data_short_phase$group.f, 
#                                              type = "HC0"))

## Model 3
## OLS
# with homoscedastic standard errors
reg_3 <- lm(cooprate*100 ~ treatment.f * period.f,
                 data = data_short_phase)
summary(reg_3)
# with heteroskedasticity robust standard error
r4 <- coeftest(reg_3, vcov = vcovHC(reg_3, type = "HC0"))
# with standard errors clustered at the group level
v <- cluster.vcov(reg_3, data_short_phase$group.f)
r5 <- coeftest(reg_3, v)
## Tobit
#reg_all_3C_T <- tobit(cooprate*100 ~ treatment.f * period.f,
#                      left = -0.20*100, right = 1.13*100,
#                      data = data_short_phase)
#summary(reg_all_3C_T)
# with heteroskedasticity robust standard error
#r5_T <- coeftest(reg_all_3C_T, vcov = vcovHC(reg_all_3C, type = "HC0"))
# with standard errors clustered at the group level
#r6_T <- coeftest(reg_all_3C_T, vcov. = vcovCL(reg_all_3C_T, 
#                                              cluster = data_short_phase$group.f, 
#                                              type = "HC0"))


# Tables

# Out of space - Table 2. Ratchet effect
stargazer(reg_1, r1,
          reg_2, r2,
          reg_3, r4,
          type = "html", out = paste("Tab2_", st, ".doc",sep = ""),
          title = "Table 2. Ratchet effects", align = TRUE, no.space = TRUE,
          omit.stat = c("f","ser"),
          dep.var.caption = "Dependent Variable: Cooperation rate (in percent)",
          covariate.labels = c("strongR", "weakR",
                               "period 2", "period 3", "period 4", "period 5",
                               "strongR x period 2", "weakR x period 2", 
                               "strongR x period 3", "weakR x period 3",
                               "strongR x period 4", "weakR x period 4", 
                               "strongR x period 5", "weakR x period 5"))
     
# Out of space - Table A3.1. Ratchet Effects: Robustness checks
stargazer(reg_2, r3,
          reg_3, r5,
          type = "html", out = paste("TabA3.1_", st, ".doc",sep = ""),
          title = "Table A3.1. Ratchet effects: Robustness checks", align = TRUE, no.space = TRUE,
          omit.stat = c("f","ser"),
          dep.var.caption = "Dependent Variable: Cooperation rate (in percent)",
          covariate.labels = c("strongR", "weakR",
                               "period 2", "period 3", "period 4", "period 5",
                               "strongR x period 2", "weakR x period 2", 
                               "strongR x period 3", "weakR x period 3",
                               "strongR x period 4", "weakR x period 4", 
                               "strongR x period 5", "weakR x period 5"))

###
# Fig 3: Trends in cooperation rates per group by period and treatment
###

## data preparation
## base
# Tests
reg_base_2 <- linearHypothesis(reg_3, "period.f2 = 0", verbose = TRUE, vcov = vcovHC(reg_3, type = "HC0")) 
reg_base_3 <- linearHypothesis(reg_3, "period.f3 = 0", verbose = TRUE, vcov = vcovHC(reg_3, type = "HC0"))
reg_base_4 <- linearHypothesis(reg_3, "period.f4 = 0", verbose = TRUE, vcov = vcovHC(reg_3, type = "HC0"))
reg_base_5 <- linearHypothesis(reg_3, "period.f5 = 0", verbose = TRUE, vcov = vcovHC(reg_3, type = "HC0"))
# Coefficients
t_base_2 <- attr(reg_base_2, "value")[1]
t_base_3 <- attr(reg_base_3, "value")[1]
t_base_4 <- attr(reg_base_4, "value")[1]
t_base_5 <- attr(reg_base_5, "value")[1]
# SEs
SE_base_2 <- sqrt(attr(reg_base_2, "vcov")[1])
SE_base_3 <- sqrt(attr(reg_base_3, "vcov")[1])
SE_base_4 <- sqrt(attr(reg_base_4, "vcov")[1])
SE_base_5 <- sqrt(attr(reg_base_5, "vcov")[1])
# CIs
CI_base_2 <- qnorm(1-(alpha/2))*SE_base_2
CI_base_3 <- qnorm(1-(alpha/2))*SE_base_3
CI_base_4 <- qnorm(1-(alpha/2))*SE_base_4
CI_base_5 <- qnorm(1-(alpha/2))*SE_base_5

## weakR
# Test
reg_weakR_2 <- linearHypothesis(reg_3, "period.f2 + treatment.fweakR:period.f2= 0", verbose = TRUE, vcov = vcovHC(reg_3, type = "HC0"))
reg_weakR_3 <- linearHypothesis(reg_3, "period.f3 + treatment.fweakR:period.f3= 0", verbose = TRUE, vcov = vcovHC(reg_3, type = "HC0"))
reg_weakR_4 <- linearHypothesis(reg_3, "period.f4 + treatment.fweakR:period.f4= 0", verbose = TRUE, vcov = vcovHC(reg_3, type = "HC0"))
reg_weakR_5 <- linearHypothesis(reg_3, "period.f5 + treatment.fweakR:period.f5= 0", verbose = TRUE, vcov = vcovHC(reg_3, type = "HC0"))
# Coefficients
t_weakR_2 <- attr(reg_weakR_2, "value")[1]
t_weakR_3 <- attr(reg_weakR_3, "value")[1]
t_weakR_4 <- attr(reg_weakR_4, "value")[1]
t_weakR_5 <- attr(reg_weakR_5, "value")[1]
# SEs
SE_weakR_2 <- sqrt(attr(reg_weakR_2, "vcov")[1])
SE_weakR_3 <- sqrt(attr(reg_weakR_3, "vcov")[1])
SE_weakR_4 <- sqrt(attr(reg_weakR_4, "vcov")[1])
SE_weakR_5 <- sqrt(attr(reg_weakR_5, "vcov")[1])
# CIs
CI_weakR_2 <- qnorm(1-(alpha/2))*SE_weakR_2
CI_weakR_3 <- qnorm(1-(alpha/2))*SE_weakR_3
CI_weakR_4 <- qnorm(1-(alpha/2))*SE_weakR_4
CI_weakR_5 <- qnorm(1-(alpha/2))*SE_weakR_5

## strongR
# Tests
reg_strongR_2 <- linearHypothesis(reg_3, "period.f2 + treatment.fstrongR:period.f2= 0", verbose = TRUE, vcov = vcovHC(reg_3, type = "HC0"))
reg_strongR_3 <- linearHypothesis(reg_3, "period.f3 + treatment.fstrongR:period.f3= 0", verbose = TRUE, vcov = vcovHC(reg_3, type = "HC0"))
reg_strongR_4 <- linearHypothesis(reg_3, "period.f4 + treatment.fstrongR:period.f4= 0", verbose = TRUE, vcov = vcovHC(reg_3, type = "HC0"))
reg_strongR_5 <- linearHypothesis(reg_3, "period.f5 + treatment.fstrongR:period.f5= 0", verbose = TRUE, vcov = vcovHC(reg_3, type = "HC0"))
# Coefficients
t_strongR_2 <- attr(reg_strongR_2, "value")[1]
t_strongR_3 <- attr(reg_strongR_3, "value")[1]
t_strongR_4 <- attr(reg_strongR_4, "value")[1]
t_strongR_5 <- attr(reg_strongR_5, "value")[1]
# SEs
SE_strongR_2 <- sqrt(attr(reg_strongR_2, "vcov")[1])
SE_strongR_3 <- sqrt(attr(reg_strongR_3, "vcov")[1])
SE_strongR_4 <- sqrt(attr(reg_strongR_4, "vcov")[1])
SE_strongR_5 <- sqrt(attr(reg_strongR_5, "vcov")[1])
# CIs
CI_strongR_2 <- qnorm(1-(alpha/2))*SE_strongR_2
CI_strongR_3 <- qnorm(1-(alpha/2))*SE_strongR_3
CI_strongR_4 <- qnorm(1-(alpha/2))*SE_strongR_4
CI_strongR_5 <- qnorm(1-(alpha/2))*SE_strongR_5

# Data
trend <- 2:5
t_base <- c(t_base_2, t_base_3, t_base_4, t_base_5)
CI_base <- c(CI_base_2, CI_base_3, CI_base_4, CI_base_5)
t_weakR <- c(t_weakR_2, t_weakR_3, t_weakR_4, t_weakR_5)
CI_weakR <- c(CI_weakR_2, CI_weakR_3, CI_weakR_4, CI_weakR_5)
t_strongR <- c(t_strongR_2, t_strongR_3, t_strongR_4, t_strongR_5)
CI_strongR <- c(CI_strongR_2, CI_strongR_3, CI_strongR_4, CI_strongR_5)
data_trend <- data.frame(trend, 
                         t_base,
                         CI_base,
                         t_weakR,
                         CI_weakR,
                         t_strongR,
                         CI_strongR) 

Treatments <- c("base", "base", "base", "base", "weakR","weakR","weakR","weakR", "strongR", "strongR", "strongR","strongR")
Colors <- c(cols[1], cols[3], cols[5])
Rounds <- as.vector(cbind(trend,trend,trend))
Trends <- as.vector(cbind(t_base, t_weakR, t_strongR))
CIs <- as.vector(cbind(CI_base,CI_weakR,CI_strongR))
data_trend_v1 <- data.frame(Rounds,
                            Trends,
                            CIs,
                            Treatments)

data_trend_v1$Treatments <- factor( data_trend_v1$Treatments, 
                                    levels = unique( as.character(data_trend_v1$Treatments)))
# Fig.3

fig3 <- ggplot(data_trend_v1, aes(x=as.factor(Rounds), y=Trends, fill=Treatments)) +
  theme_minimal() +
  #ylim(-35,35) +
  scale_y_continuous(breaks = c(-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  scale_fill_manual(values=Colors) +
  geom_errorbar(aes(ymin=(Trends-CIs), ymax=(Trends+CIs)), width=0.2, position=position_dodge(0.9)) +
  xlab("Periods") + ylab("Differences in cooperation rates \n (in percentage points)") +
  theme(axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        legend.position = "bottom", legend.text = element_text(face = "italic", size = 12),
        legend.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(color = "black", size = 12, face = "bold"),
        axis.text.y = element_text(color = "black", size = 12, face = "bold"))

# out of space
pdf(file = paste("Fig3_", st, ".pdf",sep = ""),
    width = 11.00, 
    height = 7.00) 

fig3

dev.off()

# Excursus I: Average trends per treatment --------------------------------

# base
# avg. trend: in cooperation rate
t_base_avg <- t_base_5/4
# avg. trend: in absolute terms
t_base_avg_abs <- t_base_avg*0.01*(90-15)

# weakR
# avg. tend: in cooperation rate
t_weakR_avg <- t_weakR_5/4
# avg. trend: in absolute terms
t_weakR_avg_abs <- t_weakR_avg*0.01*(90-15)

# strongR
# avg. tend: in cooperation rate
t_strongR_avg <- t_strongR_5/4
# avg. trend: in absolute terms
t_strongR_avg_abs <- t_strongR_avg*0.01*(90-15)

# test weakR vs. strongR 
# in cooperation rates
reg_E_1 <- lm(cooprate*100 ~ treatment.f * period.f,
                 data = data_short_phase)
linearHypothesis(reg_E_1, 
                       "treatment.fstrongR:period.f5 = treatment.fweakR:period.f5", 
                       verbose = TRUE, vcov = vcovHC(reg_E_1, type = "HC0"))
#reg_all_3A <- lm(cooprate*0.25*(90-15) ~ treatment.f * period.f,
#                 data = data_short_phase)
#linearHypothesis(reg_all_3A, 
#                       "treatment.fstrongR:period.f5 = treatment.fweakR:period.f5", 
#                       verbose = TRUE, vcov = vcovHC(reg_all_3C, type = "HC0"))

# Excursus I: END ---------------------------------------------------------


##
# Result 2:
# In base, cooperation rates show a significant downward trend over time. 
# Cooperation rates in weakR and strongR, in contrast, increase significantly over time. 
##

### Tests
# Differences across treatments: On average 
###

## base vs. weakR
# t-test
BaseVsWeakR_T <- t.test(data_short$cooprate[data_short$treatment=="base"], 
                        data_short$cooprate[data_short$treatment=="weakR"])
BaseVsWeakR_T
# Wilcoxon rank sum test
BaseVsWeakR_W <- wilcox.test(data_short$cooprate[data_short$treatment=="base"], 
                             data_short$cooprate[data_short$treatment=="weakR"])
BaseVsWeakR_W

## Base vs. strongR
# t-test
BaseVsStrongR_T <- t.test(data_short$cooprate[data_short$treatment=="base"], 
                        data_short$cooprate[data_short$treatment=="strongR"])
BaseVsStrongR_T
# Wilcoxon rank sum test
BaseVsStrongR_W <- wilcox.test(data_short$cooprate[data_short$treatment=="base"], 
                             data_short$cooprate[data_short$treatment=="strongR"])
BaseVsStrongR_W

## weakR vs. strongR
# t-test
WeakRVsStrongR_T <- t.test(data_short$cooprate[data_short$treatment=="weakR"], 
                          data_short$cooprate[data_short$treatment=="strongR"])
WeakRVsStrongR_T
# Wilcoxon rank sum test
WeakRVsStrongR_W <- wilcox.test(data_short$cooprate[data_short$treatment=="weakR"], 
                               data_short$cooprate[data_short$treatment=="strongR"])
WeakRVsStrongR_W

##
# Result 3. On average, cooperation rates in weakR and strongR are significantly below the
# cooperation rates in base. The difference in cooperation rates between weakR and strongR 
# is statistically indistinguishable from zero.
##

# Excursus II - Cooperation rates at the beginning from phase-to-phase ----
# baseline
# data preparation
group <- seq(1,length(data_short_period$cooprate[data_short_period$treatment == "base" &
                                                 data_short_period$period == 1 &
                                                 data_short_period$phase == 1]), 1)
# Phase I
First_1 <- data_short_period$cooprate[data_short_period$treatment == "base" & 
                                                    data_short_period$period == 1 & 
                                                    data_short_period$phase == 1]
# Phase II
First_2 <- data_short_period$cooprate[data_short_period$treatment == "base" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 2]
  
# Phase III
First_3 <- data_short_period$cooprate[data_short_period$treatment == "base" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 3]

# Phase IV
First_4 <- data_short_period$cooprate[data_short_period$treatment == "base" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 4]

# Phase V
First_5 <- data_short_period$cooprate[data_short_period$treatment == "base" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 5]
# data set

d <- data.frame(group, First_1, First_2, First_3, First_4, First_5)

# summary
mean(d$First_1)
mean(d$First_2)
mean(d$First_3)
mean(d$First_4)
mean(d$First_5)

# Tests
t.test(d$First_1, d$First_2, paired = TRUE)
wilcox.test(d$First_1, d$First_2, paired = TRUE)
t.test(d$First_1, d$First_3, paired = TRUE)
wilcox.test(d$First_1, d$First_3, paired = TRUE)
t.test(d$First_1, d$First_4, paired = TRUE)
wilcox.test(d$First_1, d$First_4, paired = TRUE)
t.test(d$First_1, d$First_5, paired = TRUE)
wilcox.test(d$First_1, d$First_5, paired = TRUE)

# weakR
# data
group <- seq(1,length(data_short_period$cooprate[data_short_period$treatment == "weakR" &
                                                   data_short_period$period == 1 &
                                                   data_short_period$phase == 1]), 1)
# Phase I
First_1 <- data_short_period$cooprate[data_short_period$treatment == "weakR" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 1]
# Phase II
First_2 <- data_short_period$cooprate[data_short_period$treatment == "weakR" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 2]

# Phase III
First_3 <- data_short_period$cooprate[data_short_period$treatment == "weakR" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 3]

# Phase IV
First_4 <- data_short_period$cooprate[data_short_period$treatment == "weakR" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 4]

# Phase V
First_5 <- data_short_period$cooprate[data_short_period$treatment == "weakR" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 5]
# data set

d <- data.frame(group, First_1, First_2, First_3, First_4, First_5)

# summary
mean(d$First_1)
mean(d$First_2)
mean(d$First_3)
mean(d$First_4)
mean(d$First_5)

# Tests
t.test(d$First_1, d$First_2, paired = TRUE)
wilcox.test(d$First_1, d$First_2, paired = TRUE)
t.test(d$First_1, d$First_3, paired = TRUE)
wilcox.test(d$First_1, d$First_3, paired = TRUE)
t.test(d$First_1, d$First_4, paired = TRUE)
wilcox.test(d$First_1, d$First_4, paired = TRUE)
t.test(d$First_1, d$First_5, paired = TRUE)
wilcox.test(d$First_1, d$First_5, paired = TRUE)

# strongR
# data
group <- seq(1,length(data_short_period$cooprate[data_short_period$treatment == "strongR" &
                                                   data_short_period$period == 1 &
                                                   data_short_period$phase == 1]), 1)
# Phase I
First_1 <- data_short_period$cooprate[data_short_period$treatment == "strongR" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 1]
# Phase II
First_2 <- data_short_period$cooprate[data_short_period$treatment == "strongR" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 2]

# Phase III
First_3 <- data_short_period$cooprate[data_short_period$treatment == "strongR" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 3]

# Phase IV
First_4 <- data_short_period$cooprate[data_short_period$treatment == "strongR" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 4]

# Phase V
First_5 <- data_short_period$cooprate[data_short_period$treatment == "strongR" & 
                                        data_short_period$period == 1 & 
                                        data_short_period$phase == 5]
# data set

d <- data.frame(group, First_1, First_2, First_3, First_4, First_5)

# summary
mean(d$First_1)
mean(d$First_2)
mean(d$First_3)
mean(d$First_4)
mean(d$First_5)

# Tests
t.test(d$First_1, d$First_2, paired = TRUE)
wilcox.test(d$First_1, d$First_2, paired = TRUE)
t.test(d$First_1, d$First_3, paired = TRUE)
wilcox.test(d$First_1, d$First_3, paired = TRUE)
t.test(d$First_1, d$First_4, paired = TRUE)
wilcox.test(d$First_1, d$First_4, paired = TRUE)
t.test(d$First_1, d$First_5, paired = TRUE)
wilcox.test(d$First_1, d$First_5, paired = TRUE)

# NEW R&R3: check contributions of other: Zerlegung von 186 die mehr als 100 machen

# Excursus XXX ----

data_long$con_own <- data_long$q
# Sum per group
for (i in 1:5) 
  for (j in 11:204)
    for (k in 1:5){ data_long$con_group[data_long$period==i&data_long$group_total==j&data_long$phase == k] <- 
      sum(data_long$con_own[data_long$period==i&data_long$group_total==j&data_long$phase == k])
    }
# Sum others
data_long$con_other <- data_long$con_group-data_long$con_own
# Mean others
data_long$con_other_mean <- data_long$con_other/3
# own diff
data_long$diff_own <- abs(data_long$con_own-90)
# other diff
data_long$diff_other <- abs(data_long$con_other_mean-90)
# Problems
data_long$problems <- ifelse(data_long$con_own>90&(data_long$diff_own>data_long$diff_other), 1, 0)
table(data_long$problems)

# Excursus xxx - END -----


# Excursus II: END ---------------------------------------------------------

###
# Section 3.2  - What drives the ratchet effect?  -------------------------
###

## Data & data preparation ------------------------------------------------
# Data ----
data_long_con<- data_long # Just a new name

# Data preparation ----
# Variables & labels
data_long_con$group <- ifelse(data_long_con$subject <= 4, 1, 
                              ifelse(data_long_con$subject <= 8, 2, 
                                     ifelse(data_long_con$subject <= 12, 3, 
                                            ifelse(data_long_con$subject <= 16, 4, 
                                                   ifelse(data_long_con$subject<= 20, 5, 6)))))

data_long_con$session_num <- ifelse(data_long_con$session == "S01", 1, 
                             ifelse(data_long_con$session == "S02", 2,
                             ifelse(data_long_con$session == "S03", 3,
                             ifelse(data_long_con$session == "S04", 4,
                             ifelse(data_long_con$session == "S05", 5,
                             ifelse(data_long_con$session == "S06", 6,
                             ifelse(data_long_con$session == "S07", 7,
                             ifelse(data_long_con$session == "S08", 8,
                             ifelse(data_long_con$session == "S09", 9,
                             ifelse(data_long_con$session == "S10", 10,
                             ifelse(data_long_con$session == "S11", 11,
                             ifelse(data_long_con$session == "S12", 12,
                             ifelse(data_long_con$session == "S13", 13,
                             ifelse(data_long_con$session == "S14", 14,
                             ifelse(data_long_con$session == "S15", 15,
                             ifelse(data_long_con$session == "S16", 16,
                             ifelse(data_long_con$session == "S17", 17,
                             ifelse(data_long_con$session == "S18", 18,
                             ifelse(data_long_con$session == "S19", 19,
                             ifelse(data_long_con$session == "S20", 20, 0))))))))))))))))))))


# Define running group
data_long_con$group_total <- as.numeric(paste(data_long_con$session_num, data_long_con$group, sep = ""))

# Define running subject
data_long_con$subject_total <- as.numeric(paste(data_long_con$session_num, data_long_con$subject, sep = ""))

## Aggregate data
# Aggreagte contribution data at tratment & group & phase level by period
data_notshort_con <- ddply(data_long_con, .(treatment,group_total,period), summarize,  q=mean(q), cooprate=mean(cooprate))

# Aggregate contribution data at game level by treatment & group & subject & period
data_notsoshort_con <- ddply(data_long_con, .(treatment,group_total,period, subject), summarize,  q=mean(q), cooprate=mean(cooprate))

# Aggregate contribution data at game level by treatment & group level
data_short_con <- ddply(data_long_con, .(treatment,group_total), summarize,  q=mean(q), cooprate=mean(cooprate))

# Aggregated contribution data at phase level by group level 
data_phase_con <- ddply(data_long_con, .(treatment,group_total, phase), summarize,  q_mean=mean(q), cooprate=mean(cooprate), q_sum=sum(q))

# Aggregated contribution data at game level by group & phase & period
data_short_period_con <- ddply(data_long_con, .(treatment,group_total, phase, period), summarize,  q=mean(q), cooprate=mean(cooprate))

# Aggregated contribution data at game level by treatment & period 
data_short_period_new_con <- ddply(data_long_con, .(treatment,group_total, period), summarize,  q=mean(q), cooprate=mean(cooprate))

# Variables
data_long_con$treatment.f <- factor(data_long_con$treatment)
data_short_period_con$treatment.f <- factor(data_short_period_con$treatment)
data_short_period_new_con$treatment.f <- factor(data_short_period_new_con$treatment)
data_short_con$treatment.f <- factor(data_short_con$treatment)
data_long_con$period.f <-factor(data_long_con$period) 
data_short_period_con$period.f <- factor(data_short_period_con$period)
data_short_period_new_con$period.f <- factor(data_short_period_new_con$period)
data_long_con$phase.f <- factor(data_long_con$phase)
data_short_period_con$phase.f <- factor(data_short_period_con$phase)

## Aggregate contribution data
# Mean own contribution per phase
for (i in 1:25) # i : subject; j : group_total; k phase
  for (j in 11:204)
    for (k in 1:5){ data_long_con$mean_own[data_long_con$subject==i&data_long_con$group_total==j&data_long_con$phase == k] <- 
      mean(data_long_con$q[data_long_con$subject==i&data_long_con$group_total==j&data_long_con$phase == k])
    }
# plausibility: check range
summary(data_long_con$mean_own)

# Sum own contribution per phase
for (i in 1:25) # i : subject; j : group_total; k phase
  for (j in 11:204)
    for (k in 1:5){ data_long_con$sum_own[data_long_con$subject==i&data_long_con$group_total==j&data_long_con$phase == k] <- 
      sum(data_long_con$q[data_long_con$subject==i&data_long_con$group_total==j&data_long_con$phase == k])
    }
# plausibility: check range
summary(data_long_con$sum_own)

# Excursus III -  Testing cumulated cont. level per participant -----------
## base vs. weakR
# t-test
BaseVsWeakR_T <- t.test(data_long_con$sum_own[data_long_con$period==5&data_long_con$treatment=="base"], 
                           data_long_con$sum_own[data_long_con$period==5&data_long_con$treatment=="weakR"])
BaseVsWeakR_T
# Wilcoxon rank sum test
BaseVsWeakR_W <- wilcox.test(data_long_con$sum_own[data_long_con$period==5&data_long_con$treatment=="base"], 
                             data_long_con$sum_own[data_long_con$period==5&data_long_con$treatment=="weakR"])
BaseVsWeakR_W

## base vs. strongR
# t-test
BaseVsWeakR_T <- t.test(data_long_con$sum_own[data_long_con$period==5&data_long_con$treatment=="base"], 
                        data_long_con$sum_own[data_long_con$period==5&data_long_con$treatment=="strongR"])
BaseVsWeakR_T
# Wilcoxon rank sum test
BaseVsWeakR_W <- wilcox.test(data_long_con$sum_own[data_long_con$period==5&data_long_con$treatment=="base"], 
                             data_long_con$sum_own[data_long_con$period==5&data_long_con$treatment=="strongR"])
BaseVsWeakR_W
# Excursus III: END -------------------------------------------------------

# Mean per group & phase
for (i in 11:204) # i : group_total; j : phase
  for (j in 1:5){  
    data_long_con$mean_group[data_long_con$group_total==i&data_long_con$phase == j] <- 
      mean(data_long_con$q[data_long_con$group_total==i&data_long_con$phase == j])
  }
# plausability: check range
summary(data_long_con$mean_group)

# Mean per group & phase & period
for (i in 1:5) # i : period; j : group_total; k : phase
  for (j in 11:204) 
    for (k in 1:5) {  
      data_long_con$mean_all[data_long_con$period==i&data_long_con$group_total==j&data_long_con$phase == k] <- 
        mean(data_long_con$q[data_long_con$period==i&data_long_con$group_total==j&data_long_con$phase == k])
    }
# plausability: check range
summary(data_long_con$mean_all)

# mean per group phase 1
#for (i in 11:204) # i : group_total; j : phase
#  for (j in 1:5) {  
#    data_long_con$mean_group[data_long_con$group_total==i&data_long_con$phase == j] <- 
#      mean(data_long_con$q[data_long_con$group_total==i&data_long_con$phase == j])
#  }

# sum per group & phase
for (i in 11:204) # i : group_total; j : phase
  for (j in 1:5) {  
   data_long_con$sum_group[data_long_con$group_total==i&data_long_con$phase == j] <- 
      sum(data_long_con$q[data_long_con$group_total==i&data_long_con$phase == j])
  }
# plausability: check range
summary(data_long_con$sum_group)

# Mean own contribution per phase
#for (i in 1:25) # i : subject; j : group_total; k phase
#  for (j in 11:204)
#    for (k in 1:5){
#      data_long_con$mean_own[data_long_con$subject==i&data_long_con$group_total==j&data_long_con$phase == k] <- 
#        mean(data_long_con$q[data_long_con$subject==i&data_long_con$group_total==j&data_long_con$phase==k])
#    }
# plausability
#summary(data_long_con$mean_own)

# Sum own contribution per phase
#for (i in 1:25) # i : subject; j : group_total; k phase
#  for (j in 11:204)
#    for (k in 1:5){
#      data_long_con$sum_own[data_long_con$subject==i&data_long_con$group_total==j&data_long_con$phase == k] <- 
#        sum(data_long_con$q[data_long_con$subject==i&data_long_con$group_total==j&data_long_con$phase==k])
#    }
# plausability
#summary(data_long_con$sum_own)

# Sum rest contributions
data_long_con$sum_rest <- data_long_con$sum_group-data_long_con$sum_own
# plausability: check range
summary(data_long_con$sum_rest)
# Mean rest contributions
data_long_con$sum_rest_mean <- data_long_con$sum_rest/3
# plausability: check range
summary(data_long_con$sum_rest_mean)

# Transfer contributions into cooperation rates. IMPORTANT at the group level
cooprate <- function(x){
  ((x-75)/(450-75))
}

# Transfer avg. cont. others
data_long_con$sum_rest_mean <- cooprate(data_long_con$sum_rest/3)*100
# plausability: check range
summary(data_long_con$sum_rest_mean)
# Transfer own cont.
data_long_con$sum_own <- cooprate(data_long_con$sum_own)*100
# plausability: check range
summary(data_long_con$sum_own)

# Exploitation variables
# indicator
data_long_con$exploited_D <- ifelse(data_long_con$sum_rest_mean < data_long_con$sum_own, 1, 0)
# numerical
data_long_con$exploited <- data_long_con$sum_own - data_long_con$sum_rest_mean
# plausability: check range
summary(data_long_con$exploited)


# Regressions -------------------------------------------------------------
# data set & preparation
diff_T_con <- data_long_con$treatment[data_long_con$phase==1&data_long_con$period==1]
exp_ph1_con <- data_long_con$exploited[data_long_con$phase==1&data_long_con$period==1]
exp_ph1_D_con <- data_long_con$exploited_D[data_long_con$phase==1&data_long_con$period==1]
sum_1_con <- data_long_con$sum_own[data_long_con$phase==1&data_long_con$period==1]
sum_2_con <- data_long_con$sum_own[data_long_con$phase==2&data_long_con$period==1]
exp_ph2_con <- data_long_con$exploited[data_long_con$phase==2&data_long_con$period==1]
exp_ph2_D_con <- data_long_con$exploited_D[data_long_con$phase==2&data_long_con$period==1]
sum_3_con <- data_long_con$sum_own[data_long_con$phase==3&data_long_con$period==1]
exp_ph3_con <- data_long_con$exploited[data_long_con$phase==3&data_long_con$period==1]
exp_ph3_D_con <- data_long_con$exploited_D[data_long_con$phase==3&data_long_con$period==1]
sum_4_con <- data_long_con$sum_own[data_long_con$phase==4&data_long_con$period==1]
exp_ph4_con <- data_long_con$exploited[data_long_con$phase==4&data_long_con$period==1]
exp_ph4_D_con <- data_long_con$exploited_D[data_long_con$phase==4&data_long_con$period==1]
sum_5_con <- data_long_con$sum_own[data_long_con$phase==5&data_long_con$period==1]
exp_ph5_con <- data_long_con$exploited[data_long_con$phase==5&data_long_con$period==1]
exp_ph5_D_con <- data_long_con$exploited_D[data_long_con$phase==5&data_long_con$period==1]

# Control variables -------------------------------------------------------
# age
age <- data_long_con$Alter[data_long_con$phase==2&data_long_con$period==1]
# female
data_long_con$female[data_long_con$Geschlecht=="Männlich"] <- 0
data_long_con$female[data_long_con$Geschlecht=="Weiblich"] <- 1
data_long_con$female[data_long_con$Geschlecht==""] <- NA
female <- data_long_con$female[data_long_con$phase==2&data_long_con$period==1]
# trust
data_long_con$trust[data_long_con$Trust_2=="1 Die Menschen nutzen einen aus"] <- 1
data_long_con$trust[data_long_con$Trust_2=="2"] <- 2
data_long_con$trust[data_long_con$Trust_2=="3"] <- 3
data_long_con$trust[data_long_con$Trust_2=="4"] <- 4
data_long_con$trust[data_long_con$Trust_2=="5 5 Die Menschen verhalten sich fair"] <- 5
trust <- data_long_con$trus[data_long_con$phase==2&data_long_con$period==1]
# motives
# challenges
data_long_con$MC_1[data_long_con$LM_1=="1 Trifft gar nicht zu"] <- 1
data_long_con$MC_1[data_long_con$LM_1=="2"] <- 2
data_long_con$MC_1[data_long_con$LM_1=="3"] <- 3
data_long_con$MC_1[data_long_con$LM_1=="4"] <- 4
data_long_con$MC_1[data_long_con$LM_1=="5 Trifft voll und ganz zu"] <- 5
data_long_con$MC_2[data_long_con$LM_2=="1 Trifft gar nicht zu"] <- 1
data_long_con$MC_2[data_long_con$LM_2=="2"] <- 2
data_long_con$MC_2[data_long_con$LM_2=="3"] <- 3
data_long_con$MC_2[data_long_con$LM_2=="4"] <- 4
data_long_con$MC_2[data_long_con$LM_2=="5 Trifft voll und ganz zu"] <- 5
data_long_con$MC_3[data_long_con$LM_3=="1 Trifft gar nicht zu"] <- 1
data_long_con$MC_3[data_long_con$LM_3=="2"] <- 2
data_long_con$MC_3[data_long_con$LM_3=="3"] <- 3
data_long_con$MC_3[data_long_con$LM_3=="4"] <- 4
data_long_con$MC_3[data_long_con$LM_3=="5 Trifft voll und ganz zu"] <- 5
data_long_con$MC_4[data_long_con$LM_4=="1 Trifft gar nicht zu"] <- 1
data_long_con$MC_4[data_long_con$LM_4=="2"] <- 2
data_long_con$MC_4[data_long_con$LM_4=="3"] <- 3
data_long_con$MC_4[data_long_con$LM_4=="4"] <- 4
data_long_con$MC_4[data_long_con$LM_4=="5 Trifft voll und ganz zu"] <- 5
data_long_con$MC_5[data_long_con$LM_5=="1 Trifft gar nicht zu"] <- 1
data_long_con$MC_5[data_long_con$LM_5=="2"] <- 2
data_long_con$MC_5[data_long_con$LM_5=="3"] <- 3
data_long_con$MC_5[data_long_con$LM_5=="4"] <- 4
data_long_con$MC_5[data_long_con$LM_5=="5 Trifft voll und ganz zu"] <- 5
data_long_con$MC_6[data_long_con$LM_6=="1 Trifft gar nicht zu"] <- 5
data_long_con$MC_6[data_long_con$LM_6=="2"] <- 4
data_long_con$MC_6[data_long_con$LM_6=="3"] <- 3
data_long_con$MC_6[data_long_con$LM_6=="4"] <- 2
data_long_con$MC_6[data_long_con$LM_6=="5 Trifft voll und ganz zu"] <- 1
data_long_con$MC_7[data_long_con$LM_7=="1 Trifft gar nicht zu"] <- 5
data_long_con$MC_7[data_long_con$LM_7=="2"] <- 4
data_long_con$MC_7[data_long_con$LM_7=="3"] <- 3
data_long_con$MC_7[data_long_con$LM_7=="4"] <- 2
data_long_con$MC_7[data_long_con$LM_7=="5 Trifft voll und ganz zu"] <- 1
data_long_con$MC_8[data_long_con$LM_8=="1 Trifft gar nicht zu"] <- 5
data_long_con$MC_8[data_long_con$LM_8=="2"] <- 4
data_long_con$MC_8[data_long_con$LM_8=="3"] <- 3
data_long_con$MC_8[data_long_con$LM_8=="4"] <- 2
data_long_con$MC_8[data_long_con$LM_8=="5 Trifft voll und ganz zu"] <- 1
data_long_con$MC_9[data_long_con$LM_9=="1 Trifft gar nicht zu"] <- 5
data_long_con$MC_9[data_long_con$LM_9=="2"] <- 4
data_long_con$MC_9[data_long_con$LM_9=="3"] <- 3
data_long_con$MC_9[data_long_con$LM_9=="4"] <- 2
data_long_con$MC_9[data_long_con$LM_9=="5 Trifft voll und ganz zu"] <- 1
data_long_con$MC_10[data_long_con$LM_10=="1 Trifft gar nicht zu"] <- 5
data_long_con$MC_10[data_long_con$LM_10=="2"] <- 4
data_long_con$MC_10[data_long_con$LM_10=="3"] <- 3
data_long_con$MC_10[data_long_con$LM_10=="4"] <- 2
data_long_con$MC_10[data_long_con$LM_10=="5 Trifft voll und ganz zu"] <- 1
# index
data_long_con$MC_I <- (data_long_con$MC_1
                       + data_long_con$MC_2
                       + data_long_con$MC_3
                       + data_long_con$MC_4
                       + data_long_con$MC_5
                       + data_long_con$MC_6
                       + data_long_con$MC_7
                       + data_long_con$MC_8
                       + data_long_con$MC_9
                       + data_long_con$MC_10)/50
MC_I <- data_long_con$MC_I[data_long_con$phase==2&data_long_con$period==1]
# responsibility
data_long_con$MR_1[data_long_con$MM_1=="1 Trifft gar nicht zu"] <- 1
data_long_con$MR_1[data_long_con$MM_1=="2"] <- 2
data_long_con$MR_1[data_long_con$MM_1=="3"] <- 3
data_long_con$MR_1[data_long_con$MM_1=="4"] <- 4
data_long_con$MR_1[data_long_con$MM_1=="5 Trifft voll und ganz zu"] <- 5
data_long_con$MR_2[data_long_con$MM_2=="1 Trifft gar nicht zu"] <- 1
data_long_con$MR_2[data_long_con$MM_2=="2"] <- 2
data_long_con$MR_2[data_long_con$MM_2=="3"] <- 3
data_long_con$MR_2[data_long_con$MM_2=="4"] <- 4
data_long_con$MR_2[data_long_con$MM_2=="5 Trifft voll und ganz zu"] <- 5
data_long_con$MR_3[data_long_con$MM_3=="1 Trifft gar nicht zu"] <- 1
data_long_con$MR_3[data_long_con$MM_3=="2"] <- 2
data_long_con$MR_3[data_long_con$MM_3=="3"] <- 3
data_long_con$MR_3[data_long_con$MM_3=="4"] <- 4
data_long_con$MR_3[data_long_con$MM_3=="5 Trifft voll und ganz zu"] <- 5
data_long_con$MR_4[data_long_con$MM_4=="1 Trifft gar nicht zu"] <- 1
data_long_con$MR_4[data_long_con$MM_4=="2"] <- 2
data_long_con$MR_4[data_long_con$MM_4=="3"] <- 3
data_long_con$MR_4[data_long_con$MM_4=="4"] <- 4
data_long_con$MR_4[data_long_con$MM_4=="5 Trifft voll und ganz zu"] <- 5
data_long_con$MR_5[data_long_con$MM_5=="1 Trifft gar nicht zu"] <- 1
data_long_con$MR_5[data_long_con$MM_5=="2"] <- 2
data_long_con$MR_5[data_long_con$MM_5=="3"] <- 3
data_long_con$MR_5[data_long_con$MM_5=="4"] <- 4
data_long_con$MR_5[data_long_con$MM_5=="5 Trifft voll und ganz zu"] <- 5
data_long_con$MR_6[data_long_con$MM_6=="1 Trifft gar nicht zu"] <- 1
data_long_con$MR_6[data_long_con$MM_6=="2"] <- 2
data_long_con$MR_6[data_long_con$MM_6=="3"] <- 3
data_long_con$MR_6[data_long_con$MM_6=="4"] <- 4
data_long_con$MR_6[data_long_con$MM_6=="5 Trifft voll und ganz zu"] <- 5
data_long_con$MR_7[data_long_con$MM_1=="1 Trifft gar nicht zu"] <- 5
data_long_con$MR_7[data_long_con$MM_1=="2"] <- 4
data_long_con$MR_7[data_long_con$MM_1=="3"] <- 3
data_long_con$MR_7[data_long_con$MM_1=="4"] <- 2
data_long_con$MR_7[data_long_con$MM_1=="5 Trifft voll und ganz zu"] <- 1
# index
data_long_con$MR_I <- (data_long_con$MR_1
                       + data_long_con$MR_2
                       + data_long_con$MR_3
                       + data_long_con$MR_4
                       + data_long_con$MR_5
                       + data_long_con$MR_6
                       + data_long_con$MR_7)/35
MR_I <- data_long_con$MR_I[data_long_con$phase==2&data_long_con$period==1]


# Con't. Regressions - data preparation -----------------------------------

# variables
group <- data_long_con$group_total[data_long_con$phase==2&data_long_con$period==1]
subject <- data_long_con$subject_total[data_long_con$phase==2&data_long_con$period==1]

# data set
data_exploited_con <- data.frame(diff_T_con, 
                                 sum_1_con, sum_2_con, sum_3_con, sum_4_con, sum_5_con,
                                 exp_ph1_con, exp_ph2_con, exp_ph3_con, exp_ph4_con, exp_ph5_con,
                                 exp_ph1_D_con, exp_ph2_D_con, exp_ph3_D_con, exp_ph4_D_con, exp_ph5_D_con,
                                 age, female, trust, MC_I, MR_I, group, subject)

data_exploited_con$treatment.f <- factor(data_exploited_con$diff_T_con)
data_exploited_con$group.f <- factor(data_exploited_con$group)
data_exploited_con$subject.f <- factor(data_exploited_con$subject)

se <- function(x) sqrt(var(x)/length(x))

# data 
data_exploited_con_long <- melt(data_exploited_con, id = c("diff_T_con", 
                                                           "exp_ph1_con", "exp_ph2_con", "exp_ph3_con", "exp_ph4_con", "exp_ph5_con",
                                                           "exp_ph1_D_con", "exp_ph2_D_con", "exp_ph3_D_con", "exp_ph4_D_con", "exp_ph5_D_con",
                                                           "age", "female", "trust", "MC_I", "MR_I", "treatment.f", "group.f", "subject.f")
  )

data_exploited_con_long$exploitation_prev <- ifelse(data_exploited_con_long$variable == "sum_2_con", data_exploited_con_long$exp_ph1_D_con,
                                                    ifelse(data_exploited_con_long$variable == "sum_3_con", data_exploited_con_long$exp_ph2_D_con,
                                                           ifelse(data_exploited_con_long$variable == "sum_4_con", data_exploited_con_long$exp_ph3_D_con,
                                                                  ifelse(data_exploited_con_long$variable == "sum_5_con", data_exploited_con_long$exp_ph4_D_con, NA))))


data_exploited_con_long$exploitation <- ifelse(data_exploited_con_long$variable == "sum_1_con", data_exploited_con_long$exp_ph1_D_con, 
                                               ifelse(data_exploited_con_long$variable == "sum_2_con", data_exploited_con_long$exp_ph2_D_con,
                                                      ifelse(data_exploited_con_long$variable == "sum_3_con", data_exploited_con_long$exp_ph3_D_con,
                                                             ifelse(data_exploited_con_long$variable == "sum_4_con", data_exploited_con_long$exp_ph4_D_con, 
                                                                    ifelse(data_exploited_con_long$variable == "sum_5_con", data_exploited_con_long$exp_ph5_D_con, NA)))))

data_exploited_con_long$exploitation_num <- ifelse(data_exploited_con_long$variable == "sum_1_con", data_exploited_con_long$exp_ph1_con, 
                                                   ifelse(data_exploited_con_long$variable == "sum_2_con", data_exploited_con_long$exp_ph2_con,
                                                          ifelse(data_exploited_con_long$variable == "sum_3_con", data_exploited_con_long$exp_ph3_con,
                                                                 ifelse(data_exploited_con_long$variable == "sum_4_con", data_exploited_con_long$exp_ph4_con, 
                                                                        ifelse(data_exploited_con_long$variable == "sum_5_con", data_exploited_con_long$exp_ph5_con, NA)))))

data_exploited_con_long$sum_prev <- ifelse(data_exploited_con_long$variable == "sum_2_con", data_exploited_con_long$value[data_exploited_con_long$variable=="sum_1_con"],
                                           ifelse(data_exploited_con_long$variable == "sum_3_con", data_exploited_con_long$value[data_exploited_con_long$variable=="sum_2_con"],
                                                  ifelse(data_exploited_con_long$variable == "sum_4_con", data_exploited_con_long$value[data_exploited_con_long$variable=="sum_3_con"],
                                                         ifelse(data_exploited_con_long$variable == "sum_5_con", data_exploited_con_long$value[data_exploited_con_long$variable=="sum_4_con"], NA))))

data_exploited_con_long$exploitation_num_prev <- ifelse(data_exploited_con_long$variable == "sum_2_con", data_exploited_con_long$exp_ph1_con, 
                                                        ifelse(data_exploited_con_long$variable == "sum_3_con", data_exploited_con_long$exp_ph2_con,
                                                               ifelse(data_exploited_con_long$variable == "sum_4_con", data_exploited_con_long$exp_ph3_con,
                                                                      ifelse(data_exploited_con_long$variable == "sum_5_con", data_exploited_con_long$exp_ph4_con, NA))))

data_exploited_con_long <- subset(data_exploited_con_long, data_exploited_con_long$exploitation_prev>=0)


# Regressions -------------------------------------------------------------


###
# Table 3. Exploitation effect 
###

## Model 1
# OLS
Reg_Exp_1 <- lm(value ~ exploitation_num_prev + sum_prev, data_exploited_con_long)
# with heteroskedasticity robust standard error 
rE1 <- coeftest(Reg_Exp_1, vcov = vcovHC(Reg_Exp_1, type = "HC0"))
# Robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_Exp_1, data_exploited_con_long$group.f)
rE2 <- coeftest(Reg_Exp_1, v)
# Tobit
Reg_Exp_1_T <- tobit(value ~ exploitation_num_prev + sum_prev,
                     left = -20, right = 113.33,
                     data = data_exploited_con_long)
# with standard errors clustered at the group level
rE3 <- coeftest(Reg_Exp_1_T, vcov. = vcovCL(Reg_Exp_1_T, 
                                              cluster = data_exploited_con_long$group.f, 
                                              type = "HC0"))

# Model 2 - only those who have been exploited
# data (restricted)
data_exploited_con_long_ex <- subset(data_exploited_con_long, data_exploited_con_long$exploitation_prev>0)
# OLS
Reg_Exp_2 <- lm(value ~ exploitation_num_prev + sum_prev, data_exploited_con_long_ex)
# with heteroskedasticity robust standard error 
rE4 <- coeftest(Reg_Exp_2, vcov = vcovHC(Reg_Exp_2, type = "HC0"))
# Robustness
# with standard errors clustered at the group level
v_ex <- cluster.vcov(Reg_Exp_2, data_exploited_con_long_ex$group.f)
rE5 <- coeftest(Reg_Exp_2, v_ex)
# Tobit
Reg_Exp_2_T <- tobit(value ~ exploitation_num_prev + sum_prev,
                     left = -20, right = 113.33,
                     data = data_exploited_con_long_ex)
# with standard errors clustered at the group level
rE6 <- coeftest(Reg_Exp_2_T, vcov. = vcovCL(Reg_Exp_2_T, 
                                             cluster = data_exploited_con_long_ex$group.f, 
                                             type = "HC0"))

## Model 3
# OLS
Reg_Exp_3 <- lm(value ~ exploitation_prev + sum_prev, data_exploited_con_long)
# with heteroskedasticity robust standard error 
rE7 <- coeftest(Reg_Exp_3, vcov = vcovHC(Reg_Exp_3, type = "HC0"))
# Robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_Exp_3, data_exploited_con_long$group.f)
rE8 <- coeftest(Reg_Exp_3, v)
# Tobit
Reg_Exp_3_T <- tobit(value ~ exploitation_prev + sum_prev,
                     left = -20, right = 113.33,
                     data = data_exploited_con_long)
# with standard errors clustered at the group level
rE9 <- coeftest(Reg_Exp_3_T, vcov. = vcovCL(Reg_Exp_3_T, 
                                             cluster = data_exploited_con_long$group.f, 
                                             type = "HC0"))

## Model 4
# OLS
Reg_Exp_4 <- lm(value ~ exploitation_num_prev + treatment.f + sum_prev, data_exploited_con_long)
# with heteroskedasticity robust standard error 
rE10 <- coeftest(Reg_Exp_4, vcov = vcovHC(Reg_Exp_4, type = "HC0"))
# Robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_Exp_4, data_exploited_con_long$group.f)
rE11 <- coeftest(Reg_Exp_4, v)
# Tobit
Reg_Exp_4_T <- tobit(value ~ exploitation_num_prev + treatment.f + sum_prev,
                     left = -20, right = 113.33,
                     data = data_exploited_con_long)
# with standard errors clustered at the group level
rE12 <- coeftest(Reg_Exp_4_T, vcov. = vcovCL(Reg_Exp_4_T, 
                                             cluster = data_exploited_con_long$group.f, 
                                             type = "HC0"))

# Model 5 - only those who have been exploited
# OLS
Reg_Exp_5 <- lm(value ~ exploitation_num_prev + treatment.f + sum_prev, data_exploited_con_long_ex)
# with heteroskedasticity robust standard error 
rE13 <- coeftest(Reg_Exp_5, vcov = vcovHC(Reg_Exp_5, type = "HC0"))
# Robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_Exp_5, data_exploited_con_long_ex$group.f)
rE14 <- coeftest(Reg_Exp_5, v)
# Tobit
Reg_Exp_5_T <- tobit(value ~ exploitation_num_prev + treatment.f + sum_prev,
                     left = -20, right = 113.33,
                     data = data_exploited_con_long_ex)
# with standard errors clustered at the group level
rE15 <- coeftest(Reg_Exp_5_T, vcov. = vcovCL(Reg_Exp_5_T, 
                                              cluster = data_exploited_con_long_ex$group.f, 
                                              type = "HC0"))

## Model 6
# OLS
Reg_Exp_6 <- lm(value ~ exploitation_prev + treatment.f + sum_prev, data_exploited_con_long)
# with heteroskedasticity robust standard error 
rE16 <- coeftest(Reg_Exp_6, vcov = vcovHC(Reg_Exp_6, type = "HC0"))
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_Exp_6, data_exploited_con_long$group.f)
rE17 <- coeftest(Reg_Exp_6, v)
## Tobit
Reg_Exp_6_T <- tobit(value ~ exploitation_prev + treatment.f + sum_prev,
                     left = -20, right = 113.33,
                     data = data_exploited_con_long)
# with standard errors clustered at the group level
rE18 <- coeftest(Reg_Exp_6_T, vcov. = vcovCL(Reg_Exp_6_T, 
                                              cluster = data_exploited_con_long$group.f, 
                                              type = "HC0"))

# Model 7
Reg_Exp_7 <- lm(value ~ exploitation_num_prev * treatment.f + sum_prev, data_exploited_con_long)
# with heteroskedasticity robust standard error 
rE19 <- coeftest(Reg_Exp_7, vcov = vcovHC(Reg_Exp_7, type = "HC0"))
# Robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_Exp_7, data_exploited_con_long$group.f)
rE20 <- coeftest(Reg_Exp_7, v)
# Tobit
Reg_Exp_7_T <- tobit(value ~ exploitation_num_prev * treatment.f + sum_prev,
                     left = -20, right = 113.33,
                     data = data_exploited_con_long)
# with standard errors clustered at the group level
rE21 <- coeftest(Reg_Exp_7_T, vcov. = vcovCL(Reg_Exp_7_T, 
                                              cluster = data_exploited_con_long$group.f, 
                                              type = "HC0"))

# Model 8 - only those who have been exploited
# OLS
Reg_Exp_8 <- lm(value ~ exploitation_num_prev * treatment.f + sum_prev, data_exploited_con_long_ex)
# with heteroskedasticity robust standard error 
rE22 <- coeftest(Reg_Exp_8, vcov = vcovHC(Reg_Exp_8, type = "HC0"))
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_Exp_8, data_exploited_con_long_ex$group.f)
rE23 <- coeftest(Reg_Exp_8, v)
# Tobit
Reg_Exp_8_T <- tobit(value ~ exploitation_num_prev * treatment.f + sum_prev,
                     left = -20, right = 113.33,
                     data = data_exploited_con_long_ex)
# with standard errors clustered at the group level
rE24 <- coeftest(Reg_Exp_8_T, vcov. = vcovCL(Reg_Exp_8_T, 
                                              cluster = data_exploited_con_long_ex$group.f, 
                                              type = "HC0"))

## Model 9
# OLS
Reg_Exp_9 <- lm(value ~ exploitation_prev * treatment.f + sum_prev, data_exploited_con_long)
# with heteroskedasticity robust standard error 
rE25 <- coeftest(Reg_Exp_9, vcov = vcovHC(Reg_Exp_9, type = "HC0"))
# Robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_Exp_9, data_exploited_con_long$group.f)
rE26 <- coeftest(Reg_Exp_9, v)
# Tobit
Reg_Exp_9_T <- tobit(value ~ exploitation_prev * treatment.f + sum_prev,
                     left = -20, right = 113.13,
                     data = data_exploited_con_long)
# with standard errors clustered at the group level
rE27 <- coeftest(Reg_Exp_9_T, vcov. = vcovCL(Reg_Exp_9_T, 
                                              cluster = data_exploited_con_long$group.f, 
                                              type = "HC0"))

# Tables

# Out of space - Table 3. Exploitation effect
stargazer(Reg_Exp_1, rE1, # Model 1 - GAP
          Reg_Exp_2, rE4, # Model 2 - GAP only those who have been exploited
          Reg_Exp_3, rE7, # Model 3 - Exploitation
          Reg_Exp_4, rE10, # Model 4 - GAP
          Reg_Exp_5, rE13, # Model 5 - GAP only those who have been exploited  
          Reg_Exp_6, rE16, # Model 6 - Exploitation
          Reg_Exp_7, rE19, # Model 7 - GAP
          Reg_Exp_8, rE22, # Model 8 - GAP only those who have been exploited
          Reg_Exp_9, rE25, # Model 9 - Exploitation
          type = "html", out = paste("Tab3_", st, ".doc",sep = ""),
          title = "Table 3. Exploitation effect", align = TRUE, no.space = TRUE,
          omit.stat = c("f","ser"),
          dep.var.caption = "Dependent Variable: Cumulated cooperation rate per phase (in percent)",
          covariate.labels = c("gap", "exploitation",
                               "strongR", "weakR", 
                               "sum con Phase prev",
                               "gap x strongR", "gap x weakR",
                               "exploitation x strongR", "exploitation x weakR"))

# Out of space - Table A3.2. Exploitation effect: Robustness checks - OLS with standard errors cluster at the group level
stargazer(Reg_Exp_1, rE2, # Model 1 - GAP
          Reg_Exp_2, rE5, # Model 2 - GAP only those who have been exploited
          Reg_Exp_3, rE8, # Model 3 - Exploitation
          Reg_Exp_4, rE11, # Model 4 - GAP
          Reg_Exp_5, rE14, # Model 5 - Gap only those who have been exploited
          Reg_Exp_6, rE17, # Model 6 - Exploitation
          Reg_Exp_7, rE20, # Model 7 - GAP
          Reg_Exp_8, rE23, # Model 8 - GAP only those who have been exploited
          Reg_Exp_9, rE26, # Model 9 - Exploitation
          type = "html", out = paste("TabA3.2_OLS_", st, ".doc",sep = ""),
          title = "Table A3.2 Exploitation effect: Robustness checks - OLS", align = TRUE, no.space = TRUE,
          omit.stat = c("f","ser"),
          dep.var.caption = "Dependent Variable: Cumulated cooperation rate per phase (in percent)",
          covariate.labels = c("gap", "exploitation",
                               "strongR", "weakR", 
                               "sum con Phase prev",
                               "gap x strongR", "gap x weakR",
                               "exploitation x strongR", "exploitation x weakR", "Log(scale)"))

# Out of space - C'ont. Table A3.2. Exploitation effect: Robustness checks - Tobit
stargazer(Reg_Exp_1_T, rE3, # Model 1 - GAP
          Reg_Exp_2_T, rE6, # Model 2 - GAP only those who have been exploited
          Reg_Exp_3_T, rE9, # Model 3 - Exploitation
          Reg_Exp_4_T, rE12, # Model 4 - GAP
          Reg_Exp_5_T, rE15, # Model 5 - GAP only those who have been exploited
          Reg_Exp_6_T, rE18, # Model 6 - Exploitation
          Reg_Exp_7_T, rE21, # Model 7 - GAP
          Reg_Exp_8_T, rE24, # Model 8 - GAP only those who have been exploited
          Reg_Exp_9_T, rE27,  # Exploitation
          type = "html", out = paste("TabA3.2_Tobit_", st, ".doc",sep = ""),
          title = "Table A3.2 Exploitation effect: Robustness checks - Tobit", align = TRUE, no.space = TRUE,
          omit.stat = c("f","ser"),
          dep.var.caption = "Dependent Variable: Cumulated cooperation rate per phase (in percent)")


# Excursus IV - Exploitation effect with GAP^2 ----------------------------
# data preparation
data_exploited_con_long$GAP <- data_exploited_con_long$exploitation_num_prev
data_exploited_con_long$GAP2 <- data_exploited_con_long$exploitation_num_prev^2
# Regresssions
# plausibility
#Exp_GAP <- lm(value ~ GAP + sum_prev, data_exploited_con_long)
#summary(Exp_GAP)
#reg_r0 <- coeftest(Exp_GAP, vcov = vcovHC(Exp_GAP, type = "HC0"))
#reg_r0
# with standard errors clustered at the group level
#v <- cluster.vcov(Exp_GAP, data_exploited_con_long$group.f)
#reg_r02 <- coeftest(Exp_GAP, v)
#reg_r02
Exp_GAP2_1 <-lm(value ~ GAP + GAP2 +sum_prev, data_exploited_con_long)
summary(Exp_GAP2_1)
# with heteroskedasticity robust standard error 
rGAP2_1 <- coeftest(Exp_GAP2_1, vcov = vcovHC(Exp_GAP2_1, type = "HC0"))
rGAP2_1
# with standard errors clustered at the group level
v <- cluster.vcov(Exp_GAP2_1, data_exploited_con_long$group.f)
rGAP2_2 <- coeftest(Exp_GAP2_1, v)
rGAP2_2

#### NEW 
# Robustness
# data preparation
# data preparation
data_exploited_con_long_ex$GAP <- data_exploited_con_long_ex$exploitation_num_prev
data_exploited_con_long_ex$GAP2 <- data_exploited_con_long_ex$exploitation_num_prev^2
# only those who have been exploited
Exp_GAP2_2 <- lm(value ~ GAP + GAP2 +sum_prev, data_exploited_con_long_ex)
summary(Exp_GAP2_2)
# with heteroskedasticity robust standard error 
rGAP2_3 <- coeftest(Exp_GAP2_2, vcov = vcovHC(Exp_GAP2_2, type = "HC0"))
rGAP2_3
# with standard errors clustered at the group level
v <- cluster.vcov(Exp_GAP2_2, data_exploited_con_long_ex$group.f)
rGAP2_4 <- coeftest(Exp_GAP2_2, v)
rGAP2_4


# Table 
# out of space - Table R4
stargazer(Exp_GAP2_1, rGAP2_1, rGAP2_2, # all
          Exp_GAP2_2, rGAP2_3, rGAP2_4, # only those who have been exploited
          type = "html", out = paste("TabR4_", st, ".doc",sep = ""),
          title = "Table R4. GAP^2", align = TRUE, no.space = TRUE,
          omit.stat = c("f","ser"),
          dep.var.caption = "Dependent Variable: Cumulated cooperation rate per phase (in percent)")
# Excursus IV - END -------------------------------------------------------


### 
# Fig. 4. Exploitation effect by treatment
###

# Data
Reg_Exp_9 <- lm(value ~ exploitation_prev * treatment.f + sum_prev, data_exploited_con_long)

# baseline
reg_base_Exp <- linearHypothesis(Reg_Exp_9, "exploitation_prev = 0", verbose = TRUE, vcov = vcovHC(Reg_Exp_9, type = "HC0")) # vcov.= function(y) cluster.vcov(y, ~ group.f, df_correction = TRUE)
# weakR
reg_weakR_Exp <- linearHypothesis(Reg_Exp_9, "exploitation_prev + exploitation_prev:treatment.fweakR = 0", verbose = TRUE, vcov = vcovHC(Reg_Exp_9, type = "HC0")) # vcov.= function(y) cluster.vcov(y, ~ group.f, df_correction = TRUE)
# strongR
reg_strongR_Exp <- linearHypothesis(Reg_Exp_9, "exploitation_prev + exploitation_prev:treatment.fstrongR = 0", verbose = TRUE, vcov = vcovHC(Reg_Exp_9, type = "HC0")) # vcov.= function(y) cluster.vcov(y, ~ group.f, df_correction = TRUE) 

# Coefficients
reg_base_Exp_C <- attr(reg_base_Exp, "value")[1]
reg_weakR_Exp_C <- attr(reg_weakR_Exp, "value")[1]
reg_strongR_Exp_C <- attr(reg_strongR_Exp, "value")[1]
# SEs
reg_base_Exp_SE <- sqrt(attr(reg_base_Exp, "vcov")[1])
reg_weakR_Exp_SE <- sqrt(attr(reg_weakR_Exp, "vcov")[1])
reg_strongR_Exp_SE <- sqrt(attr(reg_strongR_Exp, "vcov")[1])
# CIs
alpha <- 0.05 # significance level
reg_base_Exp_CI <- qnorm(1-(alpha/2))*reg_base_Exp_SE
reg_weakR_Exp_CI <- qnorm(1-(alpha/2))*reg_weakR_Exp_SE
reg_strongR_Exp_CI <- qnorm(1-(alpha/2))*reg_strongR_Exp_SE

# layout
# Colors
cols <- wes_palette("FantasticFox1",5)

# Data file
Treatments <- c("base","weakR","strongR")
Colors <- c(cols[1], cols[3], cols[5])
Trend <- as.vector(cbind(reg_base_Exp_C, reg_weakR_Exp_C, reg_strongR_Exp_C))
CIs <- as.vector(cbind(reg_base_Exp_CI, reg_weakR_Exp_CI, reg_strongR_Exp_CI))
data_exp <- data.frame(Treatments, Trend, CIs)
data_exp$Treatments <- factor(data_exp$Treatments, c("base","weakR","strongR"))

# plot
exp_plot <- ggplot(data_exp, aes(x=as.factor(Treatments), y=(Trend), fill=Treatments)) +
  theme_minimal() +
  ylim(-15,5) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  scale_fill_manual(values=Colors) +
  geom_errorbar(aes(ymin=(Trend-CIs), ymax=(Trend+CIs)), width=0.2, position=position_dodge(0.9)) +
  xlab("") + ylab("Difference in cooperation rates \n (in percentage points)") +
  ggtitle("")+
  theme(axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        legend.position = "none",
        axis.text.x = element_text(color = "black", size = 12, face = "bold.italic"),
        axis.text.y = element_text(color = "black", size = 12, face = "bold")) 

# out of space
pdf(file = paste("Fig4_", st, ".pdf",sep = ""),
    width = 11.00, # The width of the plot in inches
    height = 7.00) # The height of the plot in inches

exp_plot

dev.off()

# Tests
# base
reg_base_Exp_C # value
reg_base_Exp_SE # se
reg_base_Exp # p value
# weakR
reg_weakR_Exp_C # value
reg_weakR_Exp_SE # se
reg_weakR_Exp # p value
# strongR
reg_strongR_Exp_C # value
reg_strongR_Exp_SE # se
reg_strongR_Exp # p value


# Exploitation effect by phase --------------------------------------------
# Regression
# Phase II
# Model 1 - OLS - Exploitation
Reg_II_1 <- lm(sum_2_con ~ exp_ph1_D_con * treatment.f + sum_1_con, data_exploited_con)
summary(Reg_II_1)
# with heteroskedasticity robust standard error 
rII1 <- coeftest(Reg_II_1, vcov = vcovHC(Reg_II_1, type = "HC0"))
rII1 
# Robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_II_1, data_exploited_con$group.f)
rII2 <- coeftest(Reg_II_1, v)
rII2
# Tobit
Reg_II_1_T <- tobit(sum_2_con ~ exp_ph1_D_con * treatment.f + sum_1_con,
                     left = -20, right = 113.33,
                     data = data_exploited_con)
summary(Reg_II_1_T)
# with standard errors clustered at the group level
rII3 <- coeftest(Reg_II_1_T, vcov. = vcovCL(Reg_II_1_T, 
                                              cluster = data_exploited_con$group.f, 
                                              type = "HC0"))
rII3

# Model 2 - OLS - GAP
Reg_II_2 <- lm(sum_2_con ~ exp_ph1_con * treatment.f + sum_1_con, data_exploited_con)
summary(Reg_II_2)
# with heteroskedasticity robust standard error 
rII4 <- coeftest(Reg_II_2, vcov = vcovHC(Reg_II_2, type = "HC0"))
rII4
# robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_II_2, data_exploited_con$group.f)
rII5 <- coeftest(Reg_II_2, v)
rII5
# Tobit
Reg_II_2_T <- tobit(sum_2_con ~ exp_ph1_con * treatment.f + sum_1_con,
                    left = -20, right = 113.33,
                    data = data_exploited_con)
rII6 <- coeftest(Reg_II_2_T, vcov. = vcovCL(Reg_II_2_T, 
                                            cluster = data_exploited_con$group.f, 
                                            type = "HC0"))

# Phase III
# Model 3 - OLS - Exploitation
Reg_III_1 <- lm(sum_3_con ~ exp_ph2_D_con * treatment.f + sum_2_con, data_exploited_con)
summary(Reg_III_1)
# with heteroskedasticity robust standard error 
rIII1 <- coeftest(Reg_III_1, vcov = vcovHC(Reg_III_1, type = "HC0"))
rIII1 
# robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_III_1, data_exploited_con$group.f)
rIII2 <- coeftest(Reg_III_1, v)
rIII2
# Tobit
Reg_III_1_T <- tobit(sum_3_con ~ exp_ph2_D_con * treatment.f + sum_2_con,
                    left = -20, right = 113.33,
                    data = data_exploited_con)
summary(Reg_III_1_T)
# with standard errors clustered at the group level
rIII3 <- coeftest(Reg_III_1_T, vcov. = vcovCL(Reg_III_1_T, 
                                            cluster = data_exploited_con$group.f, 
                                            type = "HC0"))
rIII3

# Model 4 - OLS - GAP
Reg_III_2 <- lm(sum_3_con ~ exp_ph2_con * treatment.f + sum_2_con, data_exploited_con)
summary(Reg_III_2)
# with heteroskedasticity robust standard error 
rIII4 <- coeftest(Reg_III_2, vcov = vcovHC(Reg_III_2, type = "HC0"))
rIII4
# robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_III_2, data_exploited_con$group.f)
rIII5 <- coeftest(Reg_III_2, v)
rIII5
# Tobit
Reg_III_2_T <- tobit(sum_3_con ~ exp_ph2_con * treatment.f + sum_2_con,
                        left = -20, right = 113.33,
                        data = data_exploited_con)
rIII6 <- coeftest(Reg_III_2_T, vcov. = vcovCL(Reg_III_2_T, 
                                                    cluster = data_exploited_con$group.f, 
                                                    type = "HC0"))
rIII6

# Phase Iv
# Model 5 - OLS - Exploitation 
Reg_IV_1 <- lm(sum_4_con ~ exp_ph3_D_con * treatment.f + sum_3_con, data_exploited_con)
summary(Reg_IV_1)
# with heteroskedasticity robust standard error 
rIV1 <- coeftest(Reg_IV_1, vcov = vcovHC(Reg_IV_1, type = "HC0"))
rIV1
# robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_IV_1, data_exploited_con$group.f)
rIV2 <- coeftest(Reg_IV_1, v)
rIV2
# Tobit
Reg_IV_1_T <- tobit(sum_4_con ~ exp_ph3_D_con * treatment.f + sum_3_con,
                    left = -20, right = 113.33,
                    data = data_exploited_con)
summary(Reg_IV_1_T)
# with standard errors clustered at the group level
rIV3 <- coeftest(Reg_IV_1_T, vcov. = vcovCL(Reg_IV_1_T, 
                                            cluster = data_exploited_con$group.f, 
                                            type = "HC0"))
rIV3

# Model 6 - OLS - GAP 
Reg_IV_2 <- lm(sum_4_con ~ exp_ph3_con * treatment.f + sum_3_con, data_exploited_con)
summary(Reg_IV_2)
# with heteroskedasticity robust standard error 
rIV4 <- coeftest(Reg_IV_2, vcov = vcovHC(Reg_IV_2, type = "HC0"))
rIV4 
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_IV_2, data_exploited_con$group.f)
rIV5 <- coeftest(Reg_IV_2, v)
rIV5
# Tobit
Reg_IV_2_T <- tobit(sum_4_con ~ exp_ph3_con * treatment.f + sum_3_con,
                        left = -20, right = 113.33,
                        data = data_exploited_con)
# with standard errors clustered at the group level
rIV6 <- coeftest(Reg_IV_2_T, vcov. = vcovCL(Reg_IV_2_T, 
                                                    cluster = data_exploited_con$group.f, 
                                                    type = "HC0"))
rIV6

# Phase V
# Model 7 - OLS - Exploitation
Reg_V_1 <- lm(sum_5_con ~ exp_ph4_D_con * treatment.f + sum_4_con, data_exploited_con)
summary(Reg_V_1)
# with heteroskedasticity robust standard error 
rV1 <- coeftest(Reg_V_1, vcov = vcovHC(Reg_V_1, type = "HC0"))
rV1
# robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_V_1, data_exploited_con$group.f)
rV2 <- coeftest(Reg_V_1, v)
rV2
# Tobit
Reg_V_1_T <- tobit(sum_5_con ~ exp_ph4_D_con * treatment.f + sum_4_con,
                    left = -20, right = 113.33,
                    data = data_exploited_con)
summary(Reg_V_1_T)
# with standard errors clustered at the group level
rV3 <- coeftest(Reg_V_1_T, vcov. = vcovCL(Reg_V_1_T, 
                                            cluster = data_exploited_con$group.f, 
                                            type = "HC0"))
# Model 8 - OLS - GAP
Reg_V_2 <- lm(sum_5_con ~ exp_ph4_con * treatment.f + sum_4_con, data_exploited_con)
summary(Reg_V_2)
# with heteroskedasticity robust standard error 
rV4 <- coeftest(Reg_V_2, vcov = vcovHC(Reg_V_2, type = "HC0"))
rV4
# robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_V_2, data_exploited_con$group.f)
rV5 <- coeftest(Reg_V_2, v)
rV5
# Tobit
Reg_V_2_T <- tobit(sum_5_con ~ exp_ph4_con * treatment.f + sum_4_con,
                        left = -20, right = 113.33,
                        data = data_exploited_con)
rV6 <- coeftest(Reg_V_2_T, vcov. = vcovCL(Reg_V_2_T, 
                                                    cluster = data_exploited_con$group.f, 
                                                    type = "HC0"))

# Tables
# Out of space - Table A3.3: Exploitation effect by phase: Robustness checks included - Exploitation
stargazer(Reg_II_1, rII1 , rII2 , Reg_II_1_T, rII3 , # Phase II
          Reg_III_1, rIII1, rIII2, Reg_III_1_T, rIII3, # Phase III
          Reg_IV_1, rIV1, rIV2, Reg_IV_1_T, rIV3, # Phase IV
          Reg_V_1, rV1, rV2, Reg_V_1_T, rV3, # Phase V
          type = "html", out = paste("TabA3.3_Exploitation_", st, ".doc",sep = ""),
          title = "Table A3.3 Exploitation effect per phase: Robustness checks included - Exploitation", align = TRUE, no.space = TRUE,
          omit.stat = c("f","ser"),
          dep.var.caption = "Dependent Variable: Cumulated cooperation rate per phase (in percent)")


# Out of space - Table A3.3: Exploitation effect by phase: Robustness checks included - GAP
stargazer(Reg_II_2, rII4 , rII5 , Reg_II_2_T, rII6, # Phase II
          Reg_III_2, rIII4 , rIII5 , Reg_III_2_T, rIII6, # Phase III
          Reg_IV_2, rIV4 , rIV5 , Reg_IV_2_T, rIV6, # Phase IV
          Reg_V_2, rV4 , rV5 , Reg_V_2_T, rV6, # Phase V
          type = "html", out = paste("TabA3.3_GAP_", st, ".doc",sep = ""),
          title = "Table A3.3 Exploitation effect per phase: Robustness checks included - GAP", align = TRUE, no.space = TRUE,
          omit.stat = c("f","ser"),
          dep.var.caption = "Dependent Variable: Cumulated cooperation rate per phase (in percent)")


###
# Table 4. Exploitation effect by phase and treatment
###

# Overall
# Part I - Exploitation
# data
Reg <- lm(value ~ exploitation_prev * treatment.f + sum_prev, data_exploited_con_long)
# baseline
reg_base_Exp <- linearHypothesis(Reg, "exploitation_prev = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0")) # vcov.= function(y) cluster.vcov(y, ~ group.f, df_correction = TRUE)
# weakR
reg_weakR_Exp <- linearHypothesis(Reg, "exploitation_prev + exploitation_prev:treatment.fweakR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0")) # vcov.= function(y) cluster.vcov(y, ~ group.f, df_correction = TRUE)
# strongR
reg_strongR_Exp <- linearHypothesis(Reg, "exploitation_prev + exploitation_prev:treatment.fstrongR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0")) # vcov.= function(y) cluster.vcov(y, ~ group.f, df_correction = TRUE) 
# Coefficients
reg_base_Exp_C <- attr(reg_base_Exp, "value")[1]
reg_weakR_Exp_C <- attr(reg_weakR_Exp, "value")[1]
reg_strongR_Exp_C <- attr(reg_strongR_Exp, "value")[1]
# SEs
reg_base_Exp_SE <- sqrt(attr(reg_base_Exp, "vcov")[1])
reg_weakR_Exp_SE <- sqrt(attr(reg_weakR_Exp, "vcov")[1])
reg_strongR_Exp_SE <- sqrt(attr(reg_strongR_Exp, "vcov")[1])
# CIs
alpha <- 0.05 # significance level
reg_base_Exp_CI <- qnorm(1-(alpha/2))*reg_base_Exp_SE
reg_weakR_Exp_CI <- qnorm(1-(alpha/2))*reg_weakR_Exp_SE
reg_strongR_Exp_CI <- qnorm(1-(alpha/2))*reg_strongR_Exp_SE
# Tests
# base
reg_base_Exp_C # value
reg_base_Exp_SE # se
reg_base_Exp # p value
# weakR
reg_weakR_Exp_C # value
reg_weakR_Exp_SE # se
reg_weakR_Exp # p value
# strongR
reg_strongR_Exp_C # value
reg_strongR_Exp_SE # se
reg_strongR_Exp # p value

# Part II - Exploitation
# data
Reg <- lm(value ~ exploitation_num_prev * treatment.f + sum_prev, data_exploited_con_long)
# baseline
reg_base_Exp <- linearHypothesis(Reg, "exploitation_num_prev = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0")) # vcov.= function(y) cluster.vcov(y, ~ group.f, df_correction = TRUE)
# weakR
reg_weakR_Exp <- linearHypothesis(Reg, "exploitation_num_prev + exploitation_num_prev:treatment.fweakR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0")) # vcov.= function(y) cluster.vcov(y, ~ group.f, df_correction = TRUE)
# strongR
reg_strongR_Exp <- linearHypothesis(Reg, "exploitation_num_prev + exploitation_num_prev:treatment.fstrongR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0")) # vcov.= function(y) cluster.vcov(y, ~ group.f, df_correction = TRUE) 
# Coefficients
reg_base_Exp_C <- attr(reg_base_Exp, "value")[1]
reg_weakR_Exp_C <- attr(reg_weakR_Exp, "value")[1]
reg_strongR_Exp_C <- attr(reg_strongR_Exp, "value")[1]
# SEs
reg_base_Exp_SE <- sqrt(attr(reg_base_Exp, "vcov")[1])
reg_weakR_Exp_SE <- sqrt(attr(reg_weakR_Exp, "vcov")[1])
reg_strongR_Exp_SE <- sqrt(attr(reg_strongR_Exp, "vcov")[1])
# CIs
alpha <- 0.05 # significance level
reg_base_Exp_CI <- qnorm(1-(alpha/2))*reg_base_Exp_SE
reg_weakR_Exp_CI <- qnorm(1-(alpha/2))*reg_weakR_Exp_SE
reg_strongR_Exp_CI <- qnorm(1-(alpha/2))*reg_strongR_Exp_SE
# Tests
# base
reg_base_Exp_C # value
reg_base_Exp_SE # se
reg_base_Exp # p value
# weakR
reg_weakR_Exp_C # value
reg_weakR_Exp_SE # se
reg_weakR_Exp # p value
# strongR
reg_strongR_Exp_C # value
reg_strongR_Exp_SE # se
reg_strongR_Exp # p value


# Phase II
# Part I - Exploitation
# data
Reg  <- lm(sum_2_con ~ exp_ph1_D_con * treatment.f + sum_1_con, data_exploited_con)
# in baseline
reg_base_Exp <- linearHypothesis(Reg, "exp_ph1_D_con = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in weakR
reg_weakR_Exp <- linearHypothesis(Reg, "exp_ph1_D_con + exp_ph1_D_con:treatment.fweakR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in strongR
reg_strongR_Exp <- linearHypothesis(Reg, "exp_ph1_D_con + exp_ph1_D_con:treatment.fstrongR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# Coefficient
reg_base_Exp_C <- attr(reg_base_Exp, "value")[1]
reg_weakR_Exp_C <- attr(reg_weakR_Exp, "value")[1]
reg_strongR_Exp_C <- attr(reg_strongR_Exp, "value")[1]
# SEs
reg_base_Exp_SE <- sqrt(attr(reg_base_Exp, "vcov")[1])
reg_weakR_Exp_SE <- sqrt(attr(reg_weakR_Exp, "vcov")[1])
reg_strongR_Exp_SE <- sqrt(attr(reg_strongR_Exp, "vcov")[1])
# CIs
alpha <- 0.05 # significance level
reg_base_Exp_CI <- qnorm(1-(alpha/2))*reg_base_Exp_SE
reg_weakR_Exp_CI <- qnorm(1-(alpha/2))*reg_weakR_Exp_SE
reg_strongR_Exp_CI <- qnorm(1-(alpha/2))*reg_strongR_Exp_SE
# Tests
# base
reg_base_Exp_C # value
reg_base_Exp_SE # se
reg_base_Exp # p value
# weakR
reg_weakR_Exp_C # value
reg_weakR_Exp_SE # se
reg_weakR_Exp # p value
# strongR
reg_strongR_Exp_C # value
reg_strongR_Exp_SE # se
reg_strongR_Exp # p value

# Part II - Exploitation
# data
Exp_reg_5_c  <- lm(sum_2_con ~ exp_ph1_con * treatment.f + sum_1_con, data_exploited_con)
# in baseline
reg_base_Exp <- linearHypothesis(Exp_reg_5_c, "exp_ph1_con = 0", verbose = TRUE, vcov = vcovHC(Exp_reg_5_c, type = "HC0"))
# in weakR
reg_weakR_Exp <- linearHypothesis(Exp_reg_5_c, "exp_ph1_con + exp_ph1_con:treatment.fweakR = 0", verbose = TRUE, vcov = vcovHC(Exp_reg_5_c, type = "HC0"))
# in strongR
reg_strongR_Exp <- linearHypothesis(Exp_reg_5_c, "exp_ph1_con + exp_ph1_con:treatment.fstrongR = 0", verbose = TRUE, vcov = vcovHC(Exp_reg_5_c, type = "HC0"))
# Coefficient
reg_base_Exp_C <- attr(reg_base_Exp, "value")[1]
reg_weakR_Exp_C <- attr(reg_weakR_Exp, "value")[1]
reg_strongR_Exp_C <- attr(reg_strongR_Exp, "value")[1]
# SEs
reg_base_Exp_SE <- sqrt(attr(reg_base_Exp, "vcov")[1])
reg_weakR_Exp_SE <- sqrt(attr(reg_weakR_Exp, "vcov")[1])
reg_strongR_Exp_SE <- sqrt(attr(reg_strongR_Exp, "vcov")[1])
# CIs
alpha <- 0.05 # significance level
reg_base_Exp_CI <- qnorm(1-(alpha/2))*reg_base_Exp_SE
reg_weakR_Exp_CI <- qnorm(1-(alpha/2))*reg_weakR_Exp_SE
reg_strongR_Exp_CI <- qnorm(1-(alpha/2))*reg_strongR_Exp_SE
# Tests
# base
reg_base_Exp_C # value
reg_base_Exp_SE # se
reg_base_Exp # p value
# weakR
reg_weakR_Exp_C # value
reg_weakR_Exp_SE # se
reg_weakR_Exp # p value
# strongR
reg_strongR_Exp_C # value
reg_strongR_Exp_SE # se
reg_strongR_Exp # p value


# Phase III
# Part I - Exploitation
# data
Reg <- lm(sum_3_con ~ exp_ph2_D_con * treatment.f + sum_2_con, data_exploited_con)
# Interactions
# in baseline
reg_base_Exp <- linearHypothesis(Reg, "exp_ph2_D_con = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in weakR
reg_weakR_Exp <- linearHypothesis(Reg, "exp_ph2_D_con + exp_ph2_D_con:treatment.fweakR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in strongR
reg_strongR_Exp <- linearHypothesis(Reg, "exp_ph2_D_con + exp_ph2_D_con:treatment.fstrongR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# Coefficient
reg_base_Exp_C <- attr(reg_base_Exp, "value")[1]
reg_weakR_Exp_C <- attr(reg_weakR_Exp, "value")[1]
reg_strongR_Exp_C <- attr(reg_strongR_Exp, "value")[1]
# SEs
reg_base_Exp_SE <- sqrt(attr(reg_base_Exp, "vcov")[1])
reg_weakR_Exp_SE <- sqrt(attr(reg_weakR_Exp, "vcov")[1])
reg_strongR_Exp_SE <- sqrt(attr(reg_strongR_Exp, "vcov")[1])
# CIs
alpha <- 0.05 # significance level
reg_base_Exp_CI <- qnorm(1-(alpha/2))*reg_base_Exp_SE
reg_weakR_Exp_CI <- qnorm(1-(alpha/2))*reg_weakR_Exp_SE
reg_strongR_Exp_CI <- qnorm(1-(alpha/2))*reg_strongR_Exp_SE
# Tests
# base
reg_base_Exp_C # value
reg_base_Exp_SE # se
reg_base_Exp # p value
# weakR
reg_weakR_Exp_C # value
reg_weakR_Exp_SE # se
reg_weakR_Exp # p value
# strongR
reg_strongR_Exp_C # value
reg_strongR_Exp_SE # se
reg_strongR_Exp # p value

# Part II - GAP
# data
Reg <- lm(sum_3_con ~ exp_ph2_con * treatment.f + sum_2_con, data_exploited_con)
# in baseline
reg_base_Exp <- linearHypothesis(Reg, "exp_ph2_con = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in weakR
reg_weakR_Exp <- linearHypothesis(Reg, "exp_ph2_con + exp_ph2_con:treatment.fweakR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in strongR
reg_strongR_Exp <- linearHypothesis(Reg, "exp_ph2_con + exp_ph2_con:treatment.fstrongR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# Coefficient
reg_base_Exp_C <- attr(reg_base_Exp, "value")[1]
reg_weakR_Exp_C <- attr(reg_weakR_Exp, "value")[1]
reg_strongR_Exp_C <- attr(reg_strongR_Exp, "value")[1]
# SEs
reg_base_Exp_SE <- sqrt(attr(reg_base_Exp, "vcov")[1])
reg_weakR_Exp_SE <- sqrt(attr(reg_weakR_Exp, "vcov")[1])
reg_strongR_Exp_SE <- sqrt(attr(reg_strongR_Exp, "vcov")[1])
# CIs
alpha <- 0.05 # significance level
reg_base_Exp_CI <- qnorm(1-(alpha/2))*reg_base_Exp_SE
reg_weakR_Exp_CI <- qnorm(1-(alpha/2))*reg_weakR_Exp_SE
reg_strongR_Exp_CI <- qnorm(1-(alpha/2))*reg_strongR_Exp_SE
# Tests
# base
reg_base_Exp_C # value
reg_base_Exp_SE # se
reg_base_Exp # p value
# weakR
reg_weakR_Exp_C # value
reg_weakR_Exp_SE # se
reg_weakR_Exp # p value
# strongR
reg_strongR_Exp_C # value
reg_strongR_Exp_SE # se
reg_strongR_Exp # p value

# Phase IV
# Part I - Exploitation
Reg <- lm(sum_4_con ~ exp_ph3_D_con * treatment.f + sum_3_con, data_exploited_con)
# Interactions
# in baseline
reg_base_Exp <- linearHypothesis(Reg, "exp_ph3_D_con = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in weakR
reg_weakR_Exp <- linearHypothesis(Reg, "exp_ph3_D_con + exp_ph3_D_con:treatment.fweakR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in strongR
reg_strongR_Exp <- linearHypothesis(Reg, "exp_ph3_D_con + exp_ph3_D_con:treatment.fstrongR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# Coefficient
reg_base_Exp_C <- attr(reg_base_Exp, "value")[1]
reg_weakR_Exp_C <- attr(reg_weakR_Exp, "value")[1]
reg_strongR_Exp_C <- attr(reg_strongR_Exp, "value")[1]
# SEs
reg_base_Exp_SE <- sqrt(attr(reg_base_Exp, "vcov")[1])
reg_weakR_Exp_SE <- sqrt(attr(reg_weakR_Exp, "vcov")[1])
reg_strongR_Exp_SE <- sqrt(attr(reg_strongR_Exp, "vcov")[1])
# CIs
alpha <- 0.05 # significance level
reg_base_Exp_CI <- qnorm(1-(alpha/2))*reg_base_Exp_SE
reg_weakR_Exp_CI <- qnorm(1-(alpha/2))*reg_weakR_Exp_SE
reg_strongR_Exp_CI <- qnorm(1-(alpha/2))*reg_strongR_Exp_SE
# Tests
# base
reg_base_Exp_C # value
reg_base_Exp_SE # se
reg_base_Exp # p value
# weakR
reg_weakR_Exp_C # value
reg_weakR_Exp_SE # se
reg_weakR_Exp # p value
# strongR
reg_strongR_Exp_C # value
reg_strongR_Exp_SE # se
reg_strongR_Exp # p value

# Part I - GAP
# data
Reg <- lm(sum_4_con ~ exp_ph3_con * treatment.f + sum_3_con, data_exploited_con)
# Interactions
# in baseline
reg_base_Exp <- linearHypothesis(Reg, "exp_ph3_con = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in weakR
reg_weakR_Exp <- linearHypothesis(Reg, "exp_ph3_con + exp_ph3_con:treatment.fweakR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in strongR
reg_strongR_Exp <- linearHypothesis(Reg, "exp_ph3_con + exp_ph3_con:treatment.fstrongR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# Coefficient
reg_base_Exp_C <- attr(reg_base_Exp, "value")[1]
reg_weakR_Exp_C <- attr(reg_weakR_Exp, "value")[1]
reg_strongR_Exp_C <- attr(reg_strongR_Exp, "value")[1]
# SEs
reg_base_Exp_SE <- sqrt(attr(reg_base_Exp, "vcov")[1])
reg_weakR_Exp_SE <- sqrt(attr(reg_weakR_Exp, "vcov")[1])
reg_strongR_Exp_SE <- sqrt(attr(reg_strongR_Exp, "vcov")[1])
# CIs
alpha <- 0.05 # significance level
reg_base_Exp_CI <- qnorm(1-(alpha/2))*reg_base_Exp_SE
reg_weakR_Exp_CI <- qnorm(1-(alpha/2))*reg_weakR_Exp_SE
reg_strongR_Exp_CI <- qnorm(1-(alpha/2))*reg_strongR_Exp_SE
# Tests
# base
reg_base_Exp_C # value
reg_base_Exp_SE # se
reg_base_Exp # p value
# weakR
reg_weakR_Exp_C # value
reg_weakR_Exp_SE # se
reg_weakR_Exp # p value
# strongR
reg_strongR_Exp_C # value
reg_strongR_Exp_SE # se
reg_strongR_Exp # p value

# Phase V
# Part I - Exploitation
# data
Reg <- lm(sum_5_con ~ exp_ph4_D_con * treatment.f + sum_4_con, data_exploited_con)
# Interactions
# in baseline
reg_base_Exp <- linearHypothesis(Reg, "exp_ph4_D_con = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in weakR
reg_weakR_Exp <- linearHypothesis(Reg, "exp_ph4_D_con + exp_ph4_D_con:treatment.fweakR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in strongR
reg_strongR_Exp <- linearHypothesis(Reg, "exp_ph4_D_con + exp_ph4_D_con:treatment.fstrongR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# Coefficient
reg_base_Exp_C <- attr(reg_base_Exp, "value")[1]
reg_weakR_Exp_C <- attr(reg_weakR_Exp, "value")[1]
reg_strongR_Exp_C <- attr(reg_strongR_Exp, "value")[1]
# SEs
reg_base_Exp_SE <- sqrt(attr(reg_base_Exp, "vcov")[1])
reg_weakR_Exp_SE <- sqrt(attr(reg_weakR_Exp, "vcov")[1])
reg_strongR_Exp_SE <- sqrt(attr(reg_strongR_Exp, "vcov")[1])
# CIs
alpha <- 0.05 # significance level
reg_base_Exp_CI <- qnorm(1-(alpha/2))*reg_base_Exp_SE
reg_weakR_Exp_CI <- qnorm(1-(alpha/2))*reg_weakR_Exp_SE
reg_strongR_Exp_CI <- qnorm(1-(alpha/2))*reg_strongR_Exp_SE
# Tests
# base
reg_base_Exp_C # value
reg_base_Exp_SE # se
reg_base_Exp # p value
# weakR
reg_weakR_Exp_C # value
reg_weakR_Exp_SE # se
reg_weakR_Exp # p value
# strongR
reg_strongR_Exp_C # value
reg_strongR_Exp_SE # se
reg_strongR_Exp # p value

# Part I - GAP
# data
Reg <- lm(sum_5_con ~ exp_ph4_con * treatment.f + sum_4_con, data_exploited_con)
# Interactions
# in baseline
reg_base_Exp <- linearHypothesis(Reg, "exp_ph4_con = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in weakR
reg_weakR_Exp <- linearHypothesis(Reg, "exp_ph4_con + exp_ph4_con:treatment.fweakR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# in strongR
reg_strongR_Exp <- linearHypothesis(Reg, "exp_ph4_con + exp_ph4_con:treatment.fstrongR = 0", verbose = TRUE, vcov = vcovHC(Reg, type = "HC0"))
# Coefficient
reg_base_Exp_C <- attr(reg_base_Exp, "value")[1]
reg_weakR_Exp_C <- attr(reg_weakR_Exp, "value")[1]
reg_strongR_Exp_C <- attr(reg_strongR_Exp, "value")[1]
# SEs
reg_base_Exp_SE <- sqrt(attr(reg_base_Exp, "vcov")[1])
reg_weakR_Exp_SE <- sqrt(attr(reg_weakR_Exp, "vcov")[1])
reg_strongR_Exp_SE <- sqrt(attr(reg_strongR_Exp, "vcov")[1])
# CIs
alpha <- 0.05 # significance level
reg_base_Exp_CI <- qnorm(1-(alpha/2))*reg_base_Exp_SE
reg_weakR_Exp_CI <- qnorm(1-(alpha/2))*reg_weakR_Exp_SE
reg_strongR_Exp_CI <- qnorm(1-(alpha/2))*reg_strongR_Exp_SE
# Tests
# base
reg_base_Exp_C # value
reg_base_Exp_SE # se
reg_base_Exp # p value
# weakR
reg_weakR_Exp_C # value
reg_weakR_Exp_SE # se
reg_weakR_Exp # p value
# strongR
reg_strongR_Exp_C # value
reg_strongR_Exp_SE # se
reg_strongR_Exp # p value

###
# Table A3.4 Exploitation per treatment and phase
###

# Part I: Level of exploitation 
# data
data_exploited_con_long$group <- as.numeric(data_exploited_con_long$group.f)
data <- subset(data_exploited_con_long, data_exploited_con_long$variable == "sum_2_con")
# data preparation
# Player: 1 - 4 per group
data$player <- sequence(rle(as.character(data$group))$lengths)

for (i in 1:85) { # goup   
    data$GAP_1[data$group==i] <- 
      mean(abs(data$exp_ph1_con[data$group==i]))
  }

for (i in 1:85) { # goup   
  data$GAP_2[data$group==i] <- 
    mean(abs(data$exp_ph2_con[data$group==i]))
  }

for (i in 1:85) { # goup   
  data$GAP_3[data$group==i] <- 
    mean(abs(data$exp_ph3_con[data$group==i]))
  }

for (i in 1:85) { # goup   
  data$GAP_4[data$group==i] <- 
    mean(abs(data$exp_ph4_con[data$group==i]))
  }

for (i in 1:85) { # goup   
  data$GAP_5[data$group==i] <- 
    mean(abs(data$exp_ph5_con[data$group==i]))
  }

data$GAP_1 <- ifelse(data$player==1, data$GAP_1, NA)
data$GAP_2 <- ifelse(data$player==1, data$GAP_2, NA)
data$GAP_3 <- ifelse(data$player==1, data$GAP_3, NA)
data$GAP_4 <- ifelse(data$player==1, data$GAP_4, NA)
data$GAP_5 <- ifelse(data$player==1, data$GAP_5, NA)


# Per Phase: Phase I - V
# base
data_b <- subset(data, data$treatment.f=="base")
t.test(data_b$GAP_1,data_b$GAP_2)
wilcox.test(data_b$GAP_1,data_b$GAP_2, paired = TRUE)
t.test(data_b$GAP_1,data_b$GAP_3)
wilcox.test(data_b$GAP_1,data_b$GAP_3, paired = TRUE)
t.test(data_b$GAP_1,data_b$GAP_4)
wilcox.test(data_b$GAP_1,data_b$GAP_4, paired = TRUE)
t.test(data_b$GAP_1,data_b$GAP_5)
wilcox.test(data_b$GAP_1,data_b$GAP_5, paired = TRUE)
# weakR
data_b <- subset(data, data$treatment.f=="weakR")
t.test(data_b$GAP_1,data_b$GAP_2)
wilcox.test(data_b$GAP_1,data_b$GAP_2, paired = TRUE)
t.test(data_b$GAP_1,data_b$GAP_3)
wilcox.test(data_b$GAP_1,data_b$GAP_3, paired = TRUE)
t.test(data_b$GAP_1,data_b$GAP_4)
wilcox.test(data_b$GAP_1,data_b$GAP_4, paired = TRUE)
t.test(data_b$GAP_1,data_b$GAP_5)
wilcox.test(data_b$GAP_1,data_b$GAP_5, paired = TRUE)
# strongR
data_b <- subset(data, data$treatment.f=="strongR")
t.test(data_b$GAP_1,data_b$GAP_2)
wilcox.test(data_b$GAP_1,data_b$GAP_2, paired = TRUE)
t.test(data_b$GAP_1,data_b$GAP_3)
wilcox.test(data_b$GAP_1,data_b$GAP_3, paired = TRUE)
t.test(data_b$GAP_1,data_b$GAP_4)
wilcox.test(data_b$GAP_1,data_b$GAP_4, paired = TRUE)
t.test(data_b$GAP_1,data_b$GAP_5)
wilcox.test(data_b$GAP_1,data_b$GAP_5, paired = TRUE)

# Overall 
# data
names(data)[names(data) == 'value'] <- 'value_old' # rename old value variable
data_new <- melt(data, measure.vars = c("GAP_1", "GAP_2", "GAP_3", "GAP_4", "GAP_5"))

# base vs. wearkR
t.test(data_new$value[data_new$treatment.f=="base"],data_new$value[data_new$treatment.f=="weakR"])
# base vs. strongR
t.test(data_new$value[data_new$treatment.f=="base"],data_new$value[data_new$treatment.f=="strongR"])


# Part II 
# SD per group, period, & phase
for (k in 1:5)      # k: period; i : group_total; j : phase
  for (i in 11:204) 
   for (j in 1:5){  
    data_long_con$sd[data_long_con$period==k&data_long_con$group_total==i&data_long_con$phase == j] <- 
      sd(data_long_con$cooprate[data_long_con$period==k&data_long_con$group_total==i&data_long_con$phase == j]*100)
  }

# Mean sd per phase
for (i in 11:204)
  for (j in 1:5){
    data_long_con$avg_sd[data_long_con$group_total==i&data_long_con$phase == j] <- 
      mean(data_long_con$sd[data_long_con$group_total==i&data_long_con$phase == j])
  }

# Player: 1 - 4 per group
data_long_con$player <- sequence(rle(as.character(data_long_con$group_total))$lengths)

# summary
# base
# overall
summary(data_long_con$avg_sd[data_long_con$treatment=="base"])
# per phase
summary(data_long_con$avg_sd[data_long_con$treatment=="base"&data_long_con$phase.f==1])
summary(data_long_con$avg_sd[data_long_con$treatment=="base"&data_long_con$phase.f==2])
summary(data_long_con$avg_sd[data_long_con$treatment=="base"&data_long_con$phase.f==3])
summary(data_long_con$avg_sd[data_long_con$treatment=="base"&data_long_con$phase.f==4])
summary(data_long_con$avg_sd[data_long_con$treatment=="base"&data_long_con$phase.f==5])
# Tests
# data set
# Groups
group <- seq(1,length(data_long_con$avg_sd[data_long_con$treatment=="base"&
                                                data_long_con$phase.f==1&
                                                data_long_con$period==1&
                                                data_long_con$player==1]), 1)
# Phase I
sd_1 <- data_long_con$avg_sd[data_long_con$treatment=="base"&
                                   data_long_con$phase.f==1&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# Phase II
sd_2 <- data_long_con$avg_sd[data_long_con$treatment=="base"&
                                   data_long_con$phase.f==2&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# Phase III
sd_3 <- data_long_con$avg_sd[data_long_con$treatment=="base"&
                                   data_long_con$phase.f==3&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# Phase IV
sd_4 <- data_long_con$avg_sd[data_long_con$treatment=="base"&
                                   data_long_con$phase.f==4&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# Phase V
sd_5 <- data_long_con$avg_sd[data_long_con$treatment=="base"&
                                   data_long_con$phase.f==5&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# Data set
d <- data.frame(group, sd_1, sd_2, sd_3, sd_4, sd_5)

# Means
mean(d$sd_1)
mean(d$sd_2)
mean(d$sd_3)
mean(d$sd_4)
mean(d$sd_5)

# Tests
t.test(d$sd_1, d$sd_2, paired = TRUE)
wilcox.test(d$sd_1, d$sd_2, paired = TRUE)
t.test(d$sd_1, d$sd_3, paired = TRUE)
wilcox.test(d$sd_1, d$sd_3, paired = TRUE)
t.test(d$sd_1, d$sd_4, paired = TRUE)
wilcox.test(d$sd_1, d$sd_4, paired = TRUE)
t.test(d$sd_1, d$sd_5, paired = TRUE)
wilcox.test(d$sd_1, d$sd_5, paired = TRUE)

# weakR
# overall
summary(data_long_con$avg_sd[data_long_con$treatment=="weakR"])
# per phase
summary(data_long_con$avg_sd[data_long_con$treatment=="weakR"&data_long_con$phase.f==1])
summary(data_long_con$avg_sd[data_long_con$treatment=="weakR"&data_long_con$phase.f==2])
summary(data_long_con$avg_sd[data_long_con$treatment=="weakR"&data_long_con$phase.f==3])
summary(data_long_con$avg_sd[data_long_con$treatment=="weakR"&data_long_con$phase.f==4])
summary(data_long_con$avg_sd[data_long_con$treatment=="weakR"&data_long_con$phase.f==5])
# Test
# Data set
# groups
group <- seq(1,length(data_long_con$avg_sd[data_long_con$treatment=="weakR"&
                                                data_long_con$phase.f==1&
                                                data_long_con$period==1&
                                                data_long_con$player==1]), 1)
# Phase I
sd_1 <- data_long_con$avg_sd[data_long_con$treatment=="weakR"&
                                   data_long_con$phase.f==1&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# Phase II
sd_2 <- data_long_con$avg_sd[data_long_con$treatment=="weakR"&
                                   data_long_con$phase.f==2&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# Phase III
sd_3 <- data_long_con$avg_sd[data_long_con$treatment=="weakR"&
                                   data_long_con$phase.f==3&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# Phase IV
sd_4 <- data_long_con$avg_sd[data_long_con$treatment=="weakR"&
                                   data_long_con$phase.f==4&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# Phase V
sd_5 <- data_long_con$avg_sd[data_long_con$treatment=="weakR"&
                                   data_long_con$phase.f==5&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# data set
d <- data.frame(group, sd_1, sd_2, sd_3, sd_4, sd_5)

# Means
mean(d$sd_1)
mean(d$sd_2)
mean(d$sd_3)
mean(d$sd_4)
mean(d$sd_5)

# Tests
t.test(d$sd_1, d$sd_2, paired = TRUE)
wilcox.test(d$sd_1, d$sd_2, paired = TRUE)
t.test(d$sd_1, d$sd_3, paired = TRUE)
wilcox.test(d$sd_1, d$sd_3, paired = TRUE)
t.test(d$sd_1, d$sd_4, paired = TRUE)
wilcox.test(d$sd_1, d$sd_4, paired = TRUE)
t.test(d$sd_1, d$sd_5, paired = TRUE)
wilcox.test(d$sd_1, d$sd_5, paired = TRUE)

# strongR
# overall
summary(data_long_con$avg_sd[data_long_con$treatment=="strongR"])
# per phase
summary(data_long_con$avg_sd[data_long_con$treatment=="strongR"&data_long_con$phase.f==1])
summary(data_long_con$avg_sd[data_long_con$treatment=="strongR"&data_long_con$phase.f==2])
summary(data_long_con$avg_sd[data_long_con$treatment=="strongR"&data_long_con$phase.f==3])
summary(data_long_con$avg_sd[data_long_con$treatment=="strongR"&data_long_con$phase.f==4])
summary(data_long_con$avg_sd[data_long_con$treatment=="strongR"&data_long_con$phase.f==5])
# Test
# Data set
# groups
group <- seq(1,length(data_long_con$avg_sd[data_long_con$treatment=="strongR"&
                                                data_long_con$phase.f==1&
                                                data_long_con$period==1&
                                                data_long_con$player==1]), 1)

# Phase I
sd_1 <- data_long_con$avg_sd[data_long_con$treatment=="strongR"&
                                   data_long_con$phase.f==1&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# Phase II
sd_2 <- data_long_con$avg_sd[data_long_con$treatment=="strongR"&
                                   data_long_con$phase.f==2&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# Phase III
sd_3 <- data_long_con$avg_sd[data_long_con$treatment=="strongR"&
                                   data_long_con$phase.f==3&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# Phase IV
sd_4 <- data_long_con$avg_sd[data_long_con$treatment=="strongR"&
                                   data_long_con$phase.f==4&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# Phase V
sd_5 <- data_long_con$avg_sd[data_long_con$treatment=="strongR"&
                                   data_long_con$phase.f==5&
                                   data_long_con$period==1&
                                   data_long_con$player==1]
# data set
d <- data.frame(group, sd_1, sd_2, sd_3, sd_4, sd_5)

# Means
mean(d$sd_1)
mean(d$sd_2)
mean(d$sd_3)
mean(d$sd_4)
mean(d$sd_5)

# Tests
t.test(d$sd_1, d$sd_2, paired = TRUE)
wilcox.test(d$sd_1, d$sd_2, paired = TRUE)
t.test(d$sd_1, d$sd_3, paired = TRUE)
wilcox.test(d$sd_1, d$sd_3, paired = TRUE)
t.test(d$sd_1, d$sd_4, paired = TRUE)
wilcox.test(d$sd_1, d$sd_4, paired = TRUE)
t.test(d$sd_1, d$sd_5, paired = TRUE)
wilcox.test(d$sd_1, d$sd_5, paired = TRUE)

###
# Excess cooperation ------------------------------------------------------
###

# data & data preparation
data_qmin_long <- data_long # Just a new name, again

# additional variables
# cooperation rate
cooprate <- function(x){
  (x-15)/(90-15)
}

# Def. min. cont. level
# strongR: q_min + 1
data_qmin_long$qi_min[data_qmin_long$treatment=="strongR"] <- data_qmin_long$qi_min[data_qmin_long$treatment=="strongR"] + 1 
data_qmin_long$qi_min[data_qmin_long$treatment=="strongR" & data_qmin_long$qi_min==101] <- 100 # cap at 100
# Transfer into cooperation rates
data_qmin_long$q_c <- cooprate(data_qmin_long$q)*100
data_qmin_long$q_min_c <- cooprate(data_qmin_long$qi_min)*100

# Define running subject
data_qmin_long$subject_total <- as.numeric(paste(data_qmin_long$session_num, data_qmin_long$subject, sep = ""))

# data preparation
data_qmin_long$eq <- ifelse(data_qmin_long$period > 1 & data_qmin_long$treatment!="base", data_qmin_long$q_c - data_qmin_long$q_min_c, NA) # excess cooperation
data_qmin_long$treatment.f <- factor(data_qmin_long$treatment)
data_qmin_long$period.f <- factor(data_qmin_long$period)
data_qmin_long$subject.f <- factor(data_qmin_long$subject_total)

data_qmin_long$strongR <- ifelse(data_qmin_long$treatment == "strongR", 1, 0) 
data_qmin_long$strongR <- factor(data_qmin_long$strongR)

data_qmin_long$p1 <- ifelse(data_qmin_long$period==1, 1,0)
data_qmin_long$p2 <- ifelse(data_qmin_long$period==2, 1,0)
data_qmin_long$p3 <- ifelse(data_qmin_long$period==3, 1,0)
data_qmin_long$p4 <- ifelse(data_qmin_long$period==4, 1,0)
data_qmin_long$p5 <- ifelse(data_qmin_long$period==5, 1,0)

data_qmin_long$Sp1 <- ifelse(data_qmin_long$treatment=="strongR" &
                               data_qmin_long$period==1, 1, 0)
data_qmin_long$Sp2 <- ifelse(data_qmin_long$treatment=="strongR" &
                               data_qmin_long$period==2, 1, 0)
data_qmin_long$Sp3 <- ifelse(data_qmin_long$treatment=="strongR" &
                               data_qmin_long$period==3, 1, 0)
data_qmin_long$Sp4 <- ifelse(data_qmin_long$treatment=="strongR" &
                               data_qmin_long$period==4, 1, 0)
data_qmin_long$Sp5 <- ifelse(data_qmin_long$treatment=="strongR" &
                               data_qmin_long$period==5, 1, 0)
# Data reg.
data_qmin <- subset(data_qmin_long, period > 1 & treatment.f != "base")
data_qmin$treatment.f <- factor(data_qmin$treatment.f, level= c("weakR", "strongR"))
data_qmin$group.f <- factor(data_qmin$group_total)
data_qmin$subject.f <- factor(data_qmin$subject_total)



####
# Table 5. Deviation from minimum cooperation rate by treatment and period
###

## Model 1
# OLS
Reg_Qmin_1 <- lm(eq ~ treatment.f, 
                  data_qmin)
summary(Reg_Qmin_1)
# with  heteroskedasticity robust standard error 
rQmin1 <- coeftest(Reg_Qmin_1, vcov = vcovHC(Reg_Qmin_1, type = "HC0"))
rQmin1
# robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_Qmin_1, data_qmin$group.f)
rQmin2 <- coeftest(Reg_Qmin_1, v)
rQmin2
# Tobit
Reg_Qmin_1_T <- tobit(eq ~ strongR,
                     left = 0, right = 133,
                     data = data_qmin)
# with standard errors clustered at the group level
rQmin3 <- coeftest(Reg_Qmin_1_T , vcov. = vcovCL(Reg_Qmin_1_T , 
                                              cluster = data_qmin$group.f, 
                                              type = "HC0"))
rQmin3


# Model 2 
# OLS
Reg_Qmin_2 <- lm(eq ~ treatment.f + period.f,
                  data_qmin)
summary(Reg_Qmin_2)
# with  heteroskedasticity robust standard error 
rQmin4 <- coeftest(Reg_Qmin_2, vcov = vcovHC(Reg_Qmin_2, type = "HC0"))
rQmin4
# robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_Qmin_2, data_qmin$group.f)
rQmin5 <- coeftest(Reg_Qmin_2, v)
rQmin5
# Tobit
Reg_Qmin_2_T <- tobit(eq ~ strongR + p3 + p4 + p5,
                       left = 0, right = 133,
                       data = data_qmin)
# with standard errors clustered at the group level
rQmin6 <- coeftest(Reg_Qmin_2_T , vcov. = vcovCL(Reg_Qmin_2_T , 
                                                cluster = data_qmin$group.f, 
                                                type = "HC0"))
rQmin6

# Model 3
# OLS
Reg_Qmin_3 <- lm(eq ~  treatment.f*period.f, data_qmin)
summary(Reg_Qmin_3)
# with  heteroskedasticity robust standard error 
rQmin7 <- coeftest(Reg_Qmin_3, vcov = vcovHC(Reg_Qmin_3, type = "HC0"))
rQmin7
# robustness
# with standard errors clustered at the group level
v <- cluster.vcov(Reg_Qmin_3, data_qmin$group.f)
rQmin8 <- coeftest(Reg_Qmin_3, v)
rQmin8
# Tobit
Reg_Qmin_3_T <- tobit(eq ~ strongR + p3 + p4 + p5 + Sp3 + Sp4 + Sp5, 
                    left = 0, right = 133,
                    data = data_qmin)
# with standard errors clustered at the group level
rQmin9 <- coeftest(Reg_Qmin_3_T , vcov. = vcovCL(Reg_Qmin_3_T , 
                                                cluster = data_qmin$group.f, 
                                                type = "HC0"))
rQmin9

# Tables
# Out of space - Table 5. Excess cooperation rates by treatment and period
stargazer(Reg_Qmin_1, rQmin1,
          Reg_Qmin_2, rQmin4,
          Reg_Qmin_3, rQmin7,
          type = "html", out = paste("Tab5_", st, ".doc",sep = ""),
          title = "Table 5. Excess cooperation rates by treatment and period", align = TRUE, no.space = TRUE,
          omit.stat = c("f","ser"),
          dep.var.caption = "Dependent Variable: Deviation from required minimum cooperation rate",
          covariate.labels = c("strongR", 
                               "period 3", "period 4", "period 5",
                               "strongR x period 3", "strongR x period 4", "strongR x period 5"))
          
# Out of space - Table A3.5. Excess cooperation rates by treatment and period: Robustness checks
stargazer(Reg_Qmin_1, rQmin2, # OLS 
          Reg_Qmin_1_T, rQmin3, # Tobit
          Reg_Qmin_2, rQmin5, # OLS
          Reg_Qmin_2_T, rQmin6, # Tobit
          Reg_Qmin_3, rQmin8, # OLS
          Reg_Qmin_3_T, rQmin9, # Tobit
          type = "html", out = paste("TabA3.5_", st, ".doc",sep = ""))

# Out of space 
#stargazer(Qmin_reg_v0, Qmin_reg_v1, Qmin_reg,
#          type = "html", out = "Qmin_reg.doc")


# Out of space - robustness checks
#stargazer(Qmin_reg_v1, r1, r2, r3,
#          Qmin_reg_v2, r4, r5, r6,
#          Qmin_reg, r7, r8, r9,
#          type = "html", omit = "subject.f",
#          out = "Table 5 robustness.doc")

# Out of space - paper new with robust standard errors
#stargazer(Qmin_reg_v1, r1,
#          Qmin_reg_v2, r4,
#          Qmin_reg, r7,
#          type = "html", omit = "subject.f",
#          out = "Table5NEW.doc")

# Out of space - paper new robustness checks for appendix
#stargazer(Qmin_reg_v1, r1, 
#          Qmin_reg_v1_FS, r3,
#          Qmin_reg_v2, r5,
#          Qmin_reg_v2_FS, r6,
#          Qmin_reg, r8,
#          Qmin_reg_FS, r9,
#          omit = "subject.f",
#          type = "html", out = "TableA3.3_appendix.doc")

###
# Fig 5. Deviation from minimum contribution level by treatment and period
###

# Data & data preparation
Qmin_reg <- lm(eq ~  treatment.f*period.f, data_qmin)

# Regressions
reg_weakR_2 <- linearHypothesis(Qmin_reg, "(Intercept)", verbose = TRUE, vcov = vcovHC(Qmin_reg, type = "HC0"))  # singular.ok = TRUE, vcov.= function(y) cluster.vcov(y, ~ group.f, df_correction = FALSE) // 
reg_weakR_3 <- linearHypothesis(Qmin_reg, "(Intercept) + period.f3", verbose = TRUE, vcov = vcovHC(Qmin_reg, type = "HC0"))
reg_weakR_4 <- linearHypothesis(Qmin_reg, "(Intercept) + period.f4", verbose = TRUE, vcov = vcovHC(Qmin_reg, type = "HC0"))
reg_weakR_5 <- linearHypothesis(Qmin_reg, "(Intercept) + period.f5", verbose = TRUE, vcov = vcovHC(Qmin_reg, type = "HC0"))
reg_strongR_2 <- linearHypothesis(Qmin_reg, "(Intercept) + treatment.fstrongR", verbose = TRUE, vcov = vcovHC(Qmin_reg, type = "HC0"))
reg_strongR_3 <- linearHypothesis(Qmin_reg, "(Intercept) + treatment.fstrongR + period.f3 + treatment.fstrongR:period.f3", verbose = TRUE, vcov = vcovHC(Qmin_reg, type = "HC0"))
reg_strongR_4 <- linearHypothesis(Qmin_reg, "(Intercept) + treatment.fstrongR + period.f4 + treatment.fstrongR:period.f4", verbose = TRUE, vcov = vcovHC(Qmin_reg, type = "HC0"))
reg_strongR_5 <- linearHypothesis(Qmin_reg, "(Intercept) + treatment.fstrongR + period.f5 + treatment.fstrongR:period.f5", verbose = TRUE, vcov = vcovHC(Qmin_reg, type = "HC0"))
# Coefficient
strongR_eq_2 <- attr(reg_strongR_2, "value")[1]
strongR_eq_3 <- attr(reg_strongR_3, "value")[1]
strongR_eq_4 <- attr(reg_strongR_4, "value")[1]
strongR_eq_5 <- attr(reg_strongR_5, "value")[1]
weakR_eq_2 <- attr(reg_weakR_2, "value")[1]
weakR_eq_3 <- attr(reg_weakR_3, "value")[1]
weakR_eq_4 <- attr(reg_weakR_4, "value")[1]
weakR_eq_5 <- attr(reg_weakR_5, "value")[1]
# SEs
strongR_eq_2_SE <- sqrt(attr(reg_strongR_2, "vcov")[1])
strongR_eq_3_SE <- sqrt(attr(reg_strongR_3, "vcov")[1])
strongR_eq_4_SE <- sqrt(attr(reg_strongR_4, "vcov")[1])
strongR_eq_5_SE <- sqrt(attr(reg_strongR_5, "vcov")[1])
weakR_eq_2_SE <- sqrt(attr(reg_weakR_2, "vcov")[1])
weakR_eq_3_SE <- sqrt(attr(reg_weakR_3, "vcov")[1])
weakR_eq_4_SE <- sqrt(attr(reg_weakR_4, "vcov")[1])
weakR_eq_5_SE <- sqrt(attr(reg_weakR_5, "vcov")[1])
# CIs 
strongR_eq_2_CI <- qnorm(1-(alpha/2))*strongR_eq_2_SE
strongR_eq_3_CI <- qnorm(1-(alpha/2))*strongR_eq_3_SE
strongR_eq_4_CI <- qnorm(1-(alpha/2))*strongR_eq_4_SE
strongR_eq_5_CI <- qnorm(1-(alpha/2))*strongR_eq_5_SE
weakR_eq_2_CI <- qnorm(1-(alpha/2))*weakR_eq_2_SE
weakR_eq_3_CI <- qnorm(1-(alpha/2))*weakR_eq_3_SE
weakR_eq_4_CI <- qnorm(1-(alpha/2))*weakR_eq_4_SE
weakR_eq_5_CI <- qnorm(1-(alpha/2))*weakR_eq_5_SE

# Coefficients
weakR_eq <- c(mean(data_qmin$eq[data_qmin$treatment=="weakR"&data_qmin$period==2]),
                 mean(data_qmin$eq[data_qmin$treatment=="weakR"&data_qmin$period==3]),
                 mean(data_qmin$eq[data_qmin$treatment=="weakR"&data_qmin$period==4]),
                 mean(data_qmin$eq[data_qmin$treatment=="weakR"&data_qmin$period==5]))
strongR_eq <- c(mean(data_qmin$eq[data_qmin$treatment=="strongR"&data_qmin$period==2]),
                 mean(data_qmin$eq[data_qmin$treatment=="strongR"&data_qmin$period==3]),
                 mean(data_qmin$eq[data_qmin$treatment=="strongR"&data_qmin$period==4]),
                 mean(data_qmin$eq[data_qmin$treatment=="strongR"&data_qmin$period==5]))         

# Standard errors
stderr <- function(x){
  sqrt(var(x)/length(x))
}

# weakR
weakR_eq_SE <- c((sd(data_qmin$q[data_qmin$treatment=="weakR"&data_qmin$period==2])/sqrt(length(data_qmin$treatment[data_qmin$treatment=="weakR"]))),
                   (sd(data_qmin$q[data_qmin$treatment=="weakR"&data_qmin$period==3])/sqrt(length(data_qmin$treatment[data_qmin$treatment=="weakR"]))),
                   (sd(data_qmin$q[data_qmin$treatment=="weakR"&data_qmin$period==4])/sqrt(length(data_qmin$treatment[data_qmin$treatment=="weakR"]))),
                   (sd(data_qmin$q[data_qmin$treatment=="weakR"&data_qmin$period==5])/sqrt(length(data_qmin$treatment[data_qmin$treatment=="weakR"]))))
strongR_eq_SE <- c((sd(data_qmin$q[data_qmin$treatment=="strongR"&data_qmin$period==2])/sqrt(length(data_qmin$treatment[data_qmin$treatment=="strongR"]))),
                 (sd(data_qmin$q[data_qmin$treatment=="strongR"&data_qmin$period==3])/sqrt(length(data_qmin$treatment[data_qmin$treatment=="strongR"]))),
                 (sd(data_qmin$q[data_qmin$treatment=="strongR"&data_qmin$period==4])/sqrt(length(data_qmin$treatment[data_qmin$treatment=="strongR"]))),
                 (sd(data_qmin$q[data_qmin$treatment=="strongR"&data_qmin$period==5])/sqrt(length(data_qmin$treatment[data_qmin$treatment=="strongR"]))))
# Confidence intervals
# base
alpha <- 0.05 # significance level
weakR_eq_CI <- qt(1-(alpha/2), 
                    df = length(data_qmin$q[data_qmin$treatment=="weakR"])-1)*weakR_eq_SE
strongR_eq_CI <- qt(1-(alpha/2), 
                  df = length(data_qmin$q[data_qmin$treatment=="strongR"])-1)*strongR_eq_SE

# Data
trend <- 2:5
Treatments <- c("weakR", "weakR", "weakR", "weakR",
                "strongR", "strongR", "strongR", "strongR")
Colors <- c(cols[3], cols[5])
Rounds <- as.vector(cbind(trend,trend))
EQ <- as.vector(cbind(weakR_eq_2,weakR_eq_3,weakR_eq_4,weakR_eq_5,
                      strongR_eq_2, strongR_eq_3, strongR_eq_4, strongR_eq_5))
EQ_v1 <- as.vector(cbind(weakR_eq, strongR_eq))
CI <- as.vector(cbind(weakR_eq_2_CI, weakR_eq_3_CI, weakR_eq_4_CI, weakR_eq_5_CI,
                      strongR_eq_2_CI, strongR_eq_3_CI, strongR_eq_4_CI, strongR_eq_5_CI))
CI_v1 <- as.vector(cbind(weakR_eq_CI, strongR_eq_CI))
data_qmin_plot <- data.frame(Rounds,
                            EQ,
                            EQ_v1,
                            CI,
                            CI_v1,
                            Treatments)
data_qmin_plot$period.f <- as.factor(data_qmin_plot$Rounds)
data_qmin_plot$Treatments <- factor(data_qmin_plot$Treatments, level=c("weakR", "strongR"))

# Fig. 5
qmin_plot <- ggplot(data_qmin_plot, aes(x=period.f, y=EQ, fill=Treatments))+
  theme_minimal() +
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_manual(values=Colors) +
  geom_errorbar(aes(ymin=1*(EQ-CI), ymax=1*(EQ+CI)), width=0.2, position=position_dodge(0.9)) +
  xlab("Periods") + ylab("Deviation from required \n minimum cooperation rate \n (in percentage points)") +
  ggtitle("") +
  theme(axis.title.x = element_text(color = "black", size = 13, face = "bold"),
        axis.title.y = element_text(color = "black", size = 13, face = "bold"),
        legend.position = "bottom", legend.text = element_text(face = "italic", size = 12),
        legend.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(color = "black", size = 12, face = "bold"),
        axis.text.y = element_text(color = "black", size = 12, face = "bold")) 

# out of space
pdf(file = paste("Fig5_", st, ".pdf",sep = ""),
    width = 11.00, # The width of the plot in inches
    height = 7.00) # The height of the plot in inches

qmin_plot

dev.off()

###########################################################################
##################### THIS IS THE END #####################################
###########################################################################


