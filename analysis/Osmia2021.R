


library(ggplot2)
library(plyr)
library("survival")
library("survminer")

#setwd("~/Documents/Danforth Lab/Osmia Experiment 2021/analysis/OsmiaExperiment2021")
setwd("~/Box/Rachel Fordyce/Danforth Lab/Osmia Experiment 2021/analysis/OsmiaExperiment2021")
# platedata <- read.csv("OsmiaData2021_Master.csv")
# larvalscoring <- read.csv("OsmiaLarvalScoring2021.csv")
# larvalscoring <- larvalscoring[1:791,]


# #make sure data in df are correct class
# platedata$Date.closed <- as.Date(platedata$Date.closed, format = "%m/%d/%y")
# platedata$Date.opened <- as.Date(platedata$Date.opened, format = "%m/%d/%y")
# larvalscoring$sample.date <- as.Date(larvalscoring$sample.date, format = "%m/%d/%y")
# #platedata$Tube.weight <- as.numeric(platedata$Tube.weight)
# 
# class(platedata$Tube.weight)
# #merge larval scoring sheet and plate data sheet by sample id
# data <- merge(platedata, larvalscoring, by="Sample.ID")
# 
# #create new column days to maturity
# data$days.to.maturity <- as.numeric(data$sample.date-data$Date.closed)
# 
# head(data$days.to.maturity)
# #have column "died" from excel, now insert values 0 for matured 1 for dead 
# data$died <- ifelse(is.na(data$days.to.maturity) == TRUE, 1,  0)
# #status is 1 if lived (matured), 2 if dead (paskage survival uses "status" but "died" is more intuitive)
# data$status <- data$died + 1
# 
# #add "time" vector  to df (time until death, or time now (days it has survived))
# data$Date.closed <- as.Date(data$Date.closed, format = "%m/%d/%y")
# data$death.date <- as.Date(data$death.date, format = "%m/%d/%y")
# 
# data$dead.on.exp.day <- as.numeric(data$death.date - data$Date.closed)
# time <-  as.numeric(
#   ifelse(data$died > 0, data$dead.on.exp.day, data$days.to.maturity))
# dfsurv <- data.frame(data,time) 
# hist(dfsurv$time)
# hist(dfsurv$days.to.maturity)
# hist(dfsurv$dead.on.exp.day)

# write.csv(dfsurv, file = "DataMergedForAnalysis.csv")
#Manually entered death date into "datamergedforanalysis", must use this csv for data from now on

# #make variable dead.on.exp.day
# data$dead.on.exp.day <- as.numeric(data$death.date - data$Date.closed)

data <- read.csv("DataMergedForAnalysis.csv")
#wrote dfsurv into this file, including column "time", excluding rows with missing data


# data <- data[-which(data$death.date == "missing"),]

 data$Treatment <- as.factor(data$Treatment.Group)#rename column to conform with code

 
 
 
 #experimental setup
 #______________________________________________________________________________
 cdata4 <- ddply(data, c("Treatment", "Sex"), summarise,
                 N    = length(!is.na(Sample.ID))
 )
 cdata4
 
 ggplot(cdata4, aes(x= Treatment, y=N, fill=Sex)) + 
   geom_bar(position=position_dodge(), stat="identity") +
   ggtitle("Experimental Setup") +
   scale_fill_brewer(palette = "Accent")
 
#DIED VS. MATURED ------- SIMPLE PLOT
##############################################################################

#library(reshape2)

#stat = "bin" gives counts
#counts number of individuals in each treatment group, color codes treatment
ggplot(data=data, aes(x=as.numeric(Treatment), fill=Treatment)) +
    geom_bar(stat="bin", binwidth = 1, position = 'dodge')

#gives counts for status (survival binary) for each treatment group, color codes treatment, but organization is wrong -- want to be grouped by treatment, not by status
ggplot(data=data, aes(x=(status), fill=Treatment)) +
    geom_bar(stat="bin", position = 'dodge')


#new try to get organization right - WORKS
library(dplyr)
#create new df using dplyr to group by treatment$status

#create counts of how many larvae in each Treatment, status combo
class(data$status) <- "character"
newdf <- data %>%
    #filter(color %in% c("J", "D")) %>%
    group_by(Treatment, status) %>%
    dplyr::summarise(counts = n()) 
#class(newdf$status) <- "factor"
countsplot <- ggplot(data=newdf, aes(x=Treatment, y = counts, fill = status )) +
    geom_histogram(stat = "identity", position = position_dodge())+
    scale_fill_manual(values=c("goldenrod3", "orangered4"), 
                      name = "Survival",
                      breaks = c("1","2","NA"),
                      labels = c("matured","died","NA"))+
    ggtitle("Survival Counts by Treatment")
#scale_fill_brewer(palette = "Set2")
countsplot

#percentages    ##############################   PERCENTAGE MORTALITY PLOT
newdf2 <- df[which(df$Sex == "M"),] %>%
    #filter(color %in% c("J", "D")) %>%
    group_by(Treatment, status) %>%
    dplyr::summarise(Percentage = n()) %>%
    group_by(Treatment) %>% 
    mutate(Percentage=Percentage/sum(Percentage)*100)

class(newdf2$status) <- "character"
class(newdf2$Treatment) <- "factor"

percplot <- ggplot(data=newdf2, aes(x=Treatment, y = Percentage, fill = status )) +
    geom_histogram(stat = "identity", position = position_dodge())+ 
    #xlab( "Acetone", "Cap High", "Cap Lo", "Difen Hi", "Difen Lo", "None")+
    scale_fill_manual(values=c("goldenrod3", "orangered4"), 
                      name = "Survival",
                      breaks = c("1","2","NA"),
                      labels = c("matured","died","NA"))+
    ggtitle("Male Survival Percentage By Treatment Group")
percplot #+ labs( "Acetone", "Cap High", "Cap Lo", "Difen Hi", "Difen Lo", "None")

###########
# #add "time" vector  to df (time until death, or time now (days it has survived))
# data$Date.closed <- as.numeric
# data$dead.on.exp.day <- (data$death.date - data$Date.closed)
# time <-  as.numeric(
#     ifelse(data$dead.on.exp.day > 0, data$dead.on.exp.day, data$days.to.maturity)
# ) #for some reason this only works while the days columns in df are character, doesn't work once they've been converted to numeric
# dfsurv <- data.frame(data,time) 
# hist(dfsurv$time)
####################################################################################
#chisquared test
table <-table(data$Treatment,data$status)
table
test <- chisq.test(table)
test

# 1   2
# Acetone    116   7
# Cap High   116  10
# Cap Low    115  11
# Difen High 119   7
# Difen Low  108  12
# None       111  10
# 
# Pearson's Chi-squared test
# 
# data:  table
# X-squared = 2.6646, df = 5, p-value = 0.7515

test$statistic
test$p.value

###########by SEX
dfF <- data[which(data$Sex == "F"),]
tableF <- table(dfF$Treatment,dfF$status)#[2:7,]
testF <- chisq.test(tableF)
testF
# 1  2
# Acetone    56  4
# Cap High   52  8
# Cap Low    44  5
# Difen High 51  5
# Difen Low  46  5
# None       52  4
# 
# Pearson's Chi-squared test
# 
# data:  tableF
# X-squared = 2.0235, df = 5, p-value = 0.8459
#--------------------------2020 data ------------------------
# dfF <- df[which(df$Sex == "F"),]
# tableF <- table(dfF$Treatment,dfF$status)#[2:7,]
# 1  2
# Acetone  41 10
# Control  39 24
# Cyp High 35 23
# Cyp Low  45 24
# Cyp Med  40 17
# testF <- chisq.test(tableF)
# testF
# Pearson's Chi-squared test
# 
# data:  tableF
# X-squared = 6.4072, df = 4, p-value = 0.1707
#-----------------------------------------------------------------

dfM <- data[which(data$Sex == "M"),]
tableM <- table(dfM$Treatment,dfM$status)#[2:7,]
tableM
testM <- chisq.test(tableM)
testM
# 1  2
# Acetone    60  3
# Cap High   64  2
# Cap Low    71  6
# Difen High 68  2
# Difen Low  62  7
# None       59  6
# 
# Pearson's Chi-squared test
# 
# data:  tableM
# X-squared = 5.7809, df = 5, p-value = 0.3281
#--------------------------2020 data ------------------------
# dfM <- df[which(df$Sex == "M"),]
# tableM <- table(dfM$Treatment,dfM$status)#[2:7,]
# tableM
# 1  2
# Acetone  35 33
# Control  36 50
# Cyp High 47 39
# Cyp Low  42 41
# Cyp Med  48 37
# testM <- chisq.test(tableM)
# testM
# Pearson's Chi-squared test
# 
# data:  tableM
# X-squared = 4.3625, df = 4, p-value = 0.3592

############################################################################################################

# data$days.to.maturity <- as.numeric(data$spin.date-data$Date.closed)
# head(data$Date.opened)
data$larval.weight<-as.numeric(data$larval.weight)
 data2 <- subset(data, larval.weight <= .6)
hist(data$larval.weight)
hist(data$days.to.maturity)

#hist(data$Date.closed)


ggplot(data2, aes(x = Treatment.Group, y = larval.weight ))+
    geom_bar()
#larval weight box plot
qplot(Treatment.Group, larval.weight, data = data2, geom=c("boxplot", "jitter"),
      fill=Treatment.Group, main="Larval weight by group",
      xlab="", ylab="larval weight")
#larval weight over pollen weight box plot
qplot(Treatment.Group, larval.weight/Pollen.Weight, data = data2, geom=c("boxplot", "jitter"),
      fill=Treatment.Group, main="Larval weight/ Pollen weight by Treatment",
      xlab="", ylab="larval weight/ pollen weight")


qplot(Treatment.Group, larval.weight/Pollen.Weight, data = data2[which(data2$Sex == "F"),], geom=c("boxplot", "jitter"),
      fill=Treatment.Group, main="Larval weight/ Pollen weight by Treatment: Females",
      xlab="", ylab="larval weight/ pollen weight")

qplot(Treatment.Group, adult.weight/Pollen.Weight, data = data2[which(data2$Sex == "F"),], geom=c("boxplot", "jitter"),
      fill=Treatment.Group, main="Adult weight/ Pollen weight by Treatment: Females",
      xlab="", ylab="adult weight/ pollen weight")




#using ggplot to subset by sex - really weird plot
ggplot(subset(data, Treatment.Group %in% c("Acetone", "Cap Low", "Cap High", "Difen Low", "Difen High", "None") & Sex %in% c("F", "M")),
       aes(x = Sex, y = larval.weight/Pollen.Weight,  colour = interaction(Sex, Treatment.Group))) + facet_wrap( ~ Treatment.Group) +
    geom_point(alpha = 0.3, position = "jitter") +
    geom_boxplot(alpha = 0, colour = "black")+
    ggtitle("Larval Weight over Pollen Weight across Treatments and Sex")


#simple chi squared tests of pollen conversion to larval weight by treatment group and sex
# dfMpol <- data2[which(data2$Sex == "M"),]
# tableMpol <- table(dfMpol$Treatment,dfMpol$larval.weight/dfMpol$Pollen.Weight)
# tableMpol
# testM <- chisq.test(tableM)
# testM

#obviously have to do something else because this is a numeric variable and not just a count
####################################################################################
#reduce data to exclude treatment 1 ###################################################
#March03,2021:  the idea of this is to compare to heather & erin K paper, where control was acetone, and there was no negative control. We can see that the survival plot with only treatments 2-5 has acetone control doing the best (which is maybe to be expected if beneficial fungi keeping ascosphaera in check is killed by cyprodinil).
# IDEA #### How can we tell if cyprodinil is killing ascosphaera? Look at fungal sequence results!!!!
# dfsurv$Treatment <- as.numeric(dfsurv$Treatment)
# dfsurvAcetone <- dfsurv[which(dfsurv$Treatment >1),]
# dfsurv$Treatment <- as.factor(dfsurv$Treatment)
# dfsurvAcetone$Treatment <- as.factor(dfsurvAcetone$Treatment)
# dim(dfsurvAcetone)

#####################------------------SURVIVAL PLOTS

# already in csv     data$status <- as.numeric(data$status)
fit <- survfit(Surv(time, status) ~ Sex, data = data)
#survival plot of r treatments, excluding T1
summary(fit)
ggsurvplot(fit,data=data,
           pval = TRUE, 
           #conf.int = TRUE,
           #risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           # palette = c("#E7B800", "#2E9FDF"))
)

#coxph
res.cox <- coxph(Surv(time, status) ~ Treatment, data = data)
summary(res.cox)
ggsurvplot(survfit(res.cox),data=data,
           pval = TRUE, 
           #conf.int = TRUE,
           #risk.table = TRUE, # Add risk table
           #risk.table.col = "strata", # Change risk table color by groups
           #linetype = "strata", # Change line type by groups
           #surv.median.line = "hv", # Specify median survival
           #ggtheme = theme_bw(), # Change ggplot2 theme
           # palette = c("#E7B800", "#2E9FDF"),
           title = "Cox Proportional Hazard model"
)
#improve the model by adding terms
# class(data$Pollen.Weight)
res.cox2 <- coxph(Surv(time, status) ~ Treatment * Sex + Nest.Site + Experimenter, data = data)
summary(res.cox2)

#females - this shows a bit better the trend with both sexes
dataF <- data[which(data$Sex == "F"),]
fitF <- survfit(Surv(time, status) ~ Treatment, data = dataF)
summary(fitF)
ggsurvplot(fitF,
           pval = TRUE, 
           #conf.int = TRUE,
           #risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           # palette = c("#E7B800", "#2E9FDF"))
           title = "Females"
)
res.cox2 <- coxph(Surv(time, status) ~ Treatment, data = dataF)
summary(res.cox2)

res.cox2 <- coxph(Surv(time, status) ~ Treatment, data = dataM)
summary(res.cox2)

#males - show no trend of group difference
dataM <- data[which(data$Sex == "M"),]
fitM <- survfit(Surv(time, status) ~ Treatment, data = dataM)

ggsurvplot(fitM,
           pval = TRUE, 
           #conf.int = TRUE,
           #risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           title = "Males"
           # palette = c("#E7B800", "#2E9FDF"))
)

####################################################################################
#survival plot for all data
fit <- survfit(Surv(time, status) ~ Treatment, data = dfsurv)
print(fit)
summary(fit)
summary(fit)$table

d <- data.frame(time = fit$time,
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censor = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower
)
head(d)

#survival plot
ggsurvplot(fit,
           pval = TRUE, 
           #conf.int = TRUE,
           #risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           # palette = c("#E7B800", "#2E9FDF"))
)
ggsurvplot(fit)

#####################################################################################

####################################### Try separating by sex
#####################################################################################

#plot showing both sexes and all treatments, ten survival curves
fit <- survfit(Surv(time, status) ~ Treatment + Sex, data = dfsurv)
#females
dfsurvF <- dfsurv[which(dfsurv$Sex == "F"),]
fitF <- survfit(Surv(time, status) ~ Treatment, data = dfsurvF)

ggsurvplot(fitF,
           pval = TRUE, 
           #conf.int = TRUE,
           #risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           # palette = c("#E7B800", "#2E9FDF"))
           ggtitle = "Females"
)
#males
dfsurvM <- dfsurv[which(dfsurv$Sex == "M"),]
fitM <- survfit(Surv(time, status) ~ Treatment, data = dfsurvM)

ggsurvplot(fitM,
           pval = TRUE, 
           #conf.int = TRUE,
           #risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           # palette = c("#E7B800", "#2E9FDF"))
)

##############################################################################
#
#                METABOLIC RATE / POLLEN CONVERSION TO BODYMASS
#
##############################################################################

# class(data$days.to.maturity) <- "numeric"
#class(data$dead.on.exp.day) <- "numeric"
# class(data$Treatment) <- "character"

#simple table of mean larval weight, pollen weight
##############################################################################
df <- data
df$Pollen.weight.grams <- as.numeric(df$Pollen.Weight)
df$larval.weight <- as.numeric(df$larval.weight)
df$conversion.rate <-  as.numeric(df$larval.weight/df$Pollen.weight.grams)

dfconv <- data.frame(df,conversion.rate) #create new df to adhere to prev. code


######################## CONVERSION RATE PLOT ###################

library(scales)
library(plyr)
require(gridExtra)

# Run the functions length, mean, and sd on the value of "change" for each group, 
# broken down by sex + condition
cdata <- ddply(dfconv, c("Sex", "Treatment"), summarise,
               N    = sum(!is.na(conversion.rate)),
               mean = mean(conversion.rate, na.rm=TRUE),
               sd   = sd(conversion.rate, na.rm=TRUE),
               se   = sd / sqrt(N)
)
cdata

cdataM <- cdata[ which(cdata$Sex=='M'), ]
cdataF <- cdata[ which(cdata$Sex=='F'), ]


maleplot <- ggplot(cdataM, aes(y = mean, x = Treatment, fill = Treatment))+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  coord_cartesian(ylim=c(.45,.54))+
  ggtitle("Male Larvae")+
  ylab("mean Conversion Ratio \n (larval mass / pollen mass)")
femaleplot <- ggplot(cdataF, aes(y = mean, x = Treatment, fill = Treatment))+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  coord_cartesian(ylim=c(.45,.54))+
  ggtitle("Female Larvae") +
  ylab("mean Conversion Ratio \n (larval mass / pollen mass)")


grid.arrange(maleplot, femaleplot, ncol=2)
# try a boxplot instead
#using all data
ggboxplot(df, x = "Treatment", y = "conversion.rate", 
          color = "Treatment", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          #order = c("ctrl", "trt1", "trt2"),
          ylab = "Conversion Rate", xlab = "Treatment")

femplot <- ggboxplot(dfF, x = "Treatment", y = "conversion.rate", 
          color = "Treatment", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          #order = c("ctrl", "trt1", "trt2"),
          ylim = c(.3,.7),
          ylab = "Conversion Rate", xlab = "Treatment", title = "Females",
          legend = "right"
          )
malplot <- ggboxplot(dfM, x = "Treatment", y = "conversion.rate", 
          color = "Treatment", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          #order = c("ctrl", "trt1", "trt2"),
          ylim = c(.3,.7),
          ylab = "Conversion Rate", xlab = "Treatment", title = "Males",
          legend = "right")
grid.arrange(malplot, femplot, ncol=2)
malplot
femplot
#using means - NOPE
# ggboxplot(cdata, x = "Treatment", y = "mean", 
#           color = "Treatment", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#           #order = c("ctrl", "trt1", "trt2"),
#           ylab = "Conversion Rate", xlab = "Treatment")

#################################### STATS ##########################
#check out assumptions
dfM <-df[ which(df$Sex=='M'), ]
dfF <-df[ which(df$Sex=='F'), ]

hist(df$conversion.rate)
hist(dfM$conversion.rate)
hist(dfF$conversion.rate)

#one way anova
res.aovF <- aov(conversion.rate ~ Treatment, dfF)
res.aovF
summary(res.aovF)
plot(res.aovF)

res.aovM <- aov(conversion.rate ~ Treatment, dfM)
res.aovM
summary(res.aovM)
plot(res.aovM)

#conversion rate different across sexes?
res.aov <- aov(conversion.rate ~ Treatment*Sex, df)
res.aov
summary(res.aov)
plot(res.aov)

res.aov <- aov(conversion.rate ~ Treatment*Pollen.weight.grams + Nest.Site + Sex + Dev.stage, df)
res.aov
summary(res.aov)


#######################################################################

# cdata.l.w. <- ddply(df, "Sex", summarise,   # c("Sex","Treatment")
#                     N    = sum(!is.na(larval.weight)),
#                     mean = mean(larval.weight, na.rm=TRUE),
#                     sd   = sd(larval.weight, na.rm=TRUE),
#                     se   = sd / sqrt(N)
# )
# cdata.l.w.
# 
# cdata.p.w. <- ddply(df, "Sex", summarise,
#                     N    = sum(!is.na(Pollen.weight.grams)),
#                     mean = mean(Pollen.weight.grams, na.rm=TRUE),
#                     sd   = sd(Pollen.weight.grams, na.rm=TRUE),
#                     se   = sd / sqrt(N)
# )
# cdata.p.w.
# cdata <- ddply(df, "Sex", summarise,
#                N    = sum(!is.na(conversion.rate)),
#                mean = mean(conversion.rate, na.rm=TRUE),
#                sd   = sd(conversion.rate, na.rm=TRUE),
#                se   = sd / sqrt(N)
# )
# averages <- rbind(cdata.p.w.,cdata.l.w.,cdata)
# write.csv(averages, file = "averages.csv")
# write.csv(cdata.p.w., file = "Pollen.weight.Means.csv")

##############################################################################
#POLLEN CONVERSION
##############################################################################
ggplot(df, aes(x = Pollen.weight.grams, y = larval.weight , colour = Treatment)) +
  geom_point()+
  geom_smooth(method = lm, se=FALSE)

######################## REMOVE OUTLIERS ###################
#using a visual glance at the plot above of larval weight vs. pollen weight, we decide to remove pollen weights greater than 0.35, and larval weights greater than 0.125

# dfOUT <- df[ which(df$Pollen.weight.grams < 0.35 & df$larval.weight < 0.125), ]
# 
# ggplot(dfOUT, aes(x = Pollen.weight.grams, y = larval.weight , colour = Treatment)) +
#   geom_point()+
#   geom_smooth(method = lm, se=FALSE)
# 
# df <- dfOUT



# #separate by sex
# #Males 299 larvae
# dfM <- df[ which(df$Sex=='M'), ]
# #reduce data to only those larvae who matured 
# dfMmature <- dfM[which(!is.na(dfM$days.to.maturity)),]
# 
# maleplot <- ggplot(dfMmature, aes(x = Pollen.weight.grams, y = larval.weight , colour = Treatment))+
#   geom_point()+
#   geom_smooth(method= lm, se=FALSE) +
#   ggtitle("Male Larvae Mature")
# 
# 
# #Females 410 larvae
# dfF <- df[which(df$Sex == 'F'),]
# 
# dfFmature <- dfF[which(!is.na(dfF$days.to.maturity)),]
# 
# 
# femaleplot <- ggplot(dfFmature, aes(x = Pollen.weight.grams, y = larval.weight , colour = Treatment))+
#   geom_point()+
#   geom_smooth(method= lm, se=FALSE)+
#   ggtitle("Female Larvae Mature")
# 
# require(gridExtra)
# grid.arrange(maleplot, femaleplot, ncol=2) #place male and female plots side by side



##############################################################################
#Conversion.rate is na for lavrvae that died during the experiment and did not make it to spinning stage, because those larvae were not weighed

#stats ##############################################################################

library(scales)
library(plyr)
require(gridExtra)

# dfconvmature <- dfconv[which(!is.na(dfconv$days.to.maturity)),]
# #407 out of 706 larvae survived to spinning stage
# 
# # Run the functions length, mean, and sd on the value of "change" for each group, 
# # broken down by sex + condition
# cdatamat <- ddply(dfconvmature, c("Sex", "Treatment"), summarise,
#                   N    = sum(!is.na(conversion.rate)),
#                   mean = mean(conversion.rate, na.rm=TRUE),
#                   sd   = sd(conversion.rate, na.rm=TRUE),
#                   se   = sd / sqrt(N)
# )
# cdatamat
# 
# cdataM <- cdatamat[ which(cdatamat$Sex=='M'), ]
# cdataF <- cdatamat[ which(cdatamat$Sex=='F'), ]
# 
# 
# maleplot <- ggplot(cdataM, aes(y = mean, x = Treatment, fill = Treatment))+
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9))+
#   coord_cartesian(ylim=c(.45,.54))+
#   ggtitle("Male Larvae")+
#   ylab("mean Conversion Ratio \n (larval mass / pollen mass)")
# femaleplot <- ggplot(cdataF, aes(y = mean, x = Treatment, fill = Treatment))+
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9))+
#   coord_cartesian(ylim=c(.45,.54))+
#   ggtitle("Female Larvae") +
#   ylab("mean Conversion Ratio \n (larval mass / pollen mass)")
# 
# 
# grid.arrange(maleplot, femaleplot, ncol=2)

##############################################################################
# summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
#                       conf.interval=.95, .drop=TRUE) {
#   library(plyr)
#   
#   # New version of length which can handle NA's: if na.rm==T, don't count them
#   length2 <- function (x, na.rm=FALSE) {
#     if (na.rm) sum(!is.na(x))
#     else       length(x)
#   }
#   
#   # This does the summary. For each group's data frame, return a vector with
#   # N, mean, and sd
#   datac <- ddply(data, groupvars, .drop=.drop,
#                  .fun = function(xx, col) {
#                    c(N    = length2(xx[[col]], na.rm=na.rm),
#                      mean = mean   (xx[[col]], na.rm=na.rm),
#                      sd   = sd     (xx[[col]], na.rm=na.rm)
#                    )
#                  },
#                  measurevar
#   )
#   
#   # Rename the "mean" column    
#   datac <- rename(datac, c("mean" = measurevar))
#   
#   datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
#   
#   # Confidence interval multiplier for standard error
#   # Calculate t-statistic for confidence interval: 
#   # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
#   ciMult <- qt(conf.interval/2 + .5, datac$N-1)
#   datac$ci <- datac$se * ciMult
#   
#   return(datac)
# }
# ##############################################################################
# #USAGE OF ABOVE CODE
# summarySE(dfconv, measurevar = dfconv$conversion.rate, groupvars= c("Sex", "Treatment"), na.rm=TRUE)
# #not sure - error
##############################################################################




#separate by sex
#Males 299 larvae
# dfconvM <- dfconv[ which(dfconv$Sex=='M'), ]


# 
# #maleplot <- 
# maleplot <- ggplot(cdataM, aes(y = conversion.rate, x = Treatment, fill = Treatment))+
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=conversion.rate-se, ymax=conversion.rate+se),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9))+
#   ggtitle("Male Larvae")
# # Error bars represent standard error of the mean
# 
# #Females 410 larvae
# # dfconvF <- dfconv[which(dfconv$Sex == 'F'),]
# 
# femaleplot <- ggplot(cdataF, aes(y = conversion.rate, x = Treatment, colour = Treatment))+
#   geom_histogram(stat = 'identity')+
#   ggtitle("Female Larvae")
# 
# require(gridExtra)
# grid.arrange(maleplot, femaleplot, ncol=2) #place male and female plots side by side




