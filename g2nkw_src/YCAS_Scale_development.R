title:  "The Youth and Childhood Adversity Scale: 
         A step towards developing a new measure of adversity and its severity"

authors: "blinded for peer review"

setwd("")#set your wd here

###############################################################

######Content###########
#1. Load required packages
#2. Load data and prepare them for furhter analyses
#3. Descriptive Statistics
#4. Exploratory Factor analysis
#5. Confirmatory Factor analysis
#6. Validation analyses
#7. Compare prevalences between Resist and Share

###############################################################
#1. Load required packages
library (readr)
library (haven)
library (psych)
library (car)
library (sirt)
library (GPArotation)
library (lavaan)
library (dplyr)
library (fabs)

###############################################################
#2. Load data and prepare them for furhter analyses
data  <- read_csv("")#RESIST data file csv format
data1 <- read_dta("")#SHARE data file dta format

#yes/no RESIST 
dataYN  <- data.frame(data$yts_1, data$yts_2, data$yts_3, data$yts_4, data$yts_5, data$yts_6,
                      data$yts_7, data$yts_8, data$yts_9, data$yts_10,data$yts_11,data$yts_12, 
                      data$yts_13)

#severity RESIST
dataseverity  <- data.frame(data$yts_1a_full, data$yts_2a_full, data$yts_3a_full, data$yts_4a_full,
                            data$yts_5a_full, data$yts_6a_full, data$yts_7a_full, data$yts_8a_full,
                            data$yts_9a_full, data$yts_10a_full,data$yts_11a_full,data$yts_12a_full,
                            data$yts_13a_full)

#yes/no SHARE
data1YN  <- data.frame(data1$cteq_1, data1$cteq_2, data1$cteq_3, data1$cteq_4,
                       data1$cteq_5, data1$cteq_7, data1$cteq_8, data1$cteq_9, 
                       data1$cteq_10, data1$cteq_11, data1$cteq_12, data1$cteq_13)

data1severity  <- data.frame(data1$cteq_1a, data1$cteq_2a, data1$cteq_3a, data1$cteq_4a,
                             data1$cteq_5a, data1$cteq_7a, data1$cteq_8a, data1$cteq_9a, 
                             data1$cteq_10a, data1$cteq_11a, data1$cteq_12a, data1$cteq_13b)

###############################################################
#3.Descriptive Statistics
#yes/no percentage RESIST
100 * prop.table(table(dataYN$data.yts_1))
100 * prop.table(table(dataYN$data.yts_2))
100 * prop.table(table(dataYN$data.yts_3))
100 * prop.table(table(dataYN$data.yts_4))
100 * prop.table(table(dataYN$data.yts_5))
100 * prop.table(table(dataYN$data.yts_6))
100 * prop.table(table(dataYN$data.yts_7))
100 * prop.table(table(dataYN$data.yts_8))
100 * prop.table(table(dataYN$data.yts_9))
100 * prop.table(table(dataYN$data.yts_10))
100 * prop.table(table(dataYN$data.yts_11))
100 * prop.table(table(dataYN$data.yts_12))
100 * prop.table(table(dataYN$data.yts_13))

#yes/no percentage SHARE
100 * prop.table(table(data1YN$data1.cteq_1))
100 * prop.table(table(data1YN$data1.cteq_2))
100 * prop.table(table(data1YN$data1.cteq_3))
100 * prop.table(table(data1YN$data1.cteq_4))
100 * prop.table(table(data1YN$data1.cteq_5))
100 * prop.table(table(data1YN$data1.cteq_7))
100 * prop.table(table(data1YN$data1.cteq_8))
100 * prop.table(table(data1YN$data1.cteq_9))
100 * prop.table(table(data1YN$data1.cteq_10))
100 * prop.table(table(data1YN$data1.cteq_11))
100 * prop.table(table(data1YN$data1.cteq_12))
100 * prop.table(table(data1YN$data1.cteq_13))

describe (dataYN) #yes/no Resist
describe (data1YN)#yes/no SHARE
describe (dataseverity) #severity Resist
describe (data1severity) #Severity SHARE

#severity only for particpants indicating yes
dataseveritya = dataseverity
data1severitya = data1severity
dataseveritya[dataseveritya == 0] <- NA
data1severitya[data1severitya == 0] <- NA
describe (dataseveritya) #severity Resist
describe (data1severitya) #Severity SHARE

###############################################################
#4. Exploratory Factor analysis

#4.1 yes/no RESIST
R1 <- polychoric2(dataYN)$rho
nfact  <-13 
factors.pc1 <- principal(R1, nfact)
plot(c(1:nfact), factors.pc1$values, type = "b", xlab = "Factor", ylab = "Eigenvalue")
fa.parallel(R1,n.obs=nrow(R1),fa="fa",main="Parallel Analysis Scree Plots")
faYN <- fa (R1, 1, fm = "wls", rotate = "varimax")
print (faYN, digits = 2)
omega (R1,nfactors=1) 

#4.2 severity RESIST 
R2 <- polychoric2(dataseverity)$rho
nfact  <-13 
factors.pc2 <- principal(R2, nfact)
plot(c(1:nfact), factors.pc2$values, type = "b", xlab = "Factor", ylab = "Eigenvalue")
fa.parallel(R2,n.obs=nrow(R2),fa="fa",main="Parallel Analysis Scree Plots")
faYN <- fa (R2, 1, fm = "wls", rotate = "varimax")
print (faYN, digits = 2)
omega (R2,nfactors=1) 

#4.3 yes/no SHARE
R3 <- polychoric2(data1YN)$rho
nfact2  <-12 
factors.pc3 <- principal(R3, nfact2)
plot(c(1:nfact2), factors.pc3$values, type = "b", xlab = "Factor", ylab = "Eigenvalue")
fa.parallel(R3,n.obs=nrow(R3),fa="fa",main="Parallel Analysis Scree Plots")
faYN <- fa (R3, 1, fm = "wls", rotate = "varimax")
print (faYN, digits = 2)
omega (R3, nfactors = 1) 

#4.4 severity SHARE
R4 <- polychoric2(data1severity)$rho
nfact2  <-12 
factors.pc4 <- principal(R4, nfact2)
plot(c(1:nfact2), factors.pc4$values, type = "b", xlab = "Factor", ylab = "Eigenvalue")
fa.parallel(R4,n.obs=nrow(R4),fa="fa",main="Parallel Analysis Scree Plots")
faYN <- fa (R4, 1, fm = "wls", rotate = "varimax")
print (faYN, digits = 2)
omega (R4,nfactors=1) 

###############################################################
#5. Confirmatory Factor analysis
#To replicate our results without item 1, delete yts_1 from the analyses

#5.1 yes/no RESIST
dataCFA1 <- na.omit(data.frame(data$id,data$yts_1, data$yts_2, data$yts_3, 
                               data$yts_4,  data$yts_5, data$yts_6,  data$yts_7,
                               data$yts_8,  data$yts_9, data$yts_10, data$yts_11, 
                               data$yts_12, data$yts_13))

colnames (dataCFA1) <-c("id","yts_1","yts_2","yts_3","yts_4","yts_5","yts_6", 
                        "yts_7","yts_8","yts_9","yts_10","yts_11","yts_12",
                        "yts_13")

model1 <- 'EA =~ yts_1 + yts_2  + yts_3  +  yts_4  +
                 yts_5 + yts_6  + yts_7  +  yts_8  +
                 yts_9 + yts_10 + yts_11 +  yts_12 + 
                 yts_13'

fit1 <- cfa(model1, data=dataCFA1, meanstructure=T, std.lv=T, estimator = "WLSMV",
            ordered = c("yts_1", "yts_2",  "yts_3",  "yts_4", 
                        "yts_5", "yts_6",  "yts_7",  "yts_8",
                        "yts_9", "yts_10", "yts_11", "yts_12", 
                        "yts_13"))

summary(fit1, fit.measures=TRUE, standardized = T)
fitMeasures(fit1,c("chisq","df","pvalue","cfi","tli","rmsea",
                   "wrmr",'rmsea.ci.lower','rmsea.ci.upper'))
modindices(fit1, sort. = T)

####get factorscores and include them in origingal data
dataCFA1$factorscore1 <- predict(fit1)
data <- left_join(data, dataCFA1,  by = "id")
data$factorscore1

#5.2 severity RESIST
dataCFA2 <- na.omit (data.frame (data$id, data$yts_1a_full, data$yts_2a_full,
                                 data$yts_3a_full,  data$yts_4a_full,  data$yts_5a_full,
                                 data$yts_6a_full,  data$yts_7a_full,  data$yts_8a_full, 
                                 data$yts_9a_full,  data$yts_10a_full, data$yts_11a_full, 
                                 data$yts_12a_full, data$yts_13a_full))

colnames (dataCFA2) <-c("id","yts_1a_full","yts_2a_full","yts_3a_full","yts_4a_full",
                        "yts_5a_full","yts_6a_full", "yts_7a_full","yts_8a_full",
                        "yts_9a_full","yts_10a_full","yts_11a_full","yts_12a_full",
                        "yts_13a_full")

model2 <- 'EA =~ yts_1a_full + yts_2a_full  + yts_3a_full  +  yts_4a_full  +
                 yts_5a_full + yts_6a_full  + yts_7a_full  +  yts_8a_full  +
                 yts_9a_full + yts_10a_full + yts_11a_full +  yts_12a_full + 
                 yts_13a_full'

fit2 <- cfa(model2, data=dataCFA2, meanstructure=T, std.lv=T, estimator = "WLSMV",
            ordered = c("yts_1a_full", "yts_2a_full",  "yts_3a_full",  "yts_4a_full", 
                        "yts_5a_full", "yts_6a_full",  "yts_7a_full",  "yts_8a_full",
                        "yts_9a_full", "yts_10a_full", "yts_11a_full", "yts_12a_full", 
                        "yts_13a_full"))

summary(fit2, fit.measures=TRUE, standardized = T)
fitMeasures(fit2,c("chisq","df","pvalue","cfi","tli","rmsea",
                   "wrmr",'rmsea.ci.lower','rmsea.ci.upper'))
modindices(fit2, sort. = T)

dataCFA2$factorscore2 <- predict(fit2)
data <- left_join(data, dataCFA2,  by = "id")

#5.3 yes/no SHARE
dataCFA3 <- na.omit(data.frame(data1$record_id, data1$cteq_1, data1$cteq_2,
                               data1$cteq_3,  data1$cteq_4,  data1$cteq_5, 
                               data1$cteq_7,  data1$cteq_8,  data1$cteq_9, 
                               data1$cteq_10, data1$cteq_11, data1$cteq_12,
                               data1$cteq_13))

colnames (dataCFA3) <-c("record_id","yts_1","yts_2","yts_3","yts_4","yts_5",
                        "yts_6", "yts_7","yts_8","yts_9","yts_10","yts_11","yts_12")

model3 <- 'EA =~ yts_1 + yts_2  + yts_3  +  yts_4  +
                 yts_5 + yts_6  + yts_7  +  yts_8  +
                 yts_9 + yts_10 + yts_11 +  yts_12 '

fit3 <- cfa(model3, data=dataCFA3, meanstructure=T, std.lv=T, estimator = "WLSMV",
            ordered = c("yts_1", "yts_2",  "yts_3",  "yts_4", 
                        "yts_5", "yts_6",  "yts_7",  "yts_8",
                        "yts_9", "yts_10", "yts_11", "yts_12"))

modindices(fit3, sort. = T)
summary(fit3, fit.measures=TRUE, standardized = T)
fitMeasures(fit3,c("chisq","df","pvalue","cfi","tli","rmsea",
                   "wrmr",'rmsea.ci.lower','rmsea.ci.upper'))

####get factorscores and include them in origingal data
dataCFA3$factorscore3 <- predict(fit3)
data1 <- left_join(data1, dataCFA3,  by = "record_id")
data1$factorscore3

#5.4 severity SHARE
dataCFA4 <- na.omit(data.frame(data1$record_id, data1$cteq_1a, data1$cteq_2a,
                               data1$cteq_3a,  data1$cteq_4a,  data1$cteq_5a, 
                               data1$cteq_7a,  data1$cteq_8a,  data1$cteq_9a, 
                               data1$cteq_10a, data1$cteq_11a, data1$cteq_12a,
                               data1$cteq_13b))

colnames (dataCFA4) <-c("record_id","yts_1","yts_2","yts_3","yts_4","yts_5",
                        "yts_6", "yts_7","yts_8","yts_9","yts_10","yts_11","yts_12")

model4 <- 'EA =~ yts_1 + yts_2  + yts_3  +  yts_4  +
                 yts_5 + yts_6  + yts_7  +  yts_8  +
                 yts_9 + yts_10 + yts_11 +  yts_12 '

fit4 <- cfa(model4, data=dataCFA4, meanstructure=T, std.lv=T, estimator = "WLSMV",
            ordered = c("yts_1", "yts_2",  "yts_3",  "yts_4", 
                        "yts_5", "yts_6",  "yts_7",  "yts_8",
                        "yts_9", "yts_10", "yts_11", "yts_12"))

modindices(fit4, sort. = T)
summary(fit4, fit.measures=TRUE, standardized = T)
fitMeasures(fit4,c("chisq","df","pvalue","cfi","tli","rmsea",
                   "wrmr",'rmsea.ci.lower','rmsea.ci.upper'))

####get factorscores and include them in origingal data
dataCFA4$factorscore4 <- predict(fit4)
data1 <- left_join(data1, dataCFA4,  by = "record_id")
data1$factorscore4

###############################################################
#6. Validation analyses
#6.1 Resist
data$sumscore1 =  data$yts_1.x + data$yts_2.x +  data$yts_3.x +  data$yts_4.x + 
                  data$yts_5.x + data$yts_6.x +  data$yts_7.x +  data$yts_8.x +
                  data$yts_9.x + data$yts_10.x + data$yts_11.x + data$yts_12.x +
                  data$yts_13.x

data$sumscore2 =  data$yts_1a_full.x + data$yts_2a_full.x + data$yts_3a_full.x +
                  data$yts_4a_full.x + data$yts_5a_full.x + data$yts_6a_full.x + 
                  data$yts_7a_full.x + data$yts_8a_full.x + data$yts_9a_full.x + 
                  data$yts_10a_full.x + data$yts_11a_full.x + data$yts_12a_full.x +
                  data$yts_13a_full.x

data$maltreatment = data$ccms_1a + data$ccms_2a + data$ccms_3a +
                    data$ccms_4a + data$ccms_5a + data$ccms_6a

Resist <- corr.test (data.frame(data$factorscore1,data$factorscore2,
                                data$sumscore1, data$sumscore2,data$rses_sum,
                                data$w_sum, data$pss_sum, data$maltreatment, 
                                data$medication, data$therapy))

print (Resist, short = F, digits = 3)

#6.2 SHARE
data1$sumscore1 <- data1$cteq_1 +  data1$cteq_2 +  data1$cteq_3 +  data1$cteq_4 + 
                   data1$cteq_5 +  data1$cteq_7 +  data1$cteq_8 +  data1$cteq_9 + 
                   data1$cteq_10 + data1$cteq_11 + data1$cteq_12 + data1$cteq_13

data1$sumscore2 <- data1$cteq_1a  + data1$cteq_2a  + data1$cteq_3a  + data1$cteq_4a + 
                   data1$cteq_5a  + data1$cteq_7a  + data1$cteq_8a  + data1$cteq_9a + 
                   data1$cteq_10a + data1$cteq_11a + data1$cteq_12a + data1$cteq_13b 
                       

SHARE <- corr.test (data.frame(data1$factorscore3,data1$factorscore4,
                                data1$sumscore1, data1$sumscore2, 
                                data1$scared_tot, data1$mfqmean, 
                                data1$arimean, data1$rsesmean,
                                data1$nssi_1, data1$nssi_2))

print (SHARE, short = F, digits = 3)

###############################################################
#7. Compare prevalences between Resist and Share
#yes/no
a  <- data.frame(data$yts_1, data$yts_2, data$yts_3, data$yts_4, data$yts_5, data$yts_6,
                 data$yts_7, data$yts_8, data$yts_9, data$yts_10,data$yts_11, data$yts_13)

colnames (a) <-c("YCAS1", "YCAS2", "YCAS3", "YCAS4", "YCAS5",
                 "YCAS6", "YCAS7", "YCAS8", "YCAS9", "YCAS10",
                 "YCAS11", "YCAS12")

a$Study = "RESIST"

b  <- data.frame(data1$cteq_1, data1$cteq_2, data1$cteq_3, data1$cteq_4,
                 data1$cteq_5, data1$cteq_7, data1$cteq_8, data1$cteq_9, 
                 data1$cteq_10, data1$cteq_11, data1$cteq_12, data1$cteq_13)

colnames (b) <-c("YCAS1", "YCAS2", "YCAS3", "YCAS4", "YCAS5",
                 "YCAS6", "YCAS7", "YCAS8", "YCAS9", "YCAS10",
                 "YCAS11", "YCAS12")

b$Study = "SHARE"
c = rbind (a,b)
c[, 1:12] <- lapply(c[, 1:12], ordered)

#Comparison of YES/NO
YCAS1 <- glm(YCAS1 ~  Study, data = c, family = "binomial")
summary (YCAS1)
YCAS2 <- glm(YCAS2 ~  Study, data = c, family = "binomial")
summary (YCAS2) #
YCAS3 <- glm(YCAS3 ~  Study, data = c, family = "binomial")
summary (YCAS3) 
YCAS4 <- glm(YCAS4 ~  Study, data = c, family = "binomial")
summary (YCAS4)
YCAS5 <- glm(YCAS5 ~  Study, data = c, family = "binomial")
summary (YCAS5)
YCAS6 <- glm(YCAS6 ~  Study, data = c, family = "binomial")
summary (YCAS6)
YCAS7 <- glm(YCAS7 ~  Study, data = c, family = "binomial")
summary (YCAS7)
YCAS8 <- glm(YCAS8 ~  Study, data = c, family = "binomial")
summary (YCAS8)
YCAS9 <- glm(YCAS9 ~  Study, data = c, family = "binomial")
summary (YCAS9)
YCAS10 <- glm(YCAS10 ~  Study, data = c, family = "binomial")
summary (YCAS10)#
YCAS11 <- glm(YCAS11 ~  Study, data = c, family = "binomial")
summary (YCAS11) #
YCAS12 <- glm(YCAS12 ~  Study, data = c, family = "binomial")
summary (YCAS12) #

#severity
d <- data.frame(data$yts_1a_full, data$yts_2a_full, data$yts_3a_full, data$yts_4a_full,
                data$yts_5a_full, data$yts_6a_full, data$yts_7a_full, data$yts_8a_full,
                data$yts_9a_full, data$yts_10a_full,data$yts_11a_full,data$yts_13a_full)

colnames (d) <-c("YCAS1", "YCAS2", "YCAS3", "YCAS4", "YCAS5",
                 "YCAS6", "YCAS7", "YCAS8", "YCAS9", "YCAS10",
                 "YCAS11", "YCAS12")

d$Study = "RESIST"

e  <- data.frame(data1$cteq_1a, data1$cteq_2a, data1$cteq_3a, data1$cteq_4a,
                 data1$cteq_5a, data1$cteq_7a, data1$cteq_8a, data1$cteq_9a, 
                 data1$cteq_10a, data1$cteq_11a, data1$cteq_12a, data1$cteq_13b)

colnames (e) <-c("YCAS1", "YCAS2", "YCAS3", "YCAS4", "YCAS5",
                 "YCAS6", "YCAS7", "YCAS8", "YCAS9", "YCAS10",
                 "YCAS11", "YCAS12")

e$Study = "Share"

f = rbind (d,e)
f[f == 0] <- NA

#Comparison of YES/NO
t.test(f$YCAS1 ~ f$Study)
t.test(f$YCAS2 ~ f$Study)
t.test(f$YCAS3 ~ f$Study)
t.test(f$YCAS4 ~ f$Study)
t.test(f$YCAS5 ~ f$Study) 
t.test(f$YCAS6 ~ f$Study)
t.test(f$YCAS7 ~ f$Study)
t.test(f$YCAS8 ~ f$Study)
t.test(f$YCAS9 ~ f$Study)
t.test(f$YCAS10 ~ f$Study)
t.test(f$YCAS11 ~ f$Study)
t.test(f$YCAS12 ~ f$Study)
