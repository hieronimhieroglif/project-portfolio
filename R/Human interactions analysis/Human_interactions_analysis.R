# Autor: Sebastian Boruch
setwd("C:\\Users\\Sebastian\\Desktop\\Praca magisterska\\ESS8e01.stata")
library(foreign)
library(dplyr)
library(plyr)
library(car)
library(mice)
library(MASS)
library(glmnet)
library(usdm)
library(psych)
library(corrplot)
library(olsrr)

ess <- read.dta("ESS8e01.dta")
myvars <- c("chldhhe", "dvrcdeva", "facntr", "happy", "hhmmb", "inprdsc", "iphlppl",
            "iplylfr", "iprspot", "ipudrst", "jbspv", "lvgptnea", "mocntr",  "rshpsts", "sclmeet")

mydata <- ess[myvars]
attach(mydata)
summary(mydata)
#str(mydata)

mydata$hhmmb[mydata$hhmmb > 20] <- NA

happy2 <- as.factor(happy)
happy2 <- revalue(happy2, c("Extremely unhappy"=0))
happy2 <- revalue(happy2, c("Extremely happy"=10))
happy2 <- revalue(happy2, c("Refusal"=NA))
happy2 <- revalue(happy2, c("Don't know"=NA))
happy2 <- revalue(happy2, c("No answer"=NA))
happy2 <- as.numeric(levels(happy2))[happy2]

chldhhe2 <- as.factor(chldhhe)
chldhhe2 <- revalue(chldhhe2, c("Yes"=1))
chldhhe2 <- revalue(chldhhe2, c("No"=0))
chldhhe2 <- revalue(chldhhe2, c("Not applicable"=NA))
chldhhe2 <- revalue(chldhhe2, c("Refusal"=NA))
chldhhe2 <- revalue(chldhhe2, c("Don't know"=NA))
chldhhe2 <- revalue(chldhhe2, c("No answer"=NA))
chldhhe2 <-as.numeric(levels(chldhhe2))[chldhhe2]

jbspv2 <- as.factor(jbspv)
jbspv2 <- revalue(jbspv2, c("Yes"=1))
jbspv2 <- revalue(jbspv2, c("No"=0))
jbspv2 <- revalue(jbspv2, c("Not applicable"=NA))
jbspv2 <- revalue(jbspv2, c("Refusal"=NA))
jbspv2 <- revalue(jbspv2, c("Don't know"=NA))
jbspv2 <- revalue(jbspv2, c("No answer"=NA))
jbspv2 <-as.numeric(levels(jbspv2))[jbspv2]

lvgptnea2 <- as.factor(lvgptnea)
lvgptnea2 <- revalue(lvgptnea2, c("Yes"=1))
lvgptnea2 <- revalue(lvgptnea2, c("No"=0))
lvgptnea2 <- revalue(lvgptnea2, c("Not applicable"=NA))
lvgptnea2 <- revalue(lvgptnea2, c("Refusal"=NA))
lvgptnea2 <- revalue(lvgptnea2, c("Don't know"=NA))
lvgptnea2 <- revalue(lvgptnea2, c("No answer"=NA))
lvgptnea2 <-as.numeric(levels(lvgptnea2))[lvgptnea2]

dvrcdeva2 <- as.factor(dvrcdeva)
dvrcdeva2 <- revalue(dvrcdeva2, c("Yes"=1))
dvrcdeva2 <- revalue(dvrcdeva2, c("No"=0))
dvrcdeva2 <- revalue(dvrcdeva2, c("Refusal"=NA))
dvrcdeva2 <- revalue(dvrcdeva2, c("Don't know"=NA))
dvrcdeva2 <- revalue(dvrcdeva2, c("No answer"=NA))
dvrcdeva2 <- as.numeric(levels(dvrcdeva2))[dvrcdeva2]

facntr2 <- as.factor(facntr)
facntr2 <- revalue(facntr2, c("Yes"=1))
facntr2 <- revalue(facntr2, c("No"=0))
facntr2 <- revalue(facntr2, c("Refusal"=NA))
facntr2 <- revalue(facntr2, c("Don't know"=NA))
facntr2 <- revalue(facntr2, c("No answer"=NA))
facntr2 <- as.numeric(levels(facntr2))[facntr2]

mocntr2 <- as.factor(mocntr)
mocntr2 <- revalue(mocntr2, c("Yes"=1))
mocntr2 <- revalue(mocntr2, c("No"=0))
mocntr2 <- revalue(mocntr2, c("Refusal"=NA))
mocntr2 <- revalue(mocntr2, c("Don't know"=NA))
mocntr2 <- revalue(mocntr2, c("No answer"=NA))
mocntr2 <- as.numeric(levels(mocntr2))[mocntr2]

inprdsc2 <- as.factor(inprdsc)
inprdsc2 <- revalue(inprdsc2, c("None"=0))
inprdsc2 <- revalue(inprdsc2, c("4-6"=5))
inprdsc2 <- revalue(inprdsc2, c("7-9"=8))
inprdsc2 <- revalue(inprdsc2, c("10 or more"=10))
inprdsc2 <- revalue(inprdsc2, c("Refusal"=NA))
inprdsc2 <- revalue(inprdsc2, c("Don't know"=NA))
inprdsc2 <- revalue(inprdsc2, c("No answer"=NA))
inprdsc2 <- as.numeric(levels(inprdsc2))[inprdsc2]

iphlppl2 <- as.factor(iphlppl)
iphlppl2 <- revalue(iphlppl2, c("Very much like me"=5))
iphlppl2 <- revalue(iphlppl2, c("Like me"=4))
iphlppl2 <- revalue(iphlppl2, c("Somewhat like me"=3))
iphlppl2 <- revalue(iphlppl2, c("A little like me"=2))
iphlppl2 <- revalue(iphlppl2, c("Not like me"=1))
iphlppl2 <- revalue(iphlppl2, c("Not like me at all"=0))
iphlppl2 <- revalue(iphlppl2, c("Refusal"=NA))
iphlppl2 <- revalue(iphlppl2, c("Don't know"=NA))
iphlppl2 <- revalue(iphlppl2, c("No answer"=NA))
iphlppl2 <- as.numeric(levels(iphlppl2))[iphlppl2]

iplylfr2 <- as.factor(iplylfr)
iplylfr2 <- revalue(iplylfr2, c("Very much like me"=5))
iplylfr2 <- revalue(iplylfr2, c("Like me"=4))
iplylfr2 <- revalue(iplylfr2, c("Somewhat like me"=3))
iplylfr2 <- revalue(iplylfr2, c("A little like me"=2))
iplylfr2 <- revalue(iplylfr2, c("Not like me"=1))
iplylfr2 <- revalue(iplylfr2, c("Not like me at all"=0))
iplylfr2 <- revalue(iplylfr2, c("Refusal"=NA))
iplylfr2 <- revalue(iplylfr2, c("Don't know"=NA))
iplylfr2 <- revalue(iplylfr2, c("No answer"=NA))
iplylfr2 <- as.numeric(levels(iplylfr2))[iplylfr2]

iprspot2 <- as.factor(iprspot)
iprspot2 <- revalue(iprspot2, c("Very much like me"=5))
iprspot2 <- revalue(iprspot2, c("Like me"=4))
iprspot2 <- revalue(iprspot2, c("Somewhat like me"=3))
iprspot2 <- revalue(iprspot2, c("A little like me"=2))
iprspot2 <- revalue(iprspot2, c("Not like me"=1))
iprspot2 <- revalue(iprspot2, c("Not like me at all"=0))
iprspot2 <- revalue(iprspot2, c("Refusal"=NA))
iprspot2 <- revalue(iprspot2, c("Don't know"=NA))
iprspot2 <- revalue(iprspot2, c("No answer"=NA))
iprspot2 <- as.numeric(levels(iprspot2))[iprspot2]

ipudrst2 <- as.factor(ipudrst)
ipudrst2 <- revalue(ipudrst2, c("Very much like me"=5))
ipudrst2 <- revalue(ipudrst2, c("Like me"=4))
ipudrst2 <- revalue(ipudrst2, c("Somewhat like me"=3))
ipudrst2 <- revalue(ipudrst2, c("A little like me"=2))
ipudrst2 <- revalue(ipudrst2, c("Not like me"=1))
ipudrst2 <- revalue(ipudrst2, c("Not like me at all"=0))
ipudrst2 <- revalue(ipudrst2, c("Refusal"=NA))
ipudrst2 <- revalue(ipudrst2, c("Don't know"=NA))
ipudrst2 <- revalue(ipudrst2, c("No answer"=NA))
ipudrst2 <- as.numeric(levels(ipudrst2))[ipudrst2]

sclmeet2 <- as.factor(sclmeet)
sclmeet2 <- revalue(sclmeet2, c("Never"=0))
sclmeet2 <- revalue(sclmeet2, c("Less than once a month"=1))
sclmeet2 <- revalue(sclmeet2, c("Once a month"=2))
sclmeet2 <- revalue(sclmeet2, c("Several times a month"=3))
sclmeet2 <- revalue(sclmeet2, c("Once a week"=4))
sclmeet2 <- revalue(sclmeet2, c("Several times a week"=5))
sclmeet2 <- revalue(sclmeet2, c("Every day"=6))
sclmeet2 <- revalue(sclmeet2, c("Refusal"=NA))
sclmeet2 <- revalue(sclmeet2, c("Don't know"=NA))
sclmeet2 <- revalue(sclmeet2, c("No answer"=NA))
sclmeet2 <- as.numeric(levels(sclmeet2))[sclmeet2]


mydata2 <- cbind.data.frame(chldhhe2, dvrcdeva2, facntr2, happy2, inprdsc2, iphlppl2, iplylfr2,
                            iprspot2, ipudrst2, jbspv2, lvgptnea2, mocntr2, sclmeet2, hhmmb)
mydata2 <- rename(mydata2, c("chldhhe2"="chldhhe", "dvrcdeva2"="dvrcdeva", "facntr2"="facntr", "happy2"="happy",
                                       "inprdsc2"="inprdsc", "iphlppl2"="iphlppl", "iplylfr2"="iplylfr", "iprspot2"= "iprspot",
                                       "ipudrst2"="ipudrst", "jbspv2"="jbspv", "lvgptnea2"= "lvgptnea", "mocntr2"="mocntr",
                                       "sclmeet2"="sclmeet"))
# mydata2- only numerical variables
mean(is.na(mydata2)) # Bodner (2008) and White et al (2011) % NA=imputation number(m)- here: 5

imputed_Data <- mice(mydata2, m=5, maxit = 20, method = 'pmm') # method predictive mean matching

summary(imputed_Data)
densityplot(imputed_Data) 
completedata <- complete(imputed_Data,2)
detach(mydata)
attach(mydata2)

# collinearity
newdatacor = cor(completedata[1:14], method= "pearson")
corrplot(newdatacor, method = "number")

parntr<- mocntr*facntr
completedata <- mutate(completedata, parntr= mocntr*facntr)
# summary(completedata)

corrvars <- names(completedata) %in% c("mocntr", "facntr")
regressiondata <- completedata[!corrvars]
# summary(regressiondata)

# linear regression
lm.happy <- lm(happy~., data=regressiondata)
summary(lm.happy)
AIC(lm.happy)
k <- ols_all_subset(lm.happy)
plot(k)

# LASSO
x <- model.matrix(happy~., -1, data = regressiondata)
y <- regressiondata$happy
fit.lasso <- glmnet(x, y, family="multinomial")
# plot(fit.lasso, xvar="lambda", label=TRUE)


cv.lasso <- cv.glmnet(x, y)
plot(cv.lasso)
coef(cv.lasso)

# linear regression No. 2
lm.happy2 <- lm(happy~dvrcdeva+ inprdsc+ iphlppl+ iplylfr+ ipudrst+
                  jbspv+ sclmeet+ hhmmb, data=regressiondata)
summary(lm.happy2)
AIC(lm.happy2)

