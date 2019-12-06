library(tidyverse)
library(MatchIt)
data("lalonde", package = "cobalt")
library("cobalt")

library(tableone)
library(ipw)
library(sandwich)
library(survey)

#load data

data2 <- read.csv("/Users/shawnmartin/NEU/Fall_2019/DS/Advertisement_Effectivness_Causal_Inference/data/data_2.csv")

nrow(data2)

#calculte propensity score using logistic regression
psmodel <- glm(ad.group ~ age + prev.purchase, family = binomial(link = "logit"), data = data2)

ps<-predict(psmodel, type = "response")

data2_t <- data2 %>%
  mutate(ps_score = ps)

#visualize the propensity score result to check the overlap
a = data2_t %>%
  filter(ad.group == 1) %>%
  select(ps_score)

b = data2_t %>%
  filter(ad.group == 0) %>%
  select(ps_score)

ggplot() + geom_histogram(aes(x = a$ps_score, y = ..density..), fill = "blue", bins = 30) +
  geom_histogram(aes(x = b$ps_score, y = -..density..), fill = "red", bins = 30)

# Get the Table One before IPTW
mydata <- data2%>%
  select(c(age, prev.purchase, ad.group, purchased.after.visit))

AGE <- as.numeric(data2$age)
PREV <- as.numeric(data2$prev.purchase)
TRT <- as.numeric(data2$ad.group)

table_data <- cbind(AGE, PREV, TRT)
table_data <- data.frame(table_data)
table1 <- CreateTableOne(vars=c("AGE", "PREV"), strata = "TRT", data = table_data, test = FALSE)
print(table1, smd=TRUE)

#create weights
weight <- ifelse(data2$ad.group==1, 1/(ps), 1/(1-ps))

#apply weights to data
weighteddata<-svydesign(ids = ~1, data = mydata, weights = ~weight)

#weighted table 1
weightedtable <- svyCreateTableOne(vars = c("age", "prev.purchase"), strata = "ad.group", data = weighteddata, test = FALSE)

print(weightedtable, smd = TRUE)

#visualize weights to check whether there is a extrem value
sort_weight <- sort(weight)
ggplot() + geom_point(aes(x=seq(length(sort_weight)), y=sort_weight)) +
  ylim(c(0, 20)) + xlab("index") + ylab("weight") + ggtitle("Weight Distribution")

#Weighted GLM
glm.result = glm(purchased.after.visit~ad.group, weights = weight, family = binomial(link="logit"), data = mydata)

#get the causal ratio
betaiptw<-coef(glm.result)

#get the standard error using sandwich variance estimator
SE<-sqrt(diag(vcovHC(glm.result, type="HC0")))

betaiptw

# calculate the causal ratio and its confidence interval
caualrr<-exp(betaiptw[2])
lcl<-exp(betaiptw[2] - 1.96*SE[2])
ucl<-exp(betaiptw[2] + 1.96*SE[2])
c(lcl, caualrr, ucl)

#get causal risk difference
glm.result <- glm(purchased.after.visit~ad.group, weights = weight, family = quasibinomial(link="identity"), data = mydata)

#summary(glm.obj)
betaiptw<-coef(glm.result)
SE<-sqrt(diag(vcovHC(glm.result, type="HC0")))

causalrd<-(betaiptw[2])
lcl<-(betaiptw[2]-1.96*SE[2])
ucl<-(betaiptw[2]+1.96*SE[2])
c(lcl,causalrd,ucl)

# Part 2

# remove the duplicate randomly and run our model for 1000 times
size = 1000
l_1000 <- numeric(size)
for(i in 1:size){
  rows = sample(nrow(data2))
  data2_random <- data2[rows, ]
  
  data2_random <- data2_random[!duplicated(data2_random$member.id),]
  
  psmodel <- glm(ad.group ~ age + prev.purchase, family = binomial(link = "logit"), data = data2_random)
  
  ps<-predict(psmodel, type = "response")
  
  mydata <- data2_random%>%
    select(c(age, prev.purchase, ad.group, purchased.after.visit))
  
  #create weights
  weight <- ifelse(data2_random$ad.group==1, 1/(ps), 1/(1-ps))
  
  #apply weights to data
  weighteddata<-svydesign(ids = ~1, data = mydata, weights = ~weight)
  
  #weighted table 1
  weightedtable <- svyCreateTableOne(vars = c("age", "prev.purchase"), strata = "ad.group", data = weighteddata, test = FALSE)
  
  #Weighted GLM
  glm.result = glm(purchased.after.visit~ad.group, weights = weight, family = binomial(link="logit"), data = mydata)
  
  betaiptw<-coef(glm.result)
  
  SE<-sqrt(diag(vcovHC(glm.result, type="HC0")))
  
  caualrr<-exp(betaiptw[2])
  l_1000[i]<-caualrr
}

sqrt(var(l_1000))

# remove the duplicate randomly and run our model for 500 times
l_500 <- numeric(500)
for(i in 1:size){
  rows = sample(nrow(data2))
  data2_random <- data2[rows, ]
  
  data2_random <- data2_random[!duplicated(data2_random$member.id),]
  
  psmodel <- glm(ad.group ~ age + prev.purchase, family = binomial(link = "logit"), data = data2_random)
  
  ps<-predict(psmodel, type = "response")
  
  mydata <- data2_random%>%
    select(c(age, prev.purchase, ad.group, purchased.after.visit))
  
  #create weights
  weight <- ifelse(data2_random$ad.group==1, 1/(ps), 1/(1-ps))
  
  #apply weights to data
  weighteddata<-svydesign(ids = ~1, data = mydata, weights = ~weight)
  
  #weighted table 1
  weightedtable <- svyCreateTableOne(vars = c("age", "prev.purchase"), strata = "ad.group", data = weighteddata, test = FALSE)
  
  #Weighted GLM
  glm.result = glm(purchased.after.visit~ad.group, weights = weight, family = binomial(link="logit"), data = mydata)
  
  betaiptw<-coef(glm.result)
  
  SE<-sqrt(diag(vcovHC(glm.result, type="HC0")))
  
  caualrr<-exp(betaiptw[2])
  l_500[i]<-caualrr
}

# remove the duplicate randomly and run our model 100 times
l_100 <- numeric(100)
lcl_100 <- numeric(100)
ucl_100 <- numeric(100)
for(i in 1:100){
  rows = sample(nrow(data2))
  data2_random <- data2[rows, ]
  
  data2_random <- data2_random[!duplicated(data2_random$member.id),]
  
  psmodel <- glm(ad.group ~ age + prev.purchase, family = binomial(link = "logit"), data = data2_random)
  
  ps<-predict(psmodel, type = "response")
  
  mydata <- data2_random%>%
    select(c(age, prev.purchase, ad.group, purchased.after.visit))
  
  #create weights
  weight <- ifelse(data2_random$ad.group==1, 1/(ps), 1/(1-ps))
  
  #apply weights to data
  weighteddata<-svydesign(ids = ~1, data = mydata, weights = ~weight)
  
  #weighted table 1
  weightedtable <- svyCreateTableOne(vars = c("age", "prev.purchase"), strata = "ad.group", data = weighteddata, test = FALSE)
  
  #Weighted GLM
  glm.result = glm(purchased.after.visit~ad.group, weights = weight, family = binomial(link="logit"), data = mydata)
  
  betaiptw<-coef(glm.result)
  
  SE<-sqrt(diag(vcovHC(glm.result, type="HC0")))
  
  caualrr<-exp(betaiptw[2])
  l_100[i]<-caualrr
  
  lcl<-exp(betaiptw[2] - 1.96*SE[2])
  ucl<-exp(betaiptw[2] + 1.96*SE[2])
  lcl_100[i] <- lcl
  ucl_100[i] <- ucl
}

mean(l_100)
mean(lcl_100)
mean(ucl_100)




# ===== Propensity scores data 2 matchit
model <- glm(formula = ad.group ~ age + prev.purchase, family = binomial(link="logit"), data = data2); summary(model)
pr_score = predict(model, type = "response")
data2$pr_score <- pr_score

control = data2[which(data2$ad.group==0),]
treated = data2[which(data2$ad.group==1),]

hist(control$pr_score)
hist(treated$pr_score)

get_results <- function(data) {
  model <- glm(formula = purchased.after.visit ~ ad.group , family = binomial(link="logit"), data = data)
  
  b1_std = (summary(model)$coefficients[2,2])
  b1_var = b1_std ** 2
  b1 = model$coefficients[2]
  z_score = 1.96
  
  ci_lower <- b1 - z_score * sqrt(b1_var)
  ci_upper <- b1 + z_score * sqrt(b1_var)
  
  intervals = c(ci_lower, b1, ci_upper)
  exp_intervals = exp(intervals)
  
  print(exp_intervals)
  exp_intervals
}


size = 10
lower <- numeric(size)
mid <- numeric(size)
high <- numeric(size)
for(i in 1:size) {
  data2Random <- data2[sample(nrow(data2)),]
  data2Rem <- data2Random[!duplicated(data2Random$member.id),]
  
  # ===== Matching with matchit
  mod_match <- matchit(ad.group ~ age +  prev.purchase, method = "nearest", data = data2Rem)
  matchit_data2 <- match.data(mod_match)
  
  results = get_results(matchit_data2)
  
  lower[i] <- results[1]
  mid[i] <- results[2]
  high[i] <- results[3]
}

print("Matching randommly reomving duplicates over 1000 iterations")
print(c(mean(lower), mean(mid), mean(high)))

# ----


#Data 2
##propensity weighting randomly removing duplicates
values <- numeric(size)
for(i in 1:size) {
  data2Random <- data2[sample(nrow(data2)),]
  data2Rem <- data2Random[!duplicated(data2Random$member.id),]
  
  modelPs <- glm(ad.group ~ age + prev.purchase, family = binomial(link = "logit"), data2Rem)
  data2Rem$ps <- predict(modelPs, type = 'response')
  
  data2Rem$BackDoorProb <- apply(data2Rem, 1, function(x) nrow(data2Rem[data2Rem$age == x["age"] & data2Rem$prev.purchase == x["prev.purchase"] & data2Rem$ad.group == x["ad.group"] & data2Rem$purchased.after.visit == x["purchased.after.visit"],])/nrow(data2Rem))
  
  data2RemAdShow <- data2Rem[data2Rem$ad.group == 1,]
  probAdShow <- apply(data2RemAdShow[!duplicated(data2RemAdShow[,c("age", "prev.purchase")]) & data2RemAdShow$purchased.after.visit==1, ],1, function(x) x["BackDoorProb"]/x["ps"])
  p1Rem = sum(probAdShow)
  
  data2RemNoAdShow <- data2Rem[data2Rem$ad.group == 0,]
  probNoAdShow <- apply(data2RemNoAdShow[!duplicated(data2RemNoAdShow[,c("age", "prev.purchase")]) & data2RemNoAdShow$purchased.after.visit==1, ],1, function(x) x["BackDoorProb"]/(1-x["ps"]))
  p2Rem = sum(probNoAdShow)
  
  values[i] <- ((p1Rem)/(1-p1Rem))/((p2Rem)/(1-p2Rem)) 
}
mean(values)
sqrt(var(values))

#Simple effects
values <- numeric(size)
lowerbounds <- numeric(size)
upperbounds <- numeric(size)
for(i in 1:size) {
  data2Random <- data2[sample(nrow(data2)),]
  data2Rem <- data2Random[!duplicated(data2Random$member.id),]
  modelData2Simple <- glm(purchased.after.visit ~ ad.group, family = binomial(), data2Rem)
  values[i] <- exp(modelData2Simple$coefficients[2])
  lowerbounds <- exp(confint.default(modelData2Simple)[2,])[1]
  upperbounds <- exp(confint.default(modelData2Simple)[2,])[2]
}
mean(values)
sqrt(var(values))
mean(lowerbounds)  
mean(upperbounds)

