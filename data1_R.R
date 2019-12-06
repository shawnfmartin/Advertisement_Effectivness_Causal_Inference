#Data1
data1 <- read.csv("/Users/shawnmartin/NEU/Fall_2019/DS/Advertisement_Effectivness_Causal_Inference/data/data_1.csv")

#Front door criteria

data1AdShow <- data1[data1$ad.show == 1,]
data1AdShow$PzGivenx <- apply(data1AdShow, 1, function(x) nrow(data1AdShow[data1AdShow$time.spent.onwebsite == x["time.spent.onwebsite"] & data1AdShow$activity.level == x["activity.level"] & data1AdShow$prev.purchase == x["prev.purchase"],])/nrow(data1AdShow))

data1NoAdShow <- data1[data1$ad.show == 0,]
data1NoAdShow$PzGivenx <- apply(data1NoAdShow, 1, function(x) nrow(data1NoAdShow[data1NoAdShow$time.spent.onwebsite == x["time.spent.onwebsite"] & data1NoAdShow$activity.level == x["activity.level"] & data1NoAdShow$prev.purchase == x["prev.purchase"],])/nrow(data1NoAdShow))

data1AdShow$PyGivenxz <- 0
data1NoAdShow$PyGivenxz <- 0


for(timeSpent in unique(data1$time.spent.onwebsite)){
  for(activity in unique(data1$activity.level)){
    for(prevPurchase in unique(data1$prev.purchase)) {
      
      data1AdShowControl <- data1AdShow[data1AdShow$time.spent.onwebsite == timeSpent & data1AdShow$activity.level == activity & data1AdShow$prev.purchase == prevPurchase, ]
      pAdShow <- (nrow(data1AdShow)/nrow(data1))*(nrow(data1AdShowControl[data1AdShowControl$purchased.after.visit == 1, ])/ifelse(nrow(data1AdShowControl)>0, nrow(data1AdShowControl), 1))
      
      data1NoAdShowControl <- data1NoAdShow[data1NoAdShow$time.spent.onwebsite == timeSpent & data1NoAdShow$activity.level == activity & data1NoAdShow$prev.purchase == prevPurchase, ]
      pNoAdShow <- (nrow(data1NoAdShow)/nrow(data1))*(nrow(data1NoAdShowControl[data1NoAdShowControl$purchased.after.visit == 1, ])/ifelse(nrow(data1NoAdShowControl)>0, nrow(data1NoAdShowControl), 1))
      
      if(nrow(data1AdShowControl) > 0) {
        data1AdShow[data1AdShow$time.spent.onwebsite == timeSpent & data1AdShow$activity.level == activity & data1AdShow$prev.purchase == prevPurchase,]$PyGivenxz <- pAdShow + pNoAdShow
      }
      if(nrow(data1NoAdShowControl) > 0){
        data1NoAdShow[data1NoAdShow$time.spent.onwebsite == timeSpent & data1NoAdShow$activity.level == activity & data1NoAdShow$prev.purchase == prevPurchase,]$PyGivenxz <- pAdShow + pNoAdShow
      }
    }
  }
}

probAdShowData1 <- apply(data1AdShow[!duplicated(data1AdShow[,c("time.spent.onwebsite", "activity.level", "prev.purchase")]),], 1, function(x) x["PzGivenx"]*x["PyGivenxz"])
probNoAdShowData1 <- apply(data1NoAdShow[!duplicated(data1NoAdShow[,c("time.spent.onwebsite", "activity.level", "prev.purchase")]),], 1, function(x) x["PzGivenx"]*x["PyGivenxz"])

p1Data1 <- sum(probAdShowData1)
p2Data1 <- sum(probNoAdShowData1)
oddsRatioData1 = ((p1Data1)/(1-p1Data1))/((p2Data1)/(1-p2Data1)) 

#Simple effect
modelData1Simple <- glm(purchased.after.visit ~ ad.show, family = binomial(), data1)
modelData1Simple2 <- glm(purchased.after.visit ~ ad.show + prev.purchase, family = binomial(), data1)
exp(confint.default(modelData1Simple)[2,])
exp(confint.default(modelData1Simple2)[2,])