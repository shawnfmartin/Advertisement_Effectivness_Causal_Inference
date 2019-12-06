library(tidyverse)
library(MatchIt)
data3 <- read.csv("/Users/shawnmartin/NEU/Fall_2019/DS/Advertisement_Effectivness_Causal_Inference/data/data_3Final.csv")


# ===== Skewed data 3 gaming fix

data3$n.visits <- unlist(lapply(data3$IP.id, function(x) nrow(data3[data3$IP.id == x,])))
data3$gamer <- unlist(lapply(data3$IP.id, function(x) nrow(data3[data3$IP.id == x,])>1))

model <- glm(formula = purchased.after.visit ~ age + gender + predicted.incomeleve + prev.purchase , family = binomial(link="logit"), data = data3); summary(model)

si_score = predict(model, type = "response")

data3$si_score <- si_score

head(prs_df)

non_gamers = data3[which(data3$gamer==0),]
gamers = data3[which(data3$gamer==1),]
unique_gamers = data3[which(data3$gamer==1 & !duplicated(data3$IP.id)),]

hist(non_gamers$si_score, breaks = seq(0.0, 1.0, by = 0.05), freq = FALSE, ylim = c(0,10),  col=rgb(1,0,0,0.5))
par(new =T)
hist(unique_gamers$si_score, breaks = seq(00, 1.0, by = 0.05), freq = FALSE, ylim = c(0,10),  col=rgb(0,0,1,0.5), xlab = 0, ylab = 0)

# proprtion of purchasing gamers vs. non-gamers

# non gamers purhcasing whether they see ad or not 
non_gamers_purhcase_prob = nrow(non_gamers[which(non_gamers$ad.group == 1 & non_gamers$purchased.after.visit == 1),]) / nrow(non_gamers[which(non_gamers$ad.group == 0),])
gamers_purhcase_prob = nrow(gamers[which(gamers$ad.group == 1 & gamers$purchased.after.visit == 1),]) / nrow(gamers[which(gamers$ad.group == 0),])


# when we remove gamers, odds ratio increases:
# non_gamers_purhcase_prob = 0.2530864
# gamers_purhcase_prob = 0.2307692

# similarity score comparison
get_nearest_similarity_score <- function(info, propensity) {
  scores=info$si_score
  index = which(abs(scores-propensity)==min(abs(scores-propensity)))[1]
  info[index,]
}

ids_to_duplicate = c()

# loop thorugh each gamer and get nearest non-gamer based on closest similarity score
for (gamer_pr in unique_gamers$si_score)
{
  closest_non_gamer = get_nearest_similarity_score(non_gamers, gamer_pr)
  #print(c(gamer_pr, closest_non_gamer$si_score))
  
  ids_to_duplicate = c(ids_to_duplicate, closest_non_gamer$experiment.id)
  
}

duplicated_similar_nongamers <- non_gamers[which(non_gamers$experiment.id %in% ids_to_duplicate),]


# new dataset with all gamers removed and non-gamers added to balance
fixed_data3 <- rbind(non_gamers, duplicated_similar_nongamers)

# removed non-gamers
#fixed_data3 <- non_gamers

# direct effect
#fixed_data3 <- data3



hist(fixed_data3$si_score)

control = fixed_data3[which(fixed_data3$ad.group==0),]
treated = fixed_data3[which(fixed_data3$ad.group==1),]

hist(control$si_score, breaks = seq(0.0, 1.0, by = 0.05), freq = FALSE, ylim = c(0,10),  col=rgb(1,0,0,0.5))
par(new =T)
hist(treated$si_score, breaks = seq(00, 1.0, by = 0.05), freq = FALSE, ylim = c(0,10),  col=rgb(0,0,1,0.5), xlab = 0, ylab = 0)


# treated probability
treated_probability = nrow(data_m[fixed_data3$ad.group == 1 & fixed_data3$purchased.after.visit == 1,])/nrow(fixed_data3[fixed_data3$ad.group == 1,])

# control probability
control_probability = nrow(data_m[fixed_data3$ad.group == 0 & fixed_data3$purchased.after.visit == 1,])/nrow(fixed_data3[fixed_data3$ad.group == 0,])

print(treated_probability/control_probability)


model <- glm(formula = purchased.after.visit ~ ad.group , family = binomial(link="logit"), data = fixed_data3)
# ad.group      1.4204     0.3532   4.021 5.79e-05 ***

# taking exp(1.4204) == 4.138756 (same as odds ratio)
actual = exp(model$coefficients[2])

print(abs(actual - odds_ratio) < 0.00001)

b1_std = (summary(model)$coefficients[2,2])
b1_var = b1_std ** 2
b1 = model$coefficients[2]
z_score = 1.96

ci_lower <- b1 - z_score * sqrt(b1_var)
ci_upper <- b1 + z_score * sqrt(b1_var)

intervals = c(ci_lower, b1, ci_upper)
exp_intervals = exp(intervals)

print(exp_intervals)
