#Jazmin Aguilar
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))
suppressWarnings(library(openintro))


fastfoodplaces <- fastfood %>%
filter(restaurant == "Sonic" | restaurant == "Subway" | restaurant == "Taco Bell") %>%
select(calories, total_fat, sugar, calcium)
nona <- na.omit(fastfoodplaces)
Q1 <- round(cor(nona), 2)

data2 <- fastfood %>%
filter( restaurant == "Mcdonalds" | restaurant == "Subway")
mutatemcdonalds <- data2 %>%
 mutate(ismcdonalds = ifelse(restaurant == 'Subway', 0,1))
model <- glm(ismcdonalds ~ calories + sodium + protein, data = mutatemcdonalds, family = binomial)
Q2 <- round(coefficients(model, standardization = FALSE), 2)

AIC(model)

A2 <- glm(ismcdonalds ~ calories + protein, data = mutatemcdonalds, family = binomial)
AIC(A2)
Q3 <- round(AIC(model), 2)


data3  <- lm(calories ~ sat_fat + fiber + sugar, fastfood) 
coefficients(data3, standardization = FALSE)
Q4 <- round(coef(data3)["sat_fat"], 2)

fastfood5060 <- fastfood %>%
  group_by(restaurant) %>%
  filter(n() >= 50 & n() <= 60)
logreg <- lm(total_fat ~ cholesterol + total_carb +vit_a + restaurant, fastfood5060)
summary(logreg)
logreg2 <- lm(total_fat ~ cholesterol + total_carb + restaurant, fastfood5060)
#vit_a is not needed
coefs <- round(coef(lm.beta::lm.beta(logreg2)), 2)
Q5 <- coefs[which.max(coefs)]