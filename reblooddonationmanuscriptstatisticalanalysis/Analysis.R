
library("FactoMineR")
library("factoextra")
library(extrafont)
library(ggplot2)
library(pastecs)
library(corrplot)
library(ppcor)
library(factoextra)
library(psych)
library(GPArotation)
library(Hmisc)
library(dplyr)
library(ape)
library(psych)
library(psychometric)
setwd('E:\\ResearchProject\\Sorowar Sir\\Blood donation\\reblooddonationmanuscriptstatisticalanalysis')
BDP2 <- read.csv("BDP2.csv")

bdp_prmts <- data.frame(BDP2$K1_donation_interval, BDP2$K2_age, BDP2$K3_weight, BDP2$K4_amountblood, BDP2$K5_germs)
psych::alpha(bdp_prmts)

bdp_prmts <- data.frame(BDP2$A1_save_life, BDP2$A2_health_harm, BDP2$A3_ukn_person, BDP2$A4_reg_donate, BDP2$A5_moral_respon, BDP2$A6_percep_islamic)
psych::alpha(bdp_prmts)

bdp_prmts <- data.frame(BDP2$P1_ever, BDP2$P2_donationprogram, BDP2$P3_org_encouraged, BDP2$P4_req_to_donate, BDP2$P5_relative_blood)
psych::alpha(bdp_prmts)


bdp_prmts <- data.frame(BDP2$K1_donation_interval, BDP2$K2_age, BDP2$K3_weight, BDP2$K4_amountblood, BDP2$K5_germs,
                        BDP2$A1_save_life, BDP2$A2_health_harm, BDP2$A3_ukn_person, BDP2$A4_reg_donate, BDP2$A5_moral_respon, BDP2$A6_percep_islamic,
                        BDP2$P1_ever, BDP2$P2_donationprogram, BDP2$P3_org_encouraged, BDP2$P4_req_to_donate, BDP2$P5_relative_blood)
psych::alpha(bdp_prmts)

bdp_prmts <- data.frame(BDP2$K_count , BDP2$P_count, BDP2$A_count)
colnames(bdp_prmts) <- c("Knowledge", "Practice","Attitude")
psych::alpha(bdp_prmts)


#Principal conponents analysis
res.pca <- prcomp(bdp_prmts, scale = T, center = T)
  
fviz_eig(res.pca,main = "                                        Screeplot of the 5 PCs",
         addlabels = TRUE, 
         ylim = c(0, 40))

eigs <- res.pca$sdev^2
eigs
summary(res.pca)
pca.loadings <- varimax(res.pca$rotation)
pca.loadings
class(res.pca$rotation)
dat <- c(0.70438881, -0.06038135, 0.70724147, 0.080298716, 0.996757673, -0.99677374)
PC2 <- c()

mat <- matrix(dat, nrow = 3, ncol = 2)

dat <- matri(PC1,PC2)
rownames(dat) <- c("Knowledge","Practice","Attitude")
dat <- matrix(dat)
class(dat)
dat
fviz_pca_var(mat,
             col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE )+
  labs(title ="                      Variables Factor Map (PCA)", x = "PC1 (44.15%)", y = "PC2 (33.26%)")

fviz_pca_ind(res.pca,
             col.ind = "cos2",
             pointshape = 21, fill = "#E7B800",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE  ) +
  labs(title ="                                   Individuals Factor Map (PCA)", x = "PC1 (36.81%)", y = "PC2 (18.78%)") + 
  xlim(-5, 5) + ylim (-4, 4)

#logistic odds ratio

model <- glm( relevel(factor(BDP2$K_binary), ref = "0")~ relevel(factor(BDP2$age_group),ref = "1") 
              + relevel(factor(BDP2$edu_cat),ref = "3")
              + relevel(factor(BDP2$ï..participant_type),ref = "2") + relevel(factor(BDP2$smoker),ref = "2"), family=binomial(link='logit'), data=BDP2)

summary(model)
vif(model)

exp(cbind(coef(model), confint(model)))

step.model <- stepAIC(model, direction = "backward", 
                      trace = FALSE)
summary(step.model)

exp(cbind(coef(step.model), confint(step.model)))


#logistic odds ratio
BDP$age_group <- factor(BDP$age_group)

model <- glm( relevel(factor(BDP2$A_binary), ref = "0")~ relevel(factor(BDP2$age_group),ref = "1") 
              + relevel(factor(BDP2$edu_cat),ref = "3")
              + relevel(factor(BDP2$ï..participant_type),ref = "1")+ relevel(factor(BDP2$smoker),ref = "2") , family=binomial(link='logit'), data=BDP2)

summary(model)
vif(model)

exp(cbind(coef(model), confint(model)))
 
exp(cbind(coef(model), confint(model)))

step.model <- stepAIC(model, direction = "backward", 
                      trace = FALSE)
summary(step.model)

exp(cbind(coef(step.model), confint(step.model)))

#logistic odds ratio
BDP$age_group <- factor(BDP$age_group)

model <- glm( relevel(factor(BDP2$P_binary), ref = "0")~ relevel(factor(BDP2$age_group),ref = "1") 
              + relevel(factor(BDP2$edu_cat),ref = "3")
              + relevel(factor(BDP2$ï..participant_type),ref = "1")+ relevel(factor(BDP2$smoker),ref = "2") , family=binomial(link='logit'), data=BDP2)

summary(model)
vif(model)

exp(cbind(coef(model), confint(model)))

step.model <- stepAIC(model, direction = "backward", 
                      trace = FALSE)
summary(step.model)

exp(cbind(coef(step.model), confint(step.model)))

