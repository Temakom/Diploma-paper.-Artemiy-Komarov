library(mnormt)
library(lmtest)
library(readxl)
library(dplyr)
library(AER)
library(skedastic)
library(sandwich)
library(MASS)
library(stargazer)
library(quantmod)
library(corrplot)

#######################################################
#### регрессии по компонентам Хофстеде за 2009 год ####
#######################################################

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data, Power.distance, Individualism, Uncertainty.avoidance, Long.term.orientation, Masculinity, Indulgence, GDP_growth_2009, GDP_growth_2010, GDP_growth_2011, Inflation_2009, Inflation_2010, Inflation_2011, Capital_stock_2009_growth, Capital_stock_2010_growth, Capital_stock_2011_growth, Population_15_64_2009_growth, Population_15_64_2010_growth, Population_15_64_2011_growth, Business_freedom, Property_rights,Freedom_for_corruption)
data <- na.omit(data)

#Дистанция власти
mod1 <- lm(data = data, GDP_growth_2009 ~ Power.distance + Capital_stock_2009_growth + Inflation_2009 + Population_15_64_2009_growth)
summary(mod1)
V_Pd <- vcovHC(mod1, type = "HC0")
V_diag_Pd <- sqrt(diag(V_Pd))

#Индивидуализм
mod2 <- lm(data = data, GDP_growth_2009 ~ Individualism + Capital_stock_2009_growth + Inflation_2009+Population_15_64_2009_growth)
summary(mod2)
V_Ind <- vcovHC(mod2, type = "HC0")
V_diag_Ind <- sqrt(diag(V_Ind))


#Избегание неопределённости
mod3 <- lm(data = data, GDP_growth_2009 ~ Uncertainty.avoidance + Capital_stock_2009_growth + Inflation_2009+Population_15_64_2009_growth)
summary(mod3)
V_Ua <- vcovHC(mod3, type = "HC0")
V_diag_Ua <- sqrt(diag(V_Ua))


#Долгосрочная ориентация
mod4 <- lm(data = data, GDP_growth_2009 ~ Long.term.orientation + Capital_stock_2009_growth + Inflation_2009+Population_15_64_2009_growth)
summary(mod4)
V_Lo <- vcovHC(mod4, type = "HC0")
V_diag_Lo <- sqrt(diag(V_Lo))

#Маскулинность
mod5 <- lm(data = data, GDP_growth_2009 ~ Masculinity + Capital_stock_2009_growth + Inflation_2009+Population_15_64_2009_growth)
summary(mod5)
V_Ma <- vcovHC(mod5, type = "HC0")
V_diag_Ma <- sqrt(diag(V_Ma))

#Удовлетворённость жизнью
mod6 <- lm(data = data, GDP_growth_2009 ~ Indulgence + Capital_stock_2009_growth + Inflation_2009 + Population_15_64_2009_growth)
summary(mod6)
V_Ig <- vcovHC(mod6, type = "HC0")
V_diag_Ig <- sqrt(diag(V_Ig))

stargazer(mod1,mod2,mod3,mod4,mod5,mod6, type="html",out = "Hofstede_components_2009.htm",se=list(V_diag_Pd, V_diag_Ind,V_diag_Ua,V_diag_Lo,V_diag_Ma,V_diag_Ig))

#Корреляции по Хофстеде
Hofstede <- dplyr::select(data, Power.distance, Individualism, Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence)
Hofstede <- na.omit(Hofstede)
cor_p <- cor.mtest(Hofstede, conf.level = 0.95) 
corrplot(cor(Hofstede), addgrid.col = TRUE, addCoef.col = TRUE, 
         type = 'lower', 
         p.mat = cor_p$p, sig.level = 0.1, insig='blank',diag=FALSE)

#Дистанция власти и индивидуализм
mod7 <- lm(data = data, GDP_growth_2009 ~ Power.distance +Individualism + Capital_stock_2009_growth + Inflation_2009+Population_15_64_2009_growth)
V_Pd_Ind <- vcovHC(mod7, type = "HC0")
V_diag_Pd_Ind <- sqrt(diag(V_Pd_Ind))
summary(mod7)

#Долгосрочная ориентация и удовлетворённость жизнью
mod8 <- lm(data = data, GDP_growth_2009 ~ Long.term.orientation +Indulgence + Capital_stock_2009_growth + Inflation_2009+Population_15_64_2009_growth)
V_Lo_Indul <- vcovHC(mod8, type = "HC0")
V_diag_Lo_Indul <- sqrt(diag(V_Lo_Indul))
summary(mod8)

stargazer(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8, type="html",out = "Hofstede_components_2009.htm",se=list(V_diag_Pd, V_diag_Ind,V_diag_Ua,V_diag_Lo,V_diag_Ma,V_diag_Ig, V_diag_Pd_Ind, V_diag_Lo_Indul))

#######################################################
#### регрессии по компонентам Хофстеде за 2010 год ####
#######################################################

#Дистанция власти
mod1 <- lm(data = data, GDP_growth_2010 ~ Power.distance + Capital_stock_2010_growth + Inflation_2010 + Population_15_64_2010_growth)
summary(mod1)
V_Pd <- vcovHC(mod1, type = "HC0")
V_diag_Pd <- sqrt(diag(V_Pd))

#Индивидуализм
mod2 <- lm(data = data, GDP_growth_2010 ~ Individualism + Capital_stock_2010_growth + Inflation_2010+Population_15_64_2010_growth)
summary(mod2)
V_Ind <- vcovHC(mod2, type = "HC0")
V_diag_Ind <- sqrt(diag(V_Ind))


#Избегание неопределённости
mod3 <- lm(data = data, GDP_growth_2010 ~ Uncertainty.avoidance + Capital_stock_2010_growth + Inflation_2010+Population_15_64_2010_growth)
summary(mod3)
V_Ua <- vcovHC(mod3, type = "HC0")
V_diag_Ua <- sqrt(diag(V_Ua))


#Долгосрочная ориентация
mod4 <- lm(data = data, GDP_growth_2010 ~ Long.term.orientation + Capital_stock_2010_growth + Inflation_2010+Population_15_64_2010_growth)
summary(mod4)
V_Lo <- vcovHC(mod4, type = "HC0")
V_diag_Lo <- sqrt(diag(V_Lo))

#Маскулинность
mod5 <- lm(data = data, GDP_growth_2010 ~ Masculinity + Capital_stock_2010_growth + Inflation_2010+Population_15_64_2010_growth)
summary(mod5)
V_Ma <- vcovHC(mod5, type = "HC0")
V_diag_Ma <- sqrt(diag(V_Ma))

#Удовлетворённость жизнью
mod6 <- lm(data = data, GDP_growth_2010 ~ Indulgence + Capital_stock_2010_growth + Inflation_2010 + Population_15_64_2010_growth)
summary(mod6)
V_Ig <- vcovHC(mod6, type = "HC0")
V_diag_Ig <- sqrt(diag(V_Ig))

#Дистанция власти и индивидуализм
mod7 <- lm(data = data, GDP_growth_2010 ~ Power.distance +Individualism + Capital_stock_2010_growth + Inflation_2010+Population_15_64_2010_growth)
V_Pd_Ind <- vcovHC(mod7, type = "HC0")
V_diag_Pd_Ind <- sqrt(diag(V_Pd_Ind))
summary(mod7)

#Долгосрочная ориентация и удовлетворённость жизнью
mod8 <- lm(data = data, GDP_growth_2010 ~ Long.term.orientation +Indulgence + Capital_stock_2010_growth + Inflation_2010+Population_15_64_2010_growth)
V_Lo_Indul <- vcovHC(mod8, type = "HC0")
V_diag_Lo_Indul <- sqrt(diag(V_Lo_Indul))
summary(mod8)

stargazer(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8, type="html",out = "Hofstede_components_2010.htm",se=list(V_diag_Pd, V_diag_Ind,V_diag_Ua,V_diag_Lo,V_diag_Ma,V_diag_Ig, V_diag_Pd_Ind, V_diag_Lo_Indul))

#######################################################
#### регрессии по компонентам Хофстеде за 2011 год ####
#######################################################

#Дистанция власти
mod1 <- lm(data = data, GDP_growth_2011 ~ Power.distance + Capital_stock_2011_growth + Inflation_2011 + Population_15_64_2011_growth)
summary(mod1)
V_Pd <- vcovHC(mod1, type = "HC0")
V_diag_Pd <- sqrt(diag(V_Pd))

#Индивидуализм
mod2 <- lm(data = data, GDP_growth_2011 ~ Individualism + Capital_stock_2011_growth + Inflation_2011 + Population_15_64_2011_growth)
summary(mod2)
V_Ind <- vcovHC(mod2, type = "HC0")
V_diag_Ind <- sqrt(diag(V_Ind))


#Избегание неопределённости
mod3 <- lm(data = data, GDP_growth_2011 ~ Uncertainty.avoidance + Capital_stock_2011_growth + Inflation_2011+Population_15_64_2011_growth)
summary(mod3)
V_Ua <- vcovHC(mod3, type = "HC0")
V_diag_Ua <- sqrt(diag(V_Ua))


#Долгосрочная ориентация
mod4 <- lm(data = data, GDP_growth_2011 ~ Long.term.orientation + Capital_stock_2011_growth + Inflation_2011+Population_15_64_2011_growth)
summary(mod4)
V_Lo <- vcovHC(mod4, type = "HC0")
V_diag_Lo <- sqrt(diag(V_Lo))

#Маскулинность
mod5 <- lm(data = data, GDP_growth_2011 ~ Masculinity + Capital_stock_2011_growth + Inflation_2011+Population_15_64_2011_growth)
summary(mod5)
V_Ma <- vcovHC(mod5, type = "HC0")
V_diag_Ma <- sqrt(diag(V_Ma))

#Удовлетворённость жизнью
mod6 <- lm(data = data, GDP_growth_2011 ~ Indulgence + Capital_stock_2011_growth + Inflation_2011 + Population_15_64_2011_growth)
summary(mod6)
V_Ig <- vcovHC(mod6, type = "HC0")
V_diag_Ig <- sqrt(diag(V_Ig))

#Дистанция власти и индивидуализм
mod7 <- lm(data = data, GDP_growth_2011 ~ Power.distance +Individualism + Capital_stock_2011_growth + Inflation_2011+Population_15_64_2011_growth)
V_Pd_Ind <- vcovHC(mod7, type = "HC0")
V_diag_Pd_Ind <- sqrt(diag(V_Pd_Ind))
summary(mod7)

#Долгосрочная ориентация и удовлетворённость жизнью
mod8 <- lm(data = data, GDP_growth_2011 ~ Long.term.orientation +Indulgence + Capital_stock_2011_growth + Inflation_2011+Population_15_64_2011_growth)
V_Lo_Indul <- vcovHC(mod8, type = "HC0")
V_diag_Lo_Indul <- sqrt(diag(V_Lo_Indul))
summary(mod8)

stargazer(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8, type="html",out = "Hofstede_components_2011.htm",se=list(V_diag_Pd, V_diag_Ind,V_diag_Ua,V_diag_Lo,V_diag_Ma,V_diag_Ig, V_diag_Pd_Ind, V_diag_Lo_Indul))

#####################################################
#### регрессии по компонентам Шварца за 2009 год ####
#####################################################

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data, LOCATION, Country, GDP_growth_2009, GDP_growth_2010, GDP_growth_2011, Population_15_64_2009_growth, Population_15_64_2010_growth, Population_15_64_2011_growth, Capital_stock_2009_growth, Capital_stock_2010_growth, Capital_stock_2011_growth, Inflation_2009, Inflation_2010, Inflation_2011, Harmony, Embeddedness, Hierarchy, Mastery, Aff_auton, Intel_auton, Egalitar, Business_freedom, Trade_freedom, Fiscal_freedom, Government_size, Monetary_freedom, Investment_freedom, Financial_freedom, Property_rights, Freedom_for_corruption, Labour_freedom)
data$Emb_aut <- data$Embeddedness-((data$Aff_auton+data$Intel_auton)/2)
data$Hie_Ega <- data$Hierarchy - data$Egalitar
data$Mas_Har <-  data$Mastery - data$Harmony
data <- na.omit(data)
View(data)

#1
data <- data.frame(data)
mod1sch <- lm(data = data, GDP_growth_2009 ~ Emb_aut + Population_15_64_2009_growth + Capital_stock_2009_growth+Inflation_2009)
summary(mod1sch)
V_mod1sch <- vcovHC(mod1sch, type = "HC0")
V_diag_mod1sch <- sqrt(diag(V_mod1sch))


#2
data <- data.frame(data)
mod2sch <- lm(data = data, GDP_growth_2009 ~ Hie_Ega + Population_15_64_2009_growth + Capital_stock_2009_growth+Inflation_2009)
summary(mod2sch)
V_mod2sch <- vcovHC(mod2sch, type = "HC0")
V_diag_mod2sch <- sqrt(diag(V_mod2sch))


#3
data <- data.frame(data)
mod3sch <- lm(data = data, GDP_growth_2009 ~ Mas_Har + Population_15_64_2009_growth + Capital_stock_2009_growth+Inflation_2009)
summary(mod3sch)
V_mod3sch <- vcovHC(mod3sch, type = "HC0")
V_diag_mod3sch <- sqrt(diag(V_mod3sch))


stargazer(mod1sch,mod2sch,mod3sch, type="html",out = "Schwarz_components_2009.htm",se=list(V_diag_mod1sch,V_diag_mod2sch,V_diag_mod3sch))

#####################################################
#### регрессии по компонентам Шварца за 2010 год ####
#####################################################

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data, LOCATION, Country, GDP_growth_2009, GDP_growth_2010, GDP_growth_2011, Population_15_64_2009_growth, Population_15_64_2010_growth, Population_15_64_2011_growth, Capital_stock_2009_growth, Capital_stock_2010_growth, Capital_stock_2011_growth, Inflation_2009, Inflation_2010, Inflation_2011, Harmony, Embeddedness, Hierarchy, Mastery, Aff_auton, Intel_auton, Egalitar, Business_freedom, Trade_freedom, Fiscal_freedom, Government_size, Monetary_freedom, Investment_freedom, Financial_freedom, Property_rights, Freedom_for_corruption, Labour_freedom)
data$Emb_aut <- data$Embeddedness-((data$Aff_auton+data$Intel_auton)/2)
data$Hie_Ega <- data$Hierarchy - data$Egalitar
data$Mas_Har <-  data$Mastery - data$Harmony

#1
data <- data.frame(data)
mod1sch <- lm(data = data, GDP_growth_2010 ~ Emb_aut + Population_15_64_2010_growth + Capital_stock_2010_growth+Inflation_2010)
summary(mod1sch)
V_mod1sch <- vcovHC(mod1sch, type = "HC0")
V_diag_mod1sch <- sqrt(diag(V_mod1sch))


#2
data <- data.frame(data)
mod2sch <- lm(data = data, GDP_growth_2010 ~ Hie_Ega + Population_15_64_2010_growth + Capital_stock_2010_growth+Inflation_2010)
summary(mod2sch)
V_mod2sch <- vcovHC(mod2sch, type = "HC0")
V_diag_mod2sch <- sqrt(diag(V_mod2sch))


#3
data <- data.frame(data)
mod3sch <- lm(data = data, GDP_growth_2010 ~ Mas_Har + Population_15_64_2010_growth + Capital_stock_2010_growth+Inflation_2010)
summary(mod3sch)
V_mod3sch <- vcovHC(mod3sch, type = "HC0")
V_diag_mod3sch <- sqrt(diag(V_mod3sch))


stargazer(mod1sch,mod2sch,mod3sch, type="html",out = "Schwarz_components_2010.htm",se=list(V_diag_mod1sch,V_diag_mod2sch,V_diag_mod3sch))

######################################################
#### регрессии по компонентам Шварца  за 2011 год ####
######################################################

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data, LOCATION, Country, GDP_growth_2009, GDP_growth_2010, GDP_growth_2011, Population_15_64_2009_growth, Population_15_64_2010_growth, Population_15_64_2011_growth, Capital_stock_2009_growth, Capital_stock_2010_growth, Capital_stock_2011_growth, Inflation_2009, Inflation_2010, Inflation_2011, Harmony, Embeddedness, Hierarchy, Mastery, Aff_auton, Intel_auton, Egalitar, Business_freedom, Trade_freedom, Fiscal_freedom, Government_size, Monetary_freedom, Investment_freedom, Financial_freedom, Property_rights, Freedom_for_corruption, Labour_freedom)
data$Emb_aut <- data$Embeddedness-((data$Aff_auton+data$Intel_auton)/2)
data$Hie_Ega <- data$Hierarchy - data$Egalitar
data$Mas_Har <-  data$Mastery - data$Harmony

#1
data <- data.frame(data)
mod1sch <- lm(data = data, GDP_growth_2011 ~ Emb_aut + Population_15_64_2011_growth + Capital_stock_2011_growth+Inflation_2011)
summary(mod1sch)
V_mod1sch <- vcovHC(mod1sch, type = "HC0")
V_diag_mod1sch <- sqrt(diag(V_mod1sch))

#2
data <- data.frame(data)
mod2sch <- lm(data = data, GDP_growth_2011 ~ Hie_Ega + Population_15_64_2011_growth + Capital_stock_2011_growth+Inflation_2011)
summary(mod2sch)
V_mod2sch <- vcovHC(mod2sch, type = "HC0")
V_diag_mod2sch <- sqrt(diag(V_mod2sch))

#3
data <- data.frame(data)
mod3sch <- lm(data = data, GDP_growth_2011 ~ Mas_Har + Population_15_64_2011_growth + Capital_stock_2011_growth+Inflation_2011)
summary(mod3sch)
V_mod3sch <- vcovHC(mod3sch, type = "HC0")
V_diag_mod3sch <- sqrt(diag(V_mod3sch))

stargazer(mod1sch,mod2sch,mod3sch, type="html",out = "Schwarz_components_2011.htm",se=list(V_diag_mod1sch,V_diag_mod2sch,V_diag_mod3sch))



####################################
#### регрессия Business_freedom ####
####################################

##### 1 шаг
data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
View(data)
data_business_freedom <- dplyr::select(data, LOCATION, GDP_growth_2009, Country, Power.distance, Individualism, Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence, Business_freedom, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data_business_freedom <- na.omit(data_business_freedom)
View(data_business_freedom)


#Описательные статистики
summary <- summary(data_business_freedom)
summary

#строим модели
#1
data_business_freedom <- data.frame(data_business_freedom)

mod_bus_freed_1 <- lm(data = data_business_freedom, Business_freedom ~ Power.distance  + Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_bus_freed_1)
V_bus_freed_Pd_Ind <- vcovHC(mod_bus_freed_1, type = "HC0")
V_diag_bus_freed_Pd_Ind <- sqrt(diag(V_bus_freed_Pd_Ind ))
stargazer(mod_bus_freed_1, type = "text",se=list(V_diag_bus_freed_Pd_Ind))

#1.1
mod_bus_freed_1.1 <- lm(data = data_business_freedom, Business_freedom ~ Power.distance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_bus_freed_1.1)
V_bus_freed_Pd <- vcovHC(mod_bus_freed_1.1, type = "HC0")
V_diag_bus_freed_Pd <- sqrt(diag(V_bus_freed_Pd ))
stargazer(mod_bus_freed_1.1, type = "text",se=list(V_diag_bus_freed_Pd))

#1.2
mod_bus_freed_1.2 <- lm(data = data_business_freedom, Business_freedom ~ Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_bus_freed_1.2)
V_bus_freed_Ind <- vcovHC(mod_bus_freed_1.2, type = "HC0")
V_diag_bus_freed_Ind <- sqrt(diag(V_bus_freed_Ind ))
stargazer(mod_bus_freed_1.2, type = "text",se=list(V_diag_bus_freed_Ind))

#2
mod_bus_freed_2 <- lm(data = data_business_freedom, Business_freedom ~ Masculinity + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_bus_freed_2)
V_bus_freed_Masc <- vcovHC(mod_bus_freed_2, type = "HC0")
V_diag_bus_freed_Masc <- sqrt(diag(V_bus_freed_Masc))
stargazer(mod_bus_freed_2, type = "text",se=list(V_diag_bus_freed_Masc))

#3
mod_bus_freed_3 <- lm(data = data_business_freedom, Business_freedom ~ Uncertainty.avoidance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_bus_freed_3)
V_bus_freed_Unc_av <- vcovHC(mod_bus_freed_3, type = "HC0")
V_diag_bus_freed_Unc_av <- sqrt(diag(V_bus_freed_Unc_av))
stargazer(mod_bus_freed_3, type = "text",se=list(V_diag_bus_freed_Unc_av))

#4
mod_bus_freed_4 <- lm(data = data_business_freedom, Business_freedom ~ Indulgence + Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_bus_freed_4)
V_bus_freed_Ind_Long <- vcovHC(mod_bus_freed_4, type = "HC0")
V_diag_bus_freed_Ind_Long <- sqrt(diag(V_bus_freed_Ind_Long))
stargazer(mod_bus_freed_4, type = "text",se=list(V_diag_bus_freed_Ind_Long))

#4.1
mod_bus_freed_4.1 <- lm(data = data_business_freedom, Business_freedom ~ Indulgence + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_bus_freed_4.1)
V_bus_freed_Indul <- vcovHC(mod_bus_freed_4.1, type = "HC0")
V_diag_bus_freed_Indul <- sqrt(diag(V_bus_freed_Indul))
stargazer(mod_bus_freed_4.1, type = "text",se=list(V_diag_bus_freed_Indul))

#4.2
mod_bus_freed_4.2 <- lm(data = data_business_freedom, Business_freedom ~ Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_bus_freed_4.2)
V_bus_freed_Long <- vcovHC(mod_bus_freed_4.2, type = "HC0")
V_diag_bus_freed_Long <- sqrt(diag(V_bus_freed_Long))
stargazer(mod_bus_freed_4.2, type = "text",se=list(V_diag_bus_freed_Long))

stargazer(mod_bus_freed_1, mod_bus_freed_1.1, mod_bus_freed_1.2, mod_bus_freed_2, mod_bus_freed_3, mod_bus_freed_4, mod_bus_freed_4.1, mod_bus_freed_4.2, type = "html",se=list(V_diag_bus_freed_Pd_Ind, V_diag_bus_freed_Pd, V_diag_bus_freed_Ind, V_diag_bus_freed_Masc,V_diag_bus_freed_Unc_av,V_diag_bus_freed_Ind_Long, V_diag_bus_freed_Indul, V_diag_bus_freed_Long),out="Business_freedom.htm")

##### 2 шаг

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <-  dplyr::select(data, GDP_growth_2009, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth, Business_freedom, Trade_freedom, Fiscal_freedom, Government_size, Monetary_freedom, Investment_freedom, Financial_freedom, Property_rights, Freedom_for_corruption, Labour_freedom)
data <- na.omit(data)
View(data)
#строим модель

data_business_freedom <- data.frame(data_business_freedom)
mod_step2 <- lm(data = data, GDP_growth_2009 ~ Business_freedom + Inflation_2009 + Population_15_64_2009_growth + Capital_stock_2009_growth)
summary(mod_step2)
vif(mod_step2)

crPlots(mod_step2)
#тест Рамсея
resettest(mod_step2)
#ничего не пропущено

#гетероскедастичность
bptest(mod_step2) 
white_lm(mod_step2)

V_bus_freed <- vcovHC(mod_step2, type = "HC0")
V_diag_bus_freed <- sqrt(diag(V_bus_freed))


#выгружаем модель
library(stargazer)
stargazer(mod_step2, type = "text",se=list(V_diag_bus_freed))

###################################
##### регрессия Trade_freedom #####
###################################

##### 1 шаг
data_trade_freedom <- dplyr::select(data, LOCATION, GDP_growth_2009, Country, Power.distance, Individualism, Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence, Trade_freedom, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data_trade_freedom <- na.omit(data_trade_freedom)
View(data_trade_freedom)


#Описательные статистики
summary <- summary(data_trade_freedom)
summary

#строим модели
#1
data_trade_freedom <- data.frame(data_trade_freedom)

mod_tr_freed_1 <- lm(data = data_trade_freedom, Trade_freedom ~ Power.distance  + Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_tr_freed_1)
V_tr_freed_Pd_Ind <- vcovHC(mod_tr_freed_1, type = "HC0")
V_diag_tr_freed_Pd_Ind <- sqrt(diag(V_tr_freed_Pd_Ind ))
stargazer(mod_tr_freed_1, type = "text",se=list(V_diag_tr_freed_Pd_Ind))

#1.1
mod_tr_freed_1.1 <- lm(data = data_trade_freedom, Trade_freedom ~ Power.distance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_tr_freed_1.1)
V_tr_freed_Pd <- vcovHC(mod_tr_freed_1.1, type = "HC0")
V_diag_tr_freed_Pd <- sqrt(diag(V_tr_freed_Pd ))
stargazer(mod_tr_freed_1.1, type = "text",se=list(V_diag_tr_freed_Pd))

#1.2
mod_tr_freed_1.2 <- lm(data = data_trade_freedom, Trade_freedom ~ Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_tr_freed_1.2)
V_tr_freed_Ind <- vcovHC(mod_tr_freed_1.2, type = "HC0")
V_diag_tr_freed_Ind <- sqrt(diag(V_tr_freed_Ind))
stargazer(mod_tr_freed_1.2, type = "text",se=list(V_diag_tr_freed_Ind))

#2
mod_tr_freed_2 <- lm(data = data_trade_freedom, Trade_freedom ~ Masculinity + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_tr_freed_1)
V_tr_freed_Masc <- vcovHC(mod_tr_freed_2, type = "HC0")
V_diag_tr_freed_Masc <- sqrt(diag(V_tr_freed_Masc))
stargazer(mod_tr_freed_2, type = "text",se=list(V_diag_tr_freed_Masc))

#3
mod_tr_freed_3 <- lm(data = data_trade_freedom, Trade_freedom ~ Uncertainty.avoidance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_tr_freed_3)
V_tr_freed_Unc_av <- vcovHC(mod_tr_freed_3, type = "HC0")
V_diag_tr_freed_Unc_av <- sqrt(diag(V_tr_freed_Unc_av))
stargazer(mod_tr_freed_3, type = "text",se=list(V_diag_tr_freed_Unc_av))

#4
mod_tr_freed_4 <- lm(data = data_trade_freedom, Trade_freedom ~ Indulgence + Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_tr_freed_4)
V_tr_freed_Ind_Long <- vcovHC(mod_tr_freed_4, type = "HC0")
V_diag_tr_freed_Ind_Long <- sqrt(diag(V_tr_freed_Ind_Long))
stargazer(mod_tr_freed_4, type = "text",se=list(V_diag_tr_freed_Ind_Long))

#4.1
mod_tr_freed_4.1 <- lm(data = data_trade_freedom, Trade_freedom ~ Indulgence + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_tr_freed_4.1)
V_tr_freed_Indul <- vcovHC(mod_tr_freed_4.1, type = "HC0")
V_diag_tr_freed_Indul <- sqrt(diag(V_tr_freed_Indul))
stargazer(mod_tr_freed_4.1, type = "text",se=list(V_diag_tr_freed_Indul))

#4.2
mod_tr_freed_4.2 <- lm(data = data_trade_freedom, Trade_freedom ~ Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_tr_freed_4.2)
V_tr_freed_Long <- vcovHC(mod_tr_freed_4.2, type = "HC0")
V_diag_tr_freed_Long <- sqrt(diag(V_tr_freed_Long))
stargazer(mod_tr_freed_4.2, type = "text",se=list(V_diag_tr_freed_Long))

stargazer(mod_tr_freed_1, mod_tr_freed_1.1, mod_tr_freed_1.2, mod_tr_freed_2, mod_tr_freed_3, mod_tr_freed_4, mod_tr_freed_4.1, mod_tr_freed_4.2, type = "html", se=list(V_diag_tr_freed_Pd_Ind, V_diag_tr_freed_Pd,V_diag_tr_freed_Ind, V_diag_tr_freed_Masc,V_diag_tr_freed_Unc_av,V_diag_tr_freed_Ind_Long, V_diag_tr_freed_Indul, V_diag_tr_freed_Long), out="Trade_freedom.htm")

##### 2 шаг

#строим модель
mod_step2 <- lm(data = data, GDP_growth_2009 ~ Trade_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step2)
vif(mod_step2)

crPlots(mod_step2)
#тест Рамсея
resettest(mod_step2)
#ничего не пропущено

#гетероскедастичность
bptest(mod_step2) 
white_lm(mod_step2)

V_tr_freed <- vcovHC(mod_step2, type = "HC0")
V_diag_tr_freed <- sqrt(diag(V_tr_freed))

#выгружаем модель
library(stargazer)
stargazer(mod_step2, type = "text",se=list(V_diag_tr_freed))

###################################
##### регрессия Fiscal_freedom ####
###################################

##### 1 шаг
data_fiscal_freedom <- dplyr::select(data, LOCATION, GDP_growth_2009, Country, Power.distance, Individualism, Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence, Fiscal_freedom, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data_fiscal_freedom <- na.omit(data_fiscal_freedom)
View(data_fiscal_freedom)

#Описательные статистики
summary <- summary(data_fiscal_freedom)
summary

#строим модели
#1
data_fiscal_freedom <- data.frame(data_fiscal_freedom)

mod_fisc_freed_1 <- lm(data = data_fiscal_freedom, Fiscal_freedom ~ Power.distance  + Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_fisc_freed_1)
V_fisc_freed_Pd_Ind <- vcovHC(mod_fisc_freed_1, type = "HC0")
V_diag_fisc_freed_Pd_Ind <- sqrt(diag(V_fisc_freed_Pd_Ind ))
stargazer(mod_fisc_freed_1, type = "text",se=list(V_diag_fisc_freed_Pd_Ind))

#1.1
mod_fisc_freed_1.1 <- lm(data = data_fiscal_freedom, Fiscal_freedom ~ Power.distance   + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_fisc_freed_1.1)
V_fisc_freed_Pd <- vcovHC(mod_fisc_freed_1.1, type = "HC0")
V_diag_fisc_freed_Pd <- sqrt(diag(V_fisc_freed_Pd))
stargazer(mod_fisc_freed_1.1, type = "text",se=list(V_diag_fisc_freed_Pd))

#1.2
mod_fisc_freed_1.2 <- lm(data = data_fiscal_freedom, Fiscal_freedom ~ Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_fisc_freed_1.2)
V_fisc_freed_Ind <- vcovHC(mod_fisc_freed_1.2, type = "HC0")
V_diag_fisc_freed_Ind <- sqrt(diag(V_fisc_freed_Ind))
stargazer(mod_fisc_freed_1.2, type = "text",se=list(V_diag_fisc_freed_Ind))

#2
mod_fisc_freed_2 <- lm(data = data_fiscal_freedom, Fiscal_freedom ~ Masculinity + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_fisc_freed_2)
V_fisc_freed_Masc <- vcovHC(mod_fisc_freed_2, type = "HC0")
V_diag_fisc_freed_Masc <- sqrt(diag(V_fisc_freed_Masc))
stargazer(mod_fisc_freed_2, type = "text",se=list(V_diag_fisc_freed_Masc))

#3
mod_fisc_freed_3 <- lm(data = data_fiscal_freedom, Fiscal_freedom ~ Uncertainty.avoidance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_fisc_freed_3)
V_fisc_freed_Unc_av <- vcovHC(mod_fisc_freed_3, type = "HC0")
V_diag_fisc_freed_Unc_av <- sqrt(diag(V_fisc_freed_Unc_av))
stargazer(mod_fisc_freed_3, type = "text",se=list(V_diag_fisc_freed_Unc_av))

#4
mod_fisc_freed_4 <- lm(data = data_fiscal_freedom, Fiscal_freedom ~ Indulgence + Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_fisc_freed_4)
V_fisc_freed_Ind_Long <- vcovHC(mod_fisc_freed_4, type = "HC0")
V_diag_fisc_freed_Ind_Long <- sqrt(diag(V_fisc_freed_Ind_Long))
stargazer(mod_fisc_freed_4, type = "text",se=list(V_diag_fisc_freed_Ind_Long))

#4.1
mod_fisc_freed_4.1 <- lm(data = data_fiscal_freedom, Fiscal_freedom ~ Indulgence  + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_fisc_freed_4.1)
V_fisc_freed_Indul <- vcovHC(mod_fisc_freed_4.1, type = "HC0")
V_diag_fisc_freed_Indul <- sqrt(diag(V_fisc_freed_Indul))
stargazer(mod_fisc_freed_4.1, type = "text",se=list(V_diag_fisc_freed_Indul))

#4.2
mod_fisc_freed_4.2 <- lm(data = data_fiscal_freedom, Fiscal_freedom ~ Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_fisc_freed_4.2)
V_fisc_freed_Long <- vcovHC(mod_fisc_freed_4.2, type = "HC0")
V_diag_fisc_freed_Long <- sqrt(diag(V_fisc_freed_Long))
stargazer(mod_fisc_freed_4.2, type = "text",se=list(V_diag_fisc_freed_Long))

stargazer(mod_fisc_freed_1, mod_fisc_freed_1.1, mod_fisc_freed_1.2, mod_fisc_freed_2, mod_fisc_freed_3, mod_fisc_freed_4, mod_fisc_freed_4.1, mod_fisc_freed_4.2, type = "html",se=list(V_diag_fisc_freed_Pd_Ind, V_diag_fisc_freed_Pd, V_diag_fisc_freed_Ind, V_diag_fisc_freed_Masc,V_diag_fisc_freed_Unc_av,V_diag_fisc_freed_Ind_Long, V_diag_fisc_freed_Indul, V_diag_fisc_freed_Long), out="Fiscal_freedom.htm")

##### 2 шаг

#строим модель
mod_step2 <- lm(data = data, GDP_growth_2009 ~ Fiscal_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step2)
vif(mod_step2)

crPlots(mod_step2)
#тест Рамсея
resettest(mod_step2)
#ничего не пропущено

#гетероскедастичность
bptest(mod_step2) 
white_lm(mod_step2)

V_fisc_freed <- vcovHC(mod_step2, type = "HC0")
V_diag_fisc_freed <- sqrt(diag(V_fisc_freed))

#выгружаем модель
library(stargazer)
stargazer(mod_step2, type = "text",se=list(V_diag_fisc_freed))

###################################
#### регрессия Government_size ####
###################################

##### 1 шаг
data_government_size <- dplyr::select(data, LOCATION, GDP_growth_2009, Country, Power.distance, Individualism, Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence, Government_size, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data_government_size <- na.omit(data_government_size)
View(data_government_size)


#Описательные статистики
summary <- summary(data_government_size)
summary

#строим модели
#1
data_government_size <- data.frame(data_government_size)

mod_govern_size_1 <- lm(data = data_government_size, Government_size ~ Power.distance  + Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_govern_size_1)
V_govern_size_Pd_Ind <- vcovHC(mod_govern_size_1, type = "HC0")
V_diag_govern_size_Pd_Ind <- sqrt(diag(V_govern_size_Pd_Ind ))
stargazer(mod_govern_size_1, type = "text",se=list(V_diag_govern_size_Pd_Ind))

#1.1
mod_govern_size_1.1 <- lm(data = data_government_size, Government_size ~ Power.distance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_govern_size_1.1)
V_govern_size_Pd <- vcovHC(mod_govern_size_1.1, type = "HC0")
V_diag_govern_size_Pd <- sqrt(diag(V_govern_size_Pd ))
stargazer(mod_govern_size_1.1, type = "text",se=list(V_diag_govern_size_Pd))

#1.2
mod_govern_size_1.2 <- lm(data = data_government_size, Government_size ~ Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_govern_size_1.2)
V_govern_size_Ind <- vcovHC(mod_govern_size_1.2, type = "HC0")
V_diag_govern_size_Ind <- sqrt(diag(V_govern_size_Ind ))
stargazer(mod_govern_size_1.2, type = "text",se=list(V_diag_govern_size_Ind))

#2
mod_govern_size_2 <- lm(data = data_government_size, Government_size ~ Masculinity + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_govern_size_2)
V_govern_size_Masc <- vcovHC(mod_govern_size_2, type = "HC0")
V_diag_govern_size_Masc <- sqrt(diag(V_govern_size_Masc))
stargazer(mod_govern_size_2, type = "text",se=list(V_diag_govern_size_Masc))

#3
mod_govern_size_3 <- lm(data = data_government_size, Government_size ~ Uncertainty.avoidance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_govern_size_3)
V_govern_size_Unc_av <- vcovHC(mod_govern_size_3, type = "HC0")
V_diag_govern_size_Unc_av <- sqrt(diag(V_govern_size_Unc_av))
stargazer(mod_govern_size_3, type = "text",se=list(V_diag_govern_size_Unc_av))

#4
mod_govern_size_4<- lm(data = data_government_size, Government_size ~ Indulgence + Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_govern_size_4)
V_gov_size_Ind_Long <- vcovHC(mod_govern_size_4, type = "HC0")
V_diag_gov_size_Ind_Long <- sqrt(diag(V_gov_size_Ind_Long))
stargazer(mod_govern_size_4, type = "text",se=list(V_diag_gov_size_Ind_Long))

#4.1
mod_govern_size_4.1 <- lm(data = data_government_size, Government_size ~ Indulgence + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_govern_size_4.1)
V_gov_size_Indul <- vcovHC(mod_govern_size_4.1, type = "HC0")
V_diag_gov_size_Indul<- sqrt(diag(V_gov_size_Indul))
stargazer(mod_govern_size_4.1, type = "text",se=list(V_diag_gov_size_Indul))

#4.2
mod_govern_size_4.2 <- lm(data = data_government_size, Government_size ~ Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_govern_size_4.2)
V_gov_size_Long <- vcovHC(mod_govern_size_4.2, type = "HC0")
V_diag_gov_size_Long <- sqrt(diag(V_gov_size_Long))
stargazer(mod_govern_size_4.2, type = "text",se=list(V_diag_gov_size_Long))

stargazer(mod_govern_size_1, mod_govern_size_1.1, mod_govern_size_1.2, mod_govern_size_2, mod_govern_size_3, mod_govern_size_4, mod_govern_size_4.1, mod_govern_size_4.2, type = "html",se=list(V_diag_govern_size_Pd_Ind, V_diag_govern_size_Pd, V_diag_govern_size_Ind, V_diag_govern_size_Masc,V_diag_govern_size_Unc_av,V_diag_gov_size_Ind_Long, V_diag_gov_size_Indul, V_diag_gov_size_Long), out = "Government_size.htm")

##### 2 шаг

#строим модель
mod_step2 <- lm(data = data, GDP_growth_2009 ~ Government_size + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step2)
vif(mod_step2)

crPlots(mod_step2)
#тест Рамсея
resettest(mod_step2)
#ничего не пропущено

#гетероскедастичность
bptest(mod_step2) 
white_lm(mod_step2)

V_govern_size <- vcovHC(mod_step2, type = "HC0")
V_diag_govern_size <- sqrt(diag(V_govern_size))

#выгружаем модель
library(stargazer)
stargazer(mod_step2, type = "text",se=list(V_diag_govern_size))

###################################
### регрессия Monetary_freedom ####
###################################

##### 1 шаг
data_monet_freedom <- dplyr::select(data, LOCATION, GDP_growth_2009, Country, Power.distance, Individualism, Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence, Monetary_freedom, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data_monet_freedom <- na.omit(data_monet_freedom)
View(data_monet_freedom)

#Описательные статистики
summary <- summary(data_monet_freedom)
summary

#строим модели
#1
data_monet_freedom <- data.frame(data_monet_freedom)

mod_monet_freed_1 <- lm(data = data_monet_freedom, Monetary_freedom ~ Power.distance  + Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_monet_freed_1)
V_monet_freed_Pd_Ind <- vcovHC(mod_monet_freed_1, type = "HC0")
V_diag_monet_freed_Pd_Ind <- sqrt(diag(V_monet_freed_Pd_Ind ))
stargazer(mod_monet_freed_1, type = "text",se=list(V_diag_monet_freed_Pd_Ind))

#1.1
mod_monet_freed_1.1 <- lm(data = data_monet_freedom, Monetary_freedom ~ Power.distance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_monet_freed_1.1)
V_monet_freed_Pd <- vcovHC(mod_monet_freed_1.1, type = "HC0")
V_diag_monet_freed_Pd <- sqrt(diag(V_monet_freed_Pd))
stargazer(mod_monet_freed_1.1, type = "text",se=list(V_diag_monet_freed_Pd))

#1.2
mod_monet_freed_1.2 <- lm(data = data_monet_freedom, Monetary_freedom ~ Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_monet_freed_1.2)
V_monet_freed_Ind <- vcovHC(mod_monet_freed_1.2, type = "HC0")
V_diag_monet_freed_Ind <- sqrt(diag(V_monet_freed_Ind))
stargazer(mod_monet_freed_1.2, type = "text",se=list(V_diag_monet_freed_Ind))

#2
mod_monet_freed_2 <- lm(data = data_monet_freedom, Monetary_freedom ~ Masculinity + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_monet_freed_2)
V_monet_freed_Masc <- vcovHC(mod_monet_freed_2, type = "HC0")
V_diag_monet_freed_Masc <- sqrt(diag(V_monet_freed_Masc))
stargazer(mod_monet_freed_2, type = "text",se=list(V_diag_monet_freed_Masc))

#3
mod_monet_freed_3 <- lm(data = data_monet_freedom, Monetary_freedom ~ Uncertainty.avoidance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_monet_freed_3)
V_monet_freed_Unc_av <- vcovHC(mod_monet_freed_3, type = "HC0")
V_diag_monet_freed_Unc_av <- sqrt(diag(V_monet_freed_Unc_av))
stargazer(mod_monet_freed_3, type = "text",se=list(V_diag_monet_freed_Unc_av))

#4
mod_monet_freed_4 <- lm(data = data_monet_freedom, Monetary_freedom ~ Indulgence + Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_monet_freed_4)
V_monet_freed_Ind_Long <- vcovHC(mod_monet_freed_4, type = "HC0")
V_diag_monet_freed_Ind_Long <- sqrt(diag(V_monet_freed_Ind_Long))
stargazer(mod_monet_freed_4, type = "text",se=list(V_diag_monet_freed_Ind_Long))

#4.1
mod_monet_freed_4.1 <- lm(data = data_monet_freedom, Monetary_freedom ~ Indulgence + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_monet_freed_4.1)
V_monet_freed_Indul <- vcovHC(mod_monet_freed_4.1, type = "HC0")
V_diag_monet_freed_Indul <- sqrt(diag(V_monet_freed_Indul))
stargazer(mod_monet_freed_4.1, type = "text",se=list(V_diag_monet_freed_Indul))

#4.2
mod_monet_freed_4.2 <- lm(data = data_monet_freedom, Monetary_freedom ~ Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_monet_freed_4.2)
V_monet_freed_Long <- vcovHC(mod_monet_freed_4.2, type = "HC0")
V_diag_monet_freed_Long <- sqrt(diag(V_monet_freed_Long))
stargazer(mod_monet_freed_4.2, type = "text",se=list(V_diag_monet_freed_Long))

stargazer(mod_monet_freed_1, mod_monet_freed_1.1, mod_monet_freed_1.2, mod_monet_freed_2, mod_monet_freed_3, mod_monet_freed_4, mod_monet_freed_4.1, mod_monet_freed_4.2, type = "html",se=list(V_diag_monet_freed_Pd_Ind, V_diag_monet_freed_Pd, V_diag_monet_freed_Ind,  V_diag_monet_freed_Masc,V_diag_monet_freed_Unc_av,V_diag_monet_freed_Ind_Long, V_diag_monet_freed_Indul, V_diag_monet_freed_Long), out="Monetary_freedom.htm")

##### 2 шаг

#строим модель
data_monet_freedom <- data.frame(data_monet_freedom)
mod_step2 <- lm(data = data_monet_freedom, GDP_growth_2009 ~ Monetary_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step2)
vif(mod_step2)

crPlots(mod_step2)
#тест Рамсея
resettest(mod_step2)
#ничего не пропущено

#гетероскедастичность
bptest(mod_step2) 
white_lm(mod_step2)

V_monet_freed <- vcovHC(mod_step2, type = "HC0")
V_diag_monet_freed <- sqrt(diag(V_monet_freed))


#выгружаем модель
library(stargazer)
stargazer(mod_step2, type = "text",se=list(V_diag_monet_freed))

###################################
## регрессия Investment_freedom ###
###################################

##### 1 шаг
data_invest_freedom <- dplyr::select(data, LOCATION, GDP_growth_2009, Country, Power.distance, Individualism, Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence, Investment_freedom, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data_invest_freedom <- na.omit(data_invest_freedom)
View(data_invest_freedom)


#Описательные статистики
summary <- summary(data_invest_freedom)
summary

#строим модели
#1
data_invest_freedom <- data.frame(data_invest_freedom)

mod_invest_freed_1 <- lm(data = data_invest_freedom, Investment_freedom ~ Power.distance  + Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_invest_freed_1)
V_invest_freed_Pd_Ind <- vcovHC(mod_invest_freed_1, type = "HC0")
V_diag_invest_freed_Pd_Ind <- sqrt(diag(V_invest_freed_Pd_Ind ))
stargazer(mod_invest_freed_1, type = "text",se=list(V_diag_invest_freed_Pd_Ind))

#1.1
mod_invest_freed_1.1 <- lm(data = data_invest_freedom, Investment_freedom ~ Power.distance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_invest_freed_1.1)
V_invest_freed_Pd <- vcovHC(mod_invest_freed_1.1, type = "HC0")
V_diag_invest_freed_Pd <- sqrt(diag(V_invest_freed_Pd))
stargazer(mod_invest_freed_1.1, type = "text",se=list(V_diag_invest_freed_Pd))

#1.2
mod_invest_freed_1.2 <- lm(data = data_invest_freedom, Investment_freedom ~ Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_invest_freed_1.2)
V_invest_freed_Ind <- vcovHC(mod_invest_freed_1.2, type = "HC0")
V_diag_invest_freed_Ind <- sqrt(diag(V_invest_freed_Ind))
stargazer(mod_invest_freed_1.2, type = "text",se=list(V_diag_invest_freed_Ind))

#2
mod_invest_freed_2 <- lm(data = data_invest_freedom, Investment_freedom ~ Masculinity + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_invest_freed_2)
V_invest_freed_Masc <- vcovHC(mod_invest_freed_2, type = "HC0")
V_diag_invest_freed_Masc <- sqrt(diag(V_invest_freed_Masc))
stargazer(mod_invest_freed_2, type = "text",se=list(V_diag_invest_freed_Masc))

#3
mod_invest_freed_3 <- lm(data = data_invest_freedom, Investment_freedom ~ Uncertainty.avoidance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_invest_freed_3)
V_invest_freed_Unc_av <- vcovHC(mod_invest_freed_3, type = "HC0")
V_diag_invest_freed_Unc_av <- sqrt(diag(V_invest_freed_Unc_av))
stargazer(mod_invest_freed_3, type = "text",se=list(V_diag_invest_freed_Unc_av))

#4
mod_invest_freed_4 <- lm(data = data_invest_freedom, Investment_freedom ~ Indulgence + Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_invest_freed_4)
V_invest_freed_Ind_Long <- vcovHC(mod_invest_freed_4, type = "HC0")
V_diag_invest_freed_Ind_Long <- sqrt(diag(V_invest_freed_Ind_Long))
stargazer(mod_invest_freed_4, type = "text",se=list(V_diag_invest_freed_Ind_Long))

#4.1
mod_invest_freed_4.1 <- lm(data = data_invest_freedom, Investment_freedom ~ Indulgence + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_invest_freed_4.1)
V_invest_freed_Ind <- vcovHC(mod_invest_freed_4.1, type = "HC0")
V_diag_invest_freed_Ind <- sqrt(diag(V_invest_freed_Ind))
stargazer(mod_invest_freed_4.1, type = "text",se=list(V_diag_invest_freed_Ind))

#4.2
mod_invest_freed_4.2 <- lm(data = data_invest_freedom, Investment_freedom ~ Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_invest_freed_4.2)
V_invest_freed_Long <- vcovHC(mod_invest_freed_4.2, type = "HC0")
V_diag_invest_freed_Long <- sqrt(diag(V_invest_freed_Long))
stargazer(mod_invest_freed_4.2, type = "text",se=list(V_diag_invest_freed_Long))

stargazer(mod_invest_freed_1, mod_invest_freed_1.1, mod_invest_freed_1.2, mod_invest_freed_2, mod_invest_freed_3, mod_invest_freed_4, mod_invest_freed_4.1, mod_invest_freed_4.2, type = "html",se=list(V_diag_invest_freed_Pd_Ind, V_diag_invest_freed_Pd, V_diag_invest_freed_Ind, V_diag_invest_freed_Masc,V_diag_invest_freed_Unc_av,V_diag_invest_freed_Ind_Long, V_diag_invest_freed_Ind, V_diag_invest_freed_Long), out="Investment_freedom.htm")

##### 2 шаг

#строим модель
mod_step2 <- lm(data = data, GDP_growth_2009 ~ Investment_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step2)
vif(mod_step2)

crPlots(mod_step2)
#тест Рамсея
resettest(mod_step2)
#ничего не пропущено

#гетероскедастичность
bptest(mod_step2) 
white_lm(mod_step2)

V_invest_freed <- vcovHC(mod_step2, type = "HC0")
V_diag_invest_freed <- sqrt(diag(V_invest_freed))

#выгружаем модель
library(stargazer)
stargazer(mod_step2, type = "text",se=list(V_diag_invest_freed))

#####################################
### регрессия Financial_freedom #####
#####################################

##### 1 шаг
data_finance_freedom <- dplyr::select(data, LOCATION, GDP_growth_2009, Country, Power.distance, Individualism, Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence, Financial_freedom, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data_finance_freedom <- na.omit(data_finance_freedom)
View(data_finance_freedom)



#Описательные статистики
summary <- summary(data_finance_freedom)
summary

#строим модели
#1
data_finance_freedom <- data.frame(data_finance_freedom)

mod_finance_freed_1 <- lm(data = data_finance_freedom, Financial_freedom ~ Power.distance  + Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_finance_freed_1)
V_finance_freed_Pd_Ind <- vcovHC(mod_finance_freed_1, type = "HC0")
V_diag_finance_freed_Pd_Ind <- sqrt(diag(V_finance_freed_Pd_Ind ))
stargazer(mod_finance_freed_1, type = "text",se=list(V_diag_finance_freed_Pd_Ind))

#1.1
mod_finance_freed_1.1 <- lm(data = data_finance_freedom, Financial_freedom ~ Power.distance  + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_finance_freed_1.1)
V_finance_freed_Pd <- vcovHC(mod_finance_freed_1.1, type = "HC0")
V_diag_finance_freed_Pd <- sqrt(diag(V_finance_freed_Pd))
stargazer(mod_finance_freed_1.1, type = "text",se=list(V_diag_finance_freed_Pd))

#1.2
mod_finance_freed_1.2 <- lm(data = data_finance_freedom, Financial_freedom ~ Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_finance_freed_1.2)
V_finance_freed_Ind <- vcovHC(mod_finance_freed_1.2, type = "HC0")
V_diag_finance_freed_Ind <- sqrt(diag(V_finance_freed_Ind))
stargazer(mod_finance_freed_1.2, type = "text",se=list(V_diag_finance_freed_Ind))

#2
mod_finance_freed_2 <- lm(data = data_finance_freedom, Financial_freedom ~ Masculinity + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_finance_freed_2)
V_finance_freed_Masc <- vcovHC(mod_finance_freed_2, type = "HC0")
V_diag_finance_freed_Masc <- sqrt(diag(V_finance_freed_Masc))
stargazer(mod_finance_freed_2, type = "text",se=list(V_diag_finance_freed_Masc))

#3
mod_finance_freed_3 <- lm(data = data_finance_freedom, Financial_freedom ~ Uncertainty.avoidance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_finance_freed_3)
V_finance_freed_Unc_av <- vcovHC(mod_finance_freed_3, type = "HC0")
V_diag_finance_freed_Unc_av <- sqrt(diag(V_finance_freed_Unc_av))
stargazer(mod_finance_freed_3, type = "text",se=list(V_diag_finance_freed_Unc_av))

#4
mod_finance_freed_4 <- lm(data = data_finance_freedom, Financial_freedom ~ Indulgence + Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_finance_freed_4)
V_finance_freed_Ind_Long <- vcovHC(mod_finance_freed_4, type = "HC0")
V_diag_finance_freed_Ind_Long <- sqrt(diag(V_finance_freed_Ind_Long))
stargazer(mod_finance_freed_4, type = "text",se=list(V_diag_finance_freed_Ind_Long))

#4.1
mod_finance_freed_4.1 <- lm(data = data_finance_freedom, Financial_freedom ~ Indulgence + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_finance_freed_4.1)
V_finance_freed_Indul <- vcovHC(mod_finance_freed_4.1, type = "HC0")
V_diag_finance_freed_Indul <- sqrt(diag(V_finance_freed_Indul))
stargazer(mod_finance_freed_4.1, type = "text",se=list(V_diag_finance_freed_Indul))

#4.2
mod_finance_freed_4.2 <- lm(data = data_finance_freedom, Financial_freedom ~ Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_finance_freed_4.2)
V_finance_freed_Long <- vcovHC(mod_finance_freed_4.2, type = "HC0")
V_diag_finance_freed_Long <- sqrt(diag(V_finance_freed_Long))
stargazer(mod_finance_freed_4.2, type = "text",se=list(V_diag_finance_freed_Long))


stargazer(mod_finance_freed_1, mod_finance_freed_1.1, mod_finance_freed_1.2, mod_finance_freed_2, mod_finance_freed_3, mod_finance_freed_4, mod_finance_freed_4.1, mod_finance_freed_4.2, type = "html",se=list(V_diag_finance_freed_Pd_Ind, V_diag_finance_freed_Pd, V_diag_finance_freed_Ind, V_diag_finance_freed_Masc,V_diag_finance_freed_Unc_av,V_diag_finance_freed_Ind_Long, V_diag_finance_freed_Indul, V_diag_finance_freed_Long), out="Financial_freedom.htm")

##### 2 шаг

#строим модель
mod_step2 <- lm(data = data, GDP_growth_2009 ~ Financial_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step2)
vif(mod_step2)

crPlots(mod_step2)
#тест Рамсея
resettest(mod_step2)
#ничего не пропущено

#гетероскедастичность
bptest(mod_step2) 
white_lm(mod_step2)

V_finance_freed <- vcovHC(mod_step2, type = "HC0")
V_diag_finance_freed <- sqrt(diag(V_finance_freed))


#выгружаем модель
library(stargazer)
stargazer(mod_step2, type = "text",se=list(V_diag_finance_freed))


#######################################
###### регрессия Property_rights ######
#######################################

##### 1 шаг
data_property_rights <- dplyr::select(data, LOCATION, GDP_growth_2009, Country, Power.distance, Individualism, Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence, Property_rights, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data_property_rights <- na.omit(data_property_rights)
View(data_property_rights)

#Описательные статистики
summary <- summary(data_property_rights)
summary

#строим модели
#1
data_property_rights <- data.frame(data_property_rights)

mod_prop_r_1 <- lm(data = data_property_rights, Property_rights ~ Power.distance  + Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_prop_r_1)
V_prop_Pd_Ind <- vcovHC(mod_prop_r_1, type = "HC0")
V_diag_prop_Pd_Ind <- sqrt(diag(V_prop_Pd_Ind ))
stargazer(mod_prop_r_1, type = "text",se=list(V_diag_prop_Pd_Ind))

#1.1
mod_prop_r_1.1 <- lm(data = data_property_rights, Property_rights ~ Power.distance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_prop_r_1.1)
V_prop_Pd <- vcovHC(mod_prop_r_1.1, type = "HC0")
V_diag_prop_Pd <- sqrt(diag(V_prop_Pd))
stargazer(mod_prop_r_1.1, type = "text",se=list(V_diag_prop_Pd))

#1.2
mod_prop_r_1.2 <- lm(data = data_property_rights, Property_rights ~ Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_prop_r_1.2)
V_prop_Ind <- vcovHC(mod_prop_r_1.2, type = "HC0")
V_diag_prop_Ind <- sqrt(diag(V_prop_Ind))
stargazer(mod_prop_r_1.2, type = "text",se=list(V_diag_prop_Ind))

#2
mod_prop_r_2 <- lm(data = data_property_rights, Property_rights ~ Masculinity + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_prop_r_2)
V_prop_Masc <- vcovHC(mod_prop_r_2, type = "HC0")
V_diag_prop_Masc <- sqrt(diag(V_prop_Masc))
stargazer(mod_prop_r_2, type = "text",se=list(V_diag_prop_Masc))

#3
mod_prop_r_3 <- lm(data = data_property_rights, Property_rights ~ Uncertainty.avoidance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_prop_r_3)
V_prop_Unc_av <- vcovHC(mod_prop_r_3, type = "HC0")
V_diag_prop_Unc_av <- sqrt(diag(V_prop_Unc_av))
stargazer(mod_prop_r_3, type = "text",se=list(V_diag_prop_Unc_av))

#4
mod_prop_r_4 <- lm(data = data_property_rights, Property_rights ~ Indulgence + Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_prop_r_4)
V_prop_Ind_Long <- vcovHC(mod_prop_r_4, type = "HC0")
V_diag_prop_Ind_Long <- sqrt(diag(V_prop_Ind_Long))
stargazer(mod_prop_r_4, type = "text",se=list(V_diag_prop_Ind_Long))

#4.1
mod_prop_r_4.1 <- lm(data = data_property_rights, Property_rights ~ Indulgence + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_prop_r_4.1)
V_prop_Indul <- vcovHC(mod_prop_r_4.1, type = "HC0")
V_diag_prop_Indul <- sqrt(diag(V_prop_Indul))
stargazer(mod_prop_r_4.1, type = "text",se=list(V_diag_prop_Indul))

#4.2
mod_prop_r_4.2 <- lm(data = data_property_rights, Property_rights ~ Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_prop_r_4.2)
V_prop_Long <- vcovHC(mod_prop_r_4.2, type = "HC0")
V_diag_prop_Long <- sqrt(diag(V_prop_Long))
stargazer(mod_prop_r_4.2, type = "text",se=list(V_diag_prop_Long))

stargazer(mod_prop_r_1, mod_prop_r_1.1, mod_prop_r_1.2, mod_prop_r_2, mod_prop_r_3, mod_prop_r_4, mod_prop_r_4.1, mod_prop_r_4.2, type = "html",se=list(V_diag_prop_Pd_Ind, V_diag_prop_Pd, V_diag_prop_Ind, V_diag_prop_Masc,V_diag_prop_Unc_av, V_diag_prop_Ind_Long, V_diag_prop_Indul, V_diag_prop_Long), out="Property_rights.htm")


##### 2 шаг

#строим модель
mod_step2 <- lm(data = data, GDP_growth_2009 ~ Property_rights + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step2)
vif(mod_step2)

crPlots(mod_step2)
#тест Рамсея
resettest(mod_step2)
#ничего не пропущено

#гетероскедастичность
bptest(mod_step2) 
white_lm(mod_step2) 

V_property_rights <- vcovHC(mod_step2, type = "HC0")
V_diag_property_rights <- sqrt(diag(V_property_rights))


#выгружаем модель
library(stargazer)
stargazer(mod_step2, type = "text",se=list(V_diag_property_rights))

#######################################
## регрессия Freedom_for_corruption ###
#######################################

##### 1 шаг
data_freed_for_corrupt <- dplyr::select(data, LOCATION, GDP_growth_2009, Country, Power.distance, Individualism, Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence, Freedom_for_corruption, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data_freed_for_corrupt <- na.omit(data_freed_for_corrupt)
View(data_freed_for_corrupt)

#Описательные статистики
summary <- summary(data_freed_for_corrupt)
summary

#строим модели
#1
data_freed_for_corrupt <- data.frame(data_freed_for_corrupt)

mod_freed_for_corrupt_1 <- lm(data = data_freed_for_corrupt, Freedom_for_corruption ~ Power.distance  + Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_freed_for_corrupt_1)
V_freed_for_corrupt_Pd_Ind <- vcovHC(mod_freed_for_corrupt_1, type = "HC0")
V_diag_freed_for_corrupt_Pd_Ind <- sqrt(diag(V_freed_for_corrupt_Pd_Ind ))
stargazer(mod_freed_for_corrupt_1, type = "text",se=list(V_diag_freed_for_corrupt_Pd_Ind))

#1.1
mod_freed_for_corrupt_1.1 <- lm(data = data_freed_for_corrupt, Freedom_for_corruption ~ Power.distance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_freed_for_corrupt_1.1)
V_freed_for_corrupt_Pd <- vcovHC(mod_freed_for_corrupt_1.1, type = "HC0")
V_diag_freed_for_corrupt_Pd <- sqrt(diag(V_freed_for_corrupt_Pd))
stargazer(mod_freed_for_corrupt_1.1, type = "text",se=list(V_diag_freed_for_corrupt_Pd))

#1.2
mod_freed_for_corrupt_1.2 <- lm(data = data_freed_for_corrupt, Freedom_for_corruption ~ Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_freed_for_corrupt_1.2)
V_freed_for_corrupt_Ind <- vcovHC(mod_freed_for_corrupt_1.2, type = "HC0")
V_diag_freed_for_corrupt_Ind <- sqrt(diag(V_freed_for_corrupt_Ind))
stargazer(mod_freed_for_corrupt_1.2, type = "text",se=list(V_diag_freed_for_corrupt_Ind))

#2
mod_freed_for_corrupt_2 <- lm(data = data_freed_for_corrupt, Freedom_for_corruption ~ Masculinity + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_freed_for_corrupt_2)
V_freed_for_corrupt_Masc <- vcovHC(mod_freed_for_corrupt_2, type = "HC0")
V_diag_freed_for_corrupt_Masc <- sqrt(diag(V_freed_for_corrupt_Masc))
stargazer(mod_freed_for_corrupt_2, type = "text",se=list(V_diag_freed_for_corrupt_Masc))

#3
mod_freed_for_corrupt_3 <- lm(data = data_freed_for_corrupt, Freedom_for_corruption ~ Uncertainty.avoidance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_freed_for_corrupt_3)
V_freed_for_corrupt_Unc_av <- vcovHC(mod_freed_for_corrupt_3, type = "HC0")
V_diag_freed_for_corrupt_Unc_av <- sqrt(diag(V_freed_for_corrupt_Unc_av))
stargazer(mod_freed_for_corrupt_3, type = "text",se=list(V_diag_freed_for_corrupt_Unc_av))

#4
mod_freed_for_corrupt_4 <- lm(data = data_freed_for_corrupt, Freedom_for_corruption ~ Indulgence + Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_freed_for_corrupt_4)
V_freed_for_corrupt_Ind_Long <- vcovHC(mod_freed_for_corrupt_4, type = "HC0")
V_diag_freed_for_corrupt_Ind_Long <- sqrt(diag(V_freed_for_corrupt_Ind_Long))
stargazer(mod_freed_for_corrupt_4, type = "text",se=list(V_diag_freed_for_corrupt_Ind_Long))

#4.1
mod_freed_for_corrupt_4.1 <- lm(data = data_freed_for_corrupt, Freedom_for_corruption ~ Indulgence + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_freed_for_corrupt_4.1)
V_freed_for_corrupt_Indul <- vcovHC(mod_freed_for_corrupt_4.1, type = "HC0")
V_diag_freed_for_corrupt_Indul <- sqrt(diag(V_freed_for_corrupt_Indul))
stargazer(mod_freed_for_corrupt_4.1, type = "text",se=list(V_diag_freed_for_corrupt_Indul))

#4.2
mod_freed_for_corrupt_4.2 <- lm(data = data_freed_for_corrupt, Freedom_for_corruption ~ Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_freed_for_corrupt_4.2)
V_freed_for_corrupt_Long <- vcovHC(mod_freed_for_corrupt_4.2, type = "HC0")
V_diag_freed_for_corrupt_Long <- sqrt(diag(V_freed_for_corrupt_Long))
stargazer(mod_freed_for_corrupt_4.2, type = "text",se=list(V_diag_freed_for_corrupt_Long))

stargazer(mod_freed_for_corrupt_1, mod_freed_for_corrupt_1.1, mod_freed_for_corrupt_1.2, mod_freed_for_corrupt_2, mod_freed_for_corrupt_3, mod_freed_for_corrupt_4, mod_freed_for_corrupt_4.1, mod_freed_for_corrupt_4.2, type = "html",se=list(V_diag_freed_for_corrupt_Pd_Ind, V_diag_freed_for_corrupt_Pd, V_diag_freed_for_corrupt_Ind, V_diag_freed_for_corrupt_Masc,V_diag_freed_for_corrupt_Unc_av,V_diag_freed_for_corrupt_Ind_Long, V_diag_freed_for_corrupt_Indul, V_diag_freed_for_corrupt_Long), out="Freedom for corruption.htm")


##### 2 шаг

#строим модель
mod_step2 <- lm(data = data, GDP_growth_2009 ~ Freedom_for_corruption + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step2)
vif(mod_step2)

crPlots(mod_step2)
#тест Рамсея
resettest(mod_step2)
#ничего не пропущено

#гетероскедастичность
bptest(mod_step2) 
white_lm(mod_step2) 

V_freed_for_corrupt <- vcovHC(mod_step2, type = "HC0")
V_diag_freed_for_corrupt <- sqrt(diag(V_freed_for_corrupt))


#выгружаем модель
library(stargazer)
stargazer(mod_step2, type = "text",se=list(V_diag_freed_for_corrupt))


#######################################
#### регрессия Labour_freedom #########
#######################################

##### 1 шаг
data_lab_freed <- dplyr::select(data, LOCATION, GDP_growth_2009, Country, Power.distance, Individualism, Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence, Labour_freedom, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data_lab_freed <- na.omit(data_lab_freed)
View(data_lab_freed)

#Описательные статистики
summary <- summary(data_lab_freed)
summary

#строим модели
#1
data_lab_freed <- data.frame(data_lab_freed)

mod_lab_freed_1 <- lm(data = data_lab_freed, Labour_freedom ~ Power.distance  + Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_lab_freed_1)
V_lab_freed_Pd_Ind <- vcovHC(mod_lab_freed_1, type = "HC0")
V_diag_lab_freed_Pd_Ind <- sqrt(diag(V_lab_freed_Pd_Ind ))
stargazer(mod_lab_freed_1, type = "text",se=list(V_diag_lab_freed_Pd_Ind))

#1.1
mod_lab_freed_1.1 <- lm(data = data_lab_freed, Labour_freedom ~ Power.distance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_lab_freed_1.1)
V_lab_freed_Pd <- vcovHC(mod_lab_freed_1.1, type = "HC0")
V_diag_lab_freed_Pd <- sqrt(diag(V_lab_freed_Pd))
stargazer(mod_lab_freed_1.1, type = "text",se=list(V_diag_lab_freed_Pd))

#1.2
mod_lab_freed_1.2 <- lm(data = data_lab_freed, Labour_freedom ~  Individualism + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_lab_freed_1.2)
V_lab_freed_Ind <- vcovHC(mod_lab_freed_1.2, type = "HC0")
V_diag_lab_freed_Ind <- sqrt(diag(V_lab_freed_Ind))
stargazer(data_lab_freed_1.2, type = "text",se=list(V_diag_lab_freed_Ind))


#2
mod_lab_freed_2 <- lm(data = data_lab_freed, Labour_freedom ~ Masculinity + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_lab_freed_2)
V_lab_freed_Masc <- vcovHC(mod_lab_freed_2, type = "HC0")
V_diag_lab_freed_Masc <- sqrt(diag(V_lab_freed_Masc))
stargazer(mod_lab_freed_2, type = "text",se=list(V_diag_lab_freed_Masc))

#3
mod_lab_freed_3 <- lm(data = data_lab_freed, Labour_freedom ~ Uncertainty.avoidance + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_lab_freed_3)
V_lab_freed_Unc_av <- vcovHC(mod_lab_freed_3, type = "HC0")
V_diag_lab_freed_Unc_av <- sqrt(diag(V_lab_freed_Unc_av))
stargazer(mod_lab_freed_3, type = "text",se=list(V_diag_lab_freed_Unc_av))

#4
mod_lab_freed_4 <- lm(data = data_lab_freed, Labour_freedom ~ Indulgence + Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_lab_freed_4)
V_lab_freed_Ind_Long <- vcovHC(mod_lab_freed_4, type = "HC0")
V_diag_lab_freed_Ind_Long <- sqrt(diag(V_lab_freed_Ind_Long))
stargazer(mod_lab_freed_4, type = "text",se=list(V_diag_lab_freed_Ind_Long))

#4.1
mod_lab_freed_4.1 <- lm(data = data_lab_freed, Labour_freedom ~ Indulgence + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_lab_freed_4.1)
V_lab_freed_Indul <- vcovHC(mod_lab_freed_4.1, type = "HC0")
V_diag_lab_freed_Indul <- sqrt(diag(V_lab_freed_Indul))
stargazer(mod_lab_freed_4.1, type = "text",se=list(V_diag_lab_freed_Indul))

#4.2
mod_lab_freed_4.2 <- lm(data = data_lab_freed, Labour_freedom ~  Long.term.orientation + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_lab_freed_4.2)
V_lab_freed_Long <- vcovHC(mod_lab_freed_4.2, type = "HC0")
V_diag_lab_freed_Long <- sqrt(diag(V_lab_freed_Long))
stargazer(mod_lab_freed_4.2, type = "text",se=list(V_diag_lab_freed_Long))

stargazer(mod_lab_freed_1, mod_lab_freed_1.1, mod_lab_freed_1.2, mod_lab_freed_2, mod_lab_freed_3, mod_lab_freed_4, mod_lab_freed_4.1, mod_lab_freed_4.2, type = "html",se=list(V_diag_lab_freed_Pd_Ind, V_diag_lab_freed_Pd, V_diag_lab_freed_Ind, V_diag_lab_freed_Masc, V_diag_lab_freed_Unc_av, V_diag_lab_freed_Ind_Long, V_diag_lab_freed_Indul, V_diag_lab_freed_Long), out="Labour freedom.htm")


##### 2 шаг

#строим модель
mod_step2 <- lm(data = data, GDP_growth_2009 ~ Labour_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step2)
vif(mod_step2)

crPlots(mod_step2)
#тест Рамсея
resettest(mod_step2)
#ничего не пропущено

#гетероскедастичность
bptest(mod_step2) 
white_lm(mod_step2) 

V_lab_freed <- vcovHC(mod_step2, type = "HC0")
V_diag_lab_freed <- sqrt(diag(V_lab_freed))

#выгружаем модель
library(stargazer)
stargazer(mod_step2, type = "text",se=list(V_diag_lab_freed))


#Данные
#Credit_to_private_sector

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
View(data)

#Регрессии второго шага
#Business_freedom
mod_step211 <- lm(data = data, Credit_to_private_sector ~ Business_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step211)
vif(mod_step211)

crPlots(mod_step211)

#гетероскедастичность
bptest(mod_step211) 
white_lm(mod_step211) 

stargazer(mod_step211, type = "text")


#Trade_freedom
mod_step221 <- lm(data = data, Credit_to_private_sector ~ Trade_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step221)
vif(mod_step221)

crPlots(mod_step221)

#гетероскедастичность
bptest(mod_step221) 
white_lm(mod_step221)
V_tr_freed <- vcovHC(mod_step221, type = "HC0")
V_diag_tr_freed <- sqrt(diag(V_tr_freed))

stargazer(mod_step221, type = "text",se=list(V_diag_tr_freed))

#Fiscal_freedom
mod_step231 <- lm(data = data, Credit_to_private_sector ~ Fiscal_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step231)
vif(mod_step231)

crPlots(mod_step231)

#гетероскедастичность
bptest(mod_step231) 
white_lm(mod_step231) 

stargazer(mod_step231, type = "text")

#Government_size
mod_step241 <- lm(data = data, Credit_to_private_sector ~ Government_size + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step241)
vif(mod_step241)

crPlots(mod_step241)

#гетероскедастичность
bptest(mod_step241) 
white_lm(mod_step241) 

stargazer(mod_step241, type = "text")

#Monetary_freedom
mod_step251 <- lm(data = data, Credit_to_private_sector ~ Monetary_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step251)
vif(mod_step251)

crPlots(mod_step251)

#гетероскедастичность
bptest(mod_step251) 
white_lm(mod_step251) 

stargazer(mod_step251, type = "text")

#Investment_freedom
mod_step261 <- lm(data = data, Credit_to_private_sector ~ Investment_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step261)
vif(mod_step261)

crPlots(mod_step261)

#гетероскедастичность
bptest(mod_step261) 
white_lm(mod_step261) 


stargazer(mod_step261, type = "text")

#Financial_freedom
mod_step27 <- lm(data = data, Credit_to_private_sector ~ Financial_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step27)
vif(mod_step27)

crPlots(mod_step27)

#гетероскедастичность
bptest(mod_step27) 
white_lm(mod_step27) 

stargazer(mod_step27, type = "text")

#Property_rights
mod_step28 <- lm(data = data, Credit_to_private_sector ~ Property_rights + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step28)
vif(mod_step28)

crPlots(mod_step28)

#гетероскедастичность
bptest(mod_step28) 
white_lm(mod_step28) 

stargazer(mod_step28, type = "text")

#Freedom_for_corruption
mod_step29 <- lm(data = data, Credit_to_private_sector ~ Freedom_for_corruption + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step29)
vif(mod_step29)

crPlots(mod_step29)

#гетероскедастичность
bptest(mod_step29) 
white_lm(mod_step29) 

stargazer(mod_step29, type = "text")

#Labour_freedom
mod_step30 <- lm(data = data, Credit_to_private_sector ~ Labour_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step30)
vif(mod_step30)

crPlots(mod_step30)

#гетероскедастичность
bptest(mod_step30) 
white_lm(mod_step30) 

stargazer(mod_step30, type = "text")

#Данные
#Lending_borrowing

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data, Lending_borrowing, Business_freedom, Trade_freedom, Fiscal_freedom, Government_size, Monetary_freedom, Investment_freedom, Financial_freedom, Property_rights, Freedom_for_corruption, Labour_freedom, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data <- na.omit(data)
View(data)

#Регрессии второго шага
#Business_freedom
mod_step211 <- lm(data = data, Lending_borrowing ~ Business_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step211)
vif(mod_step211)

crPlots(mod_step211)

#гетероскедастичность
bptest(mod_step211) 
white_lm(mod_step211) 

stargazer(mod_step211, type = "text")


#Trade_freedom
mod_step221 <- lm(data = data, Lending_borrowing ~ Trade_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step221)
vif(mod_step221)

crPlots(mod_step221)

#гетероскедастичность
bptest(mod_step221) 
white_lm(mod_step221)


stargazer(mod_step221, type = "text")

#Fiscal_freedom
mod_step231 <- lm(data = data, Lending_borrowing ~ Fiscal_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step231)
vif(mod_step231)

crPlots(mod_step231)

#гетероскедастичность
bptest(mod_step231) 
white_lm(mod_step231) 

stargazer(mod_step231, type = "text")

#Government_size
mod_step241 <- lm(data = data, Lending_borrowing ~ Government_size + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step241)
vif(mod_step241)

crPlots(mod_step241)

stargazer(mod_step241, type = "text")

#Monetary_freedom
mod_step251 <- lm(data = data, Lending_borrowing ~ Monetary_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step251)
vif(mod_step251)

crPlots(mod_step251)

#гетероскедастичность
bptest(mod_step251) 
white_lm(mod_step251) 

stargazer(mod_step251, type = "text")

#Investment_freedom
mod_step261 <- lm(data = data, Lending_borrowing ~ Investment_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step261)
vif(mod_step261)

crPlots(mod_step261)

#гетероскедастичность
bptest(mod_step261) 
white_lm(mod_step261) 


stargazer(mod_step261, type = "text")

#Financial_freedom
mod_step27 <- lm(data = data, Lending_borrowing ~ Financial_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step27)
vif(mod_step27)

crPlots(mod_step27)

#гетероскедастичность
bptest(mod_step27) 
white_lm(mod_step27) 

stargazer(mod_step27, type = "text")

#Property_rights
mod_step28 <- lm(data = data, Lending_borrowing ~ Property_rights + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step28)
vif(mod_step28)

crPlots(mod_step28)

#гетероскедастичность
bptest(mod_step28) 
white_lm(mod_step28) 

stargazer(mod_step28, type = "text")

#Freedom_for_corruption
mod_step29 <- lm(data = data, Lending_borrowing ~ Freedom_for_corruption + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step29)
vif(mod_step29)

crPlots(mod_step29)

#гетероскедастичность
bptest(mod_step29) 
white_lm(mod_step29) 

stargazer(mod_step29, type = "text")

#Labour_freedom
mod_step30 <- lm(data = data, Lending_borrowing ~ Labour_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step30)
vif(mod_step30)

crPlots(mod_step30)

#гетероскедастичность
bptest(mod_step30) 
white_lm(mod_step30) 

stargazer(mod_step30, type = "text")

#Данные
#Tax_revenue

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data, Tax_revenue, Business_freedom, Trade_freedom, Fiscal_freedom, Government_size, Monetary_freedom, Investment_freedom, Financial_freedom, Property_rights, Freedom_for_corruption, Labour_freedom, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data <- na.omit(data)
View(data)

#Регрессии второго шага
#Business_freedom
mod_step211 <- lm(data = data, Tax_revenue ~ Business_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step211)
vif(mod_step211)

crPlots(mod_step211)

#гетероскедастичность
bptest(mod_step211) 
white_lm(mod_step211) 
V_mod_step211 <- vcovHC(mod_step211, type = "HC0")
V_diag_mod_step211 <- sqrt(diag(V_mod_step211 ))

stargazer(mod_step211, type = "text",se=list(V_diag_mod_step211))


#Trade_freedom
mod_step221 <- lm(data = data, Tax_revenue ~ Trade_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step221)
vif(mod_step221)

crPlots(mod_step221)

#гетероскедастичность
bptest(mod_step221) 
white_lm(mod_step221)


stargazer(mod_step221, type = "text")

#Fiscal_freedom
mod_step231 <- lm(data = data, Tax_revenue ~ Fiscal_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step231)
vif(mod_step231)

crPlots(mod_step231)

#гетероскедастичность
bptest(mod_step231) 
white_lm(mod_step231) 

stargazer(mod_step231, type = "text")

#Government_size
mod_step241 <- lm(data = data, Tax_revenue ~ Government_size + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step241)
vif(mod_step241)

crPlots(mod_step241)

#гетероскедастичность
bptest(mod_step241) 
white_lm(mod_step241) 

stargazer(mod_step241, type = "text")

#Monetary_freedom
mod_step251 <- lm(data = data, Tax_revenue ~ Monetary_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step251)
vif(mod_step251)

crPlots(mod_step251)

#гетероскедастичность
bptest(mod_step251) 
white_lm(mod_step251) 

stargazer(mod_step251, type = "text")

#Investment_freedom
mod_step261 <- lm(data = data, Tax_revenue ~ Investment_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step261)
vif(mod_step261)

crPlots(mod_step261)

#гетероскедастичность
bptest(mod_step261) 
white_lm(mod_step261) 


stargazer(mod_step261, type = "text")

#Financial_freedom
mod_step27 <- lm(data = data, Tax_revenue ~ Financial_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step27)
vif(mod_step27)

crPlots(mod_step27)

#гетероскедастичность
bptest(mod_step27) 
white_lm(mod_step27) 

stargazer(mod_step27, type = "text")

#Property_rights
mod_step28 <- lm(data = data, Tax_revenue ~ Property_rights + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step28)
vif(mod_step28)

crPlots(mod_step28)

#гетероскедастичность
bptest(mod_step28) 
white_lm(mod_step28) 

stargazer(mod_step28, type = "text")

#Freedom_for_corruption
mod_step29 <- lm(data = data, Tax_revenue ~ Freedom_for_corruption + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step29)
vif(mod_step29)

crPlots(mod_step29)

#гетероскедастичность
bptest(mod_step29) 
white_lm(mod_step29) 

stargazer(mod_step29, type = "text")

#Labour_freedom
mod_step30 <- lm(data = data, Tax_revenue ~ Labour_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step30)
vif(mod_step30)

crPlots(mod_step30)

#гетероскедастичность
bptest(mod_step30) 
white_lm(mod_step30) 

V_mod_step30 <- vcovHC(mod_step30, type = "HC0")
V_diag_mod_step30 <- sqrt(diag(V_mod_step30))

stargazer(mod_step30, type = "text", se=list(V_diag_mod_step30))

#Данные
#Stocks_traded

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data, Stocks_traded, Business_freedom, Trade_freedom, Fiscal_freedom, Government_size, Monetary_freedom, Investment_freedom, Financial_freedom, Property_rights, Freedom_for_corruption, Labour_freedom, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data <- na.omit(data)
View(data)

#Регрессии второго шага
#Business_freedom
mod_step211 <- lm(data = data, Stocks_traded ~ Business_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step211)
vif(mod_step211)

crPlots(mod_step211)

#гетероскедастичность
bptest(mod_step211) 
white_lm(mod_step211) 


stargazer(mod_step211, type = "text")


#Trade_freedom
mod_step221 <- lm(data = data, Stocks_traded ~ Trade_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step221)
vif(mod_step221)

crPlots(mod_step221)

#гетероскедастичность
bptest(mod_step221) 
white_lm(mod_step221)


stargazer(mod_step221, type = "text")

#Fiscal_freedom
mod_step231 <- lm(data = data, Stocks_traded ~ Fiscal_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step231)
vif(mod_step231)

crPlots(mod_step231)

#гетероскедастичность
bptest(mod_step231) 
white_lm(mod_step231) 
V_mod_step231 <- vcovHC(mod_step231, type = "HC0")
V_diag_mod_step231 <- sqrt(diag(V_mod_step231))

stargazer(mod_step231, type = "text",se=list(V_diag_mod_step231))

#Government_size
mod_step241 <- lm(data = data, Stocks_traded ~ Government_size + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step241)
vif(mod_step241)

crPlots(mod_step241)

#гетероскедастичность
bptest(mod_step241) 
white_lm(mod_step241) 
V_mod_step241 <- vcovHC(mod_step241, type = "HC0")
V_diag_mod_step241 <- sqrt(diag(V_mod_step241))

stargazer(mod_step241, type = "text")

#Monetary_freedom
mod_step251 <- lm(data = data, Stocks_traded ~ Monetary_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step251)
vif(mod_step251)

crPlots(mod_step251)

#гетероскедастичность
bptest(mod_step251) 
white_lm(mod_step251) 

stargazer(mod_step251, type = "text")

#Investment_freedom
mod_step261 <- lm(data = data, Stocks_traded ~ Investment_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step261)
vif(mod_step261)

crPlots(mod_step261)

#гетероскедастичность
bptest(mod_step261) 
white_lm(mod_step261) 


stargazer(mod_step261, type = "text")

#Financial_freedom
mod_step27 <- lm(data = data, Stocks_traded ~ Financial_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step27)
vif(mod_step27)

crPlots(mod_step27)

#гетероскедастичность
bptest(mod_step27) 
white_lm(mod_step27) 

stargazer(mod_step27, type = "text")

#Property_rights
mod_step28 <- lm(data = data, Stocks_traded ~ Property_rights + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step28)
vif(mod_step28)

crPlots(mod_step28)

#гетероскедастичность
bptest(mod_step28) 
white_lm(mod_step28) 

stargazer(mod_step28, type = "text")

#Freedom_for_corruption
mod_step29 <- lm(data = data, Stocks_traded ~ Freedom_for_corruption + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step29)
vif(mod_step29)

crPlots(mod_step29)

#гетероскедастичность
bptest(mod_step29) 
white_lm(mod_step29) 

stargazer(mod_step29, type = "text")

#Labour_freedom
mod_step30 <- lm(data = data, Stocks_traded ~ Labour_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step30)
vif(mod_step30)

crPlots(mod_step30)

#гетероскедастичность
bptest(mod_step30) 
white_lm(mod_step30) 


stargazer(mod_step30, type = "text", se=list(V_diag_mod_step30))

#Данные
#Electricity

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data, Electricity, Business_freedom, Trade_freedom, Fiscal_freedom, Government_size, Monetary_freedom, Investment_freedom, Financial_freedom, Property_rights, Freedom_for_corruption, Labour_freedom, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data <- na.omit(data)
View(data)

#Регрессии второго шага
#Business_freedom
mod_step211 <- lm(data = data, log(Electricity) ~ Business_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step211)
vif(mod_step211)

crPlots(mod_step211)

#гетероскедастичность
bptest(mod_step211) 
white_lm(mod_step211) 

V_mod_step211 <- vcovHC(mod_step211, type = "HC0")
V_diag_mod_step211 <- sqrt(diag(V_mod_step211))


stargazer(mod_step211, type = "text", se=list(V_diag_mod_step211))

#Trade_freedom
mod_step221 <- lm(data = data, log(Electricity) ~ Trade_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step221)
vif(mod_step221)

crPlots(mod_step221)

#гетероскедастичность
bptest(mod_step221) 
white_lm(mod_step221)


V_mod_step221 <- vcovHC(mod_step221, type = "HC0")
V_diag_mod_step221 <- sqrt(diag(V_mod_step221))


stargazer(mod_step221, type = "text", se=list(V_diag_mod_step221))

#Fiscal_freedom
mod_step231 <- lm(data = data, log(Electricity) ~ Fiscal_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step231)
vif(mod_step231)

crPlots(mod_step231)

#гетероскедастичность
bptest(mod_step231) 
white_lm(mod_step231) 


V_mod_step231 <- vcovHC(mod_step231, type = "HC0")
V_diag_mod_step231 <- sqrt(diag(V_mod_step231))


stargazer(mod_step231, type = "text", se=list(V_diag_mod_step231))

#Government_size
mod_step241 <- lm(data = data, log(Electricity) ~ Government_size + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step241)
vif(mod_step241)

crPlots(mod_step241)

#гетероскедастичность
bptest(mod_step241) 
white_lm(mod_step241) 

V_mod_step241 <- vcovHC(mod_step241, type = "HC0")
V_diag_mod_step241 <- sqrt(diag(V_mod_step241))


stargazer(mod_step241, type = "text", se=list(V_diag_mod_step241))

#Monetary_freedom
mod_step251 <- lm(data = data, log(Electricity) ~ Monetary_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step251)
vif(mod_step251)

crPlots(mod_step251)

#гетероскедастичность
bptest(mod_step251) 
white_lm(mod_step251) 

V_mod_step251 <- vcovHC(mod_step251, type = "HC0")
V_diag_mod_step251 <- sqrt(diag(V_mod_step251))


stargazer(mod_step251, type = "text", se=list(V_diag_mod_step251))

#Investment_freedom
mod_step261 <- lm(data = data, log(Electricity) ~ Investment_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step261)
vif(mod_step261)

crPlots(mod_step261)

#гетероскедастичность
bptest(mod_step261) 
white_lm(mod_step261) 
V_mod_step261 <- vcovHC(mod_step261, type = "HC0")
V_diag_mod_step261 <- sqrt(diag(V_mod_step261))


stargazer(mod_step261, type = "text", se=list(V_diag_mod_step261))

#Financial_freedom
mod_step27 <- lm(data = data, log(Electricity) ~ Financial_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step27)
vif(mod_step27)

crPlots(mod_step27)

#гетероскедастичность
bptest(mod_step27) 
white_lm(mod_step27) 

V_mod_step27 <- vcovHC(mod_step27, type = "HC0")
V_diag_mod_step27 <- sqrt(diag(V_mod_step27))

stargazer(mod_step27, type = "text", se=list(V_diag_mod_step27))

#Property_rights
mod_step28 <- lm(data = data, log(Electricity) ~ Property_rights + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step28)
vif(mod_step28)

crPlots(mod_step28)

#гетероскедастичность
bptest(mod_step28) 
white_lm(mod_step28) 
V_mod_step28 <- vcovHC(mod_step28, type = "HC0")
V_diag_mod_step28 <- sqrt(diag(V_mod_step28))

stargazer(mod_step28, type = "text", se=list(V_diag_mod_step28))

#Freedom_for_corruption
mod_step29 <- lm(data = data, log(Electricity) ~ Freedom_for_corruption + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step29)
vif(mod_step29)

crPlots(mod_step29)

#гетероскедастичность
bptest(mod_step29) 
white_lm(mod_step29) 
V_mod_step29 <- vcovHC(mod_step29, type = "HC0")
V_diag_mod_step29 <- sqrt(diag(V_mod_step29))

stargazer(mod_step29, type = "text", se=list(V_diag_mod_step29))

#Labour_freedom
mod_step30 <- lm(data = data, log(Electricity) ~ Labour_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step30)
vif(mod_step30)

crPlots(mod_step30)

#гетероскедастичность
bptest(mod_step30) 
white_lm(mod_step30) 


stargazer(mod_step30, type = "text", se=list(V_diag_mod_step30))

#Данные
#Unemployment

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data, Unemployment, Business_freedom, Trade_freedom, Fiscal_freedom, Government_size, Monetary_freedom, Investment_freedom, Financial_freedom, Property_rights, Freedom_for_corruption, Labour_freedom, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data <- na.omit(data)
View(data)

#Регрессии второго шага
#Business_freedom
mod_step211 <- lm(data = data, Unemployment ~ Business_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step211)
vif(mod_step211)

crPlots(mod_step211)

#гетероскедастичность
bptest(mod_step211) 
white_lm(mod_step211) 

V_mod_step211 <- vcovHC(mod_step211, type = "HC0")
V_diag_mod_step211 <- sqrt(diag(V_mod_step211))


stargazer(mod_step211, type = "text", se=list(V_diag_mod_step211))

#Trade_freedom
mod_step221 <- lm(data = data, Unemployment ~ Trade_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step221)
vif(mod_step221)

crPlots(mod_step221)

#гетероскедастичность
bptest(mod_step221) 
white_lm(mod_step221)


V_mod_step221 <- vcovHC(mod_step221, type = "HC0")
V_diag_mod_step221 <- sqrt(diag(V_mod_step221))


stargazer(mod_step221, type = "text", se=list(V_diag_mod_step221))

#Fiscal_freedom
mod_step231 <- lm(data = data, Unemployment ~ Fiscal_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step231)
vif(mod_step231)

crPlots(mod_step231)

#гетероскедастичность
bptest(mod_step231) 
white_lm(mod_step231) 


V_mod_step231 <- vcovHC(mod_step231, type = "HC0")
V_diag_mod_step231 <- sqrt(diag(V_mod_step231))


stargazer(mod_step231, type = "text", se=list(V_diag_mod_step231))

#Government_size
mod_step241 <- lm(data = data, Unemployment ~ Government_size + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step241)
vif(mod_step241)

crPlots(mod_step241)

#гетероскедастичность
bptest(mod_step241) 
white_lm(mod_step241) 

V_mod_step241 <- vcovHC(mod_step241, type = "HC0")
V_diag_mod_step241 <- sqrt(diag(V_mod_step241))


stargazer(mod_step241, type = "text", se=list(V_diag_mod_step241))

#Monetary_freedom
mod_step251 <- lm(data = data, Unemployment ~ Monetary_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step251)
vif(mod_step251)

crPlots(mod_step251)

#гетероскедастичность
bptest(mod_step251) 
white_lm(mod_step251) 

V_mod_step251 <- vcovHC(mod_step251, type = "HC0")
V_diag_mod_step251 <- sqrt(diag(V_mod_step251))


stargazer(mod_step251, type = "text", se=list(V_diag_mod_step251))

#Investment_freedom
mod_step261 <- lm(data = data, Unemployment ~ Investment_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step261)
vif(mod_step261)

crPlots(mod_step261)

#гетероскедастичность
bptest(mod_step261) 
white_lm(mod_step261) 
V_mod_step261 <- vcovHC(mod_step261, type = "HC0")
V_diag_mod_step261 <- sqrt(diag(V_mod_step261))


stargazer(mod_step261, type = "text", se=list(V_diag_mod_step261))

#Financial_freedom
mod_step27 <- lm(data = data, Unemployment ~ Financial_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step27)
vif(mod_step27)

crPlots(mod_step27)

#гетероскедастичность
bptest(mod_step27) 
white_lm(mod_step27) 

V_mod_step27 <- vcovHC(mod_step27, type = "HC0")
V_diag_mod_step27 <- sqrt(diag(V_mod_step27))

stargazer(mod_step27, type = "text", se=list(V_diag_mod_step27))

#Property_rights
mod_step28 <- lm(data = data, Unemployment ~ Property_rights + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step28)
vif(mod_step28)

crPlots(mod_step28)

#гетероскедастичность
bptest(mod_step28) 
white_lm(mod_step28) 
V_mod_step28 <- vcovHC(mod_step28, type = "HC0")
V_diag_mod_step28 <- sqrt(diag(V_mod_step28))

stargazer(mod_step28, type = "text")

#Freedom_for_corruption
mod_step29 <- lm(data = data, Unemployment ~ Freedom_for_corruption + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step29)
vif(mod_step29)

crPlots(mod_step29)

#гетероскедастичность
bptest(mod_step29) 
white_lm(mod_step29) 
V_mod_step29 <- vcovHC(mod_step29, type = "HC0")
V_diag_mod_step29 <- sqrt(diag(V_mod_step29))

stargazer(mod_step29, type = "text")

#Labour_freedom
mod_step30 <- lm(data = data, Unemployment ~ Labour_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step30)
vif(mod_step30)

crPlots(mod_step30)

#гетероскедастичность
bptest(mod_step30) 
white_lm(mod_step30) 
V_mod_step30 <- vcovHC(mod_step30, type = "HC0")
V_diag_mod_step30 <- sqrt(diag(V_mod_step30))


stargazer(mod_step30, type = "text", se=list(V_diag_mod_step30))

#Данные
#Merchandise_trade

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data, Merchandise_trade, Business_freedom, Trade_freedom, Fiscal_freedom, Government_size, Monetary_freedom, Investment_freedom, Financial_freedom, Property_rights, Freedom_for_corruption, Labour_freedom, Inflation_2009, Population_15_64_2009_growth, Capital_stock_2009_growth)
data <- na.omit(data)
View(data)

#Регрессии второго шага
#Business_freedom
mod_step211 <- lm(data = data, Merchandise_trade ~ Business_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step211)
vif(mod_step211)

crPlots(mod_step211)

#гетероскедастичность
bptest(mod_step211) 
white_lm(mod_step211) 

V_mod_step211 <- vcovHC(mod_step211, type = "HC0")
V_diag_mod_step211 <- sqrt(diag(V_mod_step211))


stargazer(mod_step211, type = "text", se=list(V_diag_mod_step211))

#Trade_freedom
mod_step221 <- lm(data = data, Merchandise_trade ~ Trade_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step221)
vif(mod_step221)

crPlots(mod_step221)

#гетероскедастичность
bptest(mod_step221) 
white_lm(mod_step221)


V_mod_step221 <- vcovHC(mod_step221, type = "HC0")
V_diag_mod_step221 <- sqrt(diag(V_mod_step221))


stargazer(mod_step221, type = "text", se=list(V_diag_mod_step221))

#Fiscal_freedom
mod_step231 <- lm(data = data, Merchandise_trade ~ Fiscal_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step231)
vif(mod_step231)

crPlots(mod_step231)

#гетероскедастичность
bptest(mod_step231) 
white_lm(mod_step231) 


V_mod_step231 <- vcovHC(mod_step231, type = "HC0")
V_diag_mod_step231 <- sqrt(diag(V_mod_step231))


stargazer(mod_step231, type = "text", se=list(V_diag_mod_step231))

#Government_size
mod_step241 <- lm(data = data, Merchandise_trade ~ Government_size + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step241)
vif(mod_step241)

crPlots(mod_step241)

#гетероскедастичность
bptest(mod_step241) 
white_lm(mod_step241) 

V_mod_step241 <- vcovHC(mod_step241, type = "HC0")
V_diag_mod_step241 <- sqrt(diag(V_mod_step241))


stargazer(mod_step241, type = "text", se=list(V_diag_mod_step241))

#Monetary_freedom
mod_step251 <- lm(data = data, Merchandise_trade ~ Monetary_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step251)
vif(mod_step251)

crPlots(mod_step251)

#гетероскедастичность
bptest(mod_step251) 
white_lm(mod_step251) 

V_mod_step251 <- vcovHC(mod_step251, type = "HC0")
V_diag_mod_step251 <- sqrt(diag(V_mod_step251))


stargazer(mod_step251, type = "text", se=list(V_diag_mod_step251))

#Investment_freedom
mod_step261 <- lm(data = data, Merchandise_trade ~ Investment_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step261)
vif(mod_step261)

crPlots(mod_step261)

#гетероскедастичность
bptest(mod_step261) 
white_lm(mod_step261) 
V_mod_step261 <- vcovHC(mod_step261, type = "HC0")
V_diag_mod_step261 <- sqrt(diag(V_mod_step261))


stargazer(mod_step261, type = "text", se=list(V_diag_mod_step261))

#Financial_freedom
mod_step27 <- lm(data = data, Merchandise_trade ~ Financial_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step27)
vif(mod_step27)

crPlots(mod_step27)

#гетероскедастичность
bptest(mod_step27) 
white_lm(mod_step27) 

V_mod_step27 <- vcovHC(mod_step27, type = "HC0")
V_diag_mod_step27 <- sqrt(diag(V_mod_step27))

stargazer(mod_step27, type = "text", se=list(V_diag_mod_step27))

#Property_rights
mod_step28 <- lm(data = data, Merchandise_trade ~ Property_rights + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step28)
vif(mod_step28)

crPlots(mod_step28)

#гетероскедастичность
bptest(mod_step28) 
white_lm(mod_step28) 
V_mod_step28 <- vcovHC(mod_step28, type = "HC0")
V_diag_mod_step28 <- sqrt(diag(V_mod_step28))

stargazer(mod_step28, type = "text")

#Freedom_for_corruption
mod_step29 <- lm(data = data, Merchandise_trade ~ Freedom_for_corruption + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step29)
vif(mod_step29)

crPlots(mod_step29)

#гетероскедастичность
bptest(mod_step29) 
white_lm(mod_step29) 
V_mod_step29 <- vcovHC(mod_step29, type = "HC0")
V_diag_mod_step29 <- sqrt(diag(V_mod_step29))

stargazer(mod_step29, type = "text")

#Labour_freedom
mod_step30 <- lm(data = data, Merchandise_trade ~ Labour_freedom + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_step30)
vif(mod_step30)

crPlots(mod_step30)

#гетероскедастичность
bptest(mod_step30) 
white_lm(mod_step30) 
V_mod_step30 <- vcovHC(mod_step30, type = "HC0")
V_diag_mod_step30 <- sqrt(diag(V_mod_step30))


stargazer(mod_step30, type = "text", se=list(V_diag_mod_step30))

############### Шварц

#######################################
##### регрессия Business_freedom  #####
#######################################

##### 1 шаг
data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data, LOCATION, Country, Population_15_64_2009_growth, Capital_stock_2009_growth, Inflation_2009, Harmony, Embeddedness, Hierarchy, Mastery, Aff_auton, Intel_auton, Egalitar, Business_freedom, Trade_freedom, Fiscal_freedom, Government_size, Monetary_freedom, Investment_freedom, Financial_freedom, Property_rights, Freedom_for_corruption, Labour_freedom)
data$Emb_aut <- data$Embeddedness-((data$Aff_auton+data$Intel_auton)/2)
data$Hie_Ega <- data$Hierarchy - data$Egalitar
data$Mas_Har <-  data$Mastery - data$Harmony
View(data)

data <- na.omit(data)


#строим модели
#1
data <- data.frame(data)

mod_bus_freed_1 <- lm(data = data, Business_freedom ~ Emb_aut + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_bus_freed_1)
V_bus_freed_Emb_aut <- vcovHC(mod_bus_freed_1, type = "HC0")
V_diag_bus_freed_Emb_aut <- sqrt(diag(V_bus_freed_Emb_aut ))
stargazer(mod_bus_freed_1, type = "text",se=list(V_diag_bus_freed_Emb_aut))

#2
mod_bus_freed_2 <- lm(data = data, Business_freedom ~ Hie_Ega + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_bus_freed_2)
V_bus_freed_Hie_Ega <- vcovHC(mod_bus_freed_2, type = "HC0")
V_diag_bus_freed_Hie_Ega <- sqrt(diag(V_bus_freed_Hie_Ega))
stargazer(mod_bus_freed_2, type = "text",se=list(V_diag_bus_freed_Hie_Ega))

#3
mod_bus_freed_3 <- lm(data = data, Business_freedom ~ Mas_Har + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_bus_freed_3)
V_bus_freed_Mas_Har <- vcovHC(mod_bus_freed_3, type = "HC0")
V_diag_bus_freed_Mas_Har <- sqrt(diag(V_bus_freed_Mas_Har))
stargazer(mod_bus_freed_3, type = "text",se=list(V_diag_bus_freed_Mas_Har))

stargazer(mod_bus_freed_1, mod_bus_freed_2, mod_bus_freed_3, type = "html",se=list(V_diag_bus_freed_Emb_aut, V_diag_bus_freed_Hie_Ega, V_diag_bus_freed_Mas_Har), out="Business freedom.htm")

#######################################
###### регрессия Trade_freedom  #######
#######################################

##### 1 шаг

#1
mod_tr_freed_1 <- lm(data = data, Trade_freedom ~ Emb_aut + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_tr_freed_1)
V_tr_freed_Emb_aut <- vcovHC(mod_tr_freed_1, type = "HC0")
V_diag_tr_freed_Emb_aut <- sqrt(diag(V_tr_freed_Emb_aut ))
stargazer(mod_tr_freed_1, type = "text",se=list(V_diag_tr_freed_Emb_aut))

#2
mod_tr_freed_2 <- lm(data = data, Trade_freedom ~ Hie_Ega + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_tr_freed_2)
V_tr_freed_Hie_Ega <- vcovHC(mod_tr_freed_2, type = "HC0")
V_diag_tr_freed_Hie_Ega <- sqrt(diag(V_tr_freed_Hie_Ega))
stargazer(mod_tr_freed_2, type = "text",se=list(V_diag_tr_freed_Hie_Ega))

#3
mod_tr_freed_3 <- lm(data = data, Trade_freedom ~ Mas_Har + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_tr_freed_3)
V_tr_freed_Mas_Har <- vcovHC(mod_tr_freed_3, type = "HC0")
V_diag_tr_freed_Mas_Har <- sqrt(diag(V_tr_freed_Mas_Har))
stargazer(mod_tr_freed_3, type = "text",se=list(V_diag_tr_freed_Mas_Har))

stargazer(mod_tr_freed_1, mod_tr_freed_2, mod_tr_freed_3, type = "html",se=list(V_diag_tr_freed_Emb_aut, V_diag_tr_freed_Hie_Ega, V_diag_tr_freed_Mas_Har), out="Trade freedom.htm")


#######################################
##### регрессия Fiscal_freedom  #######
#######################################

##### 1 шаг

#1
mod_fisc_freed_1 <- lm(data = data, Fiscal_freedom ~ Emb_aut + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_fisc_freed_1)
V_fisc_freed_Emb_aut <- vcovHC(mod_fisc_freed_1, type = "HC0")
V_diag_fisc_freed_Emb_aut <- sqrt(diag(V_fisc_freed_Emb_aut ))
stargazer(mod_fisc_freed_1, type = "text",se=list(V_diag_fisc_freed_Emb_aut))

#2
mod_fisc_freed_2 <- lm(data = data, Fiscal_freedom ~ Hie_Ega + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_fisc_freed_2)
V_fisc_freed_Hie_Ega <- vcovHC(mod_fisc_freed_2, type = "HC0")
V_diag_fisc_freed_Hie_Ega <- sqrt(diag(V_fisc_freed_Hie_Ega))
stargazer(mod_fisc_freed_2, type = "text",se=list(V_diag_fisc_freed_Hie_Ega))

#3
mod_fisc_freed_3 <- lm(data = data, Fiscal_freedom ~ Mas_Har + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_fisc_freed_3)
V_fisc_freed_Mas_Har <- vcovHC(mod_fisc_freed_3, type = "HC0")
V_diag_fisc_freed_Mas_Har <- sqrt(diag(V_fisc_freed_Mas_Har))
stargazer(mod_fisc_freed_3, type = "text",se=list(V_diag_fisc_freed_Mas_Har))

stargazer(mod_fisc_freed_1, mod_fisc_freed_2, mod_fisc_freed_3, type = "html",se=list(V_diag_fisc_freed_Emb_aut, V_diag_fisc_freed_Hie_Ega, V_diag_fisc_freed_Mas_Har), out="Fiscal freedom.htm")

#######################################
#### регрессия Government_size  #######
#######################################

##### 1 шаг

#1
mod_gov_size_1 <- lm(data = data, Government_size ~ Emb_aut + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_gov_size_1)
V_gov_size_Emb_aut <- vcovHC(mod_gov_size_1, type = "HC0")
V_diag_gov_size_Emb_aut <- sqrt(diag(V_gov_size_Emb_aut ))
stargazer(mod_gov_size_1, type = "text",se=list(V_diag_gov_size_Emb_aut))

#2
mod_gov_size_2 <- lm(data = data, Government_size ~ Hie_Ega +Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_gov_size_2)
V_gov_size_Hie_Ega <- vcovHC(mod_gov_size_2, type = "HC0")
V_diag_gov_size_Hie_Ega <- sqrt(diag(V_gov_size_Hie_Ega))
stargazer(mod_gov_size_2, type = "text",se=list(V_diag_gov_size_Hie_Ega))

#3
mod_gov_size_3 <- lm(data = data, Government_size ~ Mas_Har + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_gov_size_3)
V_gov_size_Mas_Har <- vcovHC(mod_gov_size_3, type = "HC0")
V_diag_gov_size_Mas_Har <- sqrt(diag(V_gov_size_Mas_Har))
stargazer(mod_gov_size_3, type = "text",se=list(V_diag_gov_size_Mas_Har))

stargazer(mod_gov_size_1, mod_gov_size_2, mod_gov_size_3, type = "html",se=list(V_diag_gov_size_Emb_aut, V_diag_gov_size_Hie_Ega, V_diag_gov_size_Mas_Har), out="Government size.htm")


#######################################
#### регрессия Monetary_freedom  ######
#######################################

##### 1 шаг

#1
mod_monet_freed_1 <- lm(data = data, Monetary_freedom ~ Emb_aut + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_monet_freed_1)
V_monet_freed_Emb_aut <- vcovHC(mod_monet_freed_1, type = "HC0")
V_diag_monet_freed_Emb_aut <- sqrt(diag(V_monet_freed_Emb_aut))
stargazer(mod_monet_freed_1, type = "text",se=list(V_diag_monet_freed_Emb_aut))

#2
mod_monet_freed_2 <- lm(data = data, Monetary_freedom ~ Hie_Ega + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_monet_freed_2)
V_monet_freed_Hie_Ega <- vcovHC(mod_monet_freed_2, type = "HC0")
V_diag_monet_freed_Hie_Ega <- sqrt(diag(V_monet_freed_Hie_Ega))
stargazer(mod_monet_freed_2, type = "text",se=list(V_diag_monet_freed_Hie_Ega))

#3
mod_monet_freed_3 <- lm(data = data, Monetary_freedom ~ Mas_Har + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_monet_freed_3)
V_monet_freed_Mas_Har <- vcovHC(mod_monet_freed_3, type = "HC0")
V_diag_monet_freed_Mas_Har <- sqrt(diag(V_monet_freed_Mas_Har))
stargazer(mod_monet_freed_3, type = "text",se=list(V_diag_monet_freed_Mas_Har))

stargazer(mod_monet_freed_1, mod_monet_freed_2, mod_monet_freed_3, type = "html",se=list(V_diag_monet_freed_Emb_aut, V_diag_monet_freed_Hie_Ega, V_diag_monet_freed_Mas_Har), out="Monetary freedom.htm")

#######################################
##### регрессия Invest_freedom  #######
#######################################

##### 1 шаг

#1
mod_invest_freed_1 <- lm(data = data, Investment_freedom ~ Emb_aut + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_invest_freed_1)
V_invest_freed_Emb_aut <- vcovHC(mod_invest_freed_1, type = "HC0")
V_diag_invest_freed_Emb_aut <- sqrt(diag(V_invest_freed_Emb_aut))
stargazer(mod_invest_freed_1, type = "text",se=list(V_diag_invest_freed_Emb_aut))

#2
mod_invest_freed_2 <- lm(data = data, Investment_freedom ~ Hie_Ega  + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_invest_freed_2)
V_invest_freed_Hie_Ega <- vcovHC(mod_invest_freed_2, type = "HC0")
V_diag_invest_freed_Hie_Ega <- sqrt(diag(V_invest_freed_Hie_Ega))
stargazer(mod_invest_freed_2, type = "text",se=list(V_diag_invest_freed_Hie_Ega))

#3
mod_invest_freed_3 <- lm(data = data, Investment_freedom ~ Mas_Har + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_invest_freed_3)
V_invest_freed_Mas_Har <- vcovHC(mod_invest_freed_3, type = "HC0")
V_diag_invest_freed_Mas_Har <- sqrt(diag(V_invest_freed_Mas_Har))
stargazer(mod_invest_freed_3, type = "text",se=list(V_diag_invest_freed_Mas_Har))

stargazer(mod_invest_freed_1, mod_invest_freed_2, mod_invest_freed_3, type = "html",se=list(V_diag_invest_freed_Emb_aut, V_diag_invest_freed_Hie_Ega, V_diag_invest_freed_Mas_Har), out="Investment freedom.htm")

#######################################
#### регрессия Financial_freedom  #####
#######################################

##### 1 шаг

#1
mod_finance_freed_1 <- lm(data = data, Financial_freedom ~ Emb_aut + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_finance_freed_1)
V_finance_freed_Emb_aut <- vcovHC(mod_finance_freed_1, type = "HC0")
V_diag_finance_freed_Emb_aut <- sqrt(diag(V_finance_freed_Emb_aut))
stargazer(mod_finance_freed_1, type = "text",se=list(V_diag_finance_freed_Emb_aut))

#2
mod_finance_freed_2 <- lm(data = data, Financial_freedom ~ Hie_Ega + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_finance_freed_2)
V_finance_freed_Hie_Ega <- vcovHC(mod_finance_freed_2, type = "HC0")
V_diag_finance_freed_Hie_Ega <- sqrt(diag(V_finance_freed_Hie_Ega))
stargazer(mod_finance_freed_2, type = "text",se=list(V_diag_finance_freed_Hie_Ega))

#3
mod_finance_freed_3 <- lm(data = data, Financial_freedom ~ Mas_Har + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_finance_freed_3)
V_finance_freed_Mas_Har <- vcovHC(mod_finance_freed_3, type = "HC0")
V_diag_finance_freed_Mas_Har <- sqrt(diag(V_finance_freed_Mas_Har))
stargazer(mod_finance_freed_3, type = "text",se=list(V_diag_finance_freed_Mas_Har))

stargazer(mod_finance_freed_1, mod_finance_freed_2, mod_finance_freed_3, type = "html",se=list(V_diag_finance_freed_Emb_aut, V_diag_finance_freed_Hie_Ega, V_diag_finance_freed_Mas_Har), out="Financial freedom.htm")


#######################################
##### регрессия Property rights #######
#######################################

##### 1 шаг

#1
mod_prop_r_1 <- lm(data = data, Property_rights ~ Emb_aut + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_prop_r_1)
V_prop_r_Emb_aut <- vcovHC(mod_prop_r_1, type = "HC0")
V_diag_prop_r_Emb_aut <- sqrt(diag(V_prop_r_Emb_aut))
stargazer(mod_prop_r_1, type = "text",se=list(V_diag_prop_r_Emb_aut))

#2
mod_prop_r_2 <- lm(data = data, Property_rights ~ Hie_Ega + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_prop_r_2)
V_prop_r_Hie_Ega <- vcovHC(mod_prop_r_2, type = "HC0")
V_diag_prop_r_Hie_Ega <- sqrt(diag(V_prop_r_Hie_Ega))
stargazer(mod_prop_r_2, type = "text",se=list(V_diag_prop_r_Hie_Ega))

#3
mod_prop_r_3 <- lm(data = data, Property_rights ~ Mas_Har + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_prop_r_3)
V_prop_r_Mas_Har <- vcovHC(mod_prop_r_3, type = "HC0")
V_diag_prop_r_Mas_Har <- sqrt(diag(V_prop_r_Mas_Har))
stargazer(mod_prop_r_3, type = "text",se=list(V_diag_prop_r_Mas_Har))

stargazer(mod_prop_r_1, mod_prop_r_2, mod_prop_r_3, type = "html",se=list(V_diag_prop_r_Emb_aut, V_diag_prop_r_Hie_Ega, V_diag_prop_r_Mas_Har), out="Property rights.htm")


##########################################
### регрессия Freedom for corruption #####
##########################################

##### 1 шаг

#1
mod_freed_for_corrupt_1 <- lm(data = data, Freedom_for_corruption ~ Emb_aut + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_freed_for_corrupt_1)
V_freed_for_corrupt_Emb_aut <- vcovHC(mod_freed_for_corrupt_1, type = "HC0")
V_diag_freed_for_corrupt_Emb_aut <- sqrt(diag(V_freed_for_corrupt_Emb_aut))
stargazer(mod_freed_for_corrupt_1, type = "text",se=list(V_diag_freed_for_corrupt_Emb_aut))

#2
mod_freed_for_corrupt_2 <- lm(data = data, Freedom_for_corruption ~ Hie_Ega + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_freed_for_corrupt_2)
V_freed_for_corrupt_Hie_Ega <- vcovHC(mod_freed_for_corrupt_2, type = "HC0")
V_diag_freed_for_corrupt_Hie_Ega <- sqrt(diag(V_freed_for_corrupt_Hie_Ega))
stargazer(mod_freed_for_corrupt_2, type = "text",se=list(V_diag_freed_for_corrupt_Hie_Ega))

#3
mod_freed_for_corrupt_3 <- lm(data = data, Freedom_for_corruption ~ Mas_Har + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_freed_for_corrupt_3)
V_freed_for_corrupt_Mas_Har <- vcovHC(mod_freed_for_corrupt_3, type = "HC0")
V_diag_freed_for_corrupt_Mas_Har <- sqrt(diag(V_freed_for_corrupt_Mas_Har))
stargazer(mod_freed_for_corrupt_3, type = "text",se=list(V_diag_freed_for_corrupt_Mas_Har))

stargazer(mod_freed_for_corrupt_1, mod_freed_for_corrupt_2, mod_freed_for_corrupt_3, type = "html",se=list(V_diag_freed_for_corrupt_Emb_aut, V_diag_freed_for_corrupt_Hie_Ega, V_diag_freed_for_corrupt_Mas_Har), out="Freedom for corruption.htm")


##########################################
###### регрессия Labour freedom ##########
##########################################

##### 1 шаг

#1
mod_lab_freed_1 <- lm(data = data, Labour_freedom ~ Emb_aut + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_lab_freed_1)
V_lab_freed_Emb_aut <- vcovHC(mod_lab_freed_1, type = "HC0")
V_diag_lab_freed_Emb_aut <- sqrt(diag(V_lab_freed_Emb_aut))
stargazer(mod_lab_freed_1, type = "text",se=list(V_diag_lab_freed_Emb_aut))

#2
mod_lab_freed_2 <- lm(data = data, Labour_freedom ~ Hie_Ega + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_lab_freed_2)
V_lab_freed_Hie_Ega <- vcovHC(mod_lab_freed_2, type = "HC0")
V_diag_lab_freed_Hie_Ega <- sqrt(diag(V_lab_freed_Hie_Ega))
stargazer(mod_lab_freed_2, type = "text",se=list(V_diag_lab_freed_Hie_Ega))

#3
mod_lab_freed_3 <- lm(data = data, Labour_freedom ~ Mas_Har + Inflation_2009 + Population_15_64_2009_growth+Capital_stock_2009_growth)
summary(mod_lab_freed_3)
V_lab_freed_Mas_Har <- vcovHC(mod_lab_freed_3, type = "HC0")
V_diag_lab_freed_Mas_Har <- sqrt(diag(V_lab_freed_Mas_Har))
stargazer(mod_lab_freed_3, type = "text",se=list(V_diag_lab_freed_Mas_Har))

stargazer(mod_lab_freed_1, mod_lab_freed_2, mod_lab_freed_3, type = "html",se=list(V_diag_lab_freed_Emb_aut, V_diag_lab_freed_Hie_Ega, V_diag_lab_freed_Mas_Har), out="Labour freedom.htm")

