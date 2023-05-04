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
library(dplyr)
library(ggdendro)
library(flexclust)
library(ggplot2)
library(factoextra)
library(fmsb)
library(tseries)
library(tidyverse)
library(broom)



############################
## Кластеризация Хофстеде ##
############################


data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data,LOCATION, GDP_growth_2010,GDP_growth_2011,GDP_growth_2009, Capital_stock_2009_growth, Capital_stock_2010_growth, Capital_stock_2011_growth, Population_15_64_2009_growth, Population_15_64_2010_growth, Population_15_64_2011_growth, Country, Power.distance, Individualism, Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence, Inflation_2009, Inflation_2010, Inflation_2011)
View(data)
data <- na.omit(data)


#создание нового объекта
data_for_clusterization <- dplyr::select(data, Power.distance, Individualism, Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence)
View(data_for_clusterization)

#проводим стандартизацию
data_scale <- scale(data_for_clusterization)

#создаём матрицу попарных расстояний
data_scale_matrix <- dist(data_scale)

#построим иерархическую класеризацию
mod <- hclust(data_scale_matrix)

#строим дендрограмму
plot(mod, hang = -1)
#plot(mod, hang = -1, labels = Countries)
#fviz_dend(mod, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", labels_track_height = 0.8)


#добавляем "обводку" кластеров
rect.hclust(mod, k=4)

#Создаём вектор из номеров кластеров
cluster <- cutree(mod, k=4)

#добавляем этот вектор к данным
data_for_clusterization$cluster <- cluster
data$cluster <- as.factor(cluster)
data$cluster1 <- ifelse(cluster==1,1,0)
data$cluster2 <- ifelse(cluster==2,1,0)
data$cluster3 <- ifelse(cluster==3,1,0)

data$cluster_name <- ifelse(cluster==1, "Несклонные к риску",ifelse(cluster==2, "Индивидуалистичные",ifelse(cluster==3, "Маскулинные","Неравномерные")))
#data3$cluster_name <- ifelse(cluster==1, "Коллективизм-Осторожность",ifelse(cluster==2, "Индивидуализм",ifelse(cluster==3, "Маскулинность","Долгий взгляд")))


#вычисляем средние значения по кластерам
data_group <- data %>% group_by(cluster_name) 
data_group_1 <- dplyr::select(data_group,-c(cluster,LOCATION,Country))
data_group_mean_1 <- summarise_all(data_group_1, mean)
data_group_mean_2 <- data_group_mean_1[,11:16]
View(data_group_mean_1)
View(data_group_mean_2)

#среднее падение ВВП 2009 по кластерам
data_mean_GDP_2009 <- data %>% dplyr::select(cluster_name, GDP_growth_2009) %>% na.omit() %>% group_by(cluster_name) %>%
  dplyr::summarise(value = mean(GDP_growth_2009), sd = sd(GDP_growth_2009)/sqrt(n())) 
View(data_mean_GDP_2009)

ggplot(data_mean_GDP_2009, aes(x = cluster_name, y = value)) +
  geom_bar(stat="identity", fill="red", alpha = 0.8) +
  geom_errorbar(aes(x = cluster_name, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "black", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Кластер", y = "Средний темп прироста ВВП в 2009 году") + geom_label(aes(label = round(value, 2)))

#среднее падение ВВП 2010 по кластерам
data_mean_GDP_2010 <- data %>% dplyr::select(cluster_name, GDP_growth_2010) %>% na.omit() %>% group_by(cluster_name) %>%
  dplyr::summarise(value = mean(GDP_growth_2010), sd = sd(GDP_growth_2010)/sqrt(n())) 
View(data_mean_GDP_2010)

ggplot(data_mean_GDP_2010, aes(x = cluster_name, y = value)) +
  geom_bar(stat="identity", fill="darkgreen", alpha = 0.8) +
  geom_errorbar(aes(x = cluster_name, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "black", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Кластер", y = "Средний темп прироста ВВП в 2010 году") + geom_label(aes(label = round(value, 2)))

#среднее падение ВВП 2011 по кластерам
data_mean_GDP_2011 <- data %>% dplyr::select(cluster_name, GDP_growth_2011) %>% na.omit() %>% group_by(cluster_name) %>%
  dplyr::summarise(value = mean(GDP_growth_2011), sd = sd(GDP_growth_2011)/sqrt(n())) 
View(data_mean_GDP_2011)

ggplot(data_mean_GDP_2011, aes(x = cluster_name, y = value)) +
  geom_bar(stat="identity", fill="darkgreen", alpha = 0.8) +
  geom_errorbar(aes(x = cluster_name, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "black", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Кластер", y = "Средний темп прироста ВВП в 2011 году") + geom_label(aes(label = round(value, 2)))

#Средняя инфляция 2009 по кластерам
data_mean_Inflation_2009 <- data %>% dplyr::select(cluster_name, Inflation_2009) %>% na.omit() %>% group_by(cluster_name) %>%
  dplyr::summarise(value = mean(Inflation_2009), sd = sd(Inflation_2009)/sqrt(n())) 
View(data_mean_Inflation_2009)

ggplot(data_mean_Inflation_2009, aes(x = cluster_name, y = value)) +
  geom_bar(stat="identity", fill="orange", alpha = 0.8) +
  geom_errorbar(aes(x = cluster_name, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "black", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Кластер", y = "Инфляция в 2009 году") + geom_label(aes(label = round(value, 2)))

#среднее падение ВВП по странам, сгруппированным по кластерам
ggplot(data, aes(x=GDP_growth_2009, y=Country)) +
  geom_segment(aes(yend=Country), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=cluster_name)) +scale_colour_brewer(palette="Set1", guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),axis.text.x=element_text(size=18),axis.text.y=element_text(size=12),strip.text.y=element_text(size=10),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue")) +
  facet_grid(cluster_name ~ ., scales="free_y", space="free_y") +ylab("Страна и кластер")+xlab("Падение ВВП, %")

#Падение ВВП в зависимости от индивидуализма
ggplot(data3, aes(x=GDP_growth, y=Individualism, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1")

ggplot(data3, aes(x=GDP_growth, y=Individualism, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
     theme(legend.position="right",legend.text=element_text(size=14),axis.text.x=element_text(size=18),axis.text.y=element_text(size=18),strip.text.y=element_text(size=12),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue")) + scale_colour_brewer(palette="Set1") + stat_smooth(method=lm, se=FALSE) +ylab("Индивидуализм")+xlab("Падение ВВП, %")

#Падение ВВП в зависимости от маскулинности
ggplot(data, aes(x=GDP_growth_2009, y=Masculinity, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4.5,vjust=1) + theme_linedraw()+
  theme(legend.position="right",legend.text=element_text(size=14),axis.text.x=element_text(size=18),axis.text.y=element_text(size=18),strip.text.y=element_text(size=12),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue")) + scale_colour_brewer(palette="Set1")+ stat_smooth(method=lm, se=FALSE)+ylab("Маскулинность")+xlab("Падение ВВП, %")

#Диаграмма индивидуализм - удовлетворённость жизнью с учётом кластеров и падения ВВП
ggplot(data, aes(x=Individualism, y=Masculinity, colour=cluster_name,size=GDP_growth_2009)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1")

#Плотность и гистограмма
ggplot(data, aes(x=GDP_growth_2009)) + geom_density(fill = "red", alpha=0.8) + theme_linedraw() + theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue")) + xlab("Падение ВВП, %") + ylab("")

#Радчарт
data6 <- as.data.frame(t(data.frame(data_group_mean_2,row.names=c("Hierarchical","Latin or poor","Mostly European","Well-developed"))))
View(data6)
radarchart(data6)

#добавляем в дату строчки - максимальное и минимальное значение
data7 <- rbind(rep((120), 4), rep(0,5), data6)
radarchart(data7, pcol=c("Red","Darkblue","Darkgreen","Brown","Purple","Orange"),
           plty=1,plwd=2,cglcol="black")


library(fmsb)
radarchart(data_group_mean_2)


#Кластер 1: Колумбия, Мексика, Морокко, Аргентина, Польша, Бразилия, Турция, Испания,...

#Кластер 2: Великобритания, Ирландия, Канада, Австралия, США, Швеция, Финляндия,...

#Кластер 3: Люксембург, Германия, Швейцария, Бельгия, Франция, Италия, Япония, Венгрия,...

#Кластер 4: Латвия, Литва, Южная Корея, Китай, Болгария, Индия, Индонезия, Румыния, Россия,...

#Кластер1 - самый многочисленный. Это не самые экономически развитые страны, в них высокая дистанция власти, низкий уровень индивидуализма
#высокий уровень избегания неопределённости.
#Кластер2 - экономически успешные и развитые страны с низкой дистанцией власти, наивысшим уровенем индивидуализма
#, наименьшей склонностью бояться неопределённости и наивысшей степенью удовлетворения жизнью.
#Кластер3 - в основном, европейские страны, там наивысшая маскулинность, высокая степень избегания неопределенности, долгосрочная ориентация
#Кластер4 - экономически отшибленные страны. Низкий уровень индивидуализма, наивысший уровень долгосрочной ориентации, наименьший вроень наслаждения жизнью.

#########################
## Кластеризация Шварц ##
#########################

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data,LOCATION, GDP_growth_2010,GDP_growth_2011,GDP_growth_2009, Country, Harmony, Embeddedness, Hierarchy, Mastery, Aff_auton, Intel_auton, Egalitar)
data <- na.omit(data)

Countries <- dplyr::select(data1, Country)
Countries <- array(Countries)
View(Countries)


#создание нового объекта
data_for_clusterization <- dplyr::select(data, Harmony, Embeddedness, Hierarchy, Mastery, Aff_auton, Intel_auton, Egalitar)
View(data_for_clusterization)

#проводим стандартизацию
data_scale <- scale(data_for_clusterization)

#создаём матрицу попарных расстояний
data_scale_matrix <- dist(data_scale)

#построим иерархическую класеризацию
mod <- hclust(data_scale_matrix)

#строим дендрограмму
plot(mod, hang = -1)
#plot(mod, hang = -1, labels = Countries)
#fviz_dend(mod, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", labels_track_height = 0.8)


#добавляем "обводку" кластеров
rect.hclust(mod, k=4)

#Создаём вектор из номеров кластеров
cluster <- cutree(mod, k=4)

#добавляем этот вектор к данным
data_for_clusterization$cluster <- cluster
data$cluster <- as.factor(cluster)

data$cluster_name <- ifelse(cluster==1, "Эгалитарные и автономные",ifelse(cluster==2, "Коллективистские",ifelse(cluster==3, "Средние", "Иерархичные")))
data$cluster1 <- ifelse(cluster==1,1,0)
data$cluster2 <- ifelse(cluster==2,1,0)
data$cluster3 <- ifelse(cluster==3,1,0)

#вычисляем средние значения по кластерам
data_group <- group_by(data, cluster_name) 
data_group_1 <- dplyr::select(data_group,-c(cluster, LOCATION))
data_group_mean_1 <- summarise_all(data_group_1, mean)
View(data_group_mean_1)


#среднее падение ВВП 2009 по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=GDP_growth_2009)) +
  geom_bar(stat="identity", fill="red", colour="black", alpha = 0.8) + theme_linedraw()+ theme(axis.text.x=element_text(size=20),axis.title.x=element_text(size=20,colour="blue"),axis.text.y=element_text(size=20),axis.title.y=element_text(size=20,colour="blue")) + ylab("Темп прироста ВВП в 2009 г., %") + xlab("Группы")

#среднее падение ВВП 2010 по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=GDP_growth_2010)) +
  geom_bar(stat="identity", fill="darkgreen", colour="black", alpha = 0.8) + theme_linedraw()+ theme(axis.text.x=element_text(size=20),axis.title.x=element_text(size=20,colour="blue"),axis.text.y=element_text(size=20),axis.title.y=element_text(size=20,colour="blue")) + ylab("Темп прироста ВВП в 2010 г., %") + xlab("Группы")

#среднее падение ВВП 2011 по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=GDP_growth_2011)) +
  geom_bar(stat="identity", fill="darkgreen", colour="black", alpha = 0.8) + theme_linedraw()+ theme(axis.text.x=element_text(size=20),axis.title.x=element_text(size=20,colour="blue"),axis.text.y=element_text(size=20),axis.title.y=element_text(size=20,colour="blue")) + ylab("Темп прироста ВВП в 2011 г., %") + xlab("Группы")

#средний прирост запаса капитала по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=Capital_stock_growth)) +
  geom_bar(stat="identity", fill="blue", colour="black", alpha = 0.8) + theme_linedraw()

#средняя инфляция по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=Inflation)) +
  geom_bar(stat="identity", fill="orange", colour="black", alpha = 0.8) + theme_linedraw()

#средний индекс Джини по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=Gini)) +
  geom_bar(stat="identity", fill="purple", colour="black", alpha = 0.8) + theme_linedraw()

#средний прирост работоспособного населения по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=Population_15_64_growth)) +
  geom_bar(stat="identity", fill="darkgreen", colour="black", alpha = 0.8) + theme_linedraw()

#средний индекс человеческого капитала по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=Human_cap_index_2010)) +
  geom_bar(stat="identity", fill="yellow", colour="black", alpha = 0.8) + theme_linedraw()

#среднее падение ВВП по странам
ggplot(data, aes(x=GDP_growth_2009, y=Country)) +
  geom_segment(aes(yend=Country), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=cluster_name)) +
  scale_colour_brewer(palette="Set1") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),   
        legend.position="right")

#среднее падение ВВП по странам, сгруппированным по кластерам
ggplot(data, aes(x=GDP_growth_2009, y=Country)) +
  geom_segment(aes(yend=Country), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=cluster_name)) +scale_colour_brewer(palette="Set1", guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(cluster_name ~ ., scales="free_y", space="free_y")

#Падение ВВП в зависимости от индивидуализма
ggplot(data, aes(x=GDP_growth_2009, y=Individualism, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1")

ggplot(data, aes(x=GDP_growth_2009, y=Individualism, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1") + stat_smooth(method=lm, se=FALSE)

#Падение ВВП в зависимости от маскулинности
ggplot(data, aes(x=GDP_growth_2009, y=Masculinity, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1")

ggplot(data, aes(x=Masculinity, y=GDP_growth_2009, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=3,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1")+ stat_smooth(method=lm, se=FALSE)

#Падение ВВП в зависимости от удовлетворённости жизнью
ggplot(data3, aes(x=GDP_growth, y=Indulgence, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1")

ggplot(data3, aes(x=GDP_growth, y=Indulgence, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1")+ stat_smooth(method=lm, se=FALSE)

#Диаграмма индивидуализм - удовлетворённость жизнью с учётом кластеров и падения ВВП
ggplot(data3, aes(x=Individualism, y=Indulgence, colour=cluster_name,size=GDP_growth)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1")

#Плотность и гистограмма
ggplot(data3, aes(x=GDP_growth)) + geom_density(fill = "red", alpha=0.8) + theme_linedraw()

ggplot(data3, aes(x=GDP_growth)) + geom_histogram(fill = "red", alpha=0.8,bins = 43,colour="black") + theme_linedraw()

#"Ящик с усами" для темпа прироста ВВП
ggplot(data3, aes(x=cluster_name,y=GDP_growth)) + geom_boxplot(fill = "blue", alpha=0.8) + theme_linedraw()+
  theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16))

#Радчарт
data6 <- as.data.frame(t(data.frame(data_group_mean_2,row.names=c("Hierarchical","Latin or poor","Mostly European","Well-developed"))))
View(data6)
radarchart(data6)

#добавляем в дату строчки - максимальное и минимальное значение
data7 <- rbind(rep((120), 4), rep(0,5), data6)
radarchart(data7, pcol=c("Red","Darkblue","Darkgreen","Brown","Purple","Orange"),
           plty=1,plwd=2,cglcol="black")



#Кластер 1: Аргентина, Австралия, Бразилия, Канада, Великобритания, Греция, Ирландия, Португалия,...

#Кластер 2: Австрия, Бельгия, Дания, Финляндия, Франция, Германия, Италия, Нидерланды, Норвегия, ...

#Кластер 3: Болгария, Колумбия, Япония, Южная Корея, Польша, Румыния, Россия, ...

#Кластер 4: Чили, Чехия, Эстония, Венгрия, Индонезия, Латвия, Мексика, ...

#Кластер1 - тут доминируют ценности мастерства (он тут максимальный). Высокий уровень атвономии и эгалитаризма.
#Кластер2 - преимущественно европейские страны. Наивысший уровень гармонии и наименьший уровень иерархии. Также наивысшие уровни автономии.
#Кластер3 - наименьший уровень гармонии, высокий уровень мастерства, иерархичные.
#Кластер4 - наивысший уровень принадлежности, высокая гармония, наименьший уровень мастерства, наименьшие уровни автономии.


####################################
#### регрессия c кластеризацией ####
####################################

#Описательные статистики
summary <- summary(data)
summary

#строим модель
data <- data.frame(data)
mod <- lm(data = data, GDP_growth_2009 ~Capital_stock_2009_growth + Population_15_64_2009_growth + Inflation_2009+cluster1+cluster2+cluster3)
summary(mod)
vif(mod)

#нормальность остатков
plot(mod, which = 2)


#посмотрим на influencePlot(mod1)
# influencePlot(mod1)
# data5 <- data4[-c(19),]
# mod2 <- update(mod1, . ~ ., data = data5)
# summary(mod2)
# influencePlot(mod2)
#больше ничего удалять не хочу


#гетероскедастичность
bptest(mod) 
white_lm(mod) 
V_clust <- vcovHC(mod, type = "HC0")
V_diag_clust <- sqrt(diag(V_clust))


#выгружаем модель
library(stargazer)
stargazer(mod, type = "text",se=list())


########################
#### ARDL 2002-2009 ####
########################

#Данные
data_ardl_gdp <- read.csv("/Users/artart/Desktop/GDP_growth_quarterly.csv")
data_ardl_gdp <- dplyr::select(data_ardl_gdp,-c(INDICATOR, SUBJECT, MEASURE, FREQUENCY,Flag.Codes))
data_ardl_gdp$GDP_growth <- data_ardl_gdp$Value
data_ardl_gdp <- data_ardl_gdp[,-3]
View(data_ardl_gdp)

###
data_ardl_M1 <- read.csv("/Users/artart/Desktop/M1.csv")
data_ardl_M1 <- dplyr::select(data_ardl_M1,-c(INDICATOR, SUBJECT, MEASURE, FREQUENCY,Flag.Codes))
data_ardl_M1$M1 <- data_ardl_M1$Value
data_ardl_M1 <- data_ardl_M1[,-3]
View(data_ardl_M1)
###

data_ardl_NX <- read.csv("/Users/artart/Desktop/NX.csv")
data_ardl_NX <- dplyr::select(data_ardl_NX,-c(INDICATOR, SUBJECT, MEASURE, FREQUENCY,Flag.Codes))
data_ardl_NX$NX <- data_ardl_NX$Value
data_ardl_NX <- data_ardl_NX[,-3]
View(data_ardl_NX)

data_ardl_spi <- read.csv("/Users/artart/Desktop/Share_prices_index.csv")
data_ardl_spi <- dplyr::select(data_ardl_spi,-c(INDICATOR, SUBJECT, MEASURE, FREQUENCY,Flag.Codes))
data_ardl_spi$spi <- data_ardl_spi$Value
data_ardl_spi <- data_ardl_spi[,-3]
View(data_ardl_spi)

data_ardl_cpi <- read.csv("/Users/artart/Desktop/Inflation_ardl.csv")
data_ardl_cpi <- dplyr::select(data_ardl_cpi,-c(INDICATOR, SUBJECT, MEASURE, FREQUENCY,Flag.Codes))
data_ardl_cpi$cpi <- data_ardl_cpi$Value
data_ardl_cpi <- data_ardl_cpi[,-3]
View(data_ardl_cpi)

data_ardl <- data_ardl_gdp %>% inner_join(data_ardl_cpi)
data_ardl <- data_ardl %>% inner_join(data_ardl_NX)
data_ardl <- data_ardl %>% inner_join(data_ardl_spi)
View(data_ardl)

data_ardl$Year <- substr(data_ardl$TIME, 1, 4)
data_ardl %>% group_by(Year) %>% summarise(mean(GDP_growth))

###
data_ardl$M1_growth <- (data_ardl$M1/lag(data_ardl$M1))*100-100
###

data_ardl$NX_growth <- (data_ardl$NX-lag(data_ardl$NX))/abs(lag(data_ardl$NX))*100
data_ardl <- data_ardl[(data_ardl$TIME != "2002-Q2"),]
View(data_ardl)
data_ardl$GDP_growth_diff <- data_ardl$GDP_growth - lag(data_ardl$GDP_growth)
data_ardl$NX_growth_diff <- data_ardl$NX_growth - lag(data_ardl$NX_growth)
data_ardl$cpi_diff <- data_ardl$cpi - lag(data_ardl$cpi)
data_ardl <- data_ardl[(data_ardl$TIME != "2002-Q3"),]
View(data_ardl)


data_ardl$Crisis <- ifelse((data_ardl$TIME == "2008-Q1")|(data_ardl$TIME == "2008-Q2")|(data_ardl$TIME == "2008-Q3")|(data_ardl$TIME == "2008-Q4")|
                             (data_ardl$TIME == "2009-Q1")|(data_ardl$TIME == "2009-Q2")|(data_ardl$TIME == "2009-Q3")|(data_ardl$TIME == "2009-Q4"),1,0)

View(data_ardl)

data_countries_cluster <-  dplyr::select(data3, c(LOCATION,cluster))
View(data_countries_cluster)
data_ardl <- data_ardl %>% merge(data_countries_cluster) 
View(data_ardl)
data_ardl <- data_ardl[order(data_ardl$LOCATION,data_ardl$TIME),]

#1
mod_final <- data_ardl %>% 
  nest_by(cluster) %>% 
  mutate(model = list(lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(GDP_growth_diff,3) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis, data_ardl))) %>% 
  tidy() %>% 
  reframe() %>% 
  ungroup()

mod_final <- data_ardl %>% 
  nest_by(cluster) %>% 
  mutate(model = list(lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(GDP_growth_diff,3) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis, data_ardl))) 

reframe(mod_final) %>% ungroup()
?reframe
mod_final$model


print(mod_final,n=50)
names(data_ardl)


data_mod <- data.frame(mod_final)
data_mod  <-  data_mod %>% merge(data_countries_cluster)
View(data_mod)
data_mod$cluster <- as.factor(data_mod$cluster)

data_mod_1 <- dplyr::filter(data_mod_group, cluster==1) 
View(data_mod_1)
data_mod_1_term_lag1 <- filter(data_mod_1, term == "lag(GDP_growth_diff, 1)") 
View(data_mod_1_term_lag1)

#2
library(dplyr)
library(purrr)
library(broom)
data_ardl <- data.frame(data_ardl)

fit_model <- function(data_ardl) lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(GDP_growth_diff,3) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis, data = data_ardl)
get_slope <- function(mod) tidy(mod)$estimate[2]
get_p_value <- function(mod) tidy(mod)$p.value[2]

data_ardl %>% 
  group_nest(LOCATION) %>% 
  mutate(model = map(data_ardl, fit_model),
         slope = map_dbl(model, get_slope),
         p_value = map_dbl(model, get_p_value))

#3
library(tidyverse)
library(broom)

data_ardl  %>% 
  group_by(cluster) %>% 
  do(tidy(lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(GDP_growth_diff,3) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis))) %>%
  filter(term != "(Intercept)")

################
# ARDL вручную #
################

###Кластер 2

#Австралия
data_ardl_AUS <- dplyr::filter(data_ardl, LOCATION=="AUS") 
View(data_ardl_AUS)
mod_AUS <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_AUS)
s_AUS <- summary(mod_AUS)
s_AUS
V_AUS <- vcovHC(mod_AUS, type = "HC0")
#выгружаем модель
V_diag_AUS <- sqrt(diag(V_AUS))
stargazer(mod_AUS, type = "text", se = list(V_diag_AUS))

#Канада
data_ardl_CAN <- dplyr::filter(data_ardl, LOCATION=="CAN") 
View(data_ardl_CAN)
mod_CAN <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_CAN)
s_CAN <- summary(mod_CAN)
s_CAN
V_CAN <- vcovHC(mod_CAN, type = "HC0")
#выгружаем модель
V_diag_CAN <- sqrt(diag(V_CAN))
stargazer(mod_CAN, type = "text", se = list(V_diag_CAN))

#Дания
data_ardl_DNK <- dplyr::filter(data_ardl, LOCATION=="DNK") 
View(data_ardl_DNK)
mod_DNK <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_DNK)
s_DNK <- summary(mod_DNK)
s_DNK
V_DNK <- vcovHC(mod_DNK, type = "HC0")
#выгружаем модель
V_diag_DNK <- sqrt(diag(V_DNK))
stargazer(mod_DNK, type = "text", se = list(V_diag_DNK))


#Финляндия
data_ardl_FIN <- dplyr::filter(data_ardl, LOCATION=="FIN") 
View(data_ardl_FIN)
mod_FIN <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_FIN)
s_FIN <- summary(mod_FIN)
s_FIN
V_FIN <- vcovHC(mod_FIN, type = "HC0")
#выгружаем модель
V_diag_FIN <- sqrt(diag(V_FIN))
stargazer(mod_FIN, type = "text", se = list(V_diag_FIN))

#Великобритания 
data_ardl_GBR <- dplyr::filter(data_ardl, LOCATION=="GBR") 
View(data_ardl_GBR)
mod_GBR <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_GBR)
s_GBR <- summary(mod_GBR)
s_GBR
V_GBR <- vcovHC(mod_GBR, type = "HC0")
#выгружаем модель
V_diag_GBR <- sqrt(diag(V_GBR))
stargazer(mod_GBR, type = "text", se = list(V_diag_GBR))


#Ирландия
data_ardl_IRL <- dplyr::filter(data_ardl, LOCATION=="IRL") 
View(data_ardl_IRL)
mod_IRL <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_IRL)
s_IRL <- summary(mod_IRL)
s_IRL
V_IRL <- vcovHC(mod_IRL, type = "HC0")
#выгружаем модель
V_diag_IRL <- sqrt(diag(V_IRL))
stargazer(mod_IRL, type = "text", se = list(V_diag_IRL))

#Нидерланды
data_ardl_NLD <- dplyr::filter(data_ardl, LOCATION=="NLD") 
View(data_ardl_NLD)
mod_NLD <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_NLD)
s_NLD <- summary(mod_NLD)
s_NLD
V_NLD <- vcovHC(mod_NLD, type = "HC0")
#выгружаем модель
V_diag_NLD <- sqrt(diag(V_NLD))
stargazer(mod_NLD, type = "text", se = list(V_diag_NLD))

#Норвегия
data_ardl_NOR <- dplyr::filter(data_ardl, LOCATION=="NOR") 
View(data_ardl_NOR )
mod_NOR  <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_NOR )
s_NOR  <- summary(mod_NOR)
s_NOR 
V_NOR  <- vcovHC(mod_NOR , type = "HC0")
#выгружаем модель
V_diag_NOR  <- sqrt(diag(V_NOR ))
stargazer(mod_NOR, type = "text", se = list(V_diag_NOR))

#Швеция
data_ardl_SWE <- dplyr::filter(data_ardl, LOCATION=="SWE") 
View(data_ardl_SWE)
mod_SWE <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_SWE)
s_SWE <- summary(mod_SWE)
s_SWE 
V_SWE <- vcovHC(mod_SWE, type = "HC0")
#выгружаем модель
V_diag_SWE <- sqrt(diag(V_SWE))
stargazer(mod_SWE, type = "text", se = list(V_diag_SWE))

#Соединённые Штаты Америки
data_ardl_USA <- dplyr::filter(data_ardl, LOCATION=="USA") 
View(data_ardl_USA)
mod_USA <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_USA)
s_USA <- summary(mod_USA)
s_USA
V_USA <- vcovHC(mod_USA, type = "HC0")
#выгружаем модель
V_diag_USA <- sqrt(diag(V_USA))
stargazer(mod_USA, type = "text", se = list(V_diag_USA))

#stargazer(mod_AUS,mod_CAN,mod_DNK,mod_FIN,mod_GBR,mod_IRL,mod_NLD,mod_NOR,mod_SWE,mod_USA,type = "html",colnames <- c("AUS","CAN","DNK","FIN","GBR","IRL","NLD","NOR","SWE","USA"),out = "cluster2.htm")
stargazer(mod_AUS,mod_CAN,mod_DNK,mod_FIN,mod_GBR,mod_IRL,mod_NLD,mod_NOR,mod_SWE,mod_USA, type = "html", se = list(V_diag_AUS,V_diag_CAN,V_diag_DNK,V_diag_FIN,V_diag_GBR,V_diag_IRL,V_diag_NLD,V_diag_NOR,V_diag_SWE,V_diag_USA),out = "cluster2.htm")

###Кластер 1

#По Аргентине, Мальте и Морокко нет данных, а по Колубии доступны не все
#Бразилия
data_ardl_BRA <- dplyr::filter(data_ardl, LOCATION=="BRA") 
View(data_ardl_BRA)
mod_BRA <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_BRA)
s_BRA <- summary(mod_BRA)
s_BRA 
V_BRA <- vcovHC(mod_BRA, type = "HC0")
#выгружаем модель
V_diag_BRA <- sqrt(diag(V_BRA))
stargazer(mod_BRA, type = "text", se = list(V_diag_BRA))

#Чили
data_ardl_CHL <- dplyr::filter(data_ardl, LOCATION=="CHL") 
View(data_ardl_CHL)
mod_CHL <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_CHL)
s_CHL <- summary(mod_CHL)
s_CHL
V_CHL <- vcovHC(mod_CHL, type = "HC0")
#выгружаем модель
V_diag_CHL <- sqrt(diag(V_CHL))
stargazer(mod_CHL, type = "text", se = list(V_diag_CHL))

#Греция
data_ardl_GRC <- dplyr::filter(data_ardl, LOCATION=="GRC") 
View(data_ardl_GRC)
mod_GRC <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_GRC)
s_GRC <- summary(mod_GRC)
s_GRC
V_GRC <- vcovHC(mod_GRC, type = "HC0")
#выгружаем модель
V_diag_GRC <- sqrt(diag(V_GRC))
stargazer(mod_GRC, type = "text", se = list(V_diag_GRC))

#Мексика
data_ardl_MEX <- dplyr::filter(data_ardl, LOCATION=="MEX") 
View(data_ardl_MEX)
mod_MEX <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_MEX)
s_MEX <- summary(mod_MEX)
s_MEX
V_MEX <- vcovHC(mod_MEX, type = "HC0")
#выгружаем модель
V_diag_MEX <- sqrt(diag(V_MEX))
stargazer(mod_MEX, type = "text", se = list(V_diag_MEX))

#Польша
data_ardl_POL <- dplyr::filter(data_ardl, LOCATION=="POL") 
View(data_ardl_POL)
mod_POL <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_POL)
s_POL <- summary(mod_POL)
s_POL
V_POL <- vcovHC(mod_POL, type = "HC0")
#выгружаем модель
V_diag_POL <- sqrt(diag(V_POL))
stargazer(mod_POL, type = "text", se = list(V_diag_POL))

#Португалия
data_ardl_PRT <- dplyr::filter(data_ardl, LOCATION=="PRT") 
View(data_ardl_PRT)
mod_PRT <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_PRT)
s_PRT <- summary(mod_PRT)
s_PRT
V_PRT <- vcovHC(mod_PRT, type = "HC0")
#выгружаем модель
V_diag_PRT <- sqrt(diag(V_PRT))
stargazer(mod_PRT, type = "text", se = list(V_diag_PRT))

#Словения
data_ardl_SVN <- dplyr::filter(data_ardl, LOCATION=="SVN") 
View(data_ardl_SVN)
mod_SVN <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_SVN)
s_SVN <- summary(mod_SVN)
s_SVN
V_SVN <- vcovHC(mod_SVN, type = "HC0")
#выгружаем модель
V_diag_SVN <- sqrt(diag(V_SVN))
stargazer(mod_SVN, type = "text", se = list(V_diag_SVN))

#Испания
data_ardl_ESP <- dplyr::filter(data_ardl, LOCATION=="ESP") 
View(data_ardl_ESP)
mod_ESP <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_ESP)
s_ESP <- summary(mod_ESP)
s_ESP
V_ESP <- vcovHC(mod_ESP, type = "HC0")
#выгружаем модель
V_diag_ESP <- sqrt(diag(V_ESP))
stargazer(mod_ESP, type = "text", se = list(V_diag_ESP))

#Турция
data_ardl_TUR <- dplyr::filter(data_ardl, LOCATION=="TUR") 
View(data_ardl_TUR)
mod_TUR <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_TUR)
s_TUR <- summary(mod_TUR)
s_TUR
V_TUR <- vcovHC(mod_TUR, type = "HC0")
#выгружаем модель
V_diag_TUR <- sqrt(diag(V_TUR))
stargazer(mod_TUR, type = "text", se = list(V_diag_TUR))

stargazer(mod_BRA,mod_CHL,mod_GRC,mod_MEX,mod_POL,mod_PRT,mod_SVN,mod_ESP,mod_TUR, type = "html",se=list(V_diag_BRA,V_diag_CHL,V_diag_GRC,V_diag_MEX,V_diag_POL,V_diag_PRT,V_diag_SVN,V_diag_ESP,V_diag_TUR), 
          out = "cluster1.htm")

###Кластер 3

#Австрия
data_ardl_AUT <- dplyr::filter(data_ardl, LOCATION=="AUT") 
View(data_ardl_AUT)
mod_AUT <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_AUT)
s_AUT <- summary(mod_AUT)
s_AUT
V_AUT <- vcovHC(mod_AUT, type = "HC0")
#выгружаем модель
V_diag_AUT <- sqrt(diag(V_AUT))
stargazer(mod_AUT, type = "text", se = list(V_diag_AUT))

#Бельгия
data_ardl_BEL <- dplyr::filter(data_ardl, LOCATION=="BEL") 
View(data_ardl_BEL)
mod_BEL <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_BEL)
s_BEL <- summary(mod_BEL)
s_BEL
V_BEL <- vcovHC(mod_BEL, type = "HC0")
#выгружаем модель
V_diag_BEL <- sqrt(diag(V_BEL))
stargazer(mod_BEL, type = "text", se = list(V_diag_BEL))

#Чехия
data_ardl_CZE <- dplyr::filter(data_ardl, LOCATION=="CZE") 
View(data_ardl_CZE)
mod_CZE <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_CZE)
s_CZE <- summary(mod_CZE)
s_CZE
V_CZE <- vcovHC(mod_CZE, type = "HC0")
#выгружаем модель
V_diag_CZE <- sqrt(diag(V_CZE))
stargazer(mod_CZE, type = "text", se = list(V_diag_CZE))

#Франция
data_ardl_FRA <- dplyr::filter(data_ardl, LOCATION=="FRA") 
View(data_ardl_FRA)
mod_FRA <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_FRA)
s_FRA <- summary(mod_FRA)
s_FRA
V_FRA <- vcovHC(mod_FRA, type = "HC0")
#выгружаем модель
V_diag_FRA <- sqrt(diag(V_FRA))
stargazer(mod_FRA, type = "text", se = list(V_diag_FRA))

#Германия
data_ardl_DEU <- dplyr::filter(data_ardl, LOCATION=="DEU") 
View(data_ardl_DEU)
mod_DEU <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_DEU)
s_DEU <- summary(mod_DEU)
s_DEU
V_DEU <- vcovHC(mod_DEU, type = "HC0")
#выгружаем модель
V_diag_DEU <- sqrt(diag(V_DEU))
stargazer(mod_DEU, type = "text", se = list(V_diag_DEU))


#Венгрия
data_ardl_HUN <- dplyr::filter(data_ardl, LOCATION=="HUN") 
View(data_ardl_HUN)
mod_HUN <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_HUN)
s_HUN <- summary(mod_HUN)
s_HUN
V_HUN <- vcovHC(mod_HUN, type = "HC0")
#выгружаем модель
V_diag_HUN <- sqrt(diag(V_HUN))
stargazer(mod_HUN, type = "text", se = list(V_diag_HUN))

#Италия
data_ardl_ITA <- dplyr::filter(data_ardl, LOCATION=="ITA") 
View(data_ardl_ITA)
mod_ITA <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_ITA)
s_ITA <- summary(mod_ITA)
s_ITA
V_ITA <- vcovHC(mod_ITA, type = "HC0")
#выгружаем модель
V_diag_ITA <- sqrt(diag(V_ITA))
stargazer(mod_ITA, type = "text", se = list(V_diag_ITA))

#Япония
data_ardl_JPN <- dplyr::filter(data_ardl, LOCATION=="JPN") 
View(data_ardl_JPN)
mod_JPN <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_JPN)
s_JPN <- summary(mod_JPN)
s_JPN
V_JPN <- vcovHC(mod_JPN, type = "HC0")
#выгружаем модель
V_diag_JPN <- sqrt(diag(V_JPN))
stargazer(mod_JPN, type = "text", se = list(V_diag_JPN))

#Люксембург
data_ardl_LUX <- dplyr::filter(data_ardl, LOCATION=="LUX") 
View(data_ardl_LUX)
mod_LUX <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_LUX)
s_LUX <- summary(mod_LUX)
s_LUX
V_LUX <- vcovHC(mod_LUX, type = "HC0")
#выгружаем модель
V_diag_LUX <- sqrt(diag(V_LUX))
stargazer(mod_LUX, type = "text", se = list(V_diag_LUX))

#Швейцария
data_ardl_CHE <- dplyr::filter(data_ardl, LOCATION=="CHE") 
View(data_ardl_CHE)
mod_CHE <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_CHE)
s_CHE <- summary(mod_CHE)
s_CHE
V_CHE <- vcovHC(mod_CHE, type = "HC0")
#выгружаем модель
V_diag_CHE <- sqrt(diag(V_CHE))
stargazer(mod_CHE, type = "text", se = list(V_diag_CHE))

stargazer(mod_AUT,mod_BEL,mod_CZE,mod_FRA,mod_DEU,mod_HUN,mod_ITA,mod_JPN,mod_LUX, mod_CHE, type = "html", 
          se=list(V_diag_AUT,V_diag_BEL,V_diag_CZE,V_diag_FRA,V_diag_DEU,V_diag_HUN,V_diag_ITA,V_diag_JPN,V_diag_LUX,V_diag_CHE),out = "cluster3.htm")

###Кластер 4

#Данных по Болгарии, Китаю, Литве, Румынии нет

#Эстония 
data_ardl_EST <- dplyr::filter(data_ardl, LOCATION=="EST") 
View(data_ardl_EST)
mod_EST <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_EST)
s_EST <- summary(mod_EST)
s_EST
V_EST <- vcovHC(mod_EST, type = "HC0")
#выгружаем модель
V_diag_EST <- sqrt(diag(V_EST))
stargazer(mod_EST, type = "text", se = list(V_diag_EST))

#Индия
data_ardl_IND <- dplyr::filter(data_ardl, LOCATION=="IND") 
View(data_ardl_IND)
mod_IND <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_IND)
s_IND <- summary(mod_IND)
s_IND
V_IND <- vcovHC(mod_IND, type = "HC0")
#выгружаем модель
V_diag_IND <- sqrt(diag(V_IND))
stargazer(mod_IND, type = "text", se = list(V_diag_IND))

#Индонезия
data_ardl_IDN <- dplyr::filter(data_ardl, LOCATION=="IDN") 
View(data_ardl_IDN)
mod_IDN <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_IDN)
s_IDN <- summary(mod_IDN)
s_IDN
V_IDN <- vcovHC(mod_IDN, type = "HC0")
#выгружаем модель
V_diag_IDN <- sqrt(diag(V_IDN))
stargazer(mod_IDN, type = "text", se = list(V_diag_IDN))

#Корея
data_ardl_KOR <- dplyr::filter(data_ardl, LOCATION=="KOR") 
View(data_ardl_KOR)
mod_KOR <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_KOR)
s_KOR <- summary(mod_KOR)
s_KOR
V_KOR <- vcovHC(mod_KOR, type = "HC0")
#выгружаем модель
V_diag_KOR <- sqrt(diag(V_KOR))
stargazer(mod_KOR, type = "text", se = list(V_diag_KOR))

#Латвия
data_ardl_LVA <- dplyr::filter(data_ardl, LOCATION=="LVA") 
View(data_ardl_LVA)
mod_LVA <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_LVA)
s_LVA <- summary(mod_LVA)
s_LVA
V_LVA <- vcovHC(mod_LVA, type = "HC0")
#выгружаем модель
V_diag_LVA <- sqrt(diag(V_LVA))
stargazer(mod_LVA, type = "text", se = list(V_diag_LVA))

#Россия
data_ardl_RUS <- dplyr::filter(data_ardl, LOCATION=="RUS") 
View(data_ardl_RUS)
mod_RUS <- lm(GDP_growth_diff ~ lag(GDP_growth_diff,1) + lag(GDP_growth_diff,2) + lag(cpi_diff,1) + lag(NX_growth_diff,1) + Crisis,data_ardl_RUS)
s_RUS <- summary(mod_RUS)
s_RUS
V_RUS <- vcovHC(mod_RUS, type = "HC0")
#выгружаем модель
V_diag_RUS <- sqrt(diag(V_RUS))
stargazer(mod_RUS, type = "text", se = list(V_diag_RUS))

stargazer(mod_EST,mod_IND,mod_IDN,mod_KOR,mod_LVA,mod_RUS, type = "html",se=list(V_diag_EST,V_diag_IND,V_diag_IDN,V_diag_KOR,V_diag_LVA,V_diag_RUS), 
          out = "cluster4.htm")

#####################
# ARDL по кластерам #
#####################

library(plm)

data_ardl_1 <- filter(data_ardl,cluster==1)
data_ardl_2 <- filter(data_ardl,cluster==2)
data_ardl_3 <- filter(data_ardl,cluster==3)
data_ardl_4 <- filter(data_ardl,cluster==4)
data_ardl_BRA <- filter(data_ardl,LOCATION=="BRA")
View(data_ardl_1)

mod_cl1 <- pmg(GDP_growth ~ lag(GDP_growth,1) + lag(GDP_growth,2) + lag(cpi,1) + lag(NX_growth,1) +lag(spi,1) + Crisis,
           data = data_ardl_1, model = "mg", index = c("LOCATION", "TIME"))
summary(mod_cl1)
mod_cl1$indcoef

mod_cl2 <- pmg(GDP_growth ~ lag(GDP_growth,1) + lag(GDP_growth,2) + lag(cpi,1) + lag(NX_growth,1)+lag(spi,1) + Crisis,
               data = data_ardl_2, model = "mg", index = c("LOCATION", "TIME"))
summary(mod_cl2)

mod_cl3 <- pmg(GDP_growth ~ lag(GDP_growth,1) + lag(GDP_growth,2) + lag(cpi,1) + lag(NX_growth,1) +lag(spi,1)+ Crisis,
               data = data_ardl_3, model = "mg", index = c("LOCATION", "TIME"))
summary(mod_cl3)

mod_cl4 <- pmg(GDP_growth ~ lag(GDP_growth,1) + lag(GDP_growth,2) + lag(cpi,1) + lag(NX_growth,1)+lag(spi,1) + Crisis,
               data = data_ardl_4, model = "mg", index = c("LOCATION", "TIME"))
summary(mod_cl4)

mod_cl_all <- pmg(GDP_growth ~ lag(GDP_growth,1) + lag(GDP_growth,2) + lag(cpi,1) + lag(NX_growth,1)+lag(spi,1) + Crisis,
               data = data_ardl, model = "mg", index = c("LOCATION", "TIME"))
summary(mod_cl_all)

stargazer (mod_cl1,mod_cl2,mod_cl3,mod_cl4,mod_cl_all, type="html",out = "Hofstede_ardl.htm")
mod_cl2$indcoef

mod_cl_BRA <- plm(GDP_growth ~ lag(GDP_growth,1) + lag(GDP_growth,2) + lag(cpi,1) + lag(NX_growth,1) + Crisis,
                  data = data_ardl_BRA)
summary(mod_cl_BRA)


########################
#### ARDL 1998-2021 ####
########################

#Данные
#GDP_growth
data_ardl_gdp <- read.csv("/Users/artart/Desktop/Диплом/R/Данные/ARDL/1998-2021/GDP_growth_1998-2021.csv")
data_ardl_gdp <- dplyr::select(data_ardl_gdp,-c(INDICATOR, SUBJECT, MEASURE, FREQUENCY,Flag.Codes))
data_ardl_gdp$GDP_growth <- data_ardl_gdp$Value
data_ardl_gdp <- data_ardl_gdp[,-3]
View(data_ardl_gdp)

#Net export
data_ardl_NX <- read.csv("/Users/artart/Desktop/Диплом/R/Данные/ARDL/1998-2021/NX_bln_USD_1998-2021.csv")
data_ardl_NX <- dplyr::select(data_ardl_NX,-c(INDICATOR, SUBJECT, MEASURE, FREQUENCY,Flag.Codes))
data_ardl_NX$NX <- data_ardl_NX$Value
data_ardl_NX <- data_ardl_NX[,-3]
View(data_ardl_NX)

#Share prices (2015=100)
data_ardl_spi <- read.csv("/Users/artart/Desktop/Диплом/R/Данные/ARDL/1998-2021/Share_prices_index_1998-2021.csv")
data_ardl_spi <- dplyr::select(data_ardl_spi,-c(INDICATOR, SUBJECT, MEASURE, FREQUENCY,Flag.Codes))
data_ardl_spi$spi <- data_ardl_spi$Value
data_ardl_spi <- data_ardl_spi[,-3]
View(data_ardl_spi)

#Inflation (CPI)
data_ardl_cpi <- read.csv("/Users/artart/Desktop/Диплом/R/Данные/ARDL/1998-2021/Inflation_growth_1998-2021.csv")
data_ardl_cpi <- dplyr::select(data_ardl_cpi,-c(INDICATOR, SUBJECT, MEASURE, FREQUENCY,Flag.Codes))
data_ardl_cpi$cpi <- data_ardl_cpi$Value
data_ardl_cpi <- data_ardl_cpi[,-3]
View(data_ardl_cpi)

#Interest_rate
data_ardl_interest_rate <- read.csv("/Users/artart/Desktop/Диплом/R/Данные/ARDL/1998-2021/Interest_rates.csv")
data_ardl_interest_rate <- dplyr::select(data_ardl_interest_rate,-c(INDICATOR, SUBJECT, MEASURE, FREQUENCY,Flag.Codes))
data_ardl_interest_rate$Interest_rate <- data_ardl_interest_rate$Value
data_ardl_interest_rate <- data_ardl_interest_rate[,-3]
View(data_ardl_interest_rate)

data_ardl <- data_ardl_gdp %>% inner_join(data_ardl_cpi)
data_ardl <- data_ardl %>% inner_join(data_ardl_NX)
data_ardl <- data_ardl %>% inner_join(data_ardl_spi)
data_ardl <- data_ardl %>% inner_join(data_ardl_interest_rate)
View(data_ardl)

data_ardl$NX_growth <- (data_ardl$NX-lag(data_ardl$NX))/abs(lag(data_ardl$NX))*100
data_ardl$spi_growth <- (data_ardl$spi-lag(data_ardl$spi))/abs(lag(data_ardl$spi))*100
data_ardl <- data_ardl[(data_ardl$TIME != "1998-Q1"),]
View(data_ardl)



data_ardl$Great_Recession <- ifelse((data_ardl$TIME == "2008-Q1")|(data_ardl$TIME == "2008-Q2")|(data_ardl$TIME == "2008-Q3")|(data_ardl$TIME == "2008-Q4")|
                             (data_ardl$TIME == "2009-Q1")|(data_ardl$TIME == "2009-Q2")|(data_ardl$TIME == "2009-Q3")|(data_ardl$TIME == "2009-Q4"),1,0)

data_ardl$Pandemic <- ifelse((data_ardl$TIME == "2020-Q1")|(data_ardl$TIME == "2020-Q2")|(data_ardl$TIME == "2020-Q3")|(data_ardl$TIME == "2020-Q4"),1,0)

View(data_ardl)

data_countries_cluster <-  dplyr::select(data, c(LOCATION,cluster))
View(data_countries_cluster)
data_ardl <- data_ardl %>% merge(data_countries_cluster) 
data_ardl <- data_ardl[order(data_ardl$LOCATION,data_ardl$TIME),]
View(data_ardl)

library(plm)
data_ardl_1 <- filter(data_ardl,cluster==1,LOCATION!="CHL")
data_ardl_2 <- filter(data_ardl,cluster==2)
data_ardl_3 <- filter(data_ardl,cluster==3)
data_ardl_4 <- filter(data_ardl,cluster==4,LOCATION!="CHN",LOCATION!="IND")



View(data_ardl_1)
View(data_ardl_2)
View(data_ardl_3)
View(data_ardl_4)

mod_cl1 <- pmg(GDP_growth ~ lag(GDP_growth,1)+lag(GDP_growth,2) +cpi+ lag(cpi,1) +Interest_rate+lag(Interest_rate,1)+Great_Recession+Pandemic,
               data = data_ardl_1, model = "mg", index = c("LOCATION", "TIME"))
summary(mod_cl1)

mod_cl2 <- pmg(GDP_growth ~ lag(GDP_growth,1)+lag(GDP_growth,2) +cpi+ lag(cpi,1) +Interest_rate+lag(Interest_rate,1)+Great_Recession+Pandemic,
               data = data_ardl_2, model = "mg", index = c("LOCATION", "TIME"))
summary(mod_cl2)

mod_cl3 <- pmg(GDP_growth ~ lag(GDP_growth,1)+lag(GDP_growth,2) +cpi+ lag(cpi,1) +Interest_rate+lag(Interest_rate,1)+Great_Recession+Pandemic,
               data = data_ardl_3, model = "mg", index = c("LOCATION", "TIME"))
summary(mod_cl3)

mod_cl4 <- pmg(GDP_growth ~ lag(GDP_growth,1)+lag(GDP_growth,2) +cpi+ lag(cpi,1) +Interest_rate+lag(Interest_rate,1)+Great_Recession+Pandemic,
               data = data_ardl_4, model = "mg", index = c("LOCATION", "TIME"))
summary(mod_cl4)


stargazer (mod_cl1,mod_cl2,mod_cl3,mod_cl4, type="html",out = "Hofstede_ardl_1998-2021.htm")
mod_cl2$indcoef


################################
## Кластеризация по Экономике ##
################################


data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")

#Данные

#NX_growth
data_NX_growth <- read.csv("/Users/artart/Desktop/Диплом/R/Данные/Indicators/NX_growth_rate.csv")
data_NX_growth <- dplyr::select(data_NX_growth,-c(INDICATOR, SUBJECT, MEASURE, FREQUENCY,Flag.Codes))
data_NX_growth$NX_growth <- data_NX_growth$Value
data_NX_growth <- data_NX_growth[,-3]
View(data_NX_growth)

#Consumption share in GDP
data_Cons_growth <- read.csv("/Users/artart/Desktop/Диплом/R/Данные/Indicators/Consump_share_growth.csv")
data_Cons_growth <- dplyr::select(data_Cons_growth,-c(INDICATOR, SUBJECT, MEASURE, FREQUENCY,Flag.Codes))
data_Cons_growth$Cons_growth <- data_Cons_growth$Value
data_Cons_growth <- data_Cons_growth[,-3]
View(data_Cons_growth)

data <- data %>% inner_join(data_Cons_growth, by = "LOCATION")
data <- data %>% inner_join(data_NX_growth, by = "LOCATION")
#data <- data[-c(16,17),]
View(data)



#создание нового объекта
data_for_clusterization1 <- dplyr::select(data, GDP_growth, NX_growth, Cons_growth)
View(data_for_clusterization1)

#проводим стандартизацию
data_scale1 <- scale(data_for_clusterization1)

#создаём матрицу попарных расстояний
data_scale_matrix1 <- dist(data_scale1)

#построим иерархическую класеризацию
mod <- hclust(data_scale_matrix1)

#строим дендрограмму
plot(mod, hang = -1)
#plot(mod, hang = -1, labels = Countries)
#fviz_dend(mod, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", labels_track_height = 0.8)


#добавляем "обводку" кластеров
rect.hclust(mod, k=2)

#Создаём вектор из номеров кластеров
cluster <- cutree(mod, k=2)

#добавляем этот вектор к данным
data_for_clusterization1$cluster <- cluster
data$cluster <- as.factor(cluster)
View(data)

data_clusters <- dplyr::select(data,c("LOCATION","cluster"))
View(data_clusters)
data$cluster1 <- ifelse(cluster==1,1,0)
data$cluster2 <- ifelse(cluster==2,1,0)

data2$cluster <- as.factor(cluster)
data2$cluster_name <- ifelse(cluster==1, "Коллективизм-Осторожность",ifelse(cluster==2, "Индивидуализм",ifelse(cluster==3, "Маскулинность","Долгий взгляд")))
data3$cluster_name <- ifelse(cluster==1, "Коллективизм-Осторожность",ifelse(cluster==2, "Индивидуализм",ifelse(cluster==3, "Маскулинность","Долгий взгляд")))
View(data3)


#вычисляем средние значения по кластерам
data_group <- data %>% group_by(cluster) 
data_group_1 <- dplyr::select(data_group,-c(cluster,LOCATION,Country))
data_group_mean_1 <- summarise_all(data_group_1, mean)
data_group_mean_2 <- dplyr::select(data_group_mean_1,c(cluster_name,Power.distance,Individualism,Masculinity, Uncertainty.avoidance, Long.term.orientation, Indulgence))
data_group_mean_2 <-as.data.frame(data_group_mean_2) 
View(data_group_mean_1)

#среднее падение ВВП по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=GDP_growth)) +
  geom_bar(stat="identity", fill="red", colour="black", alpha = 0.8) + theme_linedraw()+ theme(axis.text.x=element_text(size=20),axis.title.x=element_text(size=20,colour="blue"),axis.text.y=element_text(size=20),axis.title.y=element_text(size=20,colour="blue")) + ylab("Темп прироста ВВП, %") + xlab("Группы")

t.test(data3$GDP_growth[data3$cluster_name=="Коллективизм-Осторожность"],data3$GDP_growth[data3$cluster_name=="Индивидуализм"])
t.test(data3$GDP_growth[data3$cluster_name=="Коллективизм-Осторожность"],data3$GDP_growth[data3$cluster_name=="Долгий взгляд"])
t.test(data3$GDP_growth[data3$cluster_name=="Коллективизм-Осторожность"],data3$GDP_growth[data3$cluster_name=="Маскулинность"])
t.test(data3$GDP_growth[data3$cluster_name=="Индивидуализм"],data3$GDP_growth[data3$cluster_name=="Долгий взгляд"])
t.test(data3$GDP_growth[data3$cluster_name=="Маскулинность"],data3$GDP_growth[data3$cluster_name=="Долгий взгляд"])
t.test(data3$GDP_growth[data3$cluster_name=="Индивидуализм"],data3$GDP_growth[data3$cluster_name=="Маскулинность"])

#средний прирост запаса капитала по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=Capital_stock_growth)) +
  geom_bar(stat="identity", fill="blue", colour="black", alpha = 0.8) + theme_linedraw()

#средняя инфляция по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=Inflation)) +
  geom_bar(stat="identity", fill="orange", colour="black", alpha = 0.8) + theme_linedraw()+ theme(axis.text.x=element_text(size=20),axis.title.x=element_text(size=20,colour="blue"),axis.text.y=element_text(size=20),axis.title.y=element_text(size=20,colour="blue")) + ylab("Инфляция, %") + xlab("Группы")

t.test(data3$Inflation[data3$cluster_name=="Коллективизм-Осторожность"],data3$Inflation[data3$cluster_name=="Индивидуализм"])
t.test(data3$Inflation[data3$cluster_name=="Коллективизм-Осторожность"],data3$Inflation[data3$cluster_name=="Долгий взгляд"])
t.test(data3$Inflation[data3$cluster_name=="Коллективизм-Осторожность"],data3$Inflation[data3$cluster_name=="Маскулинность"])
t.test(data3$Inflation[data3$cluster_name=="Индивидуализм"],data3$Inflation[data3$cluster_name=="Долгий взгляд"])
t.test(data3$Inflation[data3$cluster_name=="Маскулинность"],data3$Inflation[data3$cluster_name=="Долгий взгляд"])
t.test(data3$Inflation[data3$cluster_name=="Индивидуализм"],data3$Inflation[data3$cluster_name=="Маскулинность"])

#средний индекс Джини по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=Gini)) +
  geom_bar(stat="identity", fill="purple", colour="black", alpha = 0.8) + theme_linedraw()

#средний прирост работоспособного населения по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=Population_15_64_growth)) +
  geom_bar(stat="identity", fill="darkgreen", colour="black", alpha = 0.8) + theme_linedraw()

#средний индекс человеческого капитала по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=Human_cap_index_2010)) +
  geom_bar(stat="identity", fill="yellow", colour="black", alpha = 0.8) + theme_linedraw()

#Безработица в 2010 году по кластерам
ggplot(data_group_mean_1, aes(x=cluster_name, y=Unemp_rate)) +
  geom_bar(stat="identity", fill="darkgreen", colour="black", alpha = 0.8) + theme_linedraw()+ theme(axis.text.x=element_text(size=20),axis.title.x=element_text(size=20,colour="blue"),axis.text.y=element_text(size=20),axis.title.y=element_text(size=20,colour="blue")) + ylab("Безработица, %") + xlab("Группы")

t.test(data3$Unemp_rate[data3$cluster_name=="Коллективизм-Осторожность"],data3$Unemp_rate[data3$cluster_name=="Индивидуализм"])
t.test(data3$Unemp_rate[data3$cluster_name=="Коллективизм-Осторожность"],data3$Unemp_rate[data3$cluster_name=="Долгий взгляд"])
t.test(data3$Unemp_rate[data3$cluster_name=="Коллективизм-Осторожность"],data3$Unemp_rate[data3$cluster_name=="Маскулинность"])
t.test(data3$Unemp_rate[data3$cluster_name=="Индивидуализм"],data3$Unemp_rate[data3$cluster_name=="Долгий взгляд"])
t.test(data3$Unemp_rate[data3$cluster_name=="Маскулинность"],data3$Unemp_rate[data3$cluster_name=="Долгий взгляд"])
t.test(data3$Unemp_rate[data3$cluster_name=="Индивидуализм"],data3$Unemp_rate[data3$cluster_name=="Маскулинность"])

#Отношение долга к собственному капиталу
ggplot(data_group_mean_1, aes(x=cluster_name, y=D_E)) +
  geom_bar(stat="identity", fill="yellow", colour="black", alpha = 0.8) + theme_linedraw()+ theme(axis.text.x=element_text(size=20),axis.title.x=element_text(size=20,colour="blue"),axis.text.y=element_text(size=20),axis.title.y=element_text(size=20,colour="blue")) + ylab("Отношение долга к собственному капиталу") + xlab("Группы")

#среднее падение ВВП по странам
ggplot(data3, aes(x=GDP_growth, y=Country)) +
  geom_segment(aes(yend=Country), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=cluster_name)) +
  scale_colour_brewer(palette="Set1") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),   
        legend.position="right")

#среднее падение ВВП по странам, сгруппированным по кластерам
ggplot(data3, aes(x=GDP_growth, y=Country)) +
  geom_segment(aes(yend=Country), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=cluster_name)) +scale_colour_brewer(palette="Set1", guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),axis.text.x=element_text(size=18),axis.text.y=element_text(size=14),strip.text.y=element_text(size=12),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue")) +
  facet_grid(cluster_name ~ ., scales="free_y", space="free_y") +ylab("Страна и кластер")+xlab("Падение ВВП, %")

#Падение ВВП в зависимости от индивидуализма
ggplot(data3, aes(x=GDP_growth, y=Individualism, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1")

ggplot(data3, aes(x=GDP_growth, y=Individualism, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right",legend.text=element_text(size=14),axis.text.x=element_text(size=18),axis.text.y=element_text(size=18),strip.text.y=element_text(size=12),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue")) + scale_colour_brewer(palette="Set1") + stat_smooth(method=lm, se=FALSE) +ylab("Индивидуализм")+xlab("Падение ВВП, %")

#Падение ВВП в зависимости от маскулинности
ggplot(data3, aes(x=GDP_growth, y=Masculinity, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1")

ggplot(data3, aes(x=GDP_growth, y=Masculinity, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4.5,vjust=1) + theme_linedraw()+
  theme(legend.position="right",legend.text=element_text(size=14),axis.text.x=element_text(size=18),axis.text.y=element_text(size=18),strip.text.y=element_text(size=12),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue")) + scale_colour_brewer(palette="Set1")+ stat_smooth(method=lm, se=FALSE)+ylab("Маскулинность")+xlab("Падение ВВП, %")

#Падение ВВП в зависимости от удовлетворённости жизнью
ggplot(data3, aes(x=GDP_growth, y=Indulgence, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1")

ggplot(data3, aes(x=GDP_growth, y=Indulgence, colour=cluster_name)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1")+ stat_smooth(method=lm, se=FALSE)

#Диаграмма индивидуализм - удовлетворённость жизнью с учётом кластеров и падения ВВП
ggplot(data3, aes(x=Individualism, y=Indulgence, colour=cluster_name,size=GDP_growth)) + geom_point() + geom_text(aes(label=Country), size=4,vjust=1) + theme_linedraw()+
  theme(legend.position="right") + scale_colour_brewer(palette="Set1")

#Плотность и гистограмма
ggplot(data3, aes(x=GDP_growth)) + geom_density(fill = "red", alpha=0.8) + theme_linedraw() + theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue")) + xlab("Падение ВВП, %") + ylab("")

ggplot(data3, aes(x=GDP_growth)) + geom_histogram(fill = "red", alpha=0.8,bins = 43,colour="black") + theme_linedraw()

#"Ящик с усами" для темпа прироста ВВП
ggplot(data3, aes(x=cluster_name,y=GDP_growth)) + geom_boxplot(fill = "blue", alpha=0.8) + theme_linedraw()+
  theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16))

#Радчарт
data6 <- as.data.frame(t(data.frame(data_group_mean_2,row.names=c("Hierarchical","Latin or poor","Mostly European","Well-developed"))))
View(data6)
radarchart(data6)

#добавляем в дату строчки - максимальное и минимальное значение
data7 <- rbind(rep((120), 4), rep(0,5), data6)
radarchart(data7, pcol=c("Red","Darkblue","Darkgreen","Brown","Purple","Orange"),
           plty=1,plwd=2,cglcol="black")



#Кластер 1: Колумбия, Мексика, Морокко, Аргентина, Польша, Бразилия, Турция, Испания,...

#Кластер 2: Великобритания, Ирландия, Канада, Австралия, США, Швеция, Финляндия,...

#Кластер 3: Люксембург, Германия, Швейцария, Бельгия, Франция, Италия, Япония, Венгрия,...

#Кластер 4: Латвия, Литва, Южная Корея, Китай, Болгария, Индия, Индонезия, Румыния, Россия,...

#Кластер1 - самый многочисленный. Это не самые экономически развитые страны, в них высокая дистанция власти, низкий уровень индивидуализма
#высокий уровень избегания неопределённости.
#Кластер2 - экономически успешные и развитые страны с низкой дистанцией власти, наивысшим уровенем индивидуализма
#, наименьшей склонностью бояться неопределённости и наивысшей степенью удовлетворения жизнью.
#Кластер3 - в основном, европейские страны, там наивысшая маскулинность, высокая степень избегания неопределенности, долгосрочная ориентация
#Кластер4 - экономически отшибленные страны. Низкий уровень индивидуализма, наивысший уровень долгосрочной ориентации, наименьший вроень наслаждения жизнью.

