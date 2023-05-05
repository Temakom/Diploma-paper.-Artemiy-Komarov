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

Countries <- dplyr::select(data, Country)
Countries <- array(Countries)


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

data$cluster_name <- ifelse(cluster==1, "Консерватизм и традиции",ifelse(cluster==2, "Активность и радость",ifelse(cluster==3, "Упорство и решительность","Строгость и дисциплина")))
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
  labs(x = "Группа", y = "Средний темп прироста ВВП в 2009 году") + geom_label(aes(label = round(value, 2)))+
  theme(axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue"))

#среднее падение ВВП 2010 по кластерам
data_mean_GDP_2010 <- data %>% dplyr::select(cluster_name, GDP_growth_2010) %>% na.omit() %>% group_by(cluster_name) %>%
  dplyr::summarise(value = mean(GDP_growth_2010), sd = sd(GDP_growth_2010)/sqrt(n())) 
View(data_mean_GDP_2010)

ggplot(data_mean_GDP_2010, aes(x = cluster_name, y = value)) +
  geom_bar(stat="identity", fill="darkgreen", alpha = 0.8) +
  geom_errorbar(aes(x = cluster_name, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "black", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Группа", y = "Средний темп прироста ВВП в 2010 году") + geom_label(aes(label = round(value, 2)))+
  theme(axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue"))

#среднее падение ВВП 2011 по кластерам
data_mean_GDP_2011 <- data %>% dplyr::select(cluster_name, GDP_growth_2011) %>% na.omit() %>% group_by(cluster_name) %>%
  dplyr::summarise(value = mean(GDP_growth_2011), sd = sd(GDP_growth_2011)/sqrt(n())) 
View(data_mean_GDP_2011)

ggplot(data_mean_GDP_2011, aes(x = cluster_name, y = value)) +
  geom_bar(stat="identity", fill="darkgreen", alpha = 0.8) +
  geom_errorbar(aes(x = cluster_name, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "black", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Группа", y = "Средний темп прироста ВВП в 2011 году") + geom_label(aes(label = round(value, 2)))+
  theme(axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue"))

#Средний прирост работоспособного населения по кластерам
data_mean_Pop_15_64_growth_2009 <- data %>% dplyr::select(cluster_name, Population_15_64_2009_growth) %>% na.omit() %>% group_by(cluster_name) %>%
  dplyr::summarise(value = mean(Population_15_64_2009_growth), sd = sd(Population_15_64_2009_growth)/sqrt(n())) 
View(data_mean_Pop_15_64_growth_2009)

ggplot(data_mean_Pop_15_64_growth_2009, aes(x = cluster_name, y = value)) +
  geom_bar(stat="identity", fill="darkblue", alpha = 0.8) +
  geom_errorbar(aes(x = cluster_name, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "black", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Группа", y = "Прирост работоспособного населения в 2009 году") + geom_label(aes(label = round(value, 2)))+
  theme(axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue"))

#Средний прирост капитала по кластерам
data_mean_Capital_stock_growth_2009 <- data %>% dplyr::select(cluster_name, Capital_stock_2009_growth) %>% na.omit() %>% group_by(cluster_name) %>%
  dplyr::summarise(value = mean(Capital_stock_2009_growth), sd = sd(Capital_stock_2009_growth)/sqrt(n())) 
View(data_mean_Capital_stock_growth_2009)

ggplot(data_mean_Capital_stock_growth_2009, aes(x = cluster_name, y = value)) +
  geom_bar(stat="identity", fill="darkred", alpha = 0.8) +
  geom_errorbar(aes(x = cluster_name, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "black", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Группа", y = "Прирост капитала в 2009 году") + geom_label(aes(label = round(value, 2)))+
  theme(axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue"))

#Средняя инфляция 2009 по кластерам
data_mean_Inflation_2009 <- data %>% dplyr::select(cluster_name, Inflation_2009) %>% na.omit() %>% group_by(cluster_name) %>%
  dplyr::summarise(value = mean(Inflation_2009), sd = sd(Inflation_2009)/sqrt(n())) 
View(data_mean_Inflation_2009)

ggplot(data_mean_Inflation_2009, aes(x = cluster_name, y = value)) +
  geom_bar(stat="identity", fill="orange", alpha = 0.8) +
  geom_errorbar(aes(x = cluster_name, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "black", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Группа", y = "Инфляция в 2009 году") + geom_label(aes(label = round(value, 2)))+
  theme(axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue"))

#среднее падение ВВП по странам, сгруппированным по кластерам
ggplot(data, aes(x=GDP_growth_2009, y=Country)) +
  geom_segment(aes(yend=Country), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=cluster_name)) +scale_colour_brewer(palette="Set1", guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),axis.text.x=element_text(size=18),axis.text.y=element_text(size=12),strip.text.y=element_text(size=9),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue")) +
  facet_grid(cluster_name ~ ., scales="free_y", space="free_y") +ylab("Страна и группа")+xlab("Темп прироста ВВП, %")

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
ggplot(data, aes(x=GDP_growth_2009)) + geom_density(fill = "red", alpha=0.8) + theme_linedraw() + theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue")) + xlab("Темп прироста ВВП, %") + ylab("")

#Радчарт
library(fmsb)

radarchart(data_group_mean_2, axistype=0 , maxmin=F, , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8, pcol=c("darkred","darkblue","darkgreen","orange"))+
  

legend(x=1.2, y=0.7,
       legend = c("Активность и радость","Консерватизм и традиции","Строгость и дисциплина","Упорство и решительность"),
       bty = "n", pch = 20, col = c("darkred","darkblue","darkgreen","orange"),
       text.col = "grey25", pt.cex = 2)


#########################
## Кластеризация Шварц ##
#########################

data <- read_excel("/Users/artart/Desktop/Диплом/R/Данные/Diploma.xlsx")
data <- dplyr::select(data,LOCATION, GDP_growth_2010,GDP_growth_2011,GDP_growth_2009, Inflation_2009, Country, Harmony, Embeddedness, Hierarchy, Mastery, Aff_auton, Intel_auton, Egalitar)
data <- na.omit(data)

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

data$cluster_name <- ifelse(cluster==1, "Открытость и равенство",ifelse(cluster==2, "Порядок и спокойствие",ifelse(cluster==3, "Мир и справедливость", "Власть и смелость")))
data$cluster1 <- ifelse(cluster==1,1,0)
data$cluster2 <- ifelse(cluster==2,1,0)
data$cluster3 <- ifelse(cluster==3,1,0)

#вычисляем средние значения по кластерам
data_group <- group_by(data, cluster_name) 
data_group_1 <- dplyr::select(data_group,-c(cluster, LOCATION))
data_group_mean_1 <- summarise_all(data_group_1, mean)
data_group_mean_2 <- data_group_mean_1[,7:13]
View(data_group_mean_1)
View(data_group_mean_2)

#среднее падение ВВП по странам, сгруппированным по кластерам
ggplot(data, aes(x=GDP_growth_2009, y=Country)) +
  geom_segment(aes(yend=Country), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=cluster_name)) +scale_colour_brewer(palette="Set1", guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),axis.text.x=element_text(size=18),axis.text.y=element_text(size=12),strip.text.y=element_text(size=9),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue")) +
  facet_grid(cluster_name ~ ., scales="free_y", space="free_y") +ylab("Страна и группа")+xlab("Темп прироста ВВП, %")

#среднее падение ВВП 2009 по кластерам
data_mean_GDP_2009 <- data %>% dplyr::select(cluster_name, GDP_growth_2009) %>% na.omit() %>% group_by(cluster_name) %>%
  dplyr::summarise(value = mean(GDP_growth_2009), sd = sd(GDP_growth_2009)/sqrt(n())) 
View(data_mean_GDP_2009)

ggplot(data_mean_GDP_2009, aes(x = cluster_name, y = value)) +
  geom_bar(stat="identity", fill="red", alpha = 0.8) +
  geom_errorbar(aes(x = cluster_name, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "black", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Группа", y = "Средний темп прироста ВВП в 2009 году") + geom_label(aes(label = round(value, 2)))+
  theme(axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue"))


#среднее падение ВВП 2010 по кластерам
data_mean_GDP_2010 <- data %>% dplyr::select(cluster_name, GDP_growth_2010) %>% na.omit() %>% group_by(cluster_name) %>%
  dplyr::summarise(value = mean(GDP_growth_2010), sd = sd(GDP_growth_2010)/sqrt(n())) 
View(data_mean_GDP_2010)

ggplot(data_mean_GDP_2010, aes(x = cluster_name, y = value)) +
  geom_bar(stat="identity", fill="darkgreen", alpha = 0.8) +
  geom_errorbar(aes(x = cluster_name, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "black", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Группа", y = "Средний темп прироста ВВП в 2010 году") + geom_label(aes(label = round(value, 2)))+
  theme(axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue"))


#среднее падение ВВП 2011 по кластерам
data_mean_GDP_2011 <- data %>% dplyr::select(cluster_name, GDP_growth_2011) %>% na.omit() %>% group_by(cluster_name) %>%
  dplyr::summarise(value = mean(GDP_growth_2011), sd = sd(GDP_growth_2011)/sqrt(n())) 
View(data_mean_GDP_2011)

ggplot(data_mean_GDP_2011, aes(x = cluster_name, y = value)) +
  geom_bar(stat="identity", fill="darkgreen", alpha = 0.8) +
  geom_errorbar(aes(x = cluster_name, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "black", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Группа", y = "Средний темп прироста ВВП в 2011 году") + geom_label(aes(label = round(value, 2)))+
  theme(axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue"))


#Средняя инфляция 2009 по кластерам
data_mean_Inflation_2009 <- data %>% dplyr::select(cluster_name, Inflation_2009) %>% na.omit() %>% group_by(cluster_name) %>%
  dplyr::summarise(value = mean(Inflation_2009), sd = sd(Inflation_2009)/sqrt(n())) 
View(data_mean_Inflation_2009)

ggplot(data_mean_Inflation_2009, aes(x = cluster_name, y = value)) +
  geom_bar(stat="identity", fill="orange", alpha = 0.8) +
  geom_errorbar(aes(x = cluster_name, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "black", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Кластер", y = "Инфляция в 2009 году") + geom_label(aes(label = round(value, 2)))+
  theme(axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue"))


#среднее падение ВВП по странам, сгруппированным по кластерам
ggplot(data, aes(x=GDP_growth_2009, y=Country)) +
  geom_segment(aes(yend=Country), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=cluster_name)) +scale_colour_brewer(palette="Set1", guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),axis.text.x=element_text(size=18),axis.text.y=element_text(size=12),strip.text.y=element_text(size=10),axis.title.x=element_text(size=18,colour="blue"),axis.title.y=element_text(size=18,colour="blue")) +
  facet_grid(cluster_name ~ ., scales="free_y", space="free_y") +ylab("Страна и кластер")+xlab("Падение ВВП, %")


#Радчарт
library(fmsb)

radarchart(data_group_mean_2, axistype=0 , maxmin=F, , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
           #custom labels
           vlcex=0.8, pcol=c("darkred","darkblue","darkgreen","orange"))

legend(x=1.3, y=0.7,
       legend = c("Власть и смелость","Мир и справедливость","Открытость и равенство","Порядок и спокойствие"),
       bty = "n", pch = 20, col = c("darkred","darkblue","darkgreen","orange"),
       text.col = "grey25", pt.cex = 2)



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

