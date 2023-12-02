library(ggplot2)

data <- read.csv("TIME_USE_24092022.csv", encoding='utf-8')

data <- subset(data, País != "África do Sul")
filter_data<-subset(data, Sexo == 'Homens' & Ocupação %in% c("Outros","Trabalho remunerado ou estudo"))

ggplot(filter_data, aes(x=filter_data$Ocupação, y=filter_data$Tempo))+geom_boxplot()+geom_point()+xlab("Ocupação")+ylab("Tempo")