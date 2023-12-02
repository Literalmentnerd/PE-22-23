library(ggplot2)

data <- read.delim("web.tecnico.ulisboa.pt_paulo.soares_pe_projeto_GENDER_EMP_19032023152556091.txt")

data <- subset(data, Country=="France" & IND=="EMP3" & Time=="2020" & Age.Group %in% c("15-24", "25-54", "55-64") & Sex != "All persons")

ggplot(data, aes(x = Age.Group, y = Value, fill = Sex)) + geom_bar(stat="identity",position="dodge") + labs(title = "Dados de Desempregados da OCDE")
