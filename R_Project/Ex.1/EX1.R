library(ggplot2)
library(readxl)
data <- read_excel("econ.xlsx", col_types = c("date", 
                                              "skip", "numeric", "skip", "numeric", 
                                              "skip"))
data1979 <- data[data$tempo >= "1979-01-01", ]
ddesempavg <- mean(data1979$ddesemp)
popavg <- mean(data1979$pop)
ddesemps <- sd(data1979$ddesemp)
pops <- sd(data1979$pop)

zddesemp <- function(ddesemp) {
  x <- (ddesemp - ddesempavg) / ddesemps
  return(x)
}

zpop <- function(pop) {
  x <- (pop - popavg) / pops
  return(x)
}

ddesempline <- sapply(data1979$ddesemp, zddesemp)
popline <- sapply(data1979$pop, zpop)

graph1 <- ggplot(data1979, aes(tempo)) + geom_line(aes(y = ddesempline, color = "ddesemp")) + 
  geom_line(aes(y = popline, color = "pop"))
graph1 + xlab("Date") + ylab("X")