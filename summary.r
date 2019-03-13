library(ggplot2)
library(plyr)
library(forecast)


shotData = read.csv("shots.csv", header = TRUE)
colnames(shotData)[1] <- "Sid"

sid_data = shotData[, c("Sid")]

ggplot(data.frame(sid_data), aes(x = sid_data, fill = sid_data)) +
  geom_bar() + xlab("Direction") + ylab("Shots taken") + guides(fill = FALSE)

sid_data = data.frame(sid_data)
colnames(sid_data)[1] <- "Sid"

#Right is 1, Center is 2, Left is 3
sid_data$Sid <- revalue(sid_data$Sid, c("R"=1, "L"=3, "C"=2))

x <- 1:length(sid_data$Sid)
plot(x, sid_data$Sid, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
      frame = FALSE)
lines(x, sid_data$Sid, type = "l")


sid_data$ID <- seq.int(nrow(sid_data))

p1 <- ggplot(sid_data, aes(x=ID,y=sid_data$Sid)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=0))
print(p1)


autoArimaFit <- auto.arima(sid_data$Sid)
plot(forecast(autoArimaFit, h=3))
