library(gtrend)
library(GTrendsR)
library(dplyr)
library(ggplot2)
library(scales)
library(forecast)
library(rucm)
library(yaml)

#set working directory
setwd("C:/Users/jwagg/Documents/R Projects/Gtrends/")

#load file w/ username and password
config <- yaml.load_file('config.txt')

#list of terms to pull trends for
terms <- c("amazon")

#call to trends scraper to pull trends
out <- gtrend_scraper(config$uname, config$pwrd, terms)

#extract the trend from the returned data
que <- extract_trend(out)

#subset trends into data frame
x <- data.frame(date = que[[1]][1:618,2], amazon = que[[1]][1:618,3])

#put into time series representing weekly data
#x2 <- ts(x$comcast, freq=365.25/7, start=2004+10/365.25)

#plot data for visual
plot(x$amazon, type="l")

#model using UCUM
modelTest <- ucm(amazon~0, data = x, irregular = TRUE, level = TRUE, slope = FALSE, season = TRUE, season.length = 52)

#plot data and smoothed level values
plot(x$amazon, ylab = "demand", type = "l")
lines(modelTest$s.level, col = "red")

modelTest$model #Print the model

#predict next N periods
x3 <- predict(modelTest$model, n.ahead = 52) 

#convert original and forecast to go into data frame
orig <- as.matrix(x$comcast)
orig2 <- data.frame(period = "orig", demand = orig)

fcast <- as.matrix(x3)
fcast2 <- data.frame(period = "fcast", demand = fcast)

#bind together
x5 <- rbind(orig2,fcast2)
x5$ind <- as.numeric(rownames(x5))

#subset to recent data
x6 <- subset(x5, ind >= 500) 
  
#plot for the visual                
qplot(
  x = ind,
  y = demand,
  data = x6,
  color = x6$period,
  geom = "line"
)




