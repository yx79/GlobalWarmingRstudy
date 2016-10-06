
# R project Global Warming
#######################################
# install magrittr, then type library(magrittr)
# install dplyr, then type library(dplyr)
# install tidyr, library(tidyr)
# install ggplot2, library(ggplot2)

# import data
# GlobalLandTemperaturesByCountry <- read.csv("~/GlobalLandTemperatures/GlobalLandTemperaturesByCountry.csv")
View(GlobalLandTemperaturesByCountry)

# Remove na's
GlobalLandTemperaturesByCountry = na.omit(GlobalLandTemperaturesByCountry)
# Change dt column property from factor to date
GlobalLandTemperaturesByCountry$dt <- as.Date(GlobalLandTemperaturesByCountry$dt)
class(GlobalLandTemperaturesByCountry$dt)
# format YYYY-MM-DD to Month, and Year Column
GlobalLandTemperaturesByCountry$Month<-as.numeric(format(GlobalLandTemperaturesByCountry$dt,"%m"))
GlobalLandTemperaturesByCountry$Month.String<-format(GlobalLandTemperaturesByCountry$dt,"%B")
GlobalLandTemperaturesByCountry$Year<-as.numeric(format(GlobalLandTemperaturesByCountry$dt,"%Y"))

chinaData <- GlobalLandTemperaturesByCountry[GlobalLandTemperaturesByCountry$Country == "China", ]

china1975 <- chinaData[chinaData$Year >= "1975", ]

ggplot(china1975,aes(x=dt,y=AverageTemperature,colour=reorder(Month.String,-AverageTemperature,mean)))+
  geom_point()+geom_smooth()+ggtitle("Average Temperatures by\nMonth in China")+
  xlab("Year")+ylab("Average Temperature")+labs(colour='Month')



# china 1975 dataset analysis
chinaSince1975 <- GlobalLandTemperaturesByCountry %>%
  filter(Country=="China")  %>%
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE)
# remove NA data
chinaSince1975 <- na.omit(chinaSince1975)
# filtering 
chinaSince1975 <- chinaSince1975 %>%
  filter(Year > 1975) %>%
  group_by(Year) %>% 
  summarise(Temparture = mean(AverageTemperature))
# plotting
p <- qplot(Year,
           Temparture,
           data=chinaSince1975,
           main="China Average Temperature 1975-2013",
           geom=c("point","smooth")) +
  aes(colour = Temparture) +
  scale_color_gradient(low="blue", high="red") +
  ylab("Temperature (°C)")
# save plot result
ggsave("res.png", p, width=7, height=4, units="in")


# china 1820 - 2012 dataset analysis
chinaTemp1820 <- GlobalLandTemperaturesByCountry %>%
  filter(Country=="China")  %>%
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE)
# remove NA data
chinaTemp1820 <- na.omit(chinaTemp1820)
# filtering 
chinaTemp1820 <- chinaTemp1820 %>%
  filter(Year < 2013) %>%
  group_by(Year) %>% 
  summarise(Temperature = mean(AverageTemperature))
# plotting
p <- qplot(Year,
           Temperature,
           data=chinaTemp1820,
           main="China Average Temperature 1820-2012",
           geom=c("point","smooth")) +
  aes(colour = Temperature) +
  scale_color_gradient(low="blue", high="red") +
  ylab("Temperature (°C)")
# save plot graph
ggsave("ChinaAverageTemperature1820-2012.png", p, width=7, height=4, units="in")



# china 1975 - 2012 dataset analysis
chinaTemp1975 <- GlobalLandTemperaturesByCountry %>%
  filter(Country=="China")  %>%
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE)
# remove NA data
chinaTemp1975 <- na.omit(chinaTemp1975)
# filtering 
chinaTemp1975 <- chinaTemp1975 %>%
  filter(Year >= 1975) %>%
  filter(Year < 2013) %>%
  group_by(Year) %>% 
  summarise(Temperature = mean(AverageTemperature))
# plotting
p <- qplot(Year,
           Temperature,
           data=chinaTemp1975,
           main="China Average Temperature 1975-2012",
           geom=c("point","smooth")) +
  aes(colour = Temperature) +
  scale_color_gradient(low="blue", high="red") +
  ylab("Temperature (°C)")
# save plot graph
ggsave("ChinaAverageTemperature1975-2012.png", p, width=7, height=4, units="in")




# filter the country and separate dt to Year, Month, Day
ChinaLandTemp <- GlobalLandTemperaturesByCountry %>%
  filter(Country=="China")  %>%
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE)
ChinaLandTemp <- na.omit(ChinaLandTemp)

ChinaSince1975 <- ChinaLandTemp %>%
  filter(Year > 1975) 

# Plot by season
ggplot(ChinaSince1975,aes(x=dt,y=AverageTemperature,colour=reorder(Month,-AverageTemperature,mean)))+
  geom_point()+geom_smooth()+ggtitle("Average Temperatures by\nMonth in China")+
  xlab("Year")+ylab("Average Temperature")+labs(colour='Month')



#  Linear Regression  
fit <- lm(Temperature ~ Year, data=chinaTemp)
summary(fit) # show results
plot(fit)
# Temperature = 


# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

# diagnostic plots 
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
#plot(fit)

chinaTemp1975 <- chinaTemp %>%
  filter(Year > 1975) 

chinaTempModel <- lm(Temperature ~ Year, data=chinaTemp1975)

coeffs = coefficients(chinaTempModel)

coeffs 

fitted(chinaTempModel) # predicted values
Year = 2050        
china2050 = coeffs[1] + coeffs[2]*Year 
china2050 
coeffs[1]
coeffs[2]


plot(Temperature ~ Year, data = chinaTemp)
abline(chinaTempModel)


ggplotRegression <- function (chinaTempModel) {
  
  require(ggplot2)
  
  ggplot(chinaTempModel$model, aes_string(x = names(chinaTempModel$model)[2], y = names(chinaTempModel$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(chinaTempModel)$adj.r.squared, 5),
                       "Intercept =",signif(chinaTempModel$coef[[1]],5 ),
                       " Slope =",signif(chinaTempModel$coef[[2]], 5),
                       " P =",signif(summary(chinaTempModel)$coef[2,4], 5)))
}



chinaTempModel <- lm(Temperature ~ Year, data = chinaTemp1975)
ggplotRegression(chinaTempModel)


chinaData <- GlobalLandTemperaturesByCountry[GlobalLandTemperaturesByCountry$Country == "China", ]
# Remove na's
chinaData = na.omit(chinaData)

chinaData$dt <- as.Date(chinaData$dt)
chinaData[order(chinaData$dt),]
class(chinaData$dt)

chinaData$Month<-as.numeric(format(chinaData$dt,"%m"))
chinaData$Month.String<-format(chinaData$dt,"%B")
chinaData$Year<-as.numeric(format(chinaData$dt,"%Y"))

# Month == 12 (Winter)
chinaDec <- chinaData[chinaData$Month == 12, ]
ggplot(chinaDec,aes(x=dt,y=AverageTemperature,colour=reorder(Month.String,-AverageTemperature,mean)))+
  geom_point()+geom_smooth()+ggtitle("Average Temperatures by\nMonth in China")+
  xlab("Year")+ylab("Average Temperature")+labs(colour='Month')



USAData <- GlobalLandTemperaturesByCountry[GlobalLandTemperaturesByCountry$Country == "United States", ]

# Month == 11,12 Winter
USADataMonth1975 <- USAData[USAData$Month == 12, ]
USADataMonth1975 <- USAData[USAData$Year >= 1975 & USAData$Month == 12, ]
ggplot(USADataMonth1975,aes(x=dt,y=AverageTemperature,colour=reorder(Month.String,-AverageTemperature,mean)))+
  geom_point()+geom_smooth()+ggtitle("Average Temperatures by\nMonth in USA")+
  xlab("Year")+ylab("Average Temperature")+labs(colour='Month')



########################################
# install magrittr, then type library(magrittr)
# install dplyr, then type library(dplyr)
# install tidyr, library(tidyr)
# install ggplot2, library(ggplot2)

library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
# import data


# Remove na's
myGlobalTemp = na.omit(GlobalTemperatures)
# Change dt column property from factor to date
myGlobalTemp$dt <- as.Date(myGlobalTemp$dt)
class(myGlobalTemp$dt)
# format YYYY-MM-DD to Month, and Year Column
myGlobalTemp$Month<-as.numeric(format(myGlobalTemp$dt,"%m"))
myGlobalTemp$Month.String<-format(myGlobalTemp$dt,"%B")
myGlobalTemp$Year<-as.numeric(format(myGlobalTemp$dt,"%Y"))



# land 
myGlobalLand <- myGlobalTemp %>%
  group_by(Year) %>% 
  summarise(Temperature = mean(LandAverageTemperature))
# plotting
p <- qplot(Year,
           Temperature,
           data=myGlobalLand,
           main="Global Average Temperature 1850-2015",
           geom=c("point","smooth")) +
  aes(colour = Temperature) +
  scale_color_gradient(low="blue", high="red") +
  ylab("Temperature (°C)")
# save plot graph
ggsave("GlobalAverageTemperature1850-2015.png", p, width=7, height=4, units="in")



# ocean 
myGlobalLand <- myGlobalTemp %>%
  group_by(Year) %>% 
  summarise(Temperature = mean(LandAverageTemperature))
# plotting
p <- qplot(Year,
           Temperature,
           data=myGlobalLand,
           main="Global Average Temperature 1850-2015",
           geom=c("point","smooth")) +
  aes(colour = Temperature) +
  scale_color_gradient(low="blue", high="red") +
  ylab("Temperature (°C)")
# save plot graph
ggsave("GlobalAverageTemperature1850-2015.png", p, width=7, height=4, units="in")




#  Linear Regression  
chinaTemp <- GlobalLandTemperaturesByCountry %>%
  filter(Country=="China") %>% 
  filter(Year >= 1975) %>%
  filter(Year < 2013) %>%
  group_by(Year) %>% 
  summarise(Temperature = mean(AverageTemperature))


fit <- lm(Temperature ~ Year, data=chinaTemp)
summary(fit) # show results
plot(fit)
# Temperature = 


# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

# diagnostic plots 
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
#plot(fit)

chinaTemp1975 <- chinaTemp %>%
  filter(Year > 1975) 

chinaTempModel <- lm(Temperature ~ Year, data=chinaTemp1975)

coeffs = coefficients(chinaTempModel)
coeffs 

fitted(chinaTempModel) # predicted values
Year = 2050        
china2050 = coeffs[1] + coeffs[2]*Year 
china2050 
coeffs[1]
coeffs[2]


plot(Temperature ~ Year, data = chinaTemp)
abline(chinaTempModel)


ggplotRegression <- function (chinaTempModel) {
  require(ggplot2)
  ggplot(chinaTempModel$model, aes_string(x = names(chinaTempModel$model)[2], y = names(chinaTempModel$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(chinaTempModel)$adj.r.squared, 5),
                       "Intercept =",signif(chinaTempModel$coef[[1]],5 ),
                       " Slope =",signif(chinaTempModel$coef[[2]], 5),
                       " P =",signif(summary(chinaTempModel)$coef[2,4], 5)))
}


chinaTempModel <- lm(Temperature ~ Year, data = chinaTemp1975)
ggplotRegression(chinaTempModel)





#  Linear Regression of 20 years
# 1950--1960--1970--1980--1990--2010
chinaTemp <- GlobalLandTemperaturesByCountry %>%
  filter(Country=="China") %>% 
  filter(Year >= 1990) %>%
  filter(Year <= 2010) %>%
  group_by(Year) %>% 
  summarise(Temperature = mean(AverageTemperature))

chinaTempModel <- lm(Temperature ~ Year, data=chinaTemp)
coeffs = coefficients(chinaTempModel)
coeffs 
ggplotRegression <- function (chinaTempModel) {
  require(ggplot2)
  ggplot(chinaTempModel$model, aes_string(x = names(chinaTempModel$model)[2], y = names(chinaTempModel$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(chinaTempModel)$adj.r.squared, 5),
                       "Intercept =",signif(chinaTempModel$coef[[1]],5 ),
                       " Slope =",signif(chinaTempModel$coef[[2]], 5),
                       " P =",signif(summary(chinaTempModel)$coef[2,4], 5)))
}
ggplotRegression(chinaTempModel)


# linear regression function
myLinearReg <- function(startY, endY) {
  chinaTemp <- GlobalLandTemperaturesByCountry %>%
    filter(Country=="China") %>% 
    filter(Year >= startY) %>%
    filter(Year <= endY) %>%
    group_by(Year) %>% 
    summarise(Temperature = mean(AverageTemperature))
  
  chinaTempModel <- lm(Temperature ~ Year, data=chinaTemp)
  coeffs = coefficients(chinaTempModel)
  coeffs 
  ggplotRegression <- function (chinaTempModel) {
    require(ggplot2)
    ggplot(chinaTempModel$model, aes_string(x = names(chinaTempModel$model)[2], y = names(chinaTempModel$model)[1])) + 
      geom_point() +
      stat_smooth(method = "lm", col = "red") +
      labs(title = paste("Adj R2 = ",signif(summary(chinaTempModel)$adj.r.squared, 5),
                         "Intercept =",signif(chinaTempModel$coef[[1]],5 ),
                         " Slope =",signif(chinaTempModel$coef[[2]], 5),
                         " P =",signif(summary(chinaTempModel)$coef[2,4], 5)))
  }
  ggplotRegression(chinaTempModel)
}

myLinearReg(1830, 1850)
myLinearReg(1850, 1870)
myLinearReg(1870, 1890)
myLinearReg(1890, 1910)
myLinearReg(1910, 1930)
myLinearReg(1930, 1950)
myLinearReg(1950, 1970)
myLinearReg(1970, 1990)
myLinearReg(1990, 2010)

myLinearReg(1970, 1980)
myLinearReg(1980, 1990)
myLinearReg(1990, 2000)
myLinearReg(2000, 2010)

myLinearReg(1970, 2010)


myLinearReg(1830, 1890)
myLinearReg(1890, 1950)
myLinearReg(1950, 2010)



NinePiecesYears <- c("1830-1850", "1850-1870", "1870-1890", "1890-1910", "1910-1930", "1930-1950", "1950-1970", "1970-1990", "1990-2010")
NinePiecesSlope <- c(0.0046612, 0.018705, 0.011283, -0.0052818, 0.023137, 0.026878, -0.0012107, 0.022951, 0.033867)

NinePiecesSlopeTable <- data.frame(NinePiecesYears, NinePiecesSlope)
plot(NinePiecesSlope)

index <- c(1, 2, 3)
ThreePiecesYears <- c("1830-1890", "1890-1950", "1950-2010")
ThreePiecesSlope <- c(0.0055385, 0.012088, 0.018993)
ThreePiecesIntercept <- c(-4.117, -16.675, -30.542)
ThreePiecesTable <- data.frame(index, ThreePiecesYears, ThreePiecesSlope,ThreePiecesIntercept)

plot(index, ThreePiecesSlope, title(main = "Three Piecewise Model of Slope"), xlim = c(0.5, 5), ylim = c(0,0.05))
slopeModel <- lm(index ~ ThreePiecesSlope)
summary(slopeModel)
abline(slopeModel)


plot(index, ThreePiecesIntercept, title(main = "Three Piecewise Model of Intercept"))
InterceptModel <- lm(index ~ ThreePiecesIntercept, data = ThreePiecesSlopeTable)
summary(InterceptModel)
abline(InterceptModel)

test <- data.frame(index, ThreePiecesSlope)
p <- predict(lm(ThreePiecesSlope ~ index), new = test, se.fit = TRUE)
p
# -0.001248 + 0.006727 * 4




NinePiecesYears <- c("1830-1850", "1850-1870", "1870-1890", "1890-1910", "1910-1930", "1930-1950", "1950-1970", "1970-1990", "1990-2010")
NinePiecesSlope <- c(0.0046612, 0.018705, 0.011283, -0.0052818, 0.023137, 0.026878, -0.0012107, 0.022951, 0.033867)

NinePiecesSlopeTable <- data.frame(NinePiecesYears, NinePiecesSlope)
plot(NinePiecesSlope)

index <- c(1, 2, 3)
ThreePiecesYears <- c("1830-1890", "1890-1950", "1950-2010")
ThreePiecesSlope <- c(0.0055385, 0.012088, 0.018993)
ThreePiecesSlopeTable <- data.frame(index, ThreePiecesYears, ThreePiecesSlope)
plot(index, ThreePiecesSlope, title(main = "Three Piecewise Model of Slope"), 
     ylab = "Slopes", xlab = "Years",xlim = c(0.5, 4.5), ylim = c(0,0.03), xaxt = "n")
model <- lm(ThreePiecesSlope ~ index)
summary(model)
abline(model)
#points(4,0.02566,cex=2,pch=6,col="blue")
list = c("0.0055385","0.012088", "0.018993")
text(index, ThreePiecesSlope, list, cex= 0.7, pos = 3)
#text(abs_losses, percent_losses, labels=namebank, cex= 0.7)
axis(1, at=1:3, labels=ThreePiecesYears)


index <- c(1, 2, 3, 4)
ThreePiecesYears <- c("1830-1890", "1890-1950", "1950-2010", "2010-2070")
ThreePiecesSlope <- c(0.0055385, 0.012088, 0.018993, 0.02566)
ThreePiecesSlopeTable <- data.frame(index, ThreePiecesYears, ThreePiecesSlope)
plot(index, ThreePiecesSlope, title(main = "Three Piecewise Model of Slope"), 
     ylab = "Slopes", xlab = "Years",xlim = c(0.5, 4.5), ylim = c(0,0.03), xaxt = "n")
model <- lm(ThreePiecesSlope ~ index)
abline(model)
points(4,0.02566,cex=2,pch=6,col="blue")
list = c("0.0055385","0.012088", "0.018993", "0.02566")
text(index, ThreePiecesSlope, list, cex= 0.7, pos = 3)
#text(abs_losses, percent_losses, labels=namebank, cex= 0.7)
axis(1, at=1:4, labels=ThreePiecesYears)

index <- c(1, 2, 3)
ThreePiecesYears <- c("1830-1890", "1890-1950", "1950-2010")
ThreePiecesIntercept <- c(-4.117, -16.675, -30.542)
ThreePiecesInterceptTable <- data.frame(index, ThreePiecesYears, ThreePiecesIntercept)
plot(index, ThreePiecesIntercept, title(main = "Three Piecewise Model of Intercept"), 
     ylab = "Intercept", xlab = "Years",xlim = c(0.5, 4.5), ylim = c(-56, 0), xaxt = "n")
modelIntercept <- lm(ThreePiecesIntercept ~ index)
abline(modelIntercept)
summary(modelIntercept)
points(4,-43.5363,cex=2,pch=6,col="blue")
list = c(-4.117, -16.675, -30.542)
text(index, ThreePiecesSlope, list, cex= 0.7, pos = 4)

axis(1, at=1:3, labels=ThreePiecesYears)


index <- c(1, 2, 3, 4)
ThreePiecesYears <- c("1830-1890", "1890-1950", "1950-2010", "2010-2070")
ThreePiecesIntercept <- c(-4.117, -16.675, -30.542, -43.5363)
ThreePiecesInterceptTable <- data.frame(index, ThreePiecesYears, ThreePiecesIntercept)
plot(index, ThreePiecesIntercept, title(main = "Three Piecewise Model of Intercept"), 
     ylab = "Intercept", xlab = "Years",xlim = c(0.5, 4.5), ylim = c(-56, 0), xaxt = "n")
modelIntercept <- lm(ThreePiecesIntercept ~ index)
abline(modelIntercept)
summary(modelIntercept)
points(4,-43.5363,cex=2,pch=6,col="blue")
list = c(-4.117, -16.675, -30.542, -43.5363)
text(index, ThreePiecesSlope, list, cex= 0.7, pos = 4)

axis(1, at=1:4, labels=ThreePiecesYears)




years <- pretty(2010:2070, n = 10)
yearsTemp <- 0.02566*years-43.5363
plot(years, yearsTemp, xlim = c(2010, 2070), ylim = c(7.9, 9.8), type = "l", ylab = "Temperature", main = "Model of Temperature Prediction 2010-2070")

aYear <- 2050
0.02566*aYear-43.5363




test <- data.frame(index, ThreePiecesSlope)
p <- predict(lm(ThreePiecesSlope ~ index), new = test, se.fit = TRUE)
p
# -0.001248 + 0.006727 * 4
