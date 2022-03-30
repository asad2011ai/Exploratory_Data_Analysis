# upload csv file
pollution <- read.csv('ad_viz_plotval_data.csv')
# show data
head(pollution)
# 
str(pollution)
# slice the file show all rows and the following column
pollution <- pollution[,c(-3,-9:-12)]
# summary of one column
summary(pollution$Daily.Mean.PM2.5.Concentration)

# draw boxplot
boxplot(pollution$Daily.Mean.PM2.5.Concentration, col = "blue")

#--------------------------------------------
# draw histogram
hist(pollution$Daily.Mean.PM2.5.Concentration, col = 'green')
# show data intensity
rug(pollution$Daily.Mean.PM2.5.Concentration)

# if increase bins 
hist(pollution$Daily.Mean.PM2.5.Concentration, col = 'green',breaks = 100)
rug(pollution$Daily.Mean.PM2.5.Concentration)

#----------------------------------------------------------
# draw bar plot
barplot(table(pollution$COUNTY), col = 'wheat', main = 'records per county in the sate')

#------------------------------------
#overlaying Feature
#boxplot
boxplot(pollution$Daily.Mean.PM2.5.Concentration, col = "blue")
abline(h=12)

#histogram
hist(pollution$Daily.Mean.PM2.5.Concentration, col = 'green')
abline(v=12, lwd=2)
abline(v=median(pollution$POC), col = "magenta", lwd=4)

# multiple box plot
boxplot(Daily.Mean.PM2.5.Concentration ~ COUNTY, data= pollution, col= 'red')
abline(h=15)

#-------------------------------------------------
str(pollution)
# scatter plot
par(mfrow = c(2,1), mar = c(4,4,2,1))
pollution$Date = as.Date(pollution$Date, "%m%d%Y")
with(pollution, plot(Date, Daily.Mean.PM2.5.Concentration))
abline(h=12, lwd= 2, lty=2 )
plot(pollution$Date,pollution$Daily.Mean.PM2.5.Concentration)

#---------------------------------------------------------------------
# base plot system
library(datasets)
data("cars")
with(cars, plot(speed, dist))
#-----------------------------------------
# lattice plotting system
library(lattice)
state <- data.frame(state.x77, region= state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))

#--------------------------------------------------------------------
# the ggplot2 system
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

#--------------------------------------------------------
#base graphics
library(datasets)
hist(airquality$Ozone)
# annotation point and color
with(airquality, plot(Wind, Ozone, pch = 25))
with(airquality, plot(Wind, Ozone, pch = 25, col = 'green'))
#Boxplot
airquality <- transform(airquality, Month= factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

# some important parameter of base graph

par("lty")
par("col")
par("pch")
par("bg")
par('mar')
par('mfrow')

# base plotting annotation function
#add aditional information to the graph
with(airquality, plot(Wind, Ozone))
title(main = "ozone and wind in new york")
airquality <- transform(airquality, Month= factor(Month))

# show subdataset with different color
with(airquality, plot(Wind, Ozone, main = 'ozone and wind chart'))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = 'blue'))

# 
with(airquality, plot(Wind, Ozone, main = 'ozone and wind chart', type = 'n'))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = 'blue'))
with(subset(airquality, Month !=5), points(Wind, Ozone, col = 'red'))
legend("topright", pch = 1, col = c ("blue", "red"), legend = c("may", 'other months'))

# base plot with regression
with(airquality, plot(Wind, Ozone, main = 'ozone and wind chart', pch= 20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd=2)
model

# multiple base graph
par(mfrow = c(1,2))
with(airquality, {
  plot(Wind, Ozone, main = "ozone and wind", col = 'blue')
  plot(Solar.R, Ozone, main = "Ozone and solar radiation")
})

par(mfrow= c(1,3), mar = c(4,4,2,1), oma = c(0,0,2,0))
with(airquality, {
  plot(Wind, Ozone, main = "ozone and wind", col = 'blue')
  plot(Solar.R, Ozone, main = "Ozone and solar radiation")
  plot(Temp, Ozone, main = 'ozone and temprature')
  mtext("ozone and weather in newyork", outer = TRUE)
})
#---------------------------------------------------------------------
# graph with latice
#xyplot(y ~ x | f*g, data)
library(lattice)
library(datasets)
#simple scatter plot
xyplot(Ozone ~ Wind, data = airquality)

airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c (5,1))
# lattice behaviour
p <- xyplot(Ozone ~ Wind, data = airquality)
print(p)

# lattice panel function
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each=50)
y <- x + f*x + rnorm(100 , sd =0.5)
f <- factor(f, labels = c("Group 1","Group 2"))
xyplot(y ~ x | f , layout = c(2,1)) # plot with two panel

# lattice panel function
# custom panel function
xyplot(y ~ x | f, panel = function(x, y, ...){
  panel.xyplot(x,y, ...) # 1st call
  panel.abline(h= median(y), lty = 2) # 2nd call
})

# linera model
xyplot(y ~ x | f, panel = function(x, y, ...){
  panel.xyplot(x,y, ...) # 1st call
  panel.lmline(x,y, lty = 2, col = 2) # 2nd call
})
#---------------------------------------------------------------------
# gbplot
library(ggplot2)
str(mpg)

# scatter plot
qplot(displ, hwy, data = mpg)

#modifying aesthetics
qplot(displ, hwy, data = mpg, color = drv)

# adding geometry
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))
qplot(displ, hwy, data = mpg, geom = c("line", "smooth"))
#---------------------
# histogram
qplot(hwy, data = mpg, fill = drv)

# facets scatter/histogram
qplot(displ, hwy, data = mpg, facets = .~drv)
# with variation
qplot(displ, hwy, data = mpg, facets = drv~.)

qplot(displ, hwy, data = mpg, facets = drv~fl)
qplot(displ, hwy, data = mpg, facets = fl~drv)

# histogram facets
qplot(hwy, data = mpg, facets = drv~., binwidth = 2)
qplot(hwy, data = mpg, facets = drv~fl, binwidth = 2)

#----------------
#maacs plot example
maacs <- load(file = 'maacs-2.rda')
load(file = "maacs.rda")
str(maacs)

# histogram

library(ggplot2)
qplot(log(eno), data = maacs)
qplot(log(eno), data = maacs, fill = mopos)
 # density graph

qplot(log(eno), data = maacs, geom = "density", color = mopos)
# scatter plot
qplot(log(pm25), log(eno), data = maacs)
# shape
qplot(log(pm25), log(eno), data = maacs, shape = mopos)
# color
qplot(log(pm25), log(eno), data = maacs, color = mopos)
# 
qplot(log(pm25), log(eno), data = maacs, color = mopos, geom = c("point", "smooth"), method = "lm")
#
qplot(log(pm25), log(eno), data = maacs, geom = c("point","smooth"), method = "lm", facets = .~mopos)
#
#--------------------------------------------------------
# Basic component of ggplot2 
# building plot with ggplot(plot are build in layer)
# basic q plot
maacs_2 <- read.csv('bmi_pm25_no2_sim.csv', header = TRUE)
head(maacs_2)
qplot(logpm25, NocturnalSympt, data = maacs_2, facets = .~ bmicat, geom = c("point", "smooth"), method = "lm")

# building up in layers ggplot

g <- ggplot(maacs_2, aes(logpm25, NocturnalSympt))
summary(g)

print(g)
p <- g+geom_point()
print(p)

g + geom_point()

# first plot with point layer
g <- ggplot(maacs_2, aes(logpm25, NocturnalSympt))
g + geom_point() 
g + geom_point() + geom_smooth()
g + geom_point() + geom_smooth(method = "lm")
g + geom_point() + facet_grid(.~ bmicat) + geom_smooth(method = "lm")

#-----------------------------------
# how to add annotation
g + geom_point(color = "steelblue", size = 4, alpha = 1/2)

# define color on variable base
g + geom_point(aes(color = bmicat), size = 4, alpha = 1/2)

# modifying labels
g + geom_point(aes(color = bmicat)) + labs(title = "MAACS Cohort")+
  labs(x = expression("log"* PM[2.5]), y ="Noctural Symptoms")

# customizing the smooth
g + geom_point(aes(color = bmicat), size = 2, alpha = 1/2) +
  geom_smooth(size = 9, linetype = 3, method = "lm", se = FALSE)

# changing the theme
g + geom_point(aes(color = bmicat)) +
  theme_bw(base_family = "Times")
#-----------------------------------------------------

# a note about axis limits
testdat <- data.frame(x = 1:100, y= rnorm(100))
testdat[50,2] <- 100
plot(testdat$x, testdat$y, type = "l", ylim =  c(-3,3))

# ggplot2 axis limit
g <- ggplot(testdat, aes(x = x, y = y))
g + geom_line()
# axis limit
g + geom_line() + ylim(-3,3)
# is outlier exist
g + geom_line() + coord_cartesian(ylim = c(-3,3))

# example
str(maacs_2)
# calculate tertile use quantile functipn
cutpoints <- quantile(maacs_2$logno2_new, seq(0,1, length = 4), na.rm = TRUE)
cutpoints
# divide the orignal logno2_new into the range
maacs_2$no2tret <- cut(maacs_2$logno2_new, cutpoints)
str(maacs_2)
# check range 
levels(maacs_2$no2tret)

# create plot
g <- ggplot(maacs_2, aes(logpm25, NocturnalSympt))

# add layers
g + geom_point(alpha = 1/3)+
  facet_wrap(bmicat ~ no2tret, nrow = 2, ncol = 4)+
  geom_smooth(method = "lm", se = FALSE, col = "steelblue")+
  theme_bw(base_family = "Avenir", base_size = 10)+
  labs(x = expression("log"*PM[2.5]))+
  labs(y= "Nocturnal Symptoms")+
  labs(title = "MAACS Cohort")

#--------------------------------------------
