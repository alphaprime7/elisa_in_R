# import elisa data
# std has just the std expected OD values
# sorted has all the expected OD values
# Our job is using the std ODs to estimate ODs for samples
elisastd <- read.csv(file = 'elisastd.csv', header = T)
elisasorted <- read.csv(file = 'elisasorted.csv', header = T)
head(elisastd,8)

# define attributes in the data (a1= concentration (ng/ml),
# a2 = concentration (ng/ml),od = OD or optical density)

# What should the standard curve look like 
# OD @ ~450 nM (y) vs Conc ng/ml (x)

#isolate standard values only
elisastdonly <- head(elisastd, 7)

# plotting elisa data using plotly
library(plotly)
library(tidyr)
library(plyr)

#plot using plotly
plot_ly(elisastdonly, x = ~conc, y = ~odavg, 
        type = 'scatter', mode = 'line', name = 'standard curve')
plot_ly(elisastdonly, x = ~conc, y = ~od1, 
        type = 'scatter', mode = 'line', name = 'standard curve')
plot_ly(elisastdonly, x = ~conc, y = ~od2, 
        type = 'scatter', mode = 'line', name = 'standard curve')
#plotting the log of concentration for diagnostics
plot(log(elisastdonly$conc), elisastdonly$odavg, pch = 21, bg = 'black')
plot(log(elisastdonly$conc), elisastdonly$od1, pch = 21, bg = 'black')
plot(log(elisastdonly$conc), elisastdonly$od2, pch = 21, bg = 'black')

# Making a simplified linear mode for data
# notice here i am presented with the infinity problem
elisastdonly$logconc <- log(elisastdonly$conc)
plot(elisastdonly$logconc, elisastdonly$odavg, pch = 21, bg = 'black')
elisastdonly1 <- elisastdonly[-c(1),]
# generate a linear model
elisa.lm <- glm(odavg ~ logconc, data = elisastdonly1)
summary(elisa.lm)


## Working on 4pl function in R
#1. import the std data set
#2. I need an approximation of the slope 
#3. 4pl equation
  # y = (a + ( (z-a)/(1 + ((x/c)^m))) )
#4. Functions to test 4pl in R

primes_list <- list(2,3,5,7,11,13)
primes_list <- as.data.frame(primes_list) 
test <- for (i in 1:length(primes_list)) { print(primes_list[[i]])}

elisa.4pl <- function(x, a, z, c, m){
  y = (a + ((z-a)/(1 + ((x/c)^m))))
    return(y)
}
elisa.4pl(x=primes_list,a=1,z=0.1,c=0.5,m=2)

elisa.4ply1 <- function(x, a, z, c, m){
  y <- y = (z + ((a-z)/(1 + ((x/c)^m))))
  return(y)
}

elisa.4plx <- function(y, a, z, c, m){
  x <-  x = (c * ((z-a/y-a) - 1)^(1/m))
  return(x)
}

elisa.4plx1 <- function(y, a, z, c, m){
  x <-  x = (c * ((a-z/y-z) - 1)^(1/m))
  return(x)
}


start.elisa <- c(small.x.asymp = 0.2, inf.x.asymp = 1, inflec = 167, hill = -1)
#package needed for fitting data
library(minpack.lm)
uw.4pl <- nlsLM(a2 ~ M.4pl(conc, small.x.asymp, inf.x.asymp, inflec, hill), 
                     data = elisa,
                     start = start.elisa)
summary(uw.4pl)
library(MSwM)
library(gtools)
#working out problems here
plotDiag.nls <- function(nlsLM.model, title.top)
plotDiag.nls(uw.4pl, "Unweighted 4PL calibration vurve for kim elisa: uw.4pl")
#wrapper example
mean_noNA <- function(x) {
    return(mean(x, na.rm = T))
}
#wrapper function
plotDiag.nls <- function(nlsLM.model, title.top){
par(mfcol=c(1, 2), oma = c(0.5, 0.5, 2, 0))
#adapted from Brandon Greenwell's investr functions
data <- eval(nlsLM.model$data)
    x.names <- intersect(all.vars(formula(nlsLM.model)[[3]]), colnames(data))
    y.names <- all.vars(formula(nlsLM.model)[[2]])
    x <- data[, x.names]  # extract predictor columns
    x.nz.min <- min(x[x!=0])
    # Display purposes, we cheat a little to get the zero calibrators included on the
    # log(x) plot
    x.fix <- ifelse(x <= 0, x.nz.min/5, x)
    break.x <- x.nz.min/4
    y <- data[, y.names]  # extract response columns
    # Plot data and fitted curve
    plot(x.fix, y, log = "x", main = "data and fitted curve", pch = 20,
         ylab = "Response", xlab = "log(Concentration)", font.main = 3)
    grid()
    curve(M.4pl(x, coef(nlsLM.model)[[1]], coef(nlsLM.model)[[2]], 
                coef(nlsLM.model)[[3]], coef(nlsLM.model)[[4]]), add = T)
    # Technically, we should not include the zero-calibrators on a log plot, but it's nice
    # to have for visualizing the results. This line inserts a break in the x-axis as in
    # Dudley et al (1985)
    axis.break(1, break.x, brw = 0.05)
# Plot standardised weighted residuals
    # [add ifelse condition for weighted and unweighted models (title)]
    std.w.resid <- summary(nlsLM.model)$resid/sd(summary(nlsLM.model)$resid)
    plot(predict(nlsLM.model), std.w.resid, ylab = "std residuals (in SDs)", 
         xlab = "fitted response values", pch = 20, 
         main = "standardized residuals", font.main = 3)
    # Horizontal lines at y=0 and +/- 2SD
    abline(h = 0, lty = 3, col = "red")
    abline(h = 2, lty = 3)
    abline(h = -2, lty = 3)
    title(main = title.top, outer = TRUE)
    par(mfcol=c(1, 1))
}
#NEW CODE
ibrary("drc")
#####ELISA TEST
rm(list=ls())
library (drc)
###coppy data for standards from excel (header - OD, conc)
stdcrvdata <- read.table(pipe("pbpaste"), sep="\t", header=T)# in windows use pasle function
stdcrvdata$logconc <-log10(stdcrvdata$conc)# log10 from conc
 plot(stdcrvdata$logconc, stdcrvdata$OD, main="log standard curve", xlab="x=log(conc)", ylab="y=OD")
fit<-drm(formula =   OD ~ logconc , data = stdcrvdata, fct = LL.4())
x <- seq(.5,4.5, length=100)# range from logconc
y <- (fit$coefficients[2]+ (fit$coefficients[3]- fit$coefficients[2])/(1+(x/fit$coefficients[4])^ fit$coefficients[1]))# from OD ~ d + (a - d)/(1 + (logconc/cc)^b)
lines(x,y, lty="dotted", col="red")
samples <- read.table(pipe("pbpaste"), sep="\t", header=T)# data from mesurments from excel with header OD
 samples$loganswer<- fit$coefficients[4]*(((-1* fit$coefficients[3]+samples$OD)/( fit$coefficients[2]-samples$OD))^(1/ fit$coefficients[1]))
samples$conc <- 10^samples$loganswer
write.table(samples,file="mydata.csv",sep=";")
lines(samples$loganswer,samples$OD, type="points", col="blue")
#ELISA analysis in R
library(drc)
model1 <- drm(od~conc, fct=LL.4(names=c("Slope", "Lower", "Upper", "ED50")), data=elisa)
plot(model1)
plot(model1)
dosex<-ED(model1,elisa$od, type='absolute',display = F)
points(y=elisa$od,x=dosex[,1],col='blue',pch=19,cex=2)
arrows(dosex[,1],elisa$od,dosex[,1]+dosex[,2]*1.96,elisa$od,length=0.1,angle=90,lwd=3,col='blue')
arrows(dosex[,1],elisa$od,dosex[,1]-dosex[,2]*1.96,elisa$od,length=0.1,angle=90,lwd=3,col='blue')
#4pl
drm(formula = Response ~ Expected, data = SC, fct = LL.4())
summary(FourP)
approxfun() 
predict()
#inverse predict
#Now I wish to inverse predict the x for y=.75, say.  Optimize works  
Data
#x	y
#> 0	1.298
#> 2	0.605
#> 3	0.507
#> 4	0.399
#> 5	0.281
#> 6	0.203
#> 7	0.150
#> 8	0.101
p.1=lm(y~bs(x,df=4))
pred.Spl <- predict(Sp.1, data.frame(x=seq(0,8, by=0.01)  ) )
approxfun(x=pred.Spl, y=seq(0,8, by=0.01) )(0.75)
#ELISA NOTES
x <- c(478, 525, 580,  650,  700,  720,  780,  825,  850,  900,  930,  980, 1020, 1040, 1050, 1075, 1081, 1100, 1160, 1180, 1200)
y <- c(1.70, 1.45, 1.50, 1.42, 1.39, 1.90, 2.49, 2.21, 2.57, 2.90, 3.55, 3.80, 4.27, 4.10, 4.60, 4.42, 4.30, 4.52, 4.40, 4.50, 4.15)

M.4pl <- function(x, lower.asymp, upper.asymp, inflec, hill){
    f <- lower.asymp + ((upper.asymp - lower.asymp)/
                            (1 + (x / inflec)^-hill))
    return(f)
}

require(minpack.lm)
nlslmfit = nlsLM(y ~ M.4pl(x, lower.asymp, upper.asymp, inflec, hill),
                 data = data.frame(x=x, y=y),
                 start = c(lower.asymp=min(y)+1E-10, upper.asymp=max(y)-1E-10, inflec=mean(x), hill=1),
                 control = nls.control(maxiter=1000, warnOnly=TRUE) )
summary(nlslmfit)
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#   lower.asymp   1.5371     0.1080   14.24 7.06e-11 ***
#   upper.asymp   4.5508     0.1497   30.40 2.93e-16 ***
#   inflec      889.1543    14.0924   63.09  < 2e-16 ***
#   hill         13.1717     2.5475    5.17 7.68e-05 ***
require(investr)
xvals=seq(min(x),max(x),length.out=100)
require(scam)
nknots = 20 # desired nr of knots
fit = scam(y~s(conc,k=nknots,bs="mpi",m=2), family=gaussian, data=data)
predintervals = data.frame(x=xvals,predFit(nlslmfit, newdata=data.frame(x=xvals), interval="prediction"))
confintervals = data.frame(x=xvals,predFit(nlslmfit, newdata=data.frame(x=xvals), interval="confidence"))
require(ggplot2)
qplot(data=predintervals, x=x, y=fit, ymin=lwr, ymax=upr, geom="ribbon", fill=I("red"), alpha=I(0.2)) +
    geom_ribbon(data=confintervals, aes(x=x, ymin=lwr, ymax=upr), fill=I("blue"), alpha=I(0.2)) +
    geom_line(data=confintervals, aes(x=x, y=fit), colour=I("blue"), lwd=2) +
    geom_point(data=data.frame(x=x,y=y), aes(x=x, y=y, ymin=NULL, ymax=NULL), size=5, col="blue") +
    ylab("y")