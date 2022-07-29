library(httr)
library(RCurl)
library(ggplot2)
library(rhandsontable)


bodytextpost <- "\"email=ruser@assaycloud.com&key=free&xdata=1;2;3;4;5&ydata=1.0;3.5;5.0;6.0;6.5&yknown=4\""
bodytextget <- "?email=ruser@assaycloud.com&key=free&xdata=1;2;3;4;5&ydata=1.0;3.5;5.0;6.0;6.5&yknown=4"


serverget <- "https://service1.assayfit.com/service.svc/http/assayfitgetcsv"
serverpost <- "https://service1.assayfit.com/service.svc/http/assayfitpostcsv"


# get operation

getdata <- GET(paste( serverget, bodytextget, sep = ""), verbose())
gettext <- content(getdata, "text")
gettext
dfget = read.csv(text = readLines(textConnection(gettext)), sep = ";", header = FALSE)

#post operation

r = dynCurlReader()

curlPerform(postfields = bodytextpost, httpheader="Content-type:application/json", url = serverpost, verbose = TRUE,
            post = 1L, writefunction = r$update, encoding = "UTF-8")
dfpost = read.csv(text = readLines(textConnection(r$value())), sep = ";", header = FALSE)


names(dfpost) <-c("xdata",	"ydata",	"weights",	"percent",	"yfitted", 	"resid",  "paraminfo",	"param",	"yknown",	"xfromyknown",	"xknown",	"yfromxknown",	"xcurve",	"ycurve",	"infotitle", "info")

dfpost

rhandsontable(dfpost, width = 1200, height = 600)
p1 <- ggplot(dfpost, aes( xdata, ydata)) + geom_point( colour="black", pch = 3) 
p1 <- p1 + geom_line(data=dfpost, aes(xcurve, ycurve), colour="orange")
p1 <- p1 + geom_point(data=dfpost, aes(xfromyknown, yknown), colour="green")
p1

