#   UNSC Debates over the Syrian Civil War plots
#       Fig B1. basic logistic scaling 
#   Juraj Medzihorsky
#   2017-02-28

library(quanteda)


#   load the original data
load('d20160511.RData')

ls()

str(d)

d[1,]


#   shorten selected state names
d$country[grep('^Russ', d$country)] <- 'Russia'
d$country[grep('^United K', d$country)] <- 'UK'
d$country[grep('^United S', d$country)] <- 'USA'

table(d$country)


#   unique dates
dates <- substr(unique(d$date), 1, 1e1)
dates <- dates[order(dates)]


dates


#   process the data
#   load and process the dictionary
dictionary_text <- readLines('dictionary_20160527.txt')

dictionary_text

dict <- sapply(dictionary_text, function(x) strsplit(x, ': ')[[1]][-1], USE.NAMES=F)
dict <- strsplit(dict, ', ')[-5]
names(dict) <- c('proc', 'act', 'conf', 'crim')

dict



#   create the DFM 
pm <- dfm(d$speech, dictionary=dict, stem=TRUE)

head(pm)


#   a new dataframe, for now all countries
pm <- as.data.frame(as.matrix(pm))
a <- d[, 1:3]
a$wp1 <- pm$proc
a$wp2 <- pm$conf
a$wn1 <- pm$act
a$wn2 <- pm$crim
a$n1 <- pm$proc + pm$act
a$n2 <- pm$conf + pm$crim


head(a)


#  o a function for logistic scaling
logfun <- function(x, y, a=0.5) log((x+a)/(y+a))

#   scale the texts (flip, i.e., multiply by -1)
a$log_cvs <- logfun(x=a$wp2, y=a$wn2)
a$log_avp <- logfun(x=a$wp1, y=a$wn1)

head(a)


#   we're done, now let's plot


#   P5 colors
cnt <- c('China', 'France', 'Russia', 'UK', 'USA') 
ccl <- c('red3', 'blue', 'darkorange2', 'darkorchid3', 'navyblue')
names(ccl) <- cnt
cca <- apply(col2rgb(ccl), 2, function(i) rgb(i[1],i[2],i[3],75,maxColorValue=255)) 


#   CvS
par(mfrow=c(3,2))
plot(0,0,frame=F,type='n',xlab='',ylab='',xaxt='n',yaxt='n')
text(0,0, 'Crime\n vs.\n Struggle', cex=2)
yl <- range(a$log_cvs)
for (i in 1:5) {
    fil <- a$country==cnt[i]
    plot(as.Date(a$date[fil]), a$log_cvs[fil], pch=16, col=ccl[i], main=cnt[i],
         ylim=yl, ylab='', xlab='Date')
}


#   AvP
par(mfrow=c(3,2))
plot(0,0,frame=F,type='n',xlab='',ylab='',xaxt='n',yaxt='n')
text(0,0, 'Action\n vs.\n Procedure', cex=2)
yl <- range(a$log_avp)
for (i in 1:5) {
    fil <- a$country==cnt[i]
    plot(as.Date(a$date[fil]), a$log_avp[fil], pch=16, col=ccl[i], main=cnt[i],
         ylim=yl, ylab='', xlab='Date')
}









