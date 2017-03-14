#   Scraping Wikipedia
#   2017-03-14

library(XML)

options(stringsAsFactors=F)


link <- 'https://en.wikipedia.org/wiki/List_of_countries_by_public_debt'


website <- readLines(link)

str(website)

length(website)

website[1111:1122]


tabs <- readHTMLTable(website)

str(tabs)


sapply(tabs, dim)


head(tabs[[1]])
head(tabs[[2]])
head(tabs[[3]])
head(tabs[[4]])
head(tabs[[5]])


good <- tabs[[2]]

head(good)


names(good)


names(good) <- c('country', 
                 'average',
                 'public',
                 'date_public',
                 'govt_gross',
                 'govt_net',
                 'date_govt',
                 'region')

head(good)

apply(good, 2, typeof)


for (i in 2:7) {
    good[,i] <- as.numeric(good[,i])
}

plot(good$public, good$govt_net)


pairs(good[,2:7], pch=16, col=rgb(0,0,1,0.5))
