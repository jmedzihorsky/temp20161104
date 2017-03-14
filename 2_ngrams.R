#   Neoliberalism Google Ngrams
#   Juraj Medzihorsky
#   2017-03-14


#   github.com/seancarmody/ngramr

library(devtools)

install_github('seancarmody/ngramr')



library(ngramr)
#library(RColorBrewer)
#library(wesanderson)
options(stringsAsFactors=F)
gr <- (1+sqrt(5))/2


#   the data
#   --------
#   english
#   french
#   german
#   hebrew
#   spanish
#   italian
#   russian

#   wc <- c('新自由主义')
we <- c('neoliberal', 'neoliberalism', 'neo - liberal', 'neo - liberalism')
wf <- c('néolibérale', 'néolibéralisme')
wg <- c('neoliberal', 'neoliberalismus')
#   wh <- c('הניאו-ליברלי', 'ניאו-ליברליזם')
wi <- c('neoliberista', 'neoliberismo')
ws <- c('neoliberal', 'neoliberalismo')
wr <- c('неолиберальная', 'неолиберализм')


#   n <- ngram(wc, year_start=1950, corpus='chi_sim_2012', smoothing=3, count=F)
e <- ngram(we, year_start=1950, corpus='eng_2012', smoothing=3, count=F)
f <- ngram(wf, year_start=1950, corpus='fre_2012', smoothing=3, count=F)
g <- ngram(wg, year_start=1950, corpus='ger_2012', smoothing=3, count=F)
#   h <- ngram(wh, year_start=1950, corpus='heb_2012', smoothing=3, count=F)
i <- ngram(wi, year_start=1950, corpus='ita_2012', smoothing=3, count=F)
s <- ngram(ws, year_start=1950, corpus='spa_2012', smoothing=3, count=F)
r <- ngram(wr, year_start=1950, corpus='rus_2012', smoothing=3, count=F)

head(e)


af <- function(x) aggregate(x$Frequency, by=list(x$Year), FUN=sum) 

d <- data.frame(year=1950:2008, 
                eng=af(e),
                fra=af(f),
                ger=af(g),
                ita=af(i),
                spa=af(s),
                rus=af(r))
d <- d[,-grep('Group', names(d))]
names(d) <- gsub('.x$', '', names(d))


head(d)



cn <- c('English', 'French', 'German', 'Italian', 'Spanish', 'Russian')
kol <-  palette()[1:length(cn)]


yl0 <- c(0, 2.25e-05)
yv <- seq(0, 2, by=1)
xl0 <- c(1950, 2010)
yl <- c(yl0[1]-dist(yl0)*1e-2, yl0[2]+dist(yl0)*1e-2)
xl <- c(xl0[1]-dist(xl0)*1e-2, xl0[2]+dist(xl0)*1e-2)

    cairo_pdf('ngrams.pdf', 8.90, 14.40)
par(mar=c(4,4,2,4), xpd=T, xaxs='i', yaxs='i')
plot(1, 1, xlim=xl, ylim=yl, frame=F,
     xlab='', ylab='', xaxt='n', yaxt='n')
#   lines(c(1950, 2008), rep(yl0[1], 2), lwd=gr^-3, col=grey(0.5))
for(it in 2:7) {
    lines(d[,1], d[,it], lwd=gr^2, col=kol[it-1]) 
    text(d[nrow(d), 1], d[nrow(d), it], cn[it-1], pos=4, col=kol[it-1]) 
}
x0 <- seq(1950, 2010, by=1e1)
x1 <- seq(1950, 2010, by=5e0)
x2 <- seq(1950, 2010, by=1e0)
x1 <- x1[!(x1%in%x0)]
x2 <- x2[!(x2%in%x0)]
x2 <- x2[!(x2%in%x1)]
x0 <- x0[x0<2009]
x1 <- x1[x1<2009]
x2 <- c(1950, x2[x2<2009])
axis(1, x0, x0, tck=-3e-2*gr^-1, lwd.ticks=gr^-1, lwd=0) 
axis(1, x1, rep('', length(x1)), tck=-3e-2*gr^-2, lwd.ticks=gr^-2, lwd=0) 
axis(1, x2, rep('', length(x2)), tck=-3e-2*gr^-3, lwd.ticks=gr^-3, lwd=gr^-1) 
y0 <- yv
y1 <- seq(0, 2, by=0.5)
y1 <- y1[!(y1%in%y0)]
axis(2, y0*1e-5, yv, las=2, lwd.ticks=gr^-1, lwd=gr^-1, tck=-3e-2*gr^-1)
axis(2, y1*1e-5, rep('', length(y1)), las=2, lwd.ticks=gr^-2, lwd=0, tck=-3e-2*gr^-2)
#
text(xl[1], yv[length(yv)]*1e-5, 'Frequency\nin 0.00001', pos=3)
#
text(mean(xl), 2.25e-5, "'Neoliberalism' in Google Book Corpora", cex=gr)
text(1955, -.3e-5+2.300e-5, paste('English:', paste(we, collapse=', ')), cex=gr^-1, pos=4, col=kol[1])
text(1955, -.3e-5+2.275e-5, paste('French:',  paste(wf, collapse=', ')), cex=gr^-1, pos=4, col=kol[2])
text(1955, -.3e-5+2.250e-5, paste('German:',  paste(wg, collapse=', ')), cex=gr^-1, pos=4, col=kol[3])
text(1955, -.3e-5+2.225e-5, paste('Italian:', paste(wi, collapse=', ')), cex=gr^-1, pos=4, col=kol[4])
text(1955, -.3e-5+2.200e-5, paste('Spanish:', paste(ws, collapse=', ')), cex=gr^-1, pos=4, col=kol[5])
text(1955, -.3e-5+2.175e-5, paste('Russian:', paste(wr, collapse=', ')), cex=gr^-1, pos=4, col=kol[6])
#
text(2012, -max(yl)*6e-2, 'Juraj Medzihorsky\ntwitter.com/medzihorsky', cex=gr^-1)
    dev.off()





