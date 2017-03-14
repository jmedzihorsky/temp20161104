#   Merged GOP debates CATA
#   Juraj Medzihorsky
#   2017-03-14

library(austin)
library(ca)
library(tm)


load('debates.RData')

ls()

str(deb)

nchar(deb)

#   same, rounded to thousands
round(nchar(deb)*1e-3)


#
reader_options <- list(reader=readPlain,
					   language="en",
					   load=FALSE)

filter_options <- list(tolower,
					   removeNumbers=TRUE,
					   removePunctuation=TRUE,
					   stopwords=TRUE,
					   stemming=TRUE)


M <- TermDocumentMatrix(x=Corpus(VectorSource(deb),
								 readerControl=reader_options),
					   	control=filter_options)


str(M)


#	only those used by at least two

U <- trim(as.wfm(M), min.count=2, min.doc=2)

dim(U)


#   name rows and columns in U
colnames(U) <- colnames(M) <- names(deb)

U['benghazi', ]


#	Correspondence Analysis

d <- ca(U, nd=2)
plot(d)



#   jpeg('candidates.jpg', quality=1e2)
par(mar=c(5,5,3,3), xpd=T)
plot(d$colcoord[,1], d$colcoord[,2], asp=1, type='n',
     xlab='First Dimension', ylab='Second Dimension',
     main='Pooled GOP Debates 1-3, Candidates', 
     yaxt='n', frame=F)
axis(2, axTicks(2), axTicks(2), las=2)
text(d$colcoord[,1], d$colcoord[,2], colnames(U),
     col=rgb(0.5,0,0.0,0.75), cex=1.0)
#   dev.off()



simi <- c('citi', 'citizen', 'citizenship', 'border', 'mexican', 'mexico',
          'immigr','spanish', 'wall', 'fence', 'illeg', 'amnesti' )
#   jpeg('stems_imi.jpg', width=8e2, height=8e2, quality=1e2)
par(mar=c(4,4,2,2), xpd=T)
plot(d$rowcoord[,1], d$rowcoord[,2], asp=1, type='n',
     xlab='First Dimension', ylab='Second Dimension',
     main='Pooled GOP  Debates 1-3, Selected Stems Highlighted', 
     frame=T)
text(d$rowcoord[,1], d$rowcoord[,2], rownames(U), cex=1.00,
     col=ifelse(rownames(U)%in%simi, rgb(0.5,0,0.0), rgb(0.5,0.5,0.5,0.15)))
#   dev.off()


srel <- c('family', 'god', 'vaccin', 'religi', 'christi', 'christian',
          'autism', 'abort', 'gay', 'unborn', 'famili', 'marri', 'marriag',
          'parent', 'parenthood', 'marijuana', 'kid', 'gun', 'conserv')
#   jpeg('stems_rel.jpg', width=8e2, height=8e2, quality=1e2)
par(mar=c(4,4,2,2), xpd=T)
plot(d$rowcoord[,1], d$rowcoord[,2], asp=1, type='n',
     xlab='First Dimension', ylab='Second Dimension',
     main='Pooled GOP  Debates 1-3, Selected Stems Highlighted', 
     frame=T)
text(d$rowcoord[,1], d$rowcoord[,2], rownames(U), cex=1.00,
     col=ifelse(rownames(U)%in%srel, rgb(0.5,0,0.0), rgb(0.5,0.5,0.5,0.15)))
#   dev.off()


sdom <- c('debt', 'social', 'tax', 'dollar', 'economi', 'washington', 'wage',
          'incom', 'spend', 'cut', 'medicar', 'growth', 'corpor', 'trillion',
          'job', 'feder', 'class', 'retir', 'spend', 'spent', 'bank',
          'benefit', 'borrow', 'poverti', 'crime', 'reduc', 'regul', 'payrol',
          'regulatori', 'interest', 'rais', 'senior', 'fed', 'paycheck', 'oil',
          'militari', 'worker', 'lobbyist', 'health', 'compani', 'reform',
          'budget', 'market', 'mainstream', 'medicaid', 'employ', 'insur',
          'bipartisan', 'medic', 'colleg', 'obamacar', 'crimin', 
          'medic', 'climat', 'jail', 'prison', 'gas')
#   jpeg('stems_dom.jpg', width=8e2, height=8e2, quality=1e2)
par(mar=c(4,4,2,2), xpd=T)
plot(d$rowcoord[,1], d$rowcoord[,2], asp=1, type='n',
     xlab='First Dimension', ylab='Second Dimension',
     main='Pooled GOP  Debates 1-3, Selected Stems Highlighted', 
     frame=T)
text(d$rowcoord[,1], d$rowcoord[,2], rownames(U), cex=1.00,
     col=ifelse(rownames(U)%in%sdom, rgb(0.5,0,0.0), rgb(0.5,0.5,0.5,0.15)))
#   dev.off()


sfor <- c('afghanistan', 'benghazi', 'lybia', 'saudi', 'terror', 'terrorist',
          'iranian', 'arab', 'syrian', 'ukrain','japan', 'isi', 'islam',
          'jihadist', 'assad', 'jihad', 'isis', 'iraq', 'putin',
          'syria', 'russia', 'iran', 'chines', 'qaida', 'russian', 'soviet',
          'hezbollah', 'china', 'korea','cuba', 'mexico', 'ayatollah', 'invad',
          'war', 'mexican', 'spanish', 'israel', 'peac', 'secur', 'intervent',
          'europ', 'boot', 'combat', 'command')
#   jpeg('stems_for.jpg', width=8e2, height=8e2, quality=1e2)
par(mar=c(4,4,2,2), xpd=T)
plot(d$rowcoord[,1], d$rowcoord[,2], asp=1, type='n',
     xlab='First Dimension', ylab='Second Dimension',
     main='Pooled GOP  Debates 1-3, Selected Stems', 
     frame=T)
text(d$rowcoord[,1], d$rowcoord[,2], rownames(U), cex=1.00,
     col=ifelse(rownames(U)%in%sfor, rgb(0.5,0,0.0), rgb(0.5,0.5,0.5,0.15)))
#   dev.off()




