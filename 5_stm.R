#   Merged GOP debates CATA
#   Juraj Medzihorsky
#   2017-03-14

library(stm)


load('debates.RData')

ls()

str(deb)


#   nchar, rounded to thousands
round(nchar(deb)*1e-3)


names(deb)

background <- c('gov', 'out', 'gov', 'sen', 'gov', 'gov', 'sen', 'sen', 'out')


d <- data.frame(author=names(deb), background=background)

d


str(textProcessor)

p <- textProcessor(deb, metadata=d)
p <- prepDocuments(p$documents, p$vocab, p$meta)



set.seed(19260723)
s3  <- stm(p$documents, p$vocab, K=3, prevalence=~background, data=p$meta) 

s3

summary(s3)


#   let's check the output simplifed, using this custom function
gt <-
    function(x)
    {
        y <- x$theta
        rownames(y) <- rownames(d)
        colnames(y) <- paste('topic', 1:ncol(y))
        return(y)
   }


cbind(d, round(1e2*gt(s3)))


set.seed(19260723)
s4  <- stm(p$documents, p$vocab, K=4, prevalence=~background, data=p$meta) 
s5  <- stm(p$documents, p$vocab, K=5, prevalence=~background, data=p$meta) 


cbind(d, round(1e2*gt(s4)))
cbind(d, round(1e2*gt(s5)))


set.seed(19260723)
l4  <- stm(p$documents, p$vocab, K=4, data=p$meta) 
l5  <- stm(p$documents, p$vocab, K=5, data=p$meta) 

cbind(d, round(1e2*gt(l4)))
cbind(d, round(1e2*gt(l5)))

summary(l4)

