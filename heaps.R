
#Heaps from a Zipf's distribution

a <- 1.5

c <- 0

for( i in 1:10000){
  c = c +(i+1)^(-a)
}

p <- seq(1:10000)^(-a)/c


size <- seq(1000,100000, by =1000)
uwords <- list()

for( i in 1:length(size)){
  
ranks <- as.data.frame(table(sample(x = 1:10000, size = size[i],replace=TRUE, prob = p)))
#colnames(ranks) = c("rank", "freq")
#ranks$rank <- as.numeric(ranks$rank)
#ranks$freq <- as.numeric(ranks$freq)
uwords[i] <- dim(ranks)[1]
}

plot(uwords,size, log = "xy")

log.uwords <- unlist(log(as.numeric(uwords)))
log.size <- unlist(log(as.numeric(size)))
l.mod <- lm(log.size ~ log.uwords)
l.mod

#-----------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------
#Heaps from random text

library(tidyverse)
library(gutenbergr)
library(tidytext)

alphabet <- list("q","w","e","r","t","y","u","i","o","p","a","s","d","f","g",
                 "h","j","k","l","z","x","c","v","b","n","m")


df <-  data.frame(matrix(vector(), 0, 1,
                         dimnames=list(c(), c("text"))),
                  stringsAsFactors=F)

heaps <-  data.frame(matrix(vector(), 0, 2,
                         dimnames=list(c(), c("N","V"))),
                  stringsAsFactors=F)

size <- seq(1000,300000, by= 10000)

for(j in 1:length(size)){
rand.text <- ""

for(i in 1:size[j]){
  if(runif(1,0,1) < 0.5){
    rand.letter = alphabet[sample(1:26, 1)]
    rand.text <- paste(rand.text,rand.letter, sep = "", collapse = NULL)
  }
  else{
    rand.text <- paste(rand.text," ", sep = "", collapse = NULL)
  }
}

df[j,1] <- rand.text

total.words <- df%>% 
  unnest_tokens(word, text)

unique.words <- df%>% 
  unnest_tokens(word, text)%>% 
  count(word)

heaps[j,1] <- dim(total.words)[1]
heaps[j,2] <- dim(unique.words)[1]

}

plot(heaps$V, heaps$N, log="xy")

l.mod <- lm(log(heaps$N) ~ log(heaps$V))

write.csv(df, file = "random_text", row.names = FALSE)
write.csv(heaps, file = "random_heaps", row.names = FALSE)
