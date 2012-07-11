# R script
# read in some stuff (words per line)

#a <- load('/Users/lx/hack/ec_wordcloud/a.RObj')
a <- read.delim('/Users/lx/hack/ec_wordcloud/a', header=FALSE, sep='\n')
b <- sort(table(a), decreasing=TRUE)
b <- b[b>20]
library(RColorBrewer)
library(wordcloud)

pdf("/Users/lx/hack/ec_wordcloud/words.pdf", width=10, height=10)
col2 <- rainbow(24)
wordcloud(names(b), b, scale=c(9,.1), max.words=Inf, random.order=F, rot.per=.3, colors=col2)
dev.off()

png("/Users/lx/hack/ec_wordcloud/words.png", width=1000, height=1000)
pal2 <- brewer.pal(8, "Set2")
wordcloud(names(b), b, scale=c(9,.1), max.words=Inf, random.order=F, rot.per=.3, colors=pal2)
dev.off()

# svg(filename="/Users/lx/hack/ec_wordcloud/words.svg", width=10, height=10, bg="white")
# # pal2 <- brewer.pal(8, "Set2")
# wordcloud(names(b), b, scale=c(9,.1), max.words=Inf, random.order=F, rot.per=.3, colors=pal2)
# dev.off()

