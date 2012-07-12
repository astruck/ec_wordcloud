library(RCurl) # requires libcurl 7.12+ and install.packages('bitops')
library(XML)
library(snippets) # pull from R-forge
library(tm)
library(RColorBrewer)
library(wordcloud)

# output location
out.path <- '/Users/lx/hack/ec_wordcloud/'
# how many words should be in the text to be displayed?
min.freq <- 10
# set server parameter
url <- 'http://192.168.56.15/redmine/'
# what should be queried
q <- 'issues/2.xml'
# get us some XML
xobj <- xmlTreeParse(getURL(paste(url, q, sep='')), useInternal=T)
# atm we only search the description
desc <- getNodeSet(xobj, '//description')
# strip xml tags
dsc <- sapply(desc, function(x) { xmlValue(x) } )
# have single words
words <- tolower(unlist(lapply(dsc, function(x) strsplit(x, ' '))))
# remove parentheses, comma, [semi-]colon, period, quotation marks
words <- words[-grep("[\\)\\(,;:\\.\\'\\\"]", words)]
# remove English stopwords
words <- words[!words %in% stopwords()]
# frequency
wt <- sort(table(words), decreasing=TRUE)
# minimum frequency
wt <- wt[wt > min.freq]
# create wordcloud
pdf(paste(out.path, 'redmine_issues_desc_wordcloud.pdf', sep=''), width=10, height=10)
pal2 <- brewer.pal(8, 'Set2')
wordcloud(names(wt), wt, scale=c(9,.1), max.words=Inf, random.order=F, rot.per=.3, colors=pal2)
dev.off()
# create snippet
pdf(paste(out.path, 'redmine_issues_desc_snipcloud.pdf', sep=''), width=10, height=5)
cloud(wt, col = col.br(wt, fit=TRUE))
dev.off()
