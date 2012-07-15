# R script to read pdf files from directory and does text analysis

# make sure you have the libraries and System requirements installed and loaded
# library(RCurl) # requires libcurl 7.12+ and install.packages('bitops')
# library(XML)
# library(snippets) # pull from R-forge
library(tm)
# you for pdf reads you have to have poppler.freedesktop.org
# plotting tm objects requires Rgraphviz from AT&T which didnt install on Mac OS X Snow Leopard correctly
library(RColorBrewer)
library(wordcloud)
library(Rstem)

# read pdf files from this directory
directory <- '/Users/lx/hack/ec_wordcloud/publications/'
files <- list.files(path = directory)
# output location
out.path <- '/Users/lx/hack/ec_wordcloud/clouds/'
debug = TRUE
# do we want to have subtitles in our wordclouds?
sub.title = TRUE

# if (debug) cat("\n some nasty package, probably snowball, tries to have DBconnecter initialized, ignore at first run and try again\n\n\n")

## FUNCTION stemmed wordcloud
swords.cloud <- function(words, min.freq, out.path, pub.id){
    # from list/corpus object cast into char vector and ignore case
    words <- unlist(strsplit(tolower(words), ' '))
    # count nr of words left
    words.len <- length(words)
    # we only want to see words longer than 2 chars
    words <- words[nchar(words) >2]
    # remove English and German stopwords 
    words <- words[!words %in% stopwords(language='de')]
    words <- words[!words %in% stopwords(language='en')]
    # stem words
    words <- wordStem(words, language='german')
    words <- wordStem(words, language='english')
    # frequency
    wt <- sort(table(words), decreasing=TRUE)
    # minimum frequency
    wt <- wt[wt > min.freq]
    # create wordcloud
    pdf(paste(out.path, 'ec_VIDoc_stemmed_cloud_', pub.id , sep=''), width=10, height=10)
    pal2 <- brewer.pal(8, 'Set2')
    wordcloud(names(wt), wt, scale=c(9,.1), max.words=Inf, random.order=F, rot.per=.3, colors=pal2)
    if (sub.title) title(sub = list(paste("# stemmed file =",pub.id, "# nr words = ", length(wt),"/", words.len, "# char > 2 # min.freq >", min.freq, "#",sep=" "), col='black', cex=1, font=3))
    dev.off()
}

## FUNCTION wordcloud
words.cloud <- function(words, min.freq, out.path, pub.id){
    # from list/corpus object cast into char vector and ignore case
    words <- unlist(strsplit(tolower(words), ' '))
    # count nr of words left
    words.len <- length(words)
    # we only want to see words longer than 2 chars
    words <- words[nchar(words) >2]
    # remove English and German stopwords
    words <- words[!words %in% stopwords(language='de')]
    words <- words[!words %in% stopwords(language='en')]
    # frequency
    wt <- sort(table(words), decreasing=TRUE)
    # minimum frequency
    wt <- wt[wt > min.freq]
    # create wordcloud
    pdf(paste(out.path, 'ec_VIDoc_cloud_', pub.id , sep=''), width=10, height=11)
    pal2 <- brewer.pal(8, 'Set2')
    wordcloud(names(wt), wt, scale=c(9,.1), max.words=Inf, random.order=F, rot.per=.3, colors=pal2)
    if (sub.title) title(sub = list(paste("# file =",pub.id, "# nr words = ", length(wt),"/", words.len, "# char > 2 # min.freq >", min.freq, "#",sep=" "), col='black', cex=1, font=3))
    dev.off()
}

# read in files and put them in list for term-doc-mat
doc.f.list <- list()
# doc.s.list <- list()
for (f in files){
    if (debug) cat("\n looking at file: ",f,"\n")
    txtf <- readPDF(PdftotextOptions='-layout')(elem=list(uri=paste(directory,f,sep='')), language='en', id=f)
    txtf <- removeNumbers(txtf)
    txtf <- removePunctuation(txtf) # following option not implemented??, preserve_intra_word_dashes=TRUE)
    txtf <- removeWords(txtf, stopwords('english'))
    txtf <- removeWords(txtf, stopwords('german'))
    txtf <- stripWhitespace(txtf) # strip extra, leave one
    doc.f.list$f <- txtf
    words.cloud(txtf, 5, out.path, f)
    swords.cloud(txtf, 5, out.path, f)
}

save(doc.f.list, file=paste(out.path, 'doc.f.list.RObj', sep=''))
# save(doc.s.list, file=paste(out.path, 'doc.s.list.RObj', sep=''))
