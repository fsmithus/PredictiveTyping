
# Library Routines

# library(R.utils)
library(NLP)
library(tm)

# Data Loading and Exploration Routines

## Collect basic size metrics.
basic.data.stats <- function(filename,dir="../data/en_US") {
        pathname <- paste(dir,filename,sep="/")
        data <- readLines(pathname,warn=FALSE,skipNul=TRUE)
        stats <- c(length(data),sum(sapply(strsplit(data,"\\s+"),length)),sum(sapply(data,nchar)))
        data.frame(lines=stats[1],words=stats[2],characters=stats[3],row.names=filename)
}

## Randomized readLines().
readSamples <- function(path,p=0.5) {
        sample <- vector()
        line <- character()
        f <- file(path,"rb")
        while(TRUE) {
                line <- readLines(f,n=1,warn=FALSE,skipNul=TRUE)
                if (length(line) > 0) {
                        if (runif(1) <= p) {                    # Randomly include/discard this line?
                                sample <- c(sample,line)        # Concatenate. (Is this efficient?)
                        }
                }
                else {
                        break
                }
        }
        close(f)
        sample
}

## Scrub a document and return a vector of strings/documents.
scrub.data.file <- function(path) {             # perform basic scrubbing per discussion above       
        data <- readSamples(path,data.fraction) # read raw text; won't scale for large files
        data <- removePunctuation(data)         # remove punctuation
        data <- removeNumbers(data)             # remove numbers
        data <- tolower(data)                   # lower case
        print(paste0(path," - ",toString(length(data))," lines, ",
                     toString(length(unlist(strsplit(data,"\\s+"))))," words"))
        # data <- removeWords(data,stopwords("english"))  # remove english words that have no analytical value
        # data <- removeWords(data,blacklist.words)       # remove profanity or other words that don't belongh
        # data <- stemDocument(data)                      # stem words (remove common suffixes and prefixes)
        # data <- stripWhitespace(data)         # remove whitespace characters (done later, not necessary)
        unlist(data)                            # return simple vector
}

## Calculate the n-grams for a vector of ordered words. Return a vector of n-gram strings.
gramify <- function(n,words) {          # convert a vector of words into a vector of N-grams
        if ((1 < n) && (n <= length(words))) {  # N must be between 2 and the length of the input vector
                grams <- vector("character",length(words) - (n-1))
                for (i in 1:(length(grams))) {
                        gram <- words[i]
                        for (j in 1:(n-1)) {
                                gram <- paste(gram,words[i+j],sep=" ")
                        }
                        grams[i] <- gram
                }
                grams
        }
}

## Convert documents into a sorted vector of named counts (n-grams).
count.ngrams <- function(n,docs) {
        grams <- vector("character")
        for (i in 1:length(docs)) {             # Build entire list of n-gram, including duplicates
                g <- gramify(n,docs[[i]])
                grams <- c(grams,g)             # Efficient?
        }
        result <- sort(table(sort(grams)),decreasing=TRUE)  
        result        
}

## Plot an n-gram profile.
hist.buckets <- 25                              # Width of histograms
plot.ngrams <- function (f,g1,g2,g3) {
        par(mfrow=c(2,2),mai=c(0.8466,0.6806,0.6806,0.3486))
        plot(g1,main="Unigram (Word) Counts",las=2,cex.axis=0.75,xlim=range(1:hist.buckets),ylab="Count")
        
        num.words <- length(g1)                 # Number of unique words encountered
        plot.data <- cumsum(g1)                 # Cumulative counts of words
        n <- plot.data[length(plot.data)]       # Total number of (non-unique) words
        plot(plot.data,main="Cumulative Word Count",las=2,ylab="Count",ylim=c(1,n),xlab="",xaxt='n')
        abline(h=n * .25,lty=2)
        abline(h=n * .50)
        abline(h=n * .75,lty=2)
        if (num.words > 1000) {
                abline(v=1000,lty=3)
        }
        if (num.words <= 50) {
                axis(1,las=2,labels=names(g1)[1:num.words],at=(1:num.words))
        }
        else {
                axis(1,las=2)
        }
        
        plot(g2,main="Bigram Counts",las=2,cex.axis=0.75,xlim=range(1:hist.buckets),ylab="Count")
        plot(g3,main="Trigram Counts",las=2,cex.axis=0.75,xlim=range(1:hist.buckets),ylab="Count")        
}

## Analyze the n-grams in a document file.
analyze.ngrams <- function(filename,datafolder) {
        f<-paste(datafolder,filename,sep="/")   # Read file into memory. Won't scale.
        
        data <- scrub.data.file(f)              # Basic cleanup and normalization
        docs <- strsplit(data, "\\s+")
        
        unigrams <- unlist(docs)                # Calculate N-grams
        unigrams.count <- sort(table(sort(unigrams)),decreasing=TRUE)
        bigrams.count <- count.ngrams(2,docs)
        trigrams.count <- count.ngrams(3,docs)
        
        plot.ngrams(f,unigrams.count,bigrams.count,trigrams.count)
}
