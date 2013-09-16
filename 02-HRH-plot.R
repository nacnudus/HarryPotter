require(tm) # for removing punctuation, whitespace, etc.
require(stringr)
require(plyr) # for round_any
require(reshape2)
require(ggplot2)
# require(grid) # for theme(panel.margin = unit(0, "inches"))
require(Snowball) # for stemming? # install openjdk-7-jdk into Linux first, then run sudo R CMD javareconf, then install.packages("rJava")

# set up a data frame
BookNames <- c(
  "1 PS"
  , "2 CS"
  , "3 PA"
  , "4 GF"
  , "5 OP"
  , "6 HP"
  , "7 DH"
)
dfBooks <- data.frame(Name = BookNames)
rownames(dfBooks) <- dfBooks$Name
rm(BookNames)

# read the files into a corpus (for removal of crap)
cCorpus <- Corpus(DirSource("/home/nacnudus/R/HarryPotter/Texts/", pattern="Harry*"))

# Remove punctuation and whitespace but leave case so that
# the chapter boundaries can be found again.
cCorpus <- tm_map(cCorpus, removePunctuation)
cCorpus <- tm_map(cCorpus, stripWhitespace)

# Find the first line (there may be two or more because of 
# samples from the other books).
dfBooks$FirstLine <- ldply(cCorpus, function(x) (which(str_detect(x, "CHAPTER ONE"))[1]))$V1
# Find the last line (there may be two or more because of
# samples from the other books).
dfBooks$FinalLine <- ldply(cCorpus, function(x) (which(x == "Titles available in the Harry Potter series in reading order")[1]-1))$V1

cCorpus <- tm_map(cCorpus, tolower)
cCorpus <- tm_map(cCorpus, removeWords, stopwords('english'))
cCorpus <- tm_map(cCorpus, removeWords, c("wouldn", "wasn", "yeah", "yeh", "yer", "wasn", "tha", "oh", "jus", "hadn", "er", "em", "eh", "don", "didn", "couldn", "abou", "ah", "firs", "arry"))
cCorpus <- tm_map(cCorpus, stemDocument)

# Discard extraneous material.
dfBooks$Text <- sapply(seq(1, 7, 1), function(x, y, z) (cCorpus[[x]][y[[x]]:z[[x]]]), y = dfBooks$FirstLine, z = dfBooks$FinalLine)
# Tidy up cCorpus
rm(cCorpus)
