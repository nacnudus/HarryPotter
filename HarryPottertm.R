# Using tm - works out the same as vector source, it seems to me.
# I'm looking for a way to search across vector elements, so across
# line endings.  UPDATE: use makeChunks which doesn't split across
# paragraphs or documents so you can split into 1000-line chunks.
# Also look at lda which can do word counts of documents.
# UPDATE: use stringr:str_join like Para <- str_join(Thousand, collapse = " ")
require(tm)
require(stringr)

# set up a data frame
BookNames <- c(
  "PS"
  , "CS"
  , "PA"
  , "GF"
  , "OP"
  , "HP"
  , "DH"
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
dfBooks$FirstLine <- sapply(cCorpus, function(x) (which(str_detect(x, "CHAPTER ONE"))[1]))
# Find the last line (there may be two or more because of
# samples from the other books).
dfBooks$FinalLine <- sapply(cCorpus, function(x) (which(x == "Titles available in the Harry Potter series (in reading order):")[1]))-1

# Discard extraneous material.
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y ,z) (cCorpus[[x]][y[x]:z[x]]), y = dfBooks$FirstLine, z = dfBooks$FinalLine)

# Get all the chapter headings and their row numbers
dfBooks$ChapterRow <- sapply(dfBooks$Text, function(x) (which(str_detect(x, "CHAPTER"))))
dfBooks$ChapterHeading <- sapply(seq(1, 7, 1), function(x, y) (dfBooks$Text[[x]][y[[x]]]), y = dfBooks$ChapterRow)

# Remove the crap from the chapter headings.
dfBooks$ChapterHeading <- sapply(seq(1, 7, 1), function(x) (str_trim(str_replace_all(dfBooks$ChapterHeading[[x]], "(—|–)", ""))))


# Split into single words.
dfBooks$SingleWords <- sapply(dfBooks$Text, function(x) (unlist(str_split(x, " "))))

# Get the chapter headings again
# Get all the chapter headings and their row numbers
dfBooks$ChapterRow <- sapply(dfBooks$SingleWords, function(x) (which(str_detect(x, "CHAPTER"))))

# Chapter wordcount
# The first command misses the last chapter of each book.
dfBooks$ChapterWordcount <- sapply(dfBooks$ChapterRow, function(x) (diff(x)))
# Append the last chapter of each book
dfBooks$ChapterWordcount <- sapply(seq(1,7,1), function(x) (c(dfBooks$ChapterWordcount[[x]], length(dfBooks$SingleWords[[x]])-sum(dfBooks$ChapterWordcount[[x]]))))

# Group by 1000 words
dfBooks$Thousands <- sapply(dfBooks$SingleWords, function(x) (split(x, ceiling(seq_along(x)/1000))))

cCorpus <- tm_map(cCorpus, removePunctuation)
cCorpus <- tm_map(cCorpus, tolower)
cCorpus <- tm_map(cCorpus, stripWhitespace)