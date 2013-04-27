# Using tm - works out the same as vector source, it seems to me.
# I'm looking for a way to search across vector elements, so across
# line endings.  UPDATE: use makeChunks which doesn't split across
# paragraphs or documents so you can split into 1000-line chunks.
# Also look at lda which can do word counts of documents.
# UPDATE: use stringr:str_join like Para <- str_join(Thousand, collapse = " ")
require(tm)
cCorpus <- Corpus(DirSource("/home/nacnudus/R/HarryPotter/Texts/"))
cCorpus$Name <- c(
  "PS"
  , "CS"
  , "PA"
  , "GF"
  , "OP"
  , "HP"
  , "DH"
)
names(cCorpus) <- cCorpus$Name
cCorpus$FirstLine <- sapply(cCorpus, function(x) (which(str_detect(x, "CHAPTER ONE"))[1]))

cCorpus <- tm_map(cCorpus, removePunctuation)
cCorpus <- tm_map(cCorpus, tolower)
cCorpus <- tm_map(cCorpus, stripWhitespace)
# Find the first line (there may be two or more because of 
# samples from the other books).
FirstLine <- sapply(cCorpus, function(x) (which(str_detect(x, "CHAPTER ONE"))[1]))
# Find the last line (there may be two or more because of
# samples from the other books).
FinalLine <- sapply(cCorpus, function(x) (which(x == "Titles available in the Harry Potter series (in reading order):")[1]))-1

# Discard extraneous material.
cCorpus <- sapply(seq(1, 7, 1), function(x ,y ,z) (cCorpus[[x]][y[x]:z[x]]), y = FirstLine, z = FinalLine)
# Tidy up
rm(FirstLine, FinalLine)