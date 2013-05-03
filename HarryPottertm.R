# Using tm - works out the same as vector source, it seems to me.
# I'm looking for a way to search across vector elements, so across
# line endings.  UPDATE: use makeChunks which doesn't split across
# paragraphs or documents so you can split into 1000-line chunks.
# Also look at lda which can do word counts of documents.
# UPDATE: use stringr:str_join like Para <- str_join(Thousand, collapse = " ")
require(tm)
require(stringr)
require(plyr) # for round_any
require(reshape2)
require(ggplot2)

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
dfBooks$FirstLine <- sapply(cCorpus, function(x) (which(str_detect(x, "CHAPTER ONE"))[1]))
# Find the last line (there may be two or more because of
# samples from the other books).
dfBooks$FinalLine <- sapply(cCorpus, function(x) (which(x == "Titles available in the Harry Potter series in reading order")[1]-1))

# Discard extraneous material.
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y ,z) (cCorpus[[x]][y[[x]]:z[[x]]]), y = dfBooks$FirstLine, z = dfBooks$FinalLine)
# Tidy up cCorpus
rm(cCorpus)

# Fix some names
# "Dark Lord" >- "DarkLord"
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y) (str_replace_all(y[[x]], "Dark Lord", "DarkLord")), y = dfBooks$Text)

# Get all the chapter headings and their row numbers
dfBooks$ChapterRow <- sapply(dfBooks$Text, function(x) (which(str_detect(x, "CHAPTER"))+6)) # the +6 offsets from "CHAPTER ONE" to "The Boy Who Lived"
# Numbered chapter headings
dfBooks$ChapterHeading <- sapply(seq(1, 7, 1), function(x, y) (str_trim(paste(seq_along(y[[x]]), dfBooks$Text[[x]][y[[x]]]))), y = dfBooks$ChapterRow)
# Number the chapters within the books


# Split into single words.
dfBooks$SingleWords <- sapply(dfBooks$Text, function(x) (unlist(str_split(x, " "))))
# and remove blanks
dfBooks$SingleWords <- sapply(dfBooks$SingleWords, function(x) (x[x!= ""]))

# Get the chapter headings again
# Get all the chapter headings and their row numbers
# This time don't offset from "CHAPTER ONE"
dfBooks$ChapterRow <- sapply(dfBooks$SingleWords, function(x) (which(str_detect(x, "CHAPTER"))))

# Now it can be lowercase
dfBooks$SingleWords <- sapply(seq(1,7,1), function(x) (tolower(dfBooks$SingleWords[[x]])))

# Chapter wordcount
# The first command misses the last chapter of each book.
dfBooks$ChapterWordcount <- sapply(dfBooks$ChapterRow, function(x) (diff(x)))
# Append the last chapter of each book
dfBooks$ChapterWordcount <- sapply(seq(1,7,1), function(x) (c(dfBooks$ChapterWordcount[[x]], length(dfBooks$SingleWords[[x]])-sum(dfBooks$ChapterWordcount[[x]]))))

# Group by chapters
for(i in 1:length(dfBooks$Name)) {
  dfBooks$Chapter[[i]] <- sapply(seq(1, length(dfBooks$ChapterHeading[[i]]), 1), function(x, y, z, a) (y[z[x]:(z[x]+a[x]-1)]), y=dfBooks$SingleWords[[i]], z=dfBooks$ChapterRow[[i]], a=dfBooks$ChapterWordcount[[i]])
}

# Group by 1000 words
dfBooks$Thousand <- sapply(dfBooks$SingleWords, function(x) (split(x, ceiling(seq_along(x)/1000))))

# Single string chapters
for(i in 1:length(dfBooks$Name)) {
  dfBooks$ChapterString[[i]] <- sapply(seq(1, length(dfBooks$Chapter[[i]]), 1), function(x, y) (str_c(y[[x]], collapse=" ")), y=dfBooks$Chapter[[i]])
}

# Single string thousands
for(i in 1:length(dfBooks$Name)) {
  dfBooks$ThousandString[[i]] <- sapply(seq(1, length(dfBooks$Thousand[[i]]), 1), function(x, y) (str_c(y[[x]], collapse=" ")), y=dfBooks$Thousand[[i]])
}

# make a vector of Name, ThousandStrings
x <- setNames(rev(stack(with(dfBooks, setNames(ThousandString, Name)))), c("Name", "ThousandString"))

# add chapter headings to the thousand vector
chapterThousand <- function(x) (sapply(seq_along(x), function(y) ((round_any(sum(x[1:y]), 1000, ceiling)/1000) - (round_any(sum(x[0:(y-1)]), 1000, ceiling)/1000))))
dfBooks$ChapterThousand <- sapply(dfBooks$ChapterWordcount, function(x) (chapterThousand(x)))
dfBooks$ChapterThousandHeading <- sapply(seq_along(dfBooks$Name), function(x) (rep(dfBooks$ChapterHeading[[x]], dfBooks$ChapterThousand[[x]])))
y <- setNames(rev(stack(with(dfBooks, setNames(ChapterThousandHeading, Name)))), c("Name", "ChapterThousandHeading"))
# z is the vector to be graphed
z <- cbind(y[,1:2], x[,2])
# tidy
rm(x,y)

# Count regex
cbind(z[,1:2], str_count(z[,3], "harry"))

######
# wordcount function
# given a vector like z of book name, thousand per book, chapter heading and thousand words,
# returns book name, chapter heading and count of a regex pattern in the thousand words.
# Third column named by the regex pattern for nice plotting.
wordcount <- function(z, x) {
  a <- cbind(z[,1:2], Thousand=sequence(rle(as.character(z$Name))$lengths), str_count(z[,3], x))
  names(a)[4] <- x
  return(a)
}
######

#####
# example graph
voldemort <- wordcount(z, "voldemort|youknowwho|he who must not be named|darklord|riddle")
names(voldemort)[4] <- "Voldemort"
voldemort$NameFactor = factor(voldemort$Name, levels=c("PS", "CS", "PA", "GF", "OP", "HP", "DH")) # for correct series order
voldemort <- ddply(voldemort, c("Name"), transform, Wordcount=cumsum(Voldemort))
voldemort <- melt(voldemort, measure.vars="Voldemort")
Pvoldemort <- qplot(x=Thousand, y=cumsum(value), data=voldemort, geom="line") + facet_grid(. ~ NameFactor + ChapterThousandHeading, scale="free_x", space="free_x")
Pvoldemort + theme(strip.text.x = element_text(angle = 90, hjust = 1))
#####