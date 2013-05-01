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
dfBooks$FinalLine <- sapply(cCorpus, function(x) (which(x == "Titles available in the Harry Potter series in reading order")[1]-1))

# Discard extraneous material.
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y ,z) (cCorpus[[x]][y[[x]]:z[[x]]]), y = dfBooks$FirstLine, z = dfBooks$FinalLine)
# Tidy up cCorpus
rm(cCorpus)

# Fix some names
# "Dark Lord" >- "DarkLord"
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y) (str_replace_all(y[[x]], "Dark Lord", "DarkLord")), y = dfBooks$Text)

# Get all the chapter headings and their row numbers
dfBooks$ChapterRow <- sapply(dfBooks$Text, function(x) (which(str_detect(x, "CHAPTER"))))
dfBooks$ChapterHeading <- sapply(seq(1, 7, 1), function(x, y) (dfBooks$Text[[x]][y[[x]]]), y = dfBooks$ChapterRow)

# Split into single words.
dfBooks$SingleWords <- sapply(dfBooks$Text, function(x) (unlist(str_split(x, " "))))
# and remove blanks
dfBooks$SingleWords <- sapply(dfBooks$SingleWords, function(x) (x[x!= ""]))

# Get the chapter headings again
# Get all the chapter headings and their row numbers
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

# start making a vector of Name, Thousands
dfBooks$ThousandVector <- sapply(seq(1,7,1), function(x) (t(expand.grid(dfBooks$Name[[x]], dfBooks$ThousandString[[x]]))))

# Count regex
sapply(dfBooks$Thousand[[2]], function(x) (sum(str_count(x, "voldemort|youknowwho|darklord|riddle"))))

# Function to plot cumulative occurrences of a word
# Use: CumSumWord(x=regex, y=dfBooks$[[1]], z="anything")
CumSumWord <- function(x, y, z) {
  PlotWord <- x
  PlotBook <- y
  PlotTitle <- z
  WordVector <- as.vector(sapply(PlotBook, function(x, y) (sum(str_count(x, y))), y = PlotWord))
  xaxis <- seq_along(WordVector)
  yaxis <- cumsum(WordVector)
  Plot <- plot(xaxis, yaxis, type = "n")
  lines(xaxis, yaxis)
  title(main = paste(z, "-", x))
  list(Book=as.character(z), Word=x, WordVector=WordVector, Plot=Plot)
}

# CumSumWord through the whole series
# requires function CumSumWord
sapply(seq(1,7,1), function (x) (CumSumWord("voldemort|youknowwho|darklord|riddle", dfBooks$Thousand[[x]], dfBooks$Name[x])))

# Starting to plot
# Get max(xaxis) per chapter
dfBooks$ChapterXmax <- sapply(seq(1,7,1), function(x, y, z) (y[[x]] + z[[x]] - 1), y=dfBooks$ChapterRow, z=dfBooks$ChapterWordcount)
# Shade by chapter
dfPSChapter <- data.frame(Number=1:length(dfBooks$ChapterHeading[[1]]), xmin=dfBooks$ChapterRow[[1]]/1000, xmax=(dfBooks$ChapterRow[[1]]+dfBooks$ChapterWordcount[[1]]-1)/1000)
# CumSumWord
dfVoldemort <- sapply(dfBooks$Thousand[[1]], function(x) (sum(str_count(x, "voldemort|youknowwho|darklord|riddle"))))
dfVoldemort <- data.frame(x=1:length(dfVoldemort),y=cumsum(dfVoldemort)
# Plot
ggplot(dfPSChapter,aes(xmin=xmin, xmax=xmax, ymin=0, ymax=100, fill=factor(Number %% 2 == 0)), scale_colour_manual(values = c("red","blue"))) + geom_rect() + scale_fill_manual(values = c("black", "white"), guide=FALSE)
# Plot with cumsum
ggplot() + geom_rect(data=dfPSChapter,xmin=xmin, xmax=xmax, ymin=0, ymax=100, fill=factor(Number %% 2 == 0), scale_colour_manual(values = c("red","blue"))) + scale_fill_manual(values = c("black", "white"), guide=FALSE)
# this worked for dfPSChapter to do the chapter bands.
ggplot() + geom_rect(data=dfPSChapter, aes(xmin=xmin, xmax=xmax, ymin=0, ymax=100, fill=factor(Number %% 2 == 0))) + scale_fill_manual(values = c("black", "white"), guide=FALSE)