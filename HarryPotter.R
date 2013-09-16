require(tm) # for removing punctuation, whitespace, etc.
require(stringr)
require(plyr) # for round_any
require(reshape2)
require(ggplot2)
require(grid) # for theme(panel.margin = unit(0, "inches"))
require(snowball) # for stemming?

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

# TermDocumentMatrix
 cCorpus <- tm_map(cCorpus, stripWhitespace)
 cCorpus <- tm_map(cCorpus, tolower)
 cCorpus <- tm_map(cCorpus, removeWords, stopwords('english'))
 cCorpus <- tm_map(cCorpus, removeWords, c("wouldn", "wasn", "yeah", "yeh", "yer", "wasn", "tha", "oh", "jus", "hadn", "er", "em", "eh", "don", "didn", "couldn", "abou", "ah", "firs", "arry"))
require(Snowball) # install openjdk-7-jdk into Linux first, then run sudo R CMD javareconf, then install.packages("rJava")
 cCorpus <- tm_map(cCorpus, stemDocument)
 cCorpus <- tm_map(cCorpus, removePunctuation)
 TDM <- TermDocumentMatrix(cCorpus)

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
# "He Who Must Not Be Named"
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y) (str_replace_all(y[[x]], "He Who Must Not Be Named", "hewhomustnotbenamed")), y = dfBooks$Text)
# "Draco Malfoy" >- "dracomalfoy"
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y) (str_replace_all(y[[x]], "Draco Malfoy", "dracomalfoy")), y = dfBooks$Text)
# "Severus Snape" >- "severussnape"
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y) (str_replace_all(y[[x]], "Severus Snape", "severussnape")), y = dfBooks$Text)
# "Albus Dumbledore" >- "albusdumbledore"
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y) (str_replace_all(y[[x]], "Albus Dumbledore", "albusdumbledore")), y = dfBooks$Text)
# "Harry Potter" >- "harrypotter"
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y) (str_replace_all(y[[x]], "Harry Potter", "harrypotter")), y = dfBooks$Text)
# "Ronald Weasley" >- "ronaldweasley"
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y) (str_replace_all(y[[x]], "Ronald Weasley", "ronaldweasley")), y = dfBooks$Text)
# "Ron Weasley" >- "ronweasley"
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y) (str_replace_all(y[[x]], "Ron Weasley", "ronweasley")), y = dfBooks$Text)
# Todo: Fix all the other Weasleys too
# "Hermione Granger" >- "hermionegranger"
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y) (str_replace_all(y[[x]], "Hermione Granger", "hermionegranger")), y = dfBooks$Text)
# "Rubeus Hagrid" >- "rubeushagrid"
dfBooks$Text <- sapply(seq(1, 7, 1), function(x ,y) (str_replace_all(y[[x]], "Harry Potter", "harrypotter")), y = dfBooks$Text)

# Get all the chapter headings and their row numbers
dfBooks$ChapterRow <- sapply(dfBooks$Text, function(x) (which(str_detect(x, "CHAPTER"))+6)) # the +6 offsets from "CHAPTER ONE" to "The Boy Who Lived"
# Numbered chapter headings
dfBooks$ChapterHeading <- sapply(seq(1, 7, 1), function(x, y) (str_trim(paste(sprintf("%02d", seq_along(y[[x]])), dfBooks$Text[[x]][y[[x]]]))), y = dfBooks$ChapterRow) # Number the chapters within the books

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
rm(i)

# make a vector of Name, Chapter Heading, Words
x <- rep(unlist(dfBooks$ChapterHeading), unlist(dfBooks$ChapterWordcount))
y <- stack(with(dfBooks, setNames(SingleWords,Name)))
z <- cbind(x, y)
rm(x, y) # tidy
z <- z[, c(3,1,2)]
names(z) <- c("Book", "Chapter", "Word")
z <- melt(z)
z$Voldemort <- str_count(z$Word, "voldemort|youknowwho|hewhomustnotbenamed|darklord|riddle")
z$Malfoy <- str_count(z$Word, "draco|dracomalfoy|malfoy")
z$Snape <- str_count(z$Word, "severus|severussnape|snape|snivellus")
z$Dumbledore <- str_count(z$Word, "albus|albusdumbledore|dumbledore")
z$Harry <- str_count(z$Word, "harry|harrypotter|potter")
z$Ron <- str_count(z$Word, "ron|ronald|ronweasley|ronaldweasley|weasley")
z$Hermione <- str_count(z$Word, "hermione|hermionegranger|granger")
z$Hagrid <- str_count(z$Word, "rubeus|rubeushagrid|hagrid")
z$Word <- seq_along(z$Word)
a <- z[z$Voldemort!=0 | z$Malfoy!=0 | z$Snape!=0 | z$Dumbledore!=0 | z$Harry!=0 | z$Ron!=0 | z$Hermione!=0 | z$Hagrid!=0,] # restrict to interesting points
voldemort <- ggplot() + geom_line(data=a, aes(x=Word, y=cumsum(Voldemort)), colour="black") + geom_line(data=a, aes(x=Word, y=cumsum(Malfoy)), colour="red") + geom_line(data=a, aes(x=Word, y=cumsum(Snape)), colour="orange") + geom_line(data=a, aes(x=Word, y=cumsum(Dumbledore)), colour="yellow") + geom_line(data=a, aes(x=Word, y=cumsum(Hagrid)), colour="white")
# b and c are for chapter shading
b <- dcast(z, Book + Chapter ~ ., length)
names(b)[3] <- "Count"
c <- data.frame(text=unique(paste(z$Book, z$Chapter)), xmin=cumsum(b$Count)-b$Count+1, xmax=cumsum(b$Count))
# d and e are for book shading
d <- dcast(z, Book ~ ., length)
names(d)[2] <- "Count"
e <- data.frame(xmin=cumsum(d$Count)-d$Count+1, xmax=cumsum(d$Count))
voldemort + geom_rect(data=c, aes(xmin=xmin, xmax=xmax, fill=factor(seq_along(xmin) %% 2)), ymin=-Inf, ymax=Inf, linetype="blank", alpha=0.2, colour=scale_fill_manual(values=c("blue", "red"))) + theme(legend.position="none") + geom_rect(data=e, aes(xmin=xmin, xmax=xmax, fill=factor(seq_along(xmin) %% 2)), ymin=-Inf, ymax=Inf, linetype="blank", colour=scale_fill_manual(values=c("black", "white")), alpha=0.2) + geom_text(data=c, aes(x=xmin+((xmax-xmin)/2), y=0, label=text), angle=90, size=3, hjust=0, vjust=0)