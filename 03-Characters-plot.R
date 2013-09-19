require(stringr)
require(plyr) # for round_any
require(reshape2)
require(ggplot2)

# prepare a data frame
books <- c(
  "1 PS"
  , "2 CS"
  , "3 PA"
  , "4 GF"
  , "5 OP"
  , "6 HP"
  , "7 DH"
)
books <- data.frame(code = books)
rownames(books) <- books$code

# Read text files, one paragraph per row
texts <- books
texts$filePath <- list.files(path="Texts/", full.names=TRUE)
texts <- ddply(texts, .(code), function(x) {data.frame(para=readLines(file(x$filePath)))})
texts$para <- as.character(texts$para)

# Find the first and final paragraphs (there may be two or more because of 
# samples from the other books).
books$firstPara <- daply(texts, .(code), function(x) {firstPara=which(str_detect(x$para, "CHAPTER ONE"))[1]})
books$finalPara <- daply(texts, .(code), function(x) {finalPara=which(str_detect(x$para, "Titles available in the Harry Potter series"))[1]})
# Reduce texts to between those paragraphs
texts <- ddply(texts, .(code), function(x) {
  paraRange <- books[books$code==x$code, ]
  return(x[with(paraRange, firstPara:finalPara), ])
  })

# Standardise names
texts$para <- str_replace_all(texts$para, "Harry Potter", "Harry") 
texts$para <- str_replace_all(texts$para, "Potter", "Harry") 
texts$para <- str_replace_all(texts$para, "Ron Weasley", "Ron") 
texts$para <- str_replace_all(texts$para, "Ronald Weasley", "Ron") 
texts$para <- str_replace_all(texts$para, "Fred Weasley", "Fred") 
texts$para <- str_replace_all(texts$para, "George Weasley", "George") 
texts$para <- str_replace_all(texts$para, "Weasley Twins", "Twins") 
texts$para <- str_replace_all(texts$para, "Ginny Weasley", "Ginny") 
texts$para <- str_replace_all(texts$para, "Bill Weasley", "Bill") 
texts$para <- str_replace_all(texts$para, "Charlie Weasley", "Charlie") 
texts$para <- str_replace_all(texts$para, "Percy Weasley", "Percy") 
texts$para <- str_replace_all(texts$para, "Molly Weasley", "Molly") 
texts$para <- str_replace_all(texts$para, "Arthur Weasley", "Arthur") 
texts$para <- str_replace_all(texts$para, "Hermione Granger", "Hermione") 
texts$para <- str_replace_all(texts$para, "Dark Lord", "Voldemort") 
texts$para <- str_replace_all(texts$para, "You-Know-Who", "Voldemort") 
texts$para <- str_replace_all(texts$para, "He Who Must Not Be Named", "Voldemort") 
texts$para <- str_replace_all(texts$para, "Tom Riddle", "Voldemort") 
texts$para <- str_replace_all(texts$para, "Tom", "Voldemort") 
texts$para <- str_replace_all(texts$para, "Albus Dumbledore", "Dumbledore") 
texts$para <- str_replace_all(texts$para, "Albus", "Dumbledore") 
texts$para <- str_replace_all(texts$para, "Rubeus Hagrid", "Hagrid")
texts$para <- str_replace_all(texts$para, "Malfoy Manor", "Manor")
texts$para <- str_replace_all(texts$para, "Malfoy family", "Malfoys")
texts$para <- str_replace_all(texts$para, "Lucius Malfoy", "Lucius")
texts$para <- str_replace_all(texts$para, "Narcissa Malfoy", "Narcissa")
texts$para <- str_replace_all(texts$para, "Draco Malfoy", "Draco")
texts$para <- str_replace_all(texts$para, "Malfoy", "Draco")

# Reduce to one word per row
words <- ddply(texts, .(code), function(x) {
  data.frame(word=unlist(str_split(x$para, " ")))
}, .progress="text")
# remove punctuation
words$word <- str_replace_all(words$word, "[^[:alnum:]]", "")
words <- words[words$word!="", ]
# prepare graphing bands
books$words <- as.data.frame(table(words$code))$Freq
books$xmax <- cumsum(books$words)
books$xmin <- books$xmax - books$words
books$xmax <- round_any(books$xmax, 1000, floor)
books$xmin <- round_any(books$xmin, 1000, floor)
books$xmax <- books$xmax / 1000
books$xmin <- books$xmin / 1000

# plot
countName <- function(words, nameToCount) {
  x <- cumsum(words$word==nameToCount)[seq(0, nrow(words), 1000)]
  return(x)
}

Harry <- countName(words, "Harry")
Ron <- countName(words, "Ron")
Hermione <- countName(words, "Hermione")

Characters <- data.frame(x=seq_along(Harry)
                         , Harry, Ron, Hermione)
mCharacters <- melt(Characters, id.vars="x")
ggplot(mCharacters, aes(x, value
                        , group=variable
                        , colour=variable)) + 
  geom_line()

Voldemort <- countName(words, "Voldemort")
Draco <- countName(words, "Draco")

Characters <- data.frame(x=seq_along(Voldemort)
                         , Voldemort, Draco)
mCharacters <- melt(Characters, id.vars="x")
ggplot() +
  theme_bw() +
  expand_limits(x = 0, y = 0) +
  geom_rect(data=books
            , mapping=aes(xmin=xmin, xmax=xmax
                          , fill=factor(seq_along(xmin))
                          , x=NULL, y=NULL
                          , group=NULL, colour=NULL)
            , ymin=-Inf, ymax=Inf
            , show_guide=FALSE) +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_discrete(guide=FALSE) +
  geom_line(data=mCharacters
            , mapping=aes(x, value
                          , group=variable
                          , colour=variable)) +
  scale_colour_manual(values=c("black", "red")) + 
  scale_fill_discrete(guide=FALSE) +
  xlab("~1000 words") +
  ylab("mentions of name") +
  ggtitle("Harry, Ron & Hermione")