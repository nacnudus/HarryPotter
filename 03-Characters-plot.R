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
  return(x[with(paraRange, firstParafinalPara), ])
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
texts$para <- str_replace_all(texts$para, "Rubeus", "Hagrid")

# Reduce to one word per row
words <- adply(texts, 1, function(x) {
  data.frame(code=x$code
             , word=unlist(str_split(x$para, " ")))
})[, c("code", "word")]