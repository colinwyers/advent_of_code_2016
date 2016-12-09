#install.packages('zoo')
library(stringr)
library(zoo)

?rollapply
?stringr

n <- c(1:20)

rollapply(n,3,function(x) { str(x)})


str_extract_all(string, pattern, simplify = FALSE)

matches <- str_locate_all('ioxxoj[asdfgh]zxcvbn', '\\[[a-z]*\\]')

invert_match(matches[[1]])[1,2]

string1 <- substring('ioxxoj[asdfgh]zxcvbn',invert_match(matches[[1]])[1,1],invert_match(matches[[1]])[1,2])

string1_vec <- substring(string1, seq(1,nchar(string1),1), seq(1,nchar(string1),1))

rollapply(string1_vec,4,function(x) { c(x)})