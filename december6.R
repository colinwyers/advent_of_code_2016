library(readr)
#library(lambda.r)
#library(purrr)
#library(magrittr)
#library(plyr)
#library(dplyr)
#library(pander)

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

unmode <- function(x) {
  ux <- unique(x)
  ux[which.min(tabulate(match(x, ux)))]
}

#input <- read_fwf("input_day_6_test.txt",fwf_widths(c(1,1,1,1,1,1)))
input <- read_fwf("input_day_6.txt",fwf_widths(c(1,1,1,1,1,1,1,1)))

colnames(input) <- c('a','b','c','d','e','f','g','h')

#glimpse(input)

#input %>%
#  group_by(a) %>%
#  summarize(cnt = n()) %>%
#  arrange(desc(cnt)) %>%
#  pandoc.table()

paste0(sapply(input,function(x) {mode(x)}),collapse='')

paste0(sapply(input,function(x) {unmode(x)}),collapse='')