library(plyr)
library(dplyr)
library(readr)
library(magrittr)

input <- read_fwf("input_day_3.txt",fwf_widths(c(5,5,5)))

colnames(input) <- c('a','b','c')

input <-
input %>%
  mutate(triangle = ifelse(a+b > c & a+c > b & b+c > a,TRUE,FALSE))

paste0('Answer 1: ',length(which(input$triangle)))

input2 <-
rbind(matrix(input$a,ncol=3, byrow = TRUE),matrix(input$b,ncol=3, byrow = TRUE),matrix(input$c,ncol=3, byrow = TRUE)) %>%
  as.data.frame() %>%
  set_colnames(c('a','b','c')) %>%
  mutate(triangle = ifelse(a+b > c & a+c > b & b+c > a,TRUE,FALSE))
  
paste0('Answer 2: ',length(which(input2$triangle)))