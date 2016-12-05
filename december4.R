library(stringr)
library(plyr)
library(dplyr)
library(lambda.r)
library(purrr)
library(magrittr)
library(readr)

?read_csv

rooms <- read_lines("input_day_4.txt")

#test_str <- "aaaaa-bbb-z-y-x-123[abxyz]"
#test_str <- "a-b-c-d-e-f-g-h-987[abcde]"
#test_str <- "not-a-real-room-404[oarel]"
#test_str <- "totally-real-room-200[decoy]"

checksum_validate <- function(x) {
  strings <-
  str_replace(x,"\\]","") %>%
    str_split("\\[")

  char_only <- str_match_all(strings[[1]][1],"[a-z]*")

  char_only_str <- paste(char_only[[1]][,1], collapse = '')

  test <-
    as.data.frame(sapply(letters,function(x) { str_count(char_only_str,x) })) %>%
    bind_cols(as.data.frame(letters)) %>%
    set_colnames(c('count','letters')) %>%
    arrange(desc(count),letters)
  
  checksum_test <- paste(head(test$letters,5), collapse = '')
  
  ifelse(checksum_test == strings[[1]][2],TRUE,FALSE)

}

#Part 1

valid <- sapply(rooms,checksum_validate)
sector_id <- sapply(rooms,function(x) { as.integer(str_match(x,"[0-9][0-9][0-9]")[1,1]) })

paste0('Answer 1: ',sum(sector_id[valid]))

#Part 2

rot_n_chr('a',13)

rot_n <- function(string,n) {
  paste(lapply(unlist(c(strsplit(string,NULL))),function(x) { rot_n_chr(x,n) }), sep="", collapse="")
}

decrypted_rooms <- mapply(rot_n,rooms,sector_id)

np_room <- decrypted_rooms[which(sapply(decrypted_rooms, function(x) {ifelse(str_count(x,pattern= "northpole-object-storage")>0,TRUE,FALSE)} ))]

paste0('Answer 2: ',as.integer(str_match(np_room,"[0-9][0-9][0-9]")))