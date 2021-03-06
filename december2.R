library(magrittr)
library(plyr)
library(dplyr)
library(purrr)
#library(stringr)
library(lambda.r)

#test_input <- c('ULL', 'RRDDD', 'LURDL', 'UUUUD')

test_input <- c('RUDULRLLUULRURDDRRUDURULLLDRLRLUDDLUDUDDUDRRDUDULDUUULLRULLRLDDLDLDDRLRRRRUDLLDDUULDRLLUDDRRUURLULRRRDLLURRUUDURUDDURLUDDDLUDDUUDUURUDLRDRDRLRDRLDRUDRUUDLRDDRRURDDLRDDRRURDUDDLULLUDRURURRRLRRUDUULULULRRLDLUDUURRLLRUDLLDRDDLRRRULRUDLULDDLLLULDLRUDLLLLRDDLRDRLDRLLRDRRDLRDULULRLLLDRUDRRRUULRUULDRURLUDRURRDLLDLRDLDDDDRRLUDLRRLUUUURDRDDLRRURURRDUULLRLURLURUDDDRDURDUUDRLRLRRLDDLDLDLDDDUDDULURLDDLLRLRRDULUDDLULRLUDDLDLRULUUUDRLDRUDURLUDDRLLRUULDLRRRRDLLLLURULLRDRRUDLUULRRDLLRLRLUDLDDULLDLLRDLDLL',
'LLUUUUUUDUDRLRDRDLDURRRLLRRLRURLLUURRLLUDUDLULUURUUURDLUDLDDLULLRDLRUULDLRDUDURLLDDUDUDULLUDDUULLLUULRRRLULRURRDLRUDUDDURRRDRUURDURLLULLRULLDRUULLURLDRDUUDDDDDDRRLDRLRRRLULDDUURRLLLLDRURLURDRDRDURUDUURRDUDUDRLLUUDDRLUDDDRDLDLRLDRURRDLLRULDRLLURURRLUULLRLRRURDDRDRUUURUURUUUDLLRRLUDRLDLRLURLDLUDDUDDDLDUDRRLDLRURULRLLRDUULURRRULDLLLRLDDDUURRRRDULLRURRLULULDLRRUDUDDLRUURDLDUDDUDRRDLRRRDUDUUUDLLDDDDLURLURRRUUULLLULRRLLLLLLULDUUDLRUDRRDLRDUUDUDLLRLDLLRUURDUUURUUUDDLLUUDLULDURLULULUUUDRUDULLURRULRULLRDLDDU',
'RLUUURULLDLRLDUDRDURRDUURLLUDDDUULRRRLRLURDDRUULUDULDUUDDDDUDDDDRUDDLDUUDRUDLRRRLLRDDLLLRLLRUULRUULDDRURRLURRLRLULDDRRRDDURDDRDRDULRUDRUUDULRLLULDLRLLDRULRDDRRDDUDLRLLUDRDRRRLUDULRDLRDDURRUUDDRRUDURRUUUDDRRDUDURLUUDUDUURDDDLURLULLUULULURUDUUDRUDULLUUULURDLDUULLDDLLDULRLRLRDUUURUUDLRLDURUDRLDULLUDLDLLRDUURRDUDURLUUUDLLRRULRLULRLDLLURDURRULRLLRRDUDLLRDRRRRDLUUDRUUUDDLRLUDDDDDDRURRRUUURRDLLRURLDDLLDLRRLLLDRRULRRUDLDRDDRRLULURLLUURURURRRRUUUUURUDURLRLLLULULDLLDLRDRRULUDUDRDRRDRDRRDUDLLLRUDRUDDDULRULRRRDRLRUUUURUDURDUUULLULRUDDULDUUDLDURRD',
'ULRULDDLDLULLLRRRLRUDDDDDLLDDUDLRRDULUUDRDLRRURDRRLUULRURUDRRULDLLLUDRUUDULULUDDRUDDDRDURRRDRDUUURLRDULUDRDRLDRUDDLLLDRRULUDLUDLDLLRRUDUULULDLDLLUURDLDDLLUUDURLURLLLDRDLDRRLRULUURRDRULRUUURULRRUDDDDLLDLDDLLRRLRRRRDUUDUDLDRDRRURDLRURULDLRDLLLLRUDRLLRDLRLRDURDRUDURRRLRDRDLLRLUDDDDRLRLLDUURRURLUURUULUDLUURDRRUDDLUDUDDDURRDRUDRLRULDULUUUUUUDDUDRUDUUURUDRRDLUDLUUDUULUDURDLDDDLLURRURUUDUDDRRDRLLULULDRLRURRDDDRDUUURDDDRULUDRDDLDURRLDDDLRRRLDDRDURULDLUDLLLURLURRLRRULDLLDDUDRRULDRRRRLURRUULRRRUDLURDLLDLLDULUUDRRLDLLLDRLRUDLUULDLDRUDUDURDRUDRDDDLRLULLUR',
'LRLUUURRLRRRRRUURRLLULRLULLDLUDLUDRDDRLDLRLULLURDURLURDLLRLDUUDDURRRRLDLLRULLRLDLLUUDRLDDLLDRULDRLLRURDLRURRUDLULLRURDLURRURUDULLDRLLUUULUDRURRUUDUDULUUULRLDDULDRDLUDDUDDDLRURULLDLLLRLLUURDLRUDLLLLDLLRLRUUUDDRUUUUDLDLRDDURLDURUULLLUUDLLLLDULRRRLLDLDRRDRLUDRUDURLLUDLRLLUDUDRDDDRDLRDLRULUULDRLUDLRLDUURLRRLUDDDUUDDDUDRLDLDUDLURUULLDDDURUUULRLUDLDURUUDRDRURUDDUURDUUUDLLDLDLDURUURLLLLRURUURURULRULLRUDLRRUUUUUDRRLLRDDUURDRDRDDDUDRLURDRRRUDLLLDURDLUUDLLUDDULUUDLDUUULLDRDLRURUURRDURRDLURRRRLLUUULRDULDDLDUURRDLDLLULRRLLUDLDUDLUUL')

keypad <- t(matrix(c(1:9), nrow = 3, ncol = 3))

pos <- c(2, 2)

str_to_vec <- function(x) { laply(seq(1, nchar(x)), function(i) substr(x, i, i)) }

move_key(1, x, 'U') %as% c(1, x)
move_key(2, x, 'U') %as% c(1, x)
move_key(3, x, 'U') %as% c(2, x)
move_key(1, x, 'D') %as% c(2, x)
move_key(2, x, 'D') %as% c(3, x)
move_key(3, x, 'D') %as% c(3, x)
move_key(x, 1, 'L') %as% c(x, 1)
move_key(x, 2, 'L') %as% c(x, 1)
move_key(x, 3, 'L') %as% c(x, 2)
move_key(x, 1, 'R') %as% c(x, 2)
move_key(x, 2, 'R') %as% c(x, 3)
move_key(x, 3, 'R') %as% c(x, 3)
seal(move_key)

find_key <- function(char_vec, start_pos = c(2, 2)) {
  if (length(char_vec) == 0) {
    return(start_pos)
    break
  }
  
  end_pos <-
    move_key(start_pos[1], start_pos[2], char_vec[1])
  
  find_key(char_vec[-1],end_pos)
  
}

find_code <- function(input_list, start_pos = c(2, 2), result = list()) {
  if (length(input_list) == 0) {
    return(result)
    break
  }
  
  end_pos <- find_key(input_list[[1]],start_pos)
  
  result <- append(result, list(end_pos))
  
  find_code(input_list[-1],end_pos,result)
  
}

keypad_loc <- function(x) {
  keypad[x[1], x[2]]
}

#Part 1

test_vects <- lapply(test_input, str_to_vec)

locs <- find_code(test_vects)

print(paste0('Answer 1: ',paste(unlist(lapply(locs,keypad_loc)), collapse='')))

#Part 2

keypad_diag <-
rbind(c(NA,NA, 1,NA,NA),
      c(NA, 2, 3, 4,NA),
      c(5, 6, 7, 8, 9),
      c(NA,'A','B','C',NA),
      c(NA,NA,'D',NA,NA))

move_key_diag(3, 1, 'U') %as% c(3, 1)
move_key_diag(2, 2, 'U') %as% c(2, 2)
move_key_diag(3, 2, 'U') %as% c(2, 2)
move_key_diag(4, 2, 'U') %as% c(3, 2)
move_key_diag(1, 3, 'U') %as% c(1, 3)
move_key_diag(2, 3, 'U') %as% c(1, 3)
move_key_diag(3, 3, 'U') %as% c(2, 3)
move_key_diag(4, 3, 'U') %as% c(3, 3)
move_key_diag(5, 3, 'U') %as% c(4, 3)
move_key_diag(2, 4, 'U') %as% c(2, 4)
move_key_diag(3, 4, 'U') %as% c(2, 4)
move_key_diag(4, 4, 'U') %as% c(3, 4)
move_key_diag(3, 5, 'U') %as% c(3, 5)
move_key_diag(3, 1, 'D') %as% c(3, 1)
move_key_diag(2, 2, 'D') %as% c(3, 2)
move_key_diag(3, 2, 'D') %as% c(4, 2)
move_key_diag(4, 2, 'D') %as% c(4, 2)
move_key_diag(1, 3, 'D') %as% c(2, 3)
move_key_diag(2, 3, 'D') %as% c(3, 3)
move_key_diag(3, 3, 'D') %as% c(4, 3)
move_key_diag(4, 3, 'D') %as% c(5, 3)
move_key_diag(5, 3, 'D') %as% c(5, 3)
move_key_diag(2, 4, 'D') %as% c(3, 4)
move_key_diag(3, 4, 'D') %as% c(4, 4)
move_key_diag(4, 4, 'D') %as% c(4, 4)
move_key_diag(3, 5, 'D') %as% c(3, 5)
move_key_diag(1, 3, 'L') %as% c(1, 3)
move_key_diag(2, 2, 'L') %as% c(2, 2)
move_key_diag(2, 3, 'L') %as% c(2, 2)
move_key_diag(2, 4, 'L') %as% c(2, 3)
move_key_diag(3, 1, 'L') %as% c(3, 1)
move_key_diag(3, 2, 'L') %as% c(3, 1)
move_key_diag(3, 3, 'L') %as% c(3, 2)
move_key_diag(3, 4, 'L') %as% c(3, 3)
move_key_diag(3, 5, 'L') %as% c(3, 4)
move_key_diag(4, 2, 'L') %as% c(4, 2)
move_key_diag(4, 3, 'L') %as% c(4, 2)
move_key_diag(4, 4, 'L') %as% c(4, 3)
move_key_diag(5, 3, 'L') %as% c(5, 3)
move_key_diag(1, 3, 'R') %as% c(1, 3)
move_key_diag(2, 2, 'R') %as% c(2, 3)
move_key_diag(2, 3, 'R') %as% c(2, 4)
move_key_diag(2, 4, 'R') %as% c(2, 4)
move_key_diag(3, 1, 'R') %as% c(3, 2)
move_key_diag(3, 2, 'R') %as% c(3, 3)
move_key_diag(3, 3, 'R') %as% c(3, 4)
move_key_diag(3, 4, 'R') %as% c(3, 5)
move_key_diag(3, 5, 'R') %as% c(3, 5)
move_key_diag(4, 2, 'R') %as% c(4, 3)
move_key_diag(4, 3, 'R') %as% c(4, 4)
move_key_diag(4, 4, 'R') %as% c(4, 4)
move_key_diag(5, 3, 'R') %as% c(5, 3)
seal(move_key_diag)

find_key_diag <- function(char_vec, start_pos = c(3, 1)) {
  if (length(char_vec) == 0) {
    return(start_pos)
    break
  }
  
  end_pos <-
    move_key_diag(start_pos[1], start_pos[2], char_vec[1])
  
  find_key_diag(char_vec[-1],end_pos)
  
}

find_code_diag <- function(input_list, start_pos = c(3, 1), result = list()) {
  if (length(input_list) == 0) {
    return(result)
    break
  }
  
  end_pos <- find_key_diag(input_list[[1]],start_pos)
  
  result <- append(result, list(end_pos))
  
  find_code_diag(input_list[-1],end_pos,result)
  
}

keypad_diag_loc <- function(x) {
  keypad_diag[x[1], x[2]]
}

locs <- find_code_diag(test_vects)

print(paste0('Answer 1: ',paste(unlist(lapply(locs,keypad_diag_loc)), collapse='')))