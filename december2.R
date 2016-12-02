library(magrittr)
library(plyr)
library(dplyr)
library(purrr)
#library(stringr)
library(lambda.r)

test_string <- 'RRDDD'

test_input <- c('ULL', 'RRDDD', 'LURDL', 'UUUUD')

str_to_vec <- function(x) { laply(seq(1, nchar(x)), function(i) substr(x, i, i)) }

test_vec <- str_to_vec(test_string)

test_vec[-1]

test_vects <- lapply(test_input, str_to_vec)

keypad <- t(matrix(c(1:9), nrow = 3, ncol = 3))

keypad

pos <- c(2, 2)

pat.match(2,2) %as% 5
pat.match(2,n) %as% n

pat.match(2,4)

pos[1]

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

move_key(2,2,'U')

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

    end_pos <- find_key(input_list[1],start_pos)

    result <- append(result, list(end_pos))

    find_code(input_list[-1],end_pos,result)

}

find_code(test_vects)

ret_key <- function(char_vec) {
    key_loc <- find_key(char_vec)
    keypad[key_loc[1], key_loc[2]]
}

ret_key(test_vec)

key_loc <- find_key(test_vec)

keypad[key_loc[1],key_loc[2]]