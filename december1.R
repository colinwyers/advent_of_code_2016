library(magrittr)
library(purrr)
library(dplyr)

#l_test <- c('R5', 'L5', 'R5', 'R3')

l_test <- c('R1', 'R3', 'L2', 'L5', 'L2', 'L1', 'R3', 'L4', 'R2', 'L2', 'L4', 'R2', 'L1', 'R1', 'L2', 'R3', 'L1', 'L4', 'R2', 'L5', 'R3', 'R4', 'L1', 'R2', 'L1', 'R3', 'L4', 'R5', 'L4', 'L5', 'R5', 'L3', 'R2', 'L3', 'L3', 'R1', 'R3', 'L4', 'R2', 'R5', 'L4', 'R1', 'L1', 'L1', 'R5', 'L2', 'R1', 'L2', 'R188', 'L5', 'L3', 'R5', 'R1', 'L2', 'L4', 'R3', 'R5', 'L3', 'R3', 'R45', 'L4', 'R4', 'R72', 'R2', 'R3', 'L1', 'R1', 'L1', 'L1', 'R192', 'L1', 'L1', 'L1', 'L4', 'R1', 'L2', 'L5', 'L3', 'R5', 'L3', 'R3', 'L4', 'L3', 'R1', 'R4', 'L2', 'R2', 'R3', 'L5', 'R3', 'L1', 'R1', 'R4', 'L2', 'L3', 'R1', 'R3', 'L4', 'L3', 'L4', 'L2', 'L2', 'R1', 'R3', 'L5', 'L1', 'R4', 'R2', 'L4', 'L1', 'R3', 'R3', 'R1', 'L5', 'L2', 'R4', 'R4', 'R2', 'R1', 'R5', 'R5', 'L4', 'L1', 'R5', 'R3', 'R4', 'R5', 'R3', 'L1', 'L2', 'L4', 'R1', 'R4', 'R5', 'L2', 'L3', 'R4', 'L4', 'R2', 'L2', 'L4', 'L2', 'R5', 'R1', 'R4', 'R3', 'R5', 'L4', 'L4', 'L5', 'L5', 'R3', 'R4', 'L1', 'L3', 'R2', 'L2', 'R1', 'L3', 'L5', 'R5', 'R5', 'R3', 'L4', 'L2', 'R4', 'R5', 'R1', 'R4', 'L3')

string_to_tuple <- function(x) { list(turn = substring(x, 1, 1), dist = as.integer(substring(x, 2, nchar(x)))) }

tuple_to_coord <- function(tuple, heading) {

    heading <-
    case_when(tuple$turn == 'R' & heading == 'N' ~ 'E',
        tuple$turn == 'R' & heading == 'E' ~ 'S',
        tuple$turn == 'R' & heading == 'S' ~ 'W',
        tuple$turn == 'R' & heading == 'W' ~ 'N',
        tuple$turn == 'L' & heading == 'N' ~ 'W',
        tuple$turn == 'L' & heading == 'E' ~ 'N',
        tuple$turn == 'L' & heading == 'S' ~ 'E',
        tuple$turn == 'L' & heading == 'W' ~ 'S')
    
    list(x = case_when(heading == 'N' ~ 0L,
              heading == 'E' ~ tuple$dist,
              heading == 'S' ~ 0L,
              heading == 'W' ~ -tuple$dist),
     y = case_when(heading == 'N' ~ tuple$dist,
              heading == 'E' ~ 0L,
              heading == 'S' ~ -tuple$dist,
              heading == 'W' ~ 0L),
     heading = heading)
}

dist <- function(list, start=c(0,0), heading='N') {
    if (length(list) == 0) {
        return(start)
        break
    }

    travel <-
    string_to_tuple(list[1]) %>%
    tuple_to_coord(heading)

    end <- start+c(travel$x,travel$y)

    dist(list[-1],end,travel$heading)

}

#Part 1

print(paste0('Answer 1: ',sum(abs(dist(l_test)))))

#Part 2

second <- function(list, start = c(0, 0), heading = 'N', visited = list()) {
    if (length(list) == 0) {
        return(visited)
        break
    }

    travel <-
    string_to_tuple(list[1]) %>%
    tuple_to_coord(heading)

    end <- start + c(travel$x, travel$y)

    left <- if (abs(end[1] - start[1]) > 0) {
        start[1]:end[1]
    } else { rep(start[1], length(start[2]:end[2])) }
    right <- if (abs(end[2] - start[2]) > 0) {
        start[2]:end[2]
    } else { rep(start[2], length(start[1]:end[1])) }
    
    new_coords <- mapply(c, left, right, SIMPLIFY = FALSE)

    new_coords <- new_coords[-1]

    visited <- append(visited, new_coords)
    
    second(list[-1], end, travel$heading,visited)

}

positions <- second(l_test)

a <-which(duplicated(positions))

b <- positions[a[1]]

print(paste0('Answer 2: ',sum(abs(b[[1]]))))