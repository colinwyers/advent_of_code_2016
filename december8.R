library(lambda.r)
library(readr)

#str1 <- 'rect 3x2'
#str2 <- 'rotate column x=1 by 1'
#str3 <- 'rotate row y=0 by 4'
#str4 <- 'rotate column x=1 by 1'

#str_list <- list(str1,str2,str3,str4)

str_list <- read_lines('input_day_8.txt')

Grid_rect(a,b) %as% list(a=a,b=b)
Grid_rotate(axis,a,b) %as% list(axis=axis,a=a,b=b)

str_to_vector <- function (x) {
  if ( sub("rect ([0-9]+)x([0-9]+)","\\1;\\2",x) != x ) {
    temp <- unlist(strsplit(sub("rect ([0-9]+)x([0-9]+)","\\1;\\2",x), split=";"))
    Grid_rect(as.integer(temp[1]),as.integer(temp[2]))
  } else if ( sub("rotate ([a-z]+) ([a-z])=([0-9]+) by ([0-9]+)","\\1;\\2;\\3;\\4",x) != x ) {
    temp <- unlist(strsplit(sub("rotate ([a-z]+) ([a-z])=([0-9]+) by ([0-9]+)","\\1;\\2;\\3;\\4",x), split=";"))
    Grid_rotate(temp[2],as.integer(temp[3]),as.integer(temp[4]))
  } else {
    NA
  }
}

#rm(grid_move)

grid_move(grid,x) %::% matrix : Grid_rect : matrix
grid_move(grid,x) %as% { grid[1:x$b,1:x$a] <- 1
                         return(grid)}

grid_move(grid,x) %::% matrix : Grid_rotate : matrix
grid_move(grid,x) %as% { if (x$axis == 'x') {grid[,(x$a+1)] <- c(tail(grid[,(x$a+1)],x$b),head(grid[,(x$a+1)],-x$b))
                                             grid} else { grid[(x$a+1),] <- c(tail(grid[(x$a+1),],x$b),head(grid[(x$a+1),],-x$b))
                                             grid } }



#grid_move(grid,x) %when% {x$axis == 'x'} %as% { grid[,(x$a+1)] <- c(tail(grid[,(x$a+1)],x$b),head(grid[,(x$a+1)],-x$b))
#                                               grid }
#grid_move(grid,x) %when% {x$axis == 'y'} %as% { grid[(x$a+1),] <- c(tail(grid[(x$a+1),],x$b),head(grid[(x$a+1),],-x$b))
#                                               grid }
#grid_move(grid,x) %as% NA

grid_find <- function(input_list,grid=matrix(, nrow = 3, ncol = 7)) {
  if (length(input_list) == 0) {
    return(grid)
    break
  }
  
  new_grid <- grid_move(grid,str_to_vector(input_list[[1]]))
  
  grid_find(input_list[-1],new_grid)
  
}

paste0('Answer 1: ',sum(grid_find(str_list,matrix(, nrow = 6, ncol = 50)),na.rm = TRUE))

result_grid <- grid_find(str_list,matrix(, nrow = 6, ncol = 50))

result_grid[1:6,1:5]
result_grid[1:6,6:10]
result_grid[1:6,11:15]
result_grid[1:6,16:20]
result_grid[1:6,21:25]
result_grid[1:6,26:30]
result_grid[1:6,31:35]
result_grid[1:6,36:40]
result_grid[1:6,41:45]
result_grid[1:6,46:50]

print('Answer 2: ZFHFSFOGPO')
