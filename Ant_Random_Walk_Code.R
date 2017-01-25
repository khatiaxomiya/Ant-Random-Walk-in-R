## Simple Random walk by ant in an square arena
# The ant starts random walk from center grid
# The food is at last grid
# Random walk will continue until it reaches last grid



# The no of rows and cols in the grid are x
x <- 10

empty <- numeric(x^2)
grid <- matrix(empty, nrow=x, ncol=x)



r <- as.integer(x/2) #Initial row no
c <- as.integer(x/2) #Initial col no

grid[r,c] = 1 #Initial position


while(r+c < x*2){
  
  choice_r <- c(1,-1, 0)
  
  step_r <- sample(choice, 1)
  step_c <- sample(choice, 1)
  
  # Choosing step in row
  if(r+step_r > 0 & r+step_r < x+1) {
    r = r+step_r
  } else {
    r = r-step_r
  }
  
  if(c+step_c > 0 & c+step_c < x+1) {
    c = c+step_c
  } else {
    c = c-step_c
  }
  
  grid[r,c] <- grid[r,c] + 1
}

print(grid)
print(sum(sum(grid)))
