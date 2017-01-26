## Simple Random walk by ant in an square arena
# The ant starts random walk from center grid
# The food is at last grid
# Random walk will continue until it reaches last grid



# The no of rows and cols in the grid are x
x <- 100

# Creating the arena as an empty matrix
empty <- numeric(x^2)
grid <- matrix(empty, nrow=x, ncol=x)


# Putting the ant into the center of th arena
r <- as.integer(x/2) #Initial row no
c <- as.integer(x/2) #Initial col no

grid[r,c] = 1 #Initial position

# Creating lists to put all the step coordinates
row_list <- numeric()
col_list <- numeric()

# Putting the initial position in the lists
row_list[1] <- r
col_list[1] <- c


# Variable for counting number of steps
step_count <- 0


while(r+c < x*2){
  
  direction <- c(1,-1, 0)
  
  step_r <- sample(direction, 1)
  step_c <- sample(direction, 1)
  
  # Choosing step in row
  if(r+step_r > 0 & r+step_r < x+1) {
    r = r+step_r
  } else {
    r = r-step_r
  }
  
  # Choosing step in column
  if(c+step_c > 0 & c+step_c < x+1) {
    c = c+step_c
  } else {
    c = c-step_c
  }
  
  # Registering the step in grid and step count
  grid[r,c] <- grid[r,c] + 1
  step_count <- step_count + 1
  
  # Putting coordinates into lists
  row_list[step_count+1] <- r
  col_list[step_count+1] <- c
  
}

# Printing the result
cat("The number of steps taken:", step_count)

df_step <- cbind(col_list, row_list)
plot(df_step, type = "l", xlim = c(1,x), ylim = c(1,x))

