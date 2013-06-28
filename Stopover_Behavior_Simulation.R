#######################################################################################							 Stopover Ecology Simulation
####################################################################################

## OBJECTIVE: to simulate the stopover behavior of passerines given a hetergenous distribution of available resources

# Initial Conditions

time_steps <- 10 # number of days to run
location_positions <- matrix(0,time_steps,2)
habitat_matrix <- matrix(c(1,1,1,1,1, rep(0,20)),5,5,byrow=TRUE)

## function returns true false statement to inform whether a bird departs or not

is_full <- function (R){
	# 4 is a random index of condition that will determine the point at which a bird 	# should depart
	ifelse(R>=4,1,0)
	}

## function that updates a birds fuel reserves 
# R is a time series vector
# location is a two value vector x,y respectively

condition_update <- function(R, location){
	ifelse(M[location]==1,return(R+1), return(R))
	# ifelse statement puts out vector of length 2 only the first value is returned
}

## Simulate movement steps and extract your position

move <- function(current_location){
	new_location <- current_location + c(rbinom(1,1,1),rbinom(1,1,1))
	return(new_location)
}






