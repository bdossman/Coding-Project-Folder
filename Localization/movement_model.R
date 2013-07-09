##############################################################################
####				Simulating Simple Movement Model
###############################################################################				

##	Bryant Dossman July 5, 2012

##	First attempt. Creating a simple fixed velocity and turning angle model...i.e straight line movement
##	NOTE: To convert from slope(m) to angle (theta)----> theta = arctan(m)

###############################################################################

##	A simple linear movement model with constant velocity and direction in 
##	cartesian coordinate system using fixed angle and fixed velocity.
##	movement model --> S(t) = r(r,theta)

##############################################################################

## intial conditions

rinit <- c(0,0)	# inital position
h <- 1				# timestep
n <- 1000			# number of locations


## fixed parameters

r <- 10			# magnitude of movement (velocity)
theta <- pi/4	# equal to 45 degree angle

## simulating movement path

S <- matrix(0,nrow=n,ncol=2)
S[1,] <- rinit

for (i in 1:(n-1)){
	S[i+1,] <- S[i,] + c(r*cos(theta), r*sin(theta))
}

###############################################################################

## Using this simulated data create a dataset of radio hits on a collection of
## towers.

