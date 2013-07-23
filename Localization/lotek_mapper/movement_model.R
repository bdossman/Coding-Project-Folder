##############################################################################
####				Simulating Simple Movement Model
##############################################################################
#	Bryant Dossman July 5, 2012
#	First attempt. Creating a simple fixed velocity and turning angle
#	model...i.e straight line movement
#	NOTE: To convert from slope(m) to angle (theta)----> theta = arctan(m)

###############################################################################

##	A simple linear movement model with constant velocity and direction in 
##	cartesian coordinate system using fixed angle and fixed velocity.
##	movement model --> S(t) = r(r,theta) also call supersimple model a
# 	stationary point within a ring of towers

##############################################################################

## intial conditions

rinit <- c(0,0)	# inital position
h <- 1				# timestep
n <- 100			# number of locations

## Supersimple Model w/ no movement

supersimple <- matrix(c(50,50,5), ncol = 3, nrow = n, byrow=TRUE)

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
#
# Using this simulated data create a dataset of radio hits on a collection of
# towers. Implementing portions of John's code (## svn: $Id: mapper.R 11
#2010-01-14 04:46:02Z john $)
################################################################################
#
#  Parameters
#
################################################################################

## Antenna layout file: where the antennas are in space
# TODO: antenna layout file make according your simulation project

ant.layout.filename <- "data/LongpointAntennas2009.csv"


ant.pattern.filenames <- list (
                               "5" = "yagi_5_pattern.txt",
                               "9" = "yagi_9_pattern.txt"
                               )

skip.ant <- c("OC-h", "Manual")


################################################################################
#
#  Functions
#
################################################################################

## Read the antenna layout file

read.layout <- function(filename) {
  x <- read.csv(filename, header=TRUE, as.is=TRUE)
  rownames(x) <- x$ID
  x$Type <- as.character(x$Type)
  x
}

## lookup antenna gain by spherical angles
## with phi and theta in radians

gain <- function(pat, phi, theta) {
  pat[round((c(theta) %% (2*pi)) / attr(pat, "th.step.rad")) + 1, round(pmin(abs(c(phi)), pi/2) / attr(pat, "phi.step.rad")) + 1]
}

## predicted relative power from target given range and spherical
## angles from antenna axis.  This is returned in dbm.

## pat is an antenna pattern matrix
## r is in metres; phi, theta are in radians
## P is transmitter power

## We square both 1/(r+1) and voltage pattern to get power.  The
## "+1" is to avoid the singularity at 0.

pow.sph <- function(pat, r, phi, theta, P=1) {
  ## Power in dbm
  10 * log10(P * (gain(pat, phi, theta)/(r+1))^2)

  ## Power in absolute units (use this instead if the Lotek 
  ## power measurement is not in log units)
  
  ## P * (gain(pat, phi, theta)/(r+1))^2
}

## predict power from target given its cartesian coordinates
## and a row from the antenna matrix

pow <- function(ant, pats, x, y, z, P=1) {
  ax <- ant$Easting
  ay <- ant$Northing
  az <- ant$Elevation
  r <- sqrt((x-ax)^2 + (y-ay)^2 + (z-az)^2)
  phi <- asin((z-az) / max(1, r))  ## use at least r = 1
  theta <- atan2(y - ay, x - ax) - rad(90 - ant$Orientation)
  
  pow.sph(pats[[ant$Type]], r, phi, theta, P)
}
