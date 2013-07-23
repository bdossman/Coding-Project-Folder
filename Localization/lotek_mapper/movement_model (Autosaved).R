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
n <- 20			# number of locations

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

S <- cbind(S, rep(5,n))
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
#
# 				Antenna layout file: where the antennas are in space
#
#######################################################################
library(rgl)
library(circular)

## Creating Antenna Layout (Circle)

r = 200		# in meters
angular.steps =c( pi/4, pi/2, 3/4*pi, pi, 5/4*pi, 3/2*pi, 7/4*pi,0)

Easting <- r*sin(angular.steps)
Northing <- r*cos(angular.steps)
Elevation <- rep(10, length(angular.steps))	# Height of Towers

ID <- letters[1:length(angular.steps)]
Type <- rep("5", length(angular.steps))
Location <- paste("Tower",1:length(angular.steps), sep=" ")
Orientation <- rep(0, length(angular.steps))

circle_ant_layout <- cbind(ID,Type,Location,Easting,Northing,Elevation,Orientation)

write.table(circle_ant_layout,"circle_ant_layout.csv",sep=",")

# Creating Football Field Antenna Layout
n.towers = 9
Easting <- c(rep(-50,3),rep(0,3),rep(50,3))
Northing <- rep(c(-25,0,25),3)
Elevation <- rep(10, length(n.towers))	# Height of Towers
ID <- letters[1:n.towers]
Type <- rep("5", n.towers)
Location <- paste("Tower",1:n.towers, sep=" ")
Orientation <- rep(0, n.towers)

football_ant_layout <- cbind(ID,Type,Location,Easting,Northing,Elevation,Orientation)

write.table(football_ant_layout,"football_ant_layout.csv",sep=",")

# Loading antenna layout files

ant.layout.filename <- "circle_ant_layout.csv"


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

read.antenna <- function(filename) {
  f <- file(filename, "r")
  apd <- scan(f, n=2, quiet=TRUE)
  ## The gain pattern matrix has theta.Orfan changing by row, phi.Orfan changing by column.
  pat <- matrix(scan(f, quiet=TRUE), nrow=apd[1], ncol=apd[2], byrow=TRUE)
  close(f)

  ## angular resolution:
  th.step <- 360 / (apd[1] - 1)
  phi.step <- 90 / (apd[2] - 1)

  ## remap to our coordinates; create a matrix with phi changing by column, theta by row
  g <- NA * pat

  ## Our equivalents of Orfanidis' coordinates
  th <- c(rad(phi.step * (col(pat)-1)))
  phi<- c(rad(90 - th.step * (row(pat)-1)))

  ## This laborious conversion works but there is a cleaner way that I wasn't
  ## able to figure out.

  x <- cos(phi)*cos(th)
  y <- cos(phi)*sin(th)
  z <- sin(phi)

  phi2 <- deg(abs(asin(z)))
  th2 <- deg(atan2(y, x)) %% 360

  g[cbind(1 + round(th2 / th.step), 1 + round(phi2 / phi.step))] <- c(pat)

  ## fill in the other half by symmetry
  th2 <- 360 - th2
  g[cbind(1 + round(th2 / th.step), 1 + round(phi2 / phi.step))] <- c(pat)

  return(structure(g, phi.step=phi.step, th.step=th.step, phi.step.rad=rad(phi.step), th.step.rad=rad(th.step)))
}

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

## Read the antenna layout file

read.layout <- function(filename) {
  x <- read.csv(filename, header=TRUE, as.is=TRUE)
  rownames(x) <- x$ID
  x$Type <- as.character(x$Type)
  x
}

## transform points in 3d:

## 1: rotate around the origin in the xz-plane by theta degrees
## 2: scale by "scale" w.r.t. the origin
## 3: shift by offs along x, y, z axes

## l: list with components x, y, and z
## returns new list with components x, y, and z

tx3d <- function(l, offs=c(0, 0, 0), scale = 1, theta = 0) {
  theta <- rad(90 - theta)
  ct <- cos(theta)
  st <- sin(theta)
  list (x = offs[1] + scale * (l$x * ct - l$z * st),
        y = offs[3] + scale * l$y,
        z = offs[2] + scale * (l$z * ct + l$x * st))
}

map.set <- function(ants, pats, dat, win.time=60) {

  ## break up time into slots
  t <- as.numeric(dat$ts)
  tlen <- diff(range(t))
  nslot <- ceiling(tlen / win.time)
  t.cuts <- min(t) + tlen * (0:nslot) / nslot
  ## weird: why is this necessary for getting the top couple of values included?
  t.cuts[nslot+1] <- t.cuts[nslot+1] * 1.1
  
  win <- unclass(cut(t, t.cuts, include.lowest=TRUE))

  x <- y <- z <- code <- fit <- iter <- rep(NA, nslot)
  
  for (i in 1:nslot) {
    ## catch errors so that we fit the model whenever possible
    tryCatch({
      mod <- map(ants, pats, dat[win==i,])
      x[i] <- mod$par[1]
      y[i] <- mod$par[2]
      z[i] <- mod$par[3]
      code[i] <- mod$convergence
      fit[i] <- mod$value
      iter[i] <- mod$counts[1]
    }, error=function(e) print(paste("map.set error: ", as.character(e))))
  }
  return(data.frame(Easting=x, Northing=y, Elevation=z, t=min(t) + tlen * (1:nslot - 0.5) / nslot, code=code, fit=fit, iter=iter))
}

plot.pat <- function(ant, loc=c(0, 0, 0), scale=1, orient = 90, legend=TRUE) {
 
  ## convert gain to log scale and set floor at -20 db
  g <- 10*log10(.Machine$double.eps+ant)
  g <- g - max(g)
  g <- 20 + pmax(g, -20)

  ## get spherical coordinates
  th <- rad(attr(ant, "th.step") * (col(g)-1))
  phi<- rad(attr(ant, "phi.step") * (row(g)-1))
  
  ## colour palette
  hc <- heat.colors(201)
  
  ## plot points
  rgl.points(tx3d(list(x=g*cos(phi)*cos(th), y=g*sin(phi), z=g*cos(phi)*sin(th)), loc, scale, orient), col=hc[1+c(round(10*g))])

  ## reflect in the xz plane by negating y (table only holds values for phi >= 0)
  rgl.points(tx3d(list(x=g*cos(phi)*cos(th), y=g*sin(phi), z=-g*cos(phi)*sin(th)), loc, scale, orient), col=hc[1+c(round(10*g))])

  if (legend) {
    
    ## plot a gain legend
    
    rgl.texts(tx3d(list(x=20, y=2.5*-1:10, z=20), loc, scale, orient), text=c(paste(-20 + 2*(0:10), "db Max"), "Gain"), col=c(hc[1+20*(0:10)], "#FFFFFF"))
    
    ## show positive axes
    
    rgl.lines(tx3d(list(x=c(0,30), y=c(0,0), z=c(0,0)), loc, scale, orient),col="green")
    rgl.texts(tx3d(list(x=30, y=0, z=0), loc, scale, orient),text="X")
    rgl.lines(tx3d(list(x=c(0,0), y=c(0,30), z=c(0,0)), loc, scale, orient),col="green")
    rgl.texts(tx3d(list(x=0, y=30, z=0), loc, scale, orient),text="Z")
    rgl.lines(tx3d(list(x=c(0,0), y=c(0,0), z=c(0,30)), loc, scale, orient),col="green")
    rgl.texts(tx3d(list(x=0, y=0, z=30), loc, scale, orient),text="Y")
  }
}

################################################################################
#
# 							XYZ coords into power
#
################################################################################

## read antenna pattern

setwd("/Users/Dossman/Desktop/Coding Project Folder/Localization/lotek_mapper")
pats <- lapply(ant.pattern.filenames, read.antenna)

setwd("./data")
## read antenna layout
ants <- read.layout(ant.layout.filename)

## Making omnidirectional antennas/ isotropic

iso.pats <- pats
iso.pats[["5"]][] <- 1  ## uniform gain of 1 in all angular directions

## from movement data set "supersimple" generate power measurements from each
## antenna.

power_data_circle=list(Ant1=NULL,Ant2=NULL,Ant3=NULL,Ant4=NULL,Ant5=NULL,Ant6=NULL,Ant7=NULL,Ant8=NULL)

for (i in 1:nrow(supersimple)){
	power_data_circle$Ant1[i] <- pow(ants[1,],iso.pats,supersimple[i,1],supersimple[i,2],supersimple[i,3])
	power_data_circle$Ant2[i] <- pow(ants[2,],iso.pats,supersimple[i,1],supersimple[i,2],supersimple[i,3])
	power_data_circle$Ant3[i] <- pow(ants[3,],iso.pats,supersimple[i,1],supersimple[i,2],supersimple[i,3])
	power_data_circle$Ant4[i] <- pow(ants[4,],iso.pats,supersimple[i,1],supersimple[i,2],supersimple[i,3])
	power_data_circle$Ant5[i] <- pow(ants[5,],iso.pats,supersimple[i,1],supersimple[i,2],supersimple[i,3])
	power_data_circle$Ant6[i] <- pow(ants[6,],iso.pats,supersimple[i,1],supersimple[i,2],supersimple[i,3])
	power_data_circle$Ant7[i] <- pow(ants[7,],iso.pats,supersimple[i,1],supersimple[i,2],supersimple[i,3])
	power_data_circle$Ant8[i] <- pow(ants[8,],iso.pats,supersimple[i,1],supersimple[i,2],supersimple[i,3])
}

## from movement data set "S" generate power measurements from each
## antenna.

#power_data_circle_linear=list(Ant1=NULL,Ant2=NULL,Ant3=NULL,Ant4=NULL,Ant5=NULL,Ant6=NULL,Ant7=NULL,Ant8=NULL)

#for (i in 1:nrow(S)){
	#power_data_circle_linear$Ant1[i] <- pow(ants[1,],iso.pats,S[i,1],S[i,2],S[i,3])
	#power_data_circle_linear$Ant2[i] <- pow(ants[2,],iso.pats,S[i,1],S[i,2],S[i,3])
	#power_data_circle_linear$Ant3[i] <- pow(ants[3,],iso.pats,S[i,1],S[i,2],S[i,3])
	#power_data_circle_linear$Ant4[i] <- pow(ants[4,],iso.pats,S[i,1],S[i,2],S[i,3])
	#power_data_circle_linear$Ant5[i] <- pow(ants[5,],iso.pats,S[i,1],S[i,2],S[i,3])
	#power_data_circle_linear$Ant6[i] <- pow(ants[6,],iso.pats,S[i,1],S[i,2],S[i,3])
	#power_data_circle_linear$Ant7[i] <- pow(ants[7,],iso.pats,S[i,1],S[i,2],S[i,3])
	#power_data_circle_linear$Ant8[i] <- pow(ants[8,],iso.pats,S[i,1],S[i,2],S[i,3])
#}


############################################################################################################################
#
#									Alternative approach by modifying Johns Code to create
#
############################################################################################################################

do.test <- function(antfile, pats, np = 500, win=10, jitter=FALSE, simple=TRUE, init) {

  ants <- read.layout(antfile)
  
  xdim <- diff(range(ants$Easting))
  ydim <- diff(range(ants$Northing))
  zdim <- diff(range(ants$Elevation))

  scale <- max(xdim, ydim, zdim)
  
  xc <- mean(ants$Easting)
  yc <- mean(ants$Northing)
  zc <- mean(ants$Elevation)
  
  #if (stationary=TRUE) {		## simulates a stationary tag point amongst centered at init=c(x,y,z)
  
  	x <- rep(init[1], np)
  	y <- rep(init[2], np)
  	z <- rep(init[3], np)
  
  }
  #if(stationary=FALSE){

  	v <- 2    ## mean speed in metres/second
  	vth <- 0   ## mean angular speed in degrees/second
  	hmax <- 100 ## maximum height, in m
  	hstep <- 0 ## mean change in height, in metres / second
  
  	## generate motion steps, using an exponential distribution of
  	## step lengths and turning angles

  	step.r <- rexp(np, 1 / v)

  	## generate turning angles, with random signs
  	turn.th <- c(pi/4,rep(0,np-1))

  dh <- rexp(np, 1/hstep)
  
  ## positions:

  theta <- rad(cumsum(turn.th))
  dz <- pmin(hmax, pmax(0, cumsum(dh)))  ## constrain within [0, hmax]

  x <- xc + cumsum(step.r * cos(theta))
  y <- yc + cumsum(step.r * sin(theta))
  z <- zc + dz

  }
  
  ## plot true path and antennas

  rgl.open()
  points3d(x, z, y, col="green")
  lines3d(x, z, y, col="green")

  nant <- dim(ants)[1]

  for(i in 1:dim(ants)[1])
    plot.pat(pats[[ants$Type[i]]], as.numeric(ants[i,4:6]), 2, as.numeric(ants[i, 7]), i == 1)

  ## for each time step, pick an antenna at random and determine
  ## the power it would receive from the target at its current location

  ant <- rep(1:nant, length=np)
  pow <- numeric(np)

  for (i in 1:np)
    pow[i] <- pow(ants[ant[i],], pats, x[i], y[i], z[i])yÿyt4er3r3rrfffrffe34

  if (jitter)
    pow <- jitter(pow)
  
  ## create a fake data frame from the path and antenna power:

  fake <- data.frame(ts = 1:np,
                     id = "fake",
                     antenna=as.character(ants$ID[ant]),
                     power = pow,                          ## FIXME: add some noise with jitter()
                     Easting = x,
                     Northing = y,
                     Elevation = z,
                     stringsAsFactors = FALSE
                     )


  ## try estimate locations from the fake data, in 10 second groups
  m <- map.set(ants, pats, fake, win)

  ## map the reconstructed path
  points3d(m$Easting, m$Elevation, m$Northing, color="red")

  lines3d(m$Easting, m$Elevation, m$Northing, color="red")

  bbox3d()

  return(list(fake=fake, fit=m))
}





