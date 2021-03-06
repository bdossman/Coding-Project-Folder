## svn: $Id: mapper.R 11 2010-01-14 04:46:02Z john $
##
## estimate locations of targets based on multiple antenna hits, using
## the antenna radiation pattern

## FIXME: the relation of Lotek power number (0...255) to received
##        power in dbm needs to be established for this approach to
##        have much chance of working.  This is embodied in the
##        functions lotek.to.dbm and dbm.to.lotek, which are currently
##        just guesses.

## FIXME: the file yagi_pattern.m should be re-run in Octave after
##        precise measurements of the elements in the two Yagi
##        antennas have been made, and the receiver frequency is
##        known.

################################################################################
#
#  Parameters
#
################################################################################

## Data file: hits of a target by antennas

data.filename <- "data/btbw.89.csv"

## Antenna pattern file: the gain pattern for each antenna
## We put these into a list indexed by type, as a string.

## FIXME: use actual antenna measurements to generate patterns for all
##        antenna types.  (Currently, the octave code in
##        yagi_pattern.m uses a textbook example of a 5 element
##        antenna, which we use for every antenna below.)

ant.pattern.filenames <- list (
                               "5" = "yagi_5_pattern.txt",
                               "9" = "yagi_9_pattern.txt"
                               )

## Antenna layout file: where the antennas are in space

ant.layout.filename <- "data/LongpointAntennas2009.csv"

## antennas we don't use

skip.ant <- c("OC-h", "Manual")

################################################################################
#
#  Libraries
#
################################################################################

library(rgl)

################################################################################
#
#  Functions
#
################################################################################

## FIXME: the following two functions were pulled out of thin air.

## Convert Lotek power code to dbm

lotek.to.dbm <- function(p) {
  (p - 255) / 4
}

## Convert dbm to Lotek power code
dbm.to.lotek <- function(d) {
  d * 4 + 255
}

## Load the one-way antenna voltage pattern calculated by octave code in yagi_pattern.m
## This is a table of gains for a grid of phi, theta values.  The conventions
## used in Orfanidis' book and code imply the following mapping between his
## spherical angles and ours:
##
##   phi.table   = theta
##   theta.table = 90 - phi
##
## and we do a conversion to our coordinates here.

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
  
## plot the one-way antenna E gain pattern in 3d
## ant: antenna pattern matrix
## loc: location of centre of plot
## scale of plot (max plot point is is 20 * scale from loc)
## orient: compass angle of orientation, in degrees clockwise from north

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

  
## Estimate the position of the target, given hits from multiple antennas.
## The position is assumed to be constant over the duration of the set of hits.
##
## ants: antenna layout data.frame, returned by read.layout
## pats: list of antenna radiation pattern matrices, indexed by antenna type
## dat:  target hits

map <- function(ants, pats, dat) {
  ## antenna coordinates
  
  ax <- ants[dat$antenna, "Easting"]
  ay <- ants[dat$antenna, "Northing"]
  az <- ants[dat$antenna, "Elevation"]
  aazi <- rad(90 - ants[dat$antenna, "Orientation"])  ## convert from compass to math angle
  atype <- ants[dat$antenna, "Type"]

  dat$dbm <- lotek.to.dbm(dat$power)
  
  ## given estimated target position and transmitter power, get
  ## deviance between observed and predicted power
  
  dev <- function(x) {
    ## x[1:3]: target x, y, and z position

    ## compute offset from each antenna to target in spherical coordinates
    ## KLUDGE: force r to have a minimum value so that phi is defined

    ## do assignments in the parent frame so that if debugging is enabled
    ## by uncommenting the browser() call below, the variables set by
    ## dev can be examined.
    
    r <<- pmax(1, sqrt((x[1] - ax)^2 + (x[2] - ay)^2 + (x[3] - az)^2))

    theta <<- atan2(x[2] - ay, x[1] - ax)

    phi <<- asin((x[3] - az) / r)

    ## For each antenna, predict power and get deviance
    pp <<- numeric(length(ax))
    for (i in seq(along=ax))
      ## predict power
      pp[i] <<- pow.sph(pats[[atype[i]]], r[i], phi[i], theta[i] - aazi[i])
    
    rv <- sum((dat$dbm - pp)^2)

    if (is.finite(rv))
      return(rv)

    return(1e6)  ## bogus large value
  }

  ## initial target position estimate is centroid of antenna positions
  cent <<- c(mean(ax), mean(ay), mean(az))
  
  mod <- optim(cent, dev)
##  browser()
  return(mod)
}


## map a set of locations for a dataset, breaking up the time
## into non-overlapping windows
## ants: antenna layout data.frame, returned by read.layout
## pats: list of antenna radiation pattern matrices, indexed by antenna type
## dat:  target hits
## win.time: size of non-overlapping time windows
    
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

## plot the map, including antennas
plot.map <- function(dat, res) {

  ## use only good points
  res <- subset(res, code < 3 & fit >= 0 & !is.na(Easting))

  ## map the points, with digit symbols to indicate ordering in time
  plot(res$Easting, res$Northing, type="b", pch=as.character(0:9))

  ## antenna number as numeric, not factor, so we don't have unused levels
  ant <- as.numeric(dat$antenna)

  ## map the antennas
  ant.x <- tapply(dat$Easting, ant, mean)
  ant.y <- tapply(dat$Northing, ant, mean)
  ant.azi <- tapply(dat$orient, ant, mean)

  ## arrow length, in metres
  arr.len <- 100
  ant.x2 <-ant.x + cos(ant.azi) * arr.len
  ant.y2 <-ant.y + sin(ant.azi) * arr.len
    
  ## plot triangles at antennas, along with arrows for orientation
  points(ant.x, ant.y, pch=2, col=seq(along=ant.x))

  ## plot arrows
  arrows(ant.x, ant.y, ant.x2, ant.y2, col="red")
}
  

## read a data file and make some fixups
read.data <- function(filename) {

  dat <- read.csv(filename)

  ## convert antenna code back to character string
  dat$antenna <- as.character(dat$antenna)

  ## remove hits for unused antennas
  dat <- dat[!(dat$antenna %in% skip.ant),]

  ## give timestamps a class
  dat$ts<-as.POSIXct(as.character(dat$ts))

  return(dat)
}

## make fake data and test the method
## np: number of points tested
## win: size of window to test with

do.test <- function(antfile, pats, np = 500, win=10, jitter=FALSE) {

  ants <- read.layout(antfile)
  
  xdim <- diff(range(ants$Easting))
  ydim <- diff(range(ants$Northing))
  zdim <- diff(range(ants$Elevation))

  scale <- max(xdim, ydim, zdim)
  
  xc <- mean(ants$Easting)
  yc <- mean(ants$Northing)
  zc <- mean(ants$Elevation)

  v <- max(c(scale / 50, 10))    ## mean speed in metres/second
  vth <- 5   ## mean angular speed in degrees/second
  hmax <- 100 ## maximum height, in m
  hstep <- .1 ## mean change in height, in metres / second
  
  ## generate motion steps, using an exponential distribution of
  ## step lengths and turning angles

  step.r <- rexp(np, 1 / v)

  ## generate turning angles, with random signs
  turn.th <- rexp(np, 1 / vth) * floor(runif(np, 0,2) - 1) 

  dh <- rexp(np, 1/hstep)
  
  ## positions:

  theta <- rad(cumsum(turn.th))
  dz <- pmin(hmax, pmax(0, cumsum(dh)))  ## constrain within [0, hmax]

  x <- xc + cumsum(step.r * cos(theta))
  y <- yc + cumsum(step.r * sin(theta))
  z <- zc + dz

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
    pow[i] <- dbm.to.lotek(pow(ants[ant[i],], pats, x[i], y[i], z[i]))

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
  

  
################################################################################
#
#  Processing
#
################################################################################

## read antenna pattern
pats <- lapply(ant.pattern.filenames, read.antenna)
    
## read antenna layout
ants <- read.layout(ant.layout.filename)

## read data
dat <- read.data(data.filename)

## estimate target positions at 5 minute intervals
m <- map.set(ants, pats, dat, 300)

## plot them
plot.map(dat, m)

## test synthetic data with real radiation pattern and layout:
do.test("data/LongpointAntennas2009.csv", pats, 140, 7)
title3d(main=c("Test: Yagi antennas in flat layout"))

## test with real antenna radiation pattern but thick layout:
do.test("data/FakeAntennas.csv", pats, 140, 7)
title3d(main=c("Test: Yagi antennas in thick layout"))

## test with isotropic radiation pattern and thick layout:
iso.pats <- pats
iso.pats[["5"]][] <- 1  ## uniform gain of 1 in all angular directions

do.test("data/FakeAntennas.csv", iso.pats, 140, 7)
title3d(main=c("Test: Isotropic antennas in thick layout"))

## test with isotropic radiation pattern and thick layout, but jittered power,
## to assess robustness
do.test("data/FakeAntennas.csv", iso.pats, 140, 7, TRUE)
title3d(main=c("Test: Isotropic antennas in thick layout; power jittered"))

## test with isotropic radiation pattern and real layout:
do.test("data/LongpointAntennas2009.csv", iso.pats, 140, 7)
title3d(main=c("Test: Isotropic antennas in flat layout"))
