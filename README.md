
# StormR

<!-- badges: start -->
<!-- badges: end -->

## Overview

The goal of StormR is to compute regimes of wind and other indices of interests generated by 
Tropical Cyclones over the whole world.


## Installation

You can install the development version of StormR like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```



## Data source 
Insert comments here

## Models

Two cyclonic models are available within this package in order to compute radial wind speed:

Willoughby et al. 2006:\\
Insert comments about the model here\\

$$
\left\{
\begin{aligned}
v_r &= msw\left(\frac{r}{rmw}\right)^{nn} \quad if \quad r <rmw\\
v_r &= msw((1-AA))e^{-\frac{|r-rmw|}{XX1}} + AA e^{-\frac{|r-rmw|}{XX2}}) \quad if \quad r \geq rmw\\
\end{aligned}
\right.
$$

where\\
$v_r$ radial wind speed (m/s)\\
$r$ distance to the eye of the storm where $v_r$ is computed (km)\\
$msw$: Maximum sustained wind speed (m/s)\\
$rmw$: Radius of maximum sustained wind speed (km)\\
$XX1 = 287.6 - 1.942msw + 7.799\log(rmw) + 1.819|\phi|$: Coefficient, $\phi$ being the latitude\\
$XX2 = 25$: Coefficient\\
$nn = 2.1340 + 0.0077msw - 0.4522\log(rmw) - 0.0038|\phi|$: Coefficient, $\phi$ being the latitude\\



Holland et al. 1980:\\
Insert comments about the model here\\


$ v_r = \sqrt{\frac{b}{\rho}\left(\frac{rmw}{r}\right)^b (poci - pc)e^{-\left(\frac{rmw}{r}\right)^b} + \left(\frac{rf}{2}\right)^2} - \left(\frac{rf}{2}\right)$\\

where\\
$v_r$: radial wind speed (m/s)\\
$r$ distance to the eye of the storm where $v_r$ is computed (km)\\
$msw$: Maximum sustained wind speed (m/s)\\
$rmw$: Radius of maximum sustained wind speed (km)\\
$pc$: Pressure at the eye of the storm (mb)\\
$poci$: Pressure at Outermost Closed Isobar of the storm (mb)\\
$\rho = 1.15$: Air density (kg/m3)\\
$f = 2 \times 7.29 \times10^{-5} \sin(\phi)$: Coriolis force, $\phi$ being the latitude\\
$b = \frac{\rho e \times msw^2}{poci - pc}$: Shape factor\\



It is also possible to use an empirical formula derived from Willoughby et al. 2006 model
to compute the radius of maximum wind speed, as follow:\\
$rmw = 46.4e^{(-0.0155msw + 0.0169|\phi|)}$




## Asymmetry

The above models compute symmetric 2D structures of radial wind speed around the
eye of the storm. Nevertheless, in most cases, tropical cyclone are not symmetric
and it can be interesting to add asymmetry to the computations in order to get a
much more accurate 2D structures of radial wind speed. This package proposes two formula
to add such asymmetry:\\

Version 1 (Boose et al. 2001 version):\\
$v_{r_{as}} = v_r - S(1-\sin(T))\frac{v_h}{2}$

Version 2:\\
$v_{r_{as}} = v_{r_{|v_h}} + v_h\cos(\theta)$

where
$v_{r_{as}}$: New radial wind speed with asymmetry (m/s)\\
$v_r$: Former radial wind speed without asymmetry (m/s)\\
$v_h$: Velocity of storm (m/s)\\
$v_{r_{|v_h}}$: Former radial wind speed without asymmetry (m/s), where values
have been computed substracting $v_h$ to $msw$ i.e $v_{max} = msw-v_h$ in the input
$S$: Asymmetry coefficient (usually set to 1)\\
$T$: Angle between the storm direction and the point where $v_{r_{as}}$ is computed.
Clockwise in Nothern hemisphere, counterclokwise in Southern hemisphere.\\
$\theta$: Angle between the storm direction and the direction of radial wind speed at the point where $v_{r_{as}}$ is computed\\


Insert comments about the differences here\\



## Usage

This is a basic example which shows you how to solve some common problems

``` r
library(StormR)

######################
#Focus on a single TC#
######################

#Load TC
sts = getStorm(time_period = 2015, name = "PAM", loi = "Vanuatu")

#Plot TC over the location of interest
plotStorms(sts, labels = T, legends = T)

#Compute Maximum Sustained Wind raster according to Willoughby et al. 2006 analytic model
#adding version 2 formula of asymmetry 
msw = stormBehaviour(sts, asymmetry = "V2", verbose = T)

#Plot the above raster alongside with the track of the storm
plotBehaviour(sts, msw, labels = T)


######################
#Focus on several TCs#
######################

#Load TC
sts = getStorm(time_period = c(2003, 2021), name = c("ERICA", "NIRAN"), loi = "New Caledonia")

#Plot TCs over the location of interest
plotStorms(sts, labels = T, legends = T)

#Plot NIRAN alone over the location of interest
plotStorms(sts, names = "NIRAN", labels = T)

#Compute Power Dissipation index raster according to Willoughby et al. 2006 analytic model
pdi = stormBehaviour(sts, product = "PDI" , verbose = T)

#Plot the PDI for ERICA alongside with the ERICA's track 
plotBehaviour(sts, pdi[["ERICA_PDI"]], labels = T)


#Compute time series of wind speed on coordinates contained in df according Willoughby et al. 2006 #analytic model, adding version 2 formula of asymmetry 
df = data.frame(lon = c(166.5, 166.7), lat = c(-22.1, - 22.3))
wind.ts = stormBehaviour(sts, result = df, verbose = T)

##############
#Get all TCs #
##############

#Get all TCs over Tropical Depression around the world between 1980 and 2021
sts = getStorm(basin = "SA", verbose = T)

#Plot TCs over category 5
plotStorms(sts, category = 5)


```

## Getting help

If you have any question or suggestion or if you want to report a bug, please do it via the GitHub issues.
Thanks for that, this would greatly help us to improve this package.


