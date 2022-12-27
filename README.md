

# StormR

<!-- badges: start -->
<!-- badges: end -->



## Installation

You can install the development version of stormR like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Overview

stormR is a package developped to analyze past storms and tropical cyclones that occured in the whole
world beyond 1980. It let the user gather all its interested storms in a single object and then use it
to either plot storms on a map, or compute/rasterize regimes of wind speed, 2D structures of radial wind and other products.



## Data source 
stormR uses the netcdf file 'IBTrACS.ALL.v04r00.nc' that comes from the [International Best Track Archive for Climate Stewardship](https://www.ncei.noaa.gov/products/international-best-track-archive). It let this package get every informations needed to plot, compute and analyze tropical cyclones. This data base provides observations every 3 hours of storms and tropical cyclones on the whole world that occured from 1841 to present days (although observations of most recent storms are not available yet). Observations comes from a various number of agencies, nevertheless, this package only supplies observations extracted from the USA agency as it is the one that comes up with the most prolific and reliable data. The following lists all the basin names:

* NA : North Atlantic
* SA : South Atlantic
* EP : Eastern North Pacific
* WP : Western North Pacific
* SP : South Pacific
* SI : South Indian
* NI : North Indian
* ALL: the above 7 basin all together

Default value for the basin is set to SP as the developpement team are located and interested in this particular region, but it is up to the user to select his basin of interest.
For the sake of data reability, stormR focuses only on storms and tropical cyclones that occured beyond 1980. A research for older stormR will result in an error. [Click here](https://www.ncei.noaa.gov/sites/default/files/2021-07/IBTrACS_version4_Technical_Details.pdf) for a deeper insight on the documentation of this 'IBTrACS.ALL.v04r00.nc' file.

## Models

Two cyclonic models are available within this package in order to compute radial wind speed:

$\textbf{Willoughby et al. 2006}$ <br />
Insert comments about the model here <br />

$$
\left\{
\begin{aligned}
v_r &= msw\left(\frac{r}{rmw}\right)^{nn} \quad if \quad r < rmw \\
v_r &= msw((1-AA))e^{-\frac{|r-rmw|}{XX1}} + AA e^{-\frac{|r-rmw|}{XX2}}) \quad if \quad r \geq rmw \\
\end{aligned}
\right.
$$


where <br />
$v_r \quad$ Radial wind speed $(m.s^{-1})$ <br />
$r \quad$ Distance to the eye of the storm where $v_r$ is computed ($km$) <br />
$msw \quad$ Maximum sustained wind speed $(m.s^{-1})$ <br />
$rmw \quad$ Radius of maximum sustained wind speed $(km)$ <br />
$XX1 = 287.6 - 1.942msw + 7.799\log(rmw) + 1.819|\phi| \quad$ Coefficient, $\phi$ being the latitude <br />
$XX2 = 25 \quad$ Coefficient <br />
$nn = 2.1340 + 0.0077msw - 0.4522\log(rmw) - 0.0038|\phi| \quad$ Coefficient, $\phi$ being the latitude <br />



$\textbf{Holland et al. 1980}$ <br />
Insert comments about the model here <br />


$$
v_r = \sqrt{\frac{b}{\rho}\left(\frac{rmw}{r}\right)^b (poci - pc)e^{-\left(\frac{rmw}{r}\right)^b} + \left(\frac{rf}{2}\right)^2} - \left(\frac{rf}{2}\right)
$$

where <br />
$v_r \quad$ Radial wind speed $(m.s^{-1})$ <br />
$r \quad$ Distance to the eye of the storm where $v_r$ is computed $(km)$ <br />
$msw \quad$ Maximum sustained wind speed $(m.s^{-1})$ <br />
$rmw \quad$ Radius of maximum sustained wind speed $(km)$ <br />
$pc \quad$ Pressure at the eye of the storm $(mb)$ <br />
$poci \quad$ Pressure at Outermost Closed Isobar of the storm $(mb)$ <br />
$\rho = 1.15 \quad$ Air density $(kg.m^{-3})$ <br />
$f = 2 \times 7.29 \times10^{-5} \sin(\phi) \quad$ Coriolis force, $\phi$ being the latitude <br />
$b = \frac{\rho e \times msw^2}{poci - pc} \quad$ Shape factor <br />



It is also possible to use an empirical formula derived from Willoughby et al. 2006 model
to compute the radius of maximum wind speed, as follow: <br />
$rmw = 46.4e^{(-0.0155msw + 0.0169|\phi|)}$




## Asymmetry

The above models compute symmetric 2D structures of radial wind speed around the
eye of the storm. Nevertheless, in most cases, tropical cyclone are not symmetric
and it can be interesting to add asymmetry to the computations in order to get a
much more accurate 2D structures of radial wind speed. This package proposes two formula
to add such asymmetries: <br />

$\textbf{Version 1 (Boose et al. 2001 version)}$ <br />
Insert comments here  <br />
$v_{r_{as}} = v_r - S(1-\sin(\alpha))\frac{v_h}{2}$

$\textbf{Version 2}$ <br />
Insert comments here  <br />
$v_{r_{as}} = v_{r_{|v_h}} + v_h\cos(\theta)$

where <br />
$v_{r_{as}} \quad$ New radial wind speed with asymmetry $(m.s^{-1})$ <br />
$v_r \quad$ Former radial wind speed without asymmetry $(m.s^{-1})$ <br />
$v_h \quad$ Velocity of storm $(m.s^{-1})$ <br />
$v_{r_{|v_h}} \quad$ Former radial wind speed without asymmetry $(m.s^{-1})$, where values
have been computed substracting $v_h$ to $msw$ i.e $v_{max} = msw-v_h$ in the input
$S$ Asymmetry coefficient (usually set to 1) <br />
$\alpha \quad$ Angle between the storm direction and the point where $v_{r_{as}}$ is computed.
Clockwise in Nothern hemisphere, counterclokwise in Southern hemisphere. <br />
$\theta \quad$ Angle between the storm direction and the direction of radial wind speed at the point where $v_{r_{as}} \quad$ is computed <br />


Insert comments about the differences here? <br />


## Products
stormR let the user compute several products. They can either be computed on 
specific longitude/latitute coordinates or rasterized over the location of interest.
The following describes the products available: <br />


* Maximum Sustained Wind speed (MSW). It provides the value of the maximum sustained wind speed $(m.s^{-1})$
  at distance $r$ of the eye of the storm according to 
  
$$
\max(v_r(t) | t \in [0,T])
$$

  where $T$ stands for the whole lifecycle of the storm. <br />

* Power Dissipation Index (PDI): It provides the value of the PDI at distance $r$ of the eye of the     storm according to

$$
\int_T \rho C_d v_r^3 dt
$$

  $T$ stands for the whole lifecycle of the storm, $\rho$ represents the air density fixed here at      $10^{-3}$ $kg.m^{-3}$ ? Finally, $C_d$ models the drag coefficient of the storm. Although there exist       various methods and formula to compute this parameter that are widely debatable, we chose here the     following parametrization derived in [Wang, G., Wu, L., Mei, W. et al. Ocean currents show global     intensification of weak tropical cyclones. Nature 611, 496–500 (2022)](     https://doi.org/10.1038/s41586-022-05326-4): 
  
$$
\left\{
\begin{aligned}
C_d &= (0.8 + 0.06v_r) \times 10^{-3} \quad if \quad v_r \leq 31.5 \\
C_d &= \left(0.55 + 2.97\frac{v_r}{31.5} - 1.49\left(\frac{v_r}{31.5}\right)^2\right) \times 10^{-3} \quad if \quad v_r > 31.5 \\
\end{aligned}
\right.
$$

* Exposure: It provides the time exposure (in hours) for a given category $C$ in the Saffir Simpson     Hurricane Scale, at distance $r$ of the eye of the storm according to 

$$
\int_T c_{v_r} dt
$$ 

$$
\left\{
\begin{aligned}
c_{v_r} &= 1 \quad if \quad v_r \in [\min(v_r^c | c = C):\max(v_r^c | c = C)]\\
c_{v_r} &= 0 \quad if \quad v_r \notin [\min(v_r^c | c = C):\max(v_r^c | c = C)]\\
\end{aligned}
\right.
$$

  where $T$ stands for the whole lifecycle of the storm. <br />

* 2D radial wind speed structures (Only rasterized and not point-wised)

## Usage

These are basic examples which show how to solve some common problems

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


