

# StormR

<!-- badges: start -->
<!-- badges: end -->


## Overview

StormR is a package developped to analyze past storms and tropical cyclones that occured in the whole
world beyond 1980. It let the user gather all its interested storms in a single object and then use it
to either plot tracks on a map, or compute/rasterize regimes of wind speed, 2D structures of radial wind and various other products.

## Installation

You can install the development version of StormR like so:

``` r
#install.packages("devtools")
devtools::install_github("umr-amap/StormR")
```


## Data source 
StormR uses the netcdf file 'IBTrACS.ALL.v04r00.nc' that comes from the [International Best Track Archive for Climate Stewardship](https://www.ncei.noaa.gov/products/international-best-track-archive). It let this package get every informations needed to plot, compute and analyze tropical cyclones. This data base provides observations every 3 hours of storms and tropical cyclones on the whole world that occured from 1841 to present days (although observations of most recent storms are not available yet). Observations are derived from various agencies, nevertheless, this package only supplies observations extracted from the USA agency as it is the one that comes up with the most prolific and reliable data. The following lists all the basin names:

* NA : North Atlantic
* SA : South Atlantic
* EP : Eastern North Pacific
* WP : Western North Pacific
* SP : South Pacific
* SI : South Indian
* NI : North Indian
* ALL: the above 7 basins all together

Default value for the basin is set to SP as the developpement team is located/interested in this particular region, and this basin has been used to test this package but it is up to the user to select his basin of interest.
For the sake of data reliability, StormR focuses only on storms and tropical cyclones that occured beyond 1980. A research for older storms will result in an error. [Click here](https://www.ncei.noaa.gov/sites/default/files/2021-07/IBTrACS_version4_Technical_Details.pdf) for a deeper insight on the documentation of this 'IBTrACS.ALL.v04r00.nc' file.

## Models

Two cyclonic models are available within this package in order to compute radial wind speed:

$\textbf{Willoughby et al. 2006}$ <br />
Insert comments about the model here <br />


$$
\left\{
\begin{aligned}
v_r &= msw\left(\frac{r}{rmw}\right)^{nn} \quad if \quad r < rmw \\
v_r &= msw\left((1-AA))e^{-\frac{|r-rmw|}{XX1}} + AA e^{-\frac{|r-rmw|}{XX2}}\right) \quad if \quad r \geq rmw \\
\end{aligned}
\right.
$$


where <br />
$v_r \quad$ Radial wind speed $(m.s^{-1})$ <br />
$r \quad$ Distance to the eye of the storm where $v_r$ is computed $(km)$ <br />
$msw \quad$ Maximum sustained wind speed $(m.s^{-1})$ <br />
$rmw \quad$ Radius of maximum sustained wind speed $(km)$ <br />
$XX1 = 287.6 - 1.942msw + 7.799\log(rmw) + 1.819|\phi| \quad$ Coefficient, $\phi$ being the latitude <br />
$XX2 = 25 \quad$ Coefficient <br />
$nn = 2.1340 + 0.0077msw - 0.4522\log(rmw) - 0.0038|\phi| \quad$ Coefficient, $\phi$ being the latitude <br />



$\textbf{Holland 1980}$ <br />
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
$f = 2 \times 7.29 \times10^{-5} \sin(\phi) \quad$ Coriolis force $(N.kg^{-1})$, $\phi$ being the latitude <br />
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
StormR let the user compute several products. They can either be computed on 
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

* Exposure: It provides the time exposure (in hours) for a given category $C$ (in the Saffir Simpson     Hurricane Scale), where radial wind speed $v_r^C$ spans in $C_r := [\min(v_r^c | c = C):\max(v_r^c | c = C)]$ , and at distance $r$ of the eye of the storm, according to 

$$
\int_T c(v_r) dt
$$ 

$$
\left\{
\begin{aligned}
c(v_r) &= 1 \quad if \quad v_r \in C_r\\
c(v_r) &= 0 \quad if \quad v_r \notin C_r\\
\end{aligned}
\right.
$$

  where $T$ stands for the whole lifecycle of the storm. <br />

* 2D radial wind speed structures (Only rasterized and not point-wised)

## Usage

These are basic examples which show how to solve some common problems

``` r
library(StormR)

##############################################
#Single tropical cyclone over a given country#
##############################################

#Load the data for the tropical cyclone Pam which hit the Vanuatu in 2015
st <- getStorms(seasons = 2015, names = "PAM", loi = "Vanuatu")

#Plot the tropical cyclone track and observations over or around the location of interest
plotStorms(st, labels = T, legends = T)

#Compute maximum sustained wind speed (MSW), power dissipation index (PDI), and exposure time (EXP) with default settings (the analytic model from Willoughby et al. 2006 with asymmetry from REF?). The function returns a raster with a 10 km spatial resolution by default .
st_msw <- stormBehaviour(st, verbose = T)
st_pdi <- stormBehaviour(st, product = c("PDI", verbose = T)
st_exposure <- stormBehaviour(st, product = "Exposure", wind_threshold = c(40,50), verbose = T)

#Plot the MSW, PDI, and EXP rasters alongside with the track of the storm and the limit of the location of interest
split.screen(c(1,3))
screen(1)
plotBehaviour(st, st_msw, labels = T)
screen(2)
plotBehaviour(st, st_pdi, labels = T)
screen(3)
plotBehaviour(st, st_exposure[["PAM_Exposure_40-50"]], labels = T)


#Export the MSW raster in a given directory (here a temporary directory)
writeRast(st_msw, path = paste0(tempdir(),"/"))



################################################
#Several tropical cyclones over a given country#
################################################

#Load all tropical cyclones that have passed nearby New Caledonia between 2019 and 2021
sts <- getStorms(seasons = c(2019, 2021), loi = "New Caledonia")

#Plot all tropical cyclone tracks and observations over or around the location of interest
plotStorms(sts, labels = T, legends = T)

#Plot only the track and observations for only one of the tropical cyclones (here Niran)
plotStorms(sts, names = "NIRAN", labels = T)

#Compute PDI rasters for all tropical cyclones with the default values
sts_pdi <- stormBehaviour(sts, product = "PDI" , verbose = T)

#Plot the PDI for the tropical cyclone Niran alongside with the its track
plotBehaviour(sts, sts_pdi[["NIRAN_PDI"]], labels = T)



##################################################################
#Tropical cyclones around a spatial polygon (created or imported)#
##################################################################

#Load all tropical cyclones that have passed nearby the EEZ of New Caledonia between 1980 and 2021
stsEEZnc <- getStorms(loi = eezNC)

#Plot category 3 tropical cyclones (Saffir-Simpson hurricane wind scale, SSHWS) 
plotStorms(stsEEZnc, category = 3)


#################################################
#Tropical cyclones around a given point location#
#################################################

#Set point location coordinates, lat/long, in decimal degrees (WGS84)
pt <- c(188.17,-13.92)
#Get all tropical cyclones that had passed near the point (by default <= 300 km away)
stsPt <- getStorms(loi = pt)

#Plot all tropical cyclone tracks and observations around the point of interest
plotStorms(stsPt)

#Plot only category 4 or 5 tropical cyclones (Saffir-Simpson hurricane wind scale, SSHWS) 
plotStorms(stsPt, category = c(4,5), labels = T)


################################
#Time series at given locations#
################################


#Compute time series of wind speed at given location using coordinates provided in a data frame
df <- data.frame(lon = c(166.5, 166.7), lat = c(-22.1, - 22.3))
wind_ts <- Unknow(sts, points = df)

plot(wind_ts$NIRAN[,2], type = "b", ylab = "maximum sustained wind speed (m/s)")




##########################
#Tropical cyclone profile#
##########################

#ADD AN EXAMPLE WHERE WE CAN COMPARE THE PROFILES GENERATED BY THE DIFFERENT MODELS AND ASYMMETRY (i.e. one cyclone with the six profiles) AT A GIVEN TIME OF OBSERVATION YOU CAN USE WHAT YOU DID FOR HAROLD FOR INSTANCE

#Make a location of interest around Espiritu Santo in Vanuatu
pol <- sf::st_sfc(sf::st_polygon(list(cbind(c(167,168,168,167,167),c(-16,-16,-13,-13,-16)))))
loi <- sf::st_sf(pol, crs = 4326)

#Load the data for the tropical cyclone Harold which hit the Vanuatu in 2020
harold <- getStorms(seasons = 2020, names= "HAROLD", loi = loi)

#Compute wind profiles using Willoughby model with version 1 of asymmetry
profWillV1 <- stormBehaviour(harold, format = "profiles", asymmetry = "V1", verbose = T)

#Compute wind profiles using Holland model with version 2 of asymmetry
profHollV2 <- stormBehaviour(harold, format = "profiles", method = "Holland", verbose = T)

#Compare few profiles between the two above differents methods and asymmetries
plotBehaviour(harold,profWillV1["HAROLD_profile40"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))
plotBehaviour(harold,profHollV2["HAROLD_profile40"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))

plotBehaviour(harold,profWillV1["HAROLD_profile41"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))
plotBehaviour(harold,profHollV2["HAROLD_profile41"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))

plotBehaviour(harold,profWillV1["HAROLD_profile42"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))
plotBehaviour(harold,profHollV2["HAROLD_profile42"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))

plotBehaviour(harold,profWillV1["HAROLD_profile43"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))
plotBehaviour(harold,profHollV2["HAROLD_profile43"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))




```

## References

 * Willoughby, H. & Darling, Richard & Rahn, M.. (2006). Parametric Representation of the Primary     Hurricane Vortex. Part II: A New Family of Sectionally Continuous Profiles. Monthly Weather Review - MON WEATHER REV. 134. 1102-1120. 10.1175/MWR3106.1.  <br />
 
 * Holland, Greg. (1980). An Analytic Model of the Wind and Pressure Profiles in Hurricanes. Mon. Weather Rev.. 108. 1212-1218. 10.1175/1520-0493(1980)108<1212:AAMOTW>2.0.CO;2. 
 
 * Boose, Emery & Chamberlin, Kristen & Foster, David. (2001). Landscape and Regional Impacts of Hurricanes in New England. Ecological Monographs - ECOL MONOGR. 71. 27-48. 10.2307/3100043. 

* Wang, G., Wu, L., Mei, W. et al. Ocean currents show global     intensification of weak tropical cyclones. Nature 611, 496–500 (2022)

## Getting help

If you have any question or suggestion or if you want to report a bug, please do it via the GitHub issues.
Thanks for that, this would greatly help us to improve this package.


