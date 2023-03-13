

# StormR

<!-- badges: start -->
<!-- badges: end -->


## Overview

StormR is a R package allowing to easily extract tropical cyclone data for given locations or areas of interests, generate tropical cyclone wind fields, and to compute statistics characterising the behaviour of tropical cyclone winds (maximum sustained wind speed, power dissipation index, time of exposure to different wind speeds).

## Installation

StormR can be installed from GitHub as follows:

``` r
#install.packages("devtools")
devtools::install_github("umr-amap/StormR")
```

## Data source

To run StormR functions users have to provide a tropical cyclone storm track dataset in which the location and some characteristics of storms are given across their lifetime. By default we propose to use the data provided by USA agencies in the IBTrACS database [International Best Track Archive for Climate Stewardship](https://www.ncei.noaa.gov/products/international-best-track-archive). This database provides a fairly comprehensive record of worldwide tropical storms and cyclones with a 3-hours temporal resolution since 1841. Other databases can be used as long as the following fields are provided:

| **Field name** | **Description** | **Example** | **Type** |
|:---|:---|:---:|:---:|
| $basin$ | Name of the area where the storm originated. Traditionally divided into seven basins: <br/>NA (North Atlantic)<br/>EP (Eastern North Pacific)<br/>WP (Western North Pacific)<br/>NI (North Indian)<br/>SI (South Indian)<br/>SP (Southern Pacific)<br/>SA (South Atlantic) | SP | Mandatory |
| $name$ | Name of the storm in capital letters | PAM | Mandatory |
| $seasons$ | Year of observation | 2015 | Mandatory |
| $isoTime$ | Date and time of observation (YYYY-MM-DD HH:mm:ss) | 13/03/2015 12:00 | Mandatory |
| $lon$ | Longitude of the observation (decimal degrees north) | 168.7 | Mandatory |
| $lat$ | Latitude of the observation (decimal degrees east) | -17.6 | Mandatory |
| $msw$ | Maximum Sustained Wind speed in knots | 150 | Mandatory |
| $sshs$ | Saffir Simpson Hurricane Scale rating based on $msw$:<br/>$-1 =$ tropical depression [$msw < 34$]<br/>$0 =$ tropical storm [$34 < msw < 64$], $1 =$ category 1 [$64 \le msw < 83$]<br/>$2 =$ Category 2 [$83 \le msw < 96$]<br/>$3 =$ Category 3 [$96 \le msw < 113$]<br/>$4 =$ Category 4 [$113 \le msw < 137$]<br/>$5 =$ Category 5 [$msw \ge 137$]) | 5 | Recommended |
| $rmw$ | Radius of maximum winds, distance between the center of the storm and its band of strongest winds in nautical miles | 12 | Recommended |
| $pressure$ | Central pressure in millibar | 911 | Optional |
| $poci$ | Pressure of the last closed isobar in millibar | 922 | Optional |

## Wind field models

Using these data StormR computes radial wind speed $v_r$ at the distance $r$ from the center of the storm using two parametric models developed by Holland (1980) and Willoughby et al. (2006)
<br />
<br />
$\textbf{Holland (1980)}$ <br />

$$
v_r = \sqrt{\frac{b}{\rho}\left(\frac{rmw}{r}\right)^b (poci - pc)e^{-\left(\frac{rmw}{r}\right)^b} + \left(\frac{rf}{2}\right)^2} - \left(\frac{rf}{2}\right)
$$

where <br />
$v_r$ is the radial wind speed (in $m.s^{-1}$) <br />
$r$ is the distance to the eye of the storm (in $km$) <br />
$msw$ is the maximum sustained wind speed (in $m.s^{-1}$) <br />
$rmw$ is the radius of maximum sustained wind speed (in $km$) <br />
$pc$ is the pressure at the eye of the storm ($pressure$ in $mb$) <br />
$poci$ is the pressure at outermost closed isobar of the storm (in $mb$) <br />
$\rho = 1.15$ is the air density (in $kg.m^{-3}$) <br />
$f = 2 \times 7.29 \times10^{-5} \sin(\phi)$ is the coriolis force (in $N.kg^{-1}$, with $\phi$ being the latitude) <br />
$b = \frac{\rho e \times msw^2}{poci - pc}$ is the shape factor <br />
<br />
<br />
$\textbf{Willoughby et al. (2006)}$ <br />

$$
\left\{
\begin{aligned}
v_r &= msw\left(\frac{r}{rmw}\right)^{n} \quad if \quad r < rmw \\
v_r &= msw\left((1-A)e^{-\frac{|r-rmw|}{X1}} + A e^{-\frac{|r-rmw|}{X2}}\right) \quad if \quad r \geq rmw \\
\end{aligned}
\right.
$$

where <br />
$v_r$ is the radial wind speed (in $m.s^{-1}$) <br />
$r$ is the distance to the eye of the storm (in $km$) <br />
$msw$ is the maximum sustained wind speed (in $m.s^{-1}$) <br />
$rmw$ is the radius of maximum sustained wind speed (in $km$) <br />
$X1 = 287.6 - 1.942msw + 7.799\ln(rmw) + 1.819|\phi|$<br />
$X2 = 25$ <br />
$n = 2.1340 + 0.0077msw - 0.4522\ln(rmw) - 0.0038|\phi|$<br />
$A = 0.5913 + 0.0029msw - 0.1361\ln(rmw) - 0.0042|\phi| (A\ge0)$<br />
$\phi$ is the latitude of the center of the storm 

Note that for both models if $rmw$ is not provided then it is approximated using an empirical formula derived from Willoughby et al. (2006)<br />
$rmw = 46.4e^{(-0.0155msw + 0.0169|\phi|)}$

## Asymmetry

The above models compute symmetric 2D structures of radial wind speed around the
eye of the storm. Nevertheless, in most cases, tropical cyclone are not symmetric
and it can be interesting to add asymmetry to the computations in order to get a
much more accurate 2D structures of radial wind speed. This package proposes the following
formula extracted from Boose et al. (2001): <br />

$v_{r_{as}} = v_r - S(1-\sin(\alpha))\frac{v_h}{2}$

where <br />
$v_{r_{as}} \quad$ is the new radial wind speed with asymmetry $(m.s^{-1})$ <br />
$v_r \quad$ is the former radial wind speed without asymmetry $(m.s^{-1})$ <br />
$v_h \quad$ is the velocity of storm $(m.s^{-1})$ <br />
$S$ Asymmetry coefficient (usually set to 1) <br />
$\alpha \quad$ is tha angle between the storm direction and the point where $v_{r_{as}}$ is computed.
Clockwise in Nothern hemisphere, counterclokwise in Southern hemisphere. <br />

## Wind Direction
Wind direction $D$ (in degree) at each point is computed according to the following formula: <br />

$D = A_z - 90 - I$

where <br />
$A_z\quad$ is the azimuth from the point to the storm center <br/>
$I\quad$ is the cross isobar inflow angle which is either 20° on water or 40° on land <br/>

The above formula works for the northen hemisphere. As direction of wind is clockwise in the southern hemisphere,
this formula becomes $D = A_z + 90 + I$.


## Products
StormR let the user compute several products. They can either be computed on
specific longitude/latitute coordinates or rasterized over the location of interest.
The following describes the products available: <br />


* Maximum Sustained Wind speed (MSW). It provides the value of the maximum sustained wind speed $(m.s^{-1})$ according to

$$
\max(v(t) | t \in [0,T])
$$

  where $T$ stands for the whole lifecycle of the storm. <br />

* Power Dissipation Index (PDI): It provides the value of the PDI according to

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

* Exposure: It provides the time exposure (in hours) to a certain range of wind speed greater than a threshold $Thd$ $(m.s^{-1})$, according to

$$
\int_T c(v) dt
$$

$$
\left\{
\begin{aligned}
c(v) &= 1 \quad if \quad v \geq Thd\\
c(v) &= 0 \quad if \quad v < Thd\\
\end{aligned}
\right.
$$

  where $T$ stands for the whole lifecycle of the storm. <br />

* 2D radial wind speed/ direction structures (Only rasterized)

## Usage

These are basic examples which show how to solve some common problems

``` r
library(StormR)

##############################################
#Single tropical cyclone over a given country#
##############################################

#Load the data for the tropical cyclone Pam which hit the Vanuatu in 2015
st <- Storms(loi = "Vanuatu", names = "PAM")

#Plot the tropical cyclone track and observations over or around the location of interest
plotStorms(st, labels = T, legends = T)

#Compute maximum sustained wind speed (MSW), power dissipation index (PDI), and exposure time (EXP) with default settings (the analytic model from Willoughby et al. 2006 with asymmetry). The function returns a raster with a 2.5min spatial resolution by default.
st_prod <- spatialBehaviour(st, product = c("MSW", "PDI", "Exposure"))


#Plot the MSW, PDI, and an Exposure rasters alongside with the track of the storm and the limit of the location of interest
split.screen(c(1,3))
screen(1)
plotBehaviour(st, st_prod[["PAM_MSW"]], labels = T)
screen(2)
plotBehaviour(st, st_prod[["PAM_PDI"]], labels = T)
screen(3)
plotBehaviour(st, st_prod[["PAM_Exposure_50"]], labels = T)


#Export the MSW raster in a given directory (here a temporary directory)
writeRast(st_prod[["PAM_MSW"]], path = paste0(tempdir(),"/"))



################################################
#Several tropical cyclones over a given country#
################################################

#Load all tropical cyclones that have passed nearby New Caledonia between 2019 and 2021
sts <- Storms(loi = "New Caledonia", seasons = c(2019, 2021))

#Plot all tropical cyclone tracks and observations over or around the location of interest
plotStorms(sts, labels = T, legends = T)

#Plot only the track and observations for only one of the tropical cyclones (here Niran)
plotStorms(sts, names = "NIRAN", labels = T)

#Compute PDI rasters for all tropical cyclones with the default values
sts_pdi <- spatialBehaviour(sts, product = "PDI")

#Plot the PDI for the tropical cyclone Niran alongside with the its track
plotBehaviour(sts, sts_pdi[["NIRAN_PDI"]], labels = T)



##################################################################
#Tropical cyclones around a spatial polygon (created or imported)#
##################################################################

#Load all tropical cyclones that have passed nearby the EEZ of New Caledonia between 1980 and 2022
stsEEZnc <- Storms(loi = eezNC)

#Plot category 3 tropical cyclones (Saffir-Simpson hurricane wind scale, SSHWS)
plotStorms(stsEEZnc, category = 3)


#################################################
#Tropical cyclones around a given point location#
#################################################

#Set point location coordinates, lat/long, in decimal degrees (WGS84)
pt <- c(188.17,-13.92)
#Get all tropical cyclones that had passed near the point (by default <= 300 km away)
stsPt <- Storms(loi = pt)

#Plot all tropical cyclone tracks and observations around the point of interest
plotStorms(stsPt)

#Plot only category 4 or 5 tropical cyclones (Saffir-Simpson hurricane wind scale, SSHWS)
plotStorms(stsPt, category = c(4,5), labels = T)


################################
#Time series at given locations#
################################

#Compute time series of wind speed at given location using coordinates provided in a data frame
df <- data.frame(lon = c(166.5, 166.7), lat = c(-22.1, - 22.3))
wind_ts <- temporalBehaviour(sts, points = df)

plot(wind_ts$NIRAN[,2], type = "b", ylab = "maximum sustained wind speed (m/s)")




##########################
#Tropical cyclone profile#
##########################

#Make a location of interest around Espiritu Santo in Vanuatu
pol <- sf::st_sfc(sf::st_polygon(list(cbind(c(167,168,168,167,167),c(-16,-16,-13,-13,-16)))))
loi <- sf::st_sf(pol, crs = 4326)

#Load the data for the tropical cyclone Harold which hit the Vanuatu in 2020
harold <- Storms(loi = loi, names= "HAROLD")

#Compute wind profiles using Willoughby model with asymmetry
profWillV1 <- spatialBehaviour(harold, product = "Profiles")

#Compute wind profiles using Holland model with asymmetry
profHollV2 <- spatialBehaviour(harold, product = "Profiles", method = "Holland")

#Compare few profiles between the two above differents methods and asymmetries
plotBehaviour(harold,profWillV1["HAROLD_Profiles_40"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))
plotBehaviour(harold,profHollV2["HAROLD_Profiles_40"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))

plotBehaviour(harold,profWillV1["HAROLD_Profiles_41"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))
plotBehaviour(harold,profHollV2["HAROLD_Profiles_41"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))


plotBehaviour(harold,profWillV1["HAROLD_Profiles_43"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))
plotBehaviour(harold,profHollV2["HAROLD_Profiles_43"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))




```

## References

 * Willoughby, H. & Darling, Richard & Rahn, M.. (2006). Parametric Representation of the Primary     Hurricane Vortex. Part II: A New Family of Sectionally Continuous Profiles. Monthly Weather Review - MON WEATHER REV. 134. 1102-1120. 10.1175/MWR3106.1.  <br />

 * Holland, Greg. (1980). An Analytic Model of the Wind and Pressure Profiles in Hurricanes. Mon. Weather Rev.. 108. 1212-1218. 10.1175/1520-0493(1980)108<1212:AAMOTW>2.0.CO;2.

 * Boose, Emery & Chamberlin, Kristen & Foster, David. (2001). Landscape and Regional Impacts of Hurricanes in New England. Ecological Monographs - ECOL MONOGR. 71. 27-48. 10.2307/3100043.

* Wang, G., Wu, L., Mei, W. et al. Ocean currents show global     intensification of weak tropical cyclones. Nature 611, 496–500 (2022)

## Getting help

If you have any question or suggestion or if you want to report a bug, please do it via the GitHub issues.
Thanks for that, this would greatly help us to improve this package.
