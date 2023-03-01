

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
| $lon$ | Longitude of the observation (decimal degrees east) | 168.7 | Mandatory |
| $lat$ | Latitude of the observation (decimal degrees north) | -17.6 | Mandatory |
| $msw$* | Maximum Sustained Wind speed in knots | 150 | Mandatory |
| $sshs$* | Saffir Simpson Hurricane Scale rating based on $msw$:<br/>$-1 =$ tropical depression ($msw < 34$)<br/>$0 =$ tropical storm ($34 < msw < 64$), $1 =$ category 1 ($64 \le msw < 83$)<br/>$2 =$ Category 2 ($83 \le msw < 96$)<br/>$3 =$ Category 3 ($96 \le msw < 113$)<br/>$4 =$ Category 4 ($113 \le msw < 137$)<br/>$5 =$ Category 5 ($msw \ge 137$) | 5 | Recommended |
| $rmw$* | Radius of maximum winds, distance between the center of the storm and its band of strongest winds in nautical miles | 12 | Recommended |
| $pressure$* | Central pressure in millibar | 911 | Optional |
| $poci$* | Pressure of the last closed isobar in millibar | 922 | Optional |

*Units: before running the functions stormR converts nautical miles ($nm$) into kilometres ($km$), $knots$ into meters per second ($m.s^{-1}$), and millibar ($mb$) into Pascal ($Pa$) 

## Wind field models

Using these data StormR computes radial wind speed $v_r$ at the distance $r$ from the center of the storm using parametric models. Two widely used models developed by Holland (1980) and Willoughby et al. (2006) are available.
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
$pc$ is the pressure at the center of the storm ($pressure$ in $mb$) <br />
$poci$ is the pressure at outermost closed isobar of the storm (in $mb$) <br />
$\rho = 1.15$ is the air density (in $kg.m^{-3}$) <br />
$f = 2 \times 7.29 \times10^{-5} \sin(\phi)$ is the Coriolis force (in $N.kg^{-1}$, with $\phi$ being the latitude) <br />
$b = \frac{\rho e \times msw^2}{poci - pc}$ is the shape parameter <br />
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

The above models compute radial wind speeds that are symmetric around the center of the storm. However, winds are rarely symmetric around the center notably because of the translation movement of the storm (Yan and Zhang, 2022). It is therefore more realistic to add symmetry to the wind field generated by these models. For this we propose to use the model developed by Boose et al. (2001): <br />

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

$D = A_z - 90 - I \quad if \quad \phi > 0$ (North hemisphere)<br/>
$D = A_z + 90 + I \quad if \quad \phi < 0$ (South hemisphere)<br/>

where <br />
$A_z\quad$ is the azimuth from the point to the storm center <br/>
$I\quad$ is the cross isobar inflow angle which is either 20° on water or 40° on land <br/>
$\phi\quad$ is the latitude of the center of the storm 

## Products
Based on the computed wind fields, stormR allows to compute different products allowing to characterise the behaviour of winds across time and space at given specific locations (i.e., at given longitude/latitude coordinates) or for all cells of a regular grid (i.e., a raster). Three products are available:<br />

* Maximum Sustained Wind speed (MSW)<br />

MSW provides the value of the maximum sustained wind speed (in $(m.s^{-1})$) over the lifespan of a storm and is computed as follow:<br />

$$
\max(v(t) | t \in [0,T])
$$

where
$t$ is the time of the observation<br />
$T$ is the lifespan of the storm<br />

* Power Dissipation Index (PDI)<br />

The PDI (in $J.m^{2}$) is an index measuring the total power dissipated by a tropical storm over its lifespan (Kerry, 1999, 2005) and is computed as follow:<br />

$$
\int_T \rho C_d v_r^3 dt
$$

where
$t$ is the time of the observation<br />
$T$ is the lifespan of the storm<br />
$\rho$ is the air density fixed to $1$ $kg.m^{-3}$ as in Kerry (1999)<br />
$C_d$ is the drag coefficient of the storm fixed to $2$ X $10^{-3}$ as in Kerry (1999)<br />

* Time of exposure

It provides the time of exposure (in $hours$) above a minimum  wind speed threshold as follow:<br />

$$
\int_T c(v_t) dt
$$

$$
\left\{
\begin{aligned}
c(v_t) &= 1 \quad if \quad v_t \geq Thd\\
c(v_t) &= 0 \quad if \quad v_t < Thd\\
\end{aligned}
\right.
$$

where<br/>
$t$ is the time of the observation<br/>
$T$ is the lifespan of the storm<br/>
$v_t$ is the maximum sustained wind speed at time $t$ (in $m.s^{-1}$)<br/>
$Thd$ is the minimum wind sped threshold (in $m.s^{-1}$)<br/>

By default the time of exposure is computed for each Saffir-Simpson Hurricane Scale threshold values for tropical cyclone categories (i.e., $33$, $43$, $50$ ,$58$, and $70$ $m.s^{-1}$)

* 2D radial wind speed/ direction structures (Only rasterized)

## Usage

These are basic examples which show how to solve some common problems

``` r
library(StormR)

##############################################
#Single tropical cyclone over a given country#
##############################################

#Load the data for the tropical cyclone Pam which hit the Vanuatu in 2015
st <- getStorms(loi = "Vanuatu", names = "PAM")

#Plot the tropical cyclone track and observations over or around the location of interest
plotStorms(st, labels = T, legends = T)

#Compute maximum sustained wind speed (MSW), power dissipation index (PDI), and exposure time (EXP) with default settings (the analytic model from Willoughby et al. 2006 with asymmetry). The function returns a raster with a 2.5min spatial resolution by default.
st_prod <- stormBehaviour_sp(st, product = c("MSW", "PDI", "Exposure"))


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
sts <- getStorms(loi = "New Caledonia", seasons = c(2019, 2021))

#Plot all tropical cyclone tracks and observations over or around the location of interest
plotStorms(sts, labels = T, legends = T)

#Plot only the track and observations for only one of the tropical cyclones (here Niran)
plotStorms(sts, names = "NIRAN", labels = T)

#Compute PDI rasters for all tropical cyclones with the default values
sts_pdi <- stormBehaviour_sp(sts, product = "PDI")

#Plot the PDI for the tropical cyclone Niran alongside with the its track
plotBehaviour(sts, sts_pdi[["NIRAN_PDI"]], labels = T)



##################################################################
#Tropical cyclones around a spatial polygon (created or imported)#
##################################################################

#Load all tropical cyclones that have passed nearby the EEZ of New Caledonia between 1980 and 2022
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
wind_ts <- stormBehaviour_pt(sts, points = df)

plot(wind_ts$NIRAN[,2], type = "b", ylab = "maximum sustained wind speed (m/s)")




##########################
#Tropical cyclone profile#
##########################

#Make a location of interest around Espiritu Santo in Vanuatu
pol <- sf::st_sfc(sf::st_polygon(list(cbind(c(167,168,168,167,167),c(-16,-16,-13,-13,-16)))))
loi <- sf::st_sf(pol, crs = 4326)

#Load the data for the tropical cyclone Harold which hit the Vanuatu in 2020
harold <- getStorms(loi = loi, names= "HAROLD")

#Compute wind profiles using Willoughby model with asymmetry
profWillV1 <- stormBehaviour_sp(harold, product = "Profiles")

#Compute wind profiles using Holland model with asymmetry
profHollV2 <- stormBehaviour_sp(harold, product = "Profiles", method = "Holland")

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
