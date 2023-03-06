

# StormR

<!-- badges: start -->
<!-- badges: end -->


## Overview

StormR is a R package allowing to easily extract tropical cyclone data for given locations or areas of interests, generate tropical cyclone wind fields, and to compute statistics characterising the behaviour of tropical cyclone winds (maximum sustained wind speed, power dissipation index, time of exposure to different wind speeds).

## Data source

To run StormR functions users have to provide a tropical cyclone storm track dataset in which the location and some characteristics of storms are given across their lifespan. By default we propose to use the data provided by USA agencies in the IBTrACS database [International Best Track Archive for Climate Stewardship](https://www.ncei.noaa.gov/products/international-best-track-archive) (Knapp et al., 2010). This database provides a fairly comprehensive record of worldwide tropical storms and cyclones with a 3-hours temporal resolution since 1841. Other databases can be used as long as the following fields are provided:

| **Field name** | **Description** | **Example** | **Type** |
|:---|:---|:---:|:---:|
| $basin$ | Name of the area where the storm originated. Traditionally divided into seven basins: <br/>NA (North Atlantic)<br/>EP (Eastern North Pacific)<br/>WP (Western North Pacific)<br/>NI (North Indian)<br/>SI (South Indian)<br/>SP (Southern Pacific)<br/>SA (South Atlantic) | SP | Mandatory |
| $name$ | Name of the storm in capital letters | PAM | Mandatory |
| $seasons$ | Year of observation | 2015 | Mandatory |
| $isoTime$ | Date and time of observation (YYYY-MM-DD HH:mm:ss) | 13/03/2015 12:00 | Mandatory |
| $lon$ | Longitude of the observation (decimal degrees east) | 168.7 | Mandatory |
| $lat$ | Latitude of the observation (decimal degrees north) | -17.6 | Mandatory |
| $msw$* | Maximum sustained wind speed in knots ($kt$) | 150 | Mandatory |
| $sshs$* | Saffir-Simpson hurricane wind scale rating based on $msw$ (hre in $kt$):<br/>$-1 =$ tropical depression ($msw < 34$)<br/>$0 =$ tropical storm ($34 < msw < 64$), $1 =$ category 1 ($64 \le msw < 83$)<br/>$2 =$ Category 2 ($83 \le msw < 96$)<br/>$3 =$ Category 3 ($96 \le msw < 113$)<br/>$4 =$ Category 4 ($113 \le msw < 137$)<br/>$5 =$ Category 5 ($msw \ge 137$) | 5 | Recommended |
| $rmw$* | Radius of maximum winds, distance between the center of the storm and its band of strongest winds in nautical miles ($nm$) | 12 | Recommended |
| $pressure$* | Central pressure in millibar ($mb$) | 911 | Optional |
| $poci$* | Pressure of the last closed isobar in millibar ($mb$) | 922 | Optional |

*Units: before running the functions stormR converts nautical miles ($nm$) into kilometres ($km$), knots ($kt$) into meters per second ($m.s^{-1}$), and millibar ($mb$) into Pascal ($Pa$) 

## Wind field models

Using these data StormR computes radial wind speed $v_r$ (in $m.s^{-1}$) at the distance $r$ (in $km$) from the center of the storm using parametric models. Two widely used models developed by Holland (1980) and Willoughby et al. (2006) are available.

### Holland (1980)

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

### Willoughby et al. (2006)

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

### Maximum Sustained Wind speed

Maximum Sustained Wind speed (MSW, in $m.s^{-1}$) over the lifespan of a storm computed as follow:<br />

$$
\max(v(t) | t \in [0,T])
$$

where
$t$ is the time of the observation<br />
$T$ is the lifespan of the storm<br />

### Power Dissipation Index (PDI)

The power dissipation index (PDI, in $J.m^{2}$) or total power dissipated by a tropical storm over its lifespan (Kerry, 1999, 2005) computed as follow:<br />

$$
\int_T \rho C_d v_r^3 dt
$$

where
$t$ is the time of the observation<br />
$T$ is the lifespan of the storm<br />
$\rho$ is the air density fixed to $1$ $kg.m^{-3}$ as in Kerry (1999)<br />
$C_d$ is the drag coefficient of the storm fixed to $2$ X $10^{-3}$ as in Kerry (1999)<br />

### Time of exposure

Time of exposure (in $hours$) above a minimum  wind speed threshold as follow:<br />

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

## References

 * Boose, E. R., Chamberlin, K. E., & Foster, D. R. (2001). Landscape and Regional Impacts of Hurricanes in New England. Ecological Monographs, 71(1), Article 1. https://doi.org/10.1890/0012-9615(2001)071[0027:LARIOH]2.0.CO;2
 
 * Holland, G. J. (1980). An Analytic Model of the Wind and Pressure Profiles in Hurricanes. Monthly Weather Review, 108(8), 1212–1218. https://doi.org/10.1175/1520-0493(1980)108<1212:AAMOTW>2.0.CO;2
 
 * Emanuel, K. A. (1999). The power of a hurricane: An example of reckless driving on the information superhighway. Weather, 54(4), 107–108. https://doi.org/10.1002/j.1477-8696.1999.tb06435.x

 * Emanuel, K. (2005). Increasing destructiveness of tropical cyclones over the past 30 years. Nature, 436(7051), Article 7051. https://doi.org/10.1038/nature03906
 
  * Knapp, K. R., Kruk, M. C., Levinson, D. H., Diamond, H. J., & Neumann, C. J. (2010). The International Best Track Archive for Climate Stewardship (IBTrACS). Bulletin of the American Meteorological Society, 91(3), Article 3. https://doi.org/10.1175/2009bams2755.1

 * Willoughby, H. E., Darling, R. W. R., & Rahn, M. E. (2006). Parametric Representation of the Primary Hurricane Vortex. Part II: A New Family of Sectionally Continuous Profiles. Monthly Weather Review, 134(4), 1102–1120. https://doi.org/10.1175/MWR3106.1

 * Yan, D., & Zhang, T. (2022). Research progress on tropical cyclone parametric wind field models and their application. Regional Studies in Marine Science, 51, 102207. https://doi.org/10.1016/j.rsma.2022.102207
 
## Usage

### Installing StormR package from GitHub

``` r
#install.packages("devtools")
devtools::install_github("umr-amap/StormR")
```

### Loading StormR package

``` r
library(StormR)
```

### Using the data provided by USA agencies in the IBTrACS database

``` r
blabla
```

### Getting and ploting tropical cyclone track data

The getStorms function allows to get track data for a given tropical cyclone or a set of tropical cyclones nearby a given location of interest (by default up to 300 km around the specified location of interest). The location of interest can be a country, a specific point or set of points defined by their longitude and latitude coordinates, or any user imported or defined spatial polygon shapefiles. Users can also select tropical cyclones with their names or season of occurrence. The plotStorms can then be used to visualise the trajectories and points of observation of tropical cyclones on a map. 

E.g. getting data on the tropical cyclone PAM (2015) nearby Vanuatu  

``` r
st <- getStorms(loi = "Vanuatu", names = "PAM")
plotStorms(st, labels=TRUE)
```

E.g. getting data on all tropical cyclones nearby Nouméa (longitude = 166.45, latitude = -22.27) since 1980 and only plotting category 4 and 5 tropical cyclones (Saffir-Simpson hurricane wind scale) or tropical cyclone Niran.

``` r
pt <- c(166.45,-22.27)
st <- getStorms(loi = pt)
plotStorms(st, category = c(4,5), labels=TRUE)
plotStorms(st, names="NIRAN", labels=TRUE)
```

E.g. getting data on all tropical cyclones nearby an area defined using an imported shape file, here the exclusive economic zone of New Caledonia, between 2010 and 2020.

``` r
sp <- eezNC
st <- getStorms(loi = eezNC, season=c(2010,2020))
plotStorms(st, labels=FALSE)
```

### Computing and plotting 2D wind fields  

The stormBehaviour_sp function allows computing 2D tropical cyclone wind fields using parametric models at different spatial and temporal resolutions (the default spatial resolution is 2.5 $min$, i.e. ~4.5 $km$ at the equator and the default temporal resolution is 1 $h$). Then using the "Profiles" product of the stormBehaviour_sp function and the plotBehaviour function allows plotting 2D wind profile at a given time of observation.

E.g. computing and plotting 2D wind profiles generated by the topical cyclone Pam (2015) near the island of Efate (Vanuatu) 

``` r
st <- getStorms(loi = "Vanuatu", names = "PAM")
profile <- stormBehaviour_sp(st, product = "Profiles")
plotBehaviour(st,profile[["PAM_Profiles_41"]])
```

### Computing and plotting time series of wind speed and direction

The temporalBehaviour function allows computing wind speed and direction for a given location or set of locations (provided with a data frame with "long" and "lat" columns) along the lifespan of a tropical cyclone.

E.g. computing and plotting time series of the speed and direction of winds generated by the topical cyclone Pam (2015) at Efate (longitude = 168.33, latitude = -17.73) in Vanuatu.

``` r
df<-data.frame(lon=168.33,lat=-17.73)
st <- getStorms(loi = "Vanuatu", names = "PAM")
ts <- temporalBehaviour(st, points = df)
plot(ts$PAM[,1]~ts$PAM[,3],type="b",xlab="Observation ID",ylab="Wind speed (m/s)")
plot(ts$PAM[,2]~ts$PAM[,3],type="b",xlab="Observation ID",ylab="Wind direction (degree north)")
```

### Computing, plotting, and exporting wind behaviour products

Beside computing 2D tropical cyclone wind fields, the stormBehaviour_sp function allows computing different products allowing to characterise the behaviour of winds across time and space. Three products are available: maximum sustained wind speed, power dissipation index, and time of exposure above a minimum wind speed threshold. These products can be plotted using the plotBehaviour function or exported as raster files using the writeRast function.

E.g. computing and plotting 2D maximum sustained wind speed, power dissipation index, and time of exposure to category 3 or higher (>49 $m.s^{-1}$) winds generated by the topical cyclone Pam (2015) nearby Vanuatu. 

#Load the data for the tropical cyclone Pam which hit the Vanuatu in 2015
st <- Storms(loi = "Vanuatu", names = "PAM")

``` r

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

## Getting help

If you have any question or suggestion or if you want to report a bug, please do it via the GitHub issues.
Thanks for that, this would greatly help us to improve this package.
