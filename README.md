

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

######################
#Focus on a single TC#
######################

#Load TC
st = getStorms(time_period = 2015, name = "PAM", loi = "Vanuatu")

#Plot TC over the location of interest with legend activated
plotStorms(st, labels = T, legends = T)

#Compute Maximum Sustained Wind raster according to Willoughby et al. 2006 analytic model adding version 2 formula of asymmetry 
st_msw = stormBehaviour(st, asymmetry = "None", verbose = T)

#Plot the above raster alongside with the track of the storm
plotBehaviour(st, st_msw, labels = T)





######################
#Focus on several TCs#
######################

#Load TC
sts = getStorms(time_period = c(2003, 2021), name = c("ERICA", "NIRAN"), loi = "New Caledonia")

#Plot TCs over the location of interest
plotStorms(sts, labels = T, legends = T)

#Plot NIRAN over the location of interest
plotStorms(sts, names = "NIRAN", labels = T)

#Compute Power Dissipation index raster according to Willoughby et al. 2006 analytic model
sts_pdi = stormBehaviour(sts, product = "PDI" , verbose = T)

#Plot the PDI for ERICA alongside with the its track 
plotBehaviour(sts, sts_pdi[["ERICA_PDI"]], labels = T)


#Compute time series of wind speed on coordinates contained in df according to Willoughby et al. 2006 analytic model, adding version 2 formula of asymmetry 
df = data.frame(lon = c(166.5, 166.7), lat = c(-22.1, - 22.3))
<<<<<<< HEAD
wind.ts = stormBehaviour(sts, format = df, verbose = T)
=======
wind_ts = stormBehaviour(sts, format = df, verbose = T)





#############################
#Focus on a point coordinate#
#############################

pt = c(188.17,-13.92)
#Get all TCs that pass through a cirlce buffer of 300km around point pt
stsPt = getStorms(loi = pt, verbose = T)

#Check tracks
plotStorms(stsPt)

#Plot category 5 TCs
plotStorms(stsPt, category = 4, labels = T)

#Compute MSW and PDI for TC VAL 1991
val = getStorms(time_period = 1992, name = "VAL", loi = pt, verbose = T)
val_msw = stormBehaviour(val, verbose = T, empirical_rmw = T)
val_pdi = stormBehaviour(val, verbose = T,empirical_rmw = T, product = "PDI")

#Plot result
plotBehaviour(val, val_msw, labels = T)
plotBehaviour(val, val_pdi, labels = T)


 


##########################
#Eastern Pacific Analyzis#
##########################

#Get all TCs over Tropical Depression in the Eastern Pacific between 1980 and 2021
stsEP = getStorms(basin = "EP", verbose = T)

#Plot TCs over category 5
plotStorms(stsEP, category = 5)

#Plot TCs between category 1 and 3
plotStorms(stsEP, category = c(1,3))

#Plot TC IGNACIO 1979 focus on lon/lat 250-270/10-20 with labels every 12h on the right side of observations and add graticules at each degree
plotStorms(stsEP, names = "IGNACIO", category = c(1,3), labels = T, by = 4, pos = 4,
xlim = c(250,270), ylim = c(10,20), grtc = 8)


#Make loi
pol1 = sf::st_sfc(sf::st_polygon(list(cbind(c(220,250,250,220,220),c(10,10,30,30,10)))))
loi1 = sf::st_sf(pol, crs = 4326)

#Get data for TC KENNETH 2017 within loi1 
kenneth = getStorms(basin = "EP", time_period = 2017, name = "KENNETH", loi = loi1, max_dist = 10, verbose = T)

#Note: max_dist is set here to 10km, which means further computations will be performed within a 10km buffer on both sides of the track

#Check track of KENNETH 2017
plotStorms(kenneth)

#Compute MSW according to Holland80 model without asymmetry
kenneth_msw = stormBehaviour(kenneth, method = "Holland80", time_res = 0.5, verbose = T)


#Note: time_res set here to 30min to increase accuracy

#Plot results
plotBehaviour(kenneth, kenneth_msw, labels = T, xlim = c(225,235), ylim = c(16,22))



#########################################################
#Harold Exposure and profiles on Northen part of Vanuatu#
#########################################################

#Make loi
pol2 = sf::st_sfc(sf::st_polygon(list(cbind(c(167,168,168,167,167),c(-16,-16,-13,-13,-16)))))
loi2 = sf::st_sf(pol, crs = 4326)

#Get TC HAROLD 2020 data
harold = getStorms(time_period = 2020, name= "HAROLD", loi = loi2, verbose = T)

#Check track 
plotStorms(harold)

#Compute Exposure
expo = stormBehaviour(harold, product = "Exposure", asymmetry = "V1", verbose = T)

#Plot exposure to category 5
plotBehaviour(harold, expo[["HAROLD_Exposure5"]], labels = T, xlim = c(167,169), ylim = c(-16.5, -15))

#Plot exposure to category 4 and higher
plotBehaviour(harold, expo[["HAROLD_Exposure4"]], labels = T, xlim = c(165,170), ylim = c(-16.5, -15))


#Compute exposure on Luganville
luganville = data.frame(lon = 167.17, lat = -15.54)
expo_lv = stormBehaviour(harold, format = luganville, product = "Exposure", asymmetry = "V1", verbose = T)


#Compute wind profiles
pf = stormBehaviour(harold, format = "profiles", asymmetry = "V1", verbose = T)

#Plot profiles on Espiritu Santo
plotBehaviour(harold,pf["HAROLD_profile40"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))
plotBehaviour(harold,pf["HAROLD_profile41"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))
plotBehaviour(harold,pf["HAROLD_profile42"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))
plotBehaviour(harold,pf["HAROLD_profile43"], labels = T, xlim = c(166,168), ylim = c(-16.5, -14))


>>>>>>> Fix several bugs + add examples README

##############
#Get all TCs #
##############

#Get all TCs over Tropical Depression around the world between 1980 and 2021
sts = getStorms(basin = "ALL", verbose = T)

#Plot TCs over category 5
plotStorms(sts, category = 5)


```

## References

 * Willoughby, H. & Darling, Richard & Rahn, M.. (2006). Parametric Representation of the Primary     Hurricane Vortex. Part II: A New Family of Sectionally Continuous Profiles. Monthly Weather Review - MON WEATHER REV. 134. 1102-1120. 10.1175/MWR3106.1.  <br />
 
 * Holland, Greg. (1980). An Analytic Model of the Wind and Pressure Profiles in Hurricanes. Mon. Weather Rev.. 108. 1212-1218. 10.1175/1520-0493(1980)108<1212:AAMOTW>2.0.CO;2. 
 
 * Boose, Emery & Chamberlin, Kristen & Foster, David. (2001). Landscape and Regional Impacts of Hurricanes in New England. Ecological Monographs - ECOL MONOGR. 71. 27-48. 10.2307/3100043. 

* Wang, G., Wu, L., Mei, W. et al. Ocean currents show global     intensification of weak tropical cyclones. Nature 611, 496–500 (2022)

## Getting help

If you have any question or suggestion or if you want to report a bug, please do it via the GitHub issues.
Thanks for that, this would greatly help us to improve this package.


