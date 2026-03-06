# Computing wind behaviour and summary statistics over given areas

The spatialBehaviour() function allows computing wind speed and
direction for each cell of a regular grid (i.e., a raster) for a given
tropical cyclone or set of tropical cyclones. It also allows to compute
three associated summary statistics.

## Usage

``` r
spatialBehaviour(
  sts,
  product = "MSW",
  windThreshold = NULL,
  method = "Willoughby",
  asymmetry = "Chen",
  empiricalRMW = FALSE,
  spaceRes = "2.5min",
  tempRes = 60,
  verbose = 2
)
```

## Arguments

- sts:

  `StormsList` object

- product:

  character vector. Desired output statistics:

  - `"Profiles"`, for 2D wind speed and direction fields,

  - `"MSW"`, for maximum sustained wind speed (default setting),

  - `"PDI"`, for power dissipation index, or

  - `"Exposure"`, for duration of exposure.

- windThreshold:

  numeric vector. Minimal wind threshold(s) (in \\m.s^{-1}\\) used to
  compute the duration of exposure when `product="Exposure"`. Default
  value is to set NULL, in this case, the windthresholds are the one
  used in the scale defined in the stromsList.

- method:

  character. Model used to compute wind speed and direction. Three
  different models are implemented:

  - `"Willoughby"`, for the symmetrical model developed by Willoughby et
    al. (2006) (default setting),

  - `"Holland"`, for the symmetrical model developed by Holland (1980),
    or

  - `"Boose"`, for the asymmetrical model developed by Boose et al.
    (2004).

- asymmetry:

  character. If `method="Holland"` or `method="Willoughby"`, this
  argument specifies the method used to add asymmetry. Can be:

  - `"Chen"`, for the model developed by Chen (1994) (default setting),

  - `"Miyazaki"`, for the model developed by Miyazaki et al. (1962), or

  - `"None"`, for no asymmetry.

- empiricalRMW:

  logical. Whether (TRUE) or not (FALSE) to compute the radius of
  maximum wind (`rmw`) empirically using the model developed by
  Willoughby et al. (2006). If `empiricalRMW==FALSE` (default setting)
  then the `rmw` provided in the `StormsList` is used.

- spaceRes:

  character. Spatial resolution. Can be `"30 sec"` (~1 km at the
  equator), `"2.5 min"` (~4.5 km at the equator), `"5 min"` (~9 km at
  the equator) or `"10 min"` (~18.6 km at the equator). Default setting
  is `"2.5 min"`.

- tempRes:

  numeric. Temporal resolution (min). Can be `60` ( default setting),
  `30` or `15`.

- verbose:

  numeric. Whether or not the function should display informations about
  the process and/or outputs. Can be:

  - `2`, information about the processes and outputs are displayed
    (default setting),

  - `1`, information about the processes are displayed, pr

  - `0`, no information displayed.

## Value

The spatialBehaviour() function returns SpatRaster objects (in WGS84).
The number of layers in the output depends on the number of storms in
the inputs, on the desired `product`, as well as the `tempRes` argument:

- if `product = "MSW"`, the function returns one layer for each `Storm`.
  The names of the layer follow the following terminology, the name of
  the storm in capital letters and “MSW” separated by underscores (e.g.,
  "PAM_MSW"),

- if `product = "PDI"`, the function returns one layer for each `Storm`.
  The names of the layer follow the following terminology, the name of
  the storm in capital letters and “PDI” separated by underscores (e.g.,
  "PAM_PDI"),

- if `product ="Exposure"`, the function returns one layer for each wind
  speed values in the `windThreshold` argument and for each `Storm`. The
  names of the layer follow the following terminology, the name of the
  storm in capital letters, "Exposure", and the threshold value
  separated by underscores (e.g., "PAM_Exposure_18", "PAM_Exposure_33",
  ...),

- if `product = "Profiles"` the function returns one layer for wind
  speed and one layer for wind direction for each observation or
  interpolated observation and each `Storm`. The names of the layer
  follow the following terminology, the name of the storm in capital
  letters, "Speed" or "Direction", and the indices of the observation
  separated by underscores (e.g., "PAM_Speed_41",
  "PAM_Direction_41",...).

## Details

Storm track data sets, such as those extracted from IBRTrACKS (Knapp et
al., 2010), usually provide observation at a 3- or 6-hours temporal
resolution. In the spatialBehaviour() function, linear interpolations
are used to reach the temporal resolution specified in the `tempRes`
argument (default value = 60 min). When `product = "MSW"`,
`product = "PDI"`, or `product = "Exposure"` the `focal()` function from
the `terra` R package is used to smooth the results using moving
windows.

The Holland (1980) model, widely used in the literature, is based on the
'gradient wind balance in mature tropical cyclones. The wind speed
distribution is computed from the circular air pressure field, which can
be derived from the central and environmental pressure and the radius of
maximum winds.

\\v_r = \sqrt{\frac{b}{\rho} \times \left(\frac{R_m}{r}\right)^b \times
(p\_{oci} - p_c) \times e^{-\left(\frac{R_m}{r}\right)^b} +
\left(\frac{r \times f}{2}\right)^2} - \left(\frac{r \times
f}{2}\right)\\

with,

\\b = \frac{\rho \times e \times v_m^2}{p\_{oci} - p_c}\\

\\f = 2 \times 7.29 \times 10^{-5} \sin(\phi)\\

where, \\v_r\\ is the tangential wind speed (in \\m.s^{-1}\\), \\b\\ is
the shape parameter, \\\rho\\ is the air density set to \\1.15
kg.m^{-3}\\, \\e\\ being the base of natural logarithms (~2.718282),
\\v_m\\ the maximum sustained wind speed (in \\m.s^{-1}\\), \\p\_{oci}\\
is the pressure at outermost closed isobar of the storm (in \\Pa\\),
\\p_c\\ is the pressure at the centre of the storm (in \\Pa\\), \\r\\ is
the distance to the eye of the storm (in \\km\\), \\R_m\\ is the radius
of maximum sustained wind speed (in \\km\\), \\f\\ is the Coriolis force
(in \\N.kg^{-1}\\, and \\\phi\\ being the latitude).

The Willoughby et al. (2006) model is an empirical model fitted to
aircraft observations. The model considers two regions: inside the eye
and at external radii, for which the wind formulations use different
exponents to better match observations. In this model, the wind speed
increases as a power function of the radius inside the eye and decays
exponentially outside the eye after a smooth polynomial transition
across the eyewall.

\\\left\\\begin{aligned} v_r &= v_m \times
\left(\frac{r}{R_m}\right)^{n} \quad if \quad r \< R_m \\ v_r &= v_m
\times \left((1-A) \times e^{-\frac{\|r-R_m\|}{X1}} + A \times
e^{-\frac{\|r-R_m\|}{X2}}\right) \quad if \quad r \geq R_m \\
\end{aligned} \right. \\

with,

\\n = 2.1340 + 0.0077 \times v_m - 0.4522 \times \ln(R_m) - 0.0038
\times \|\phi\|\\

\\X1 = 287.6 - 1.942 \times v_m + 7.799 \times \ln(R_m) + 1.819 \times
\|\phi\|\\

\\A = 0.5913 + 0.0029 \times v_m - 0.1361 \times \ln(R_m) - 0.0042
\times \|\phi\|\\ and \\A\ge0\\

where, \\v_r\\ is the tangential wind speed (in \\m.s^{-1}\\), \\v_m\\
is the maximum sustained wind speed (in \\m.s^{-1}\\), \\r\\ is the
distance to the eye of the storm (in \\km\\), \\R_m\\ is the radius of
maximum sustained wind speed (in \\km\\), \\\phi\\ is the latitude of
the centre of the storm, \\X2 = 25\\.

Asymmetry can be added to Holland (1980) and Willoughby et al. (2006)
wind fields as follows,

\\\vec{V} = \vec{V_c} + C \times \vec{V_t}\\

where, \\\vec{V}\\ is the combined asymmetric wind field, \\\vec{V_c}\\
is symmetric wind field, \\\vec{V_t}\\ is the translation speed of the
storm, and \\C\\ is function of \\r\\, the distance to the eye of the
storm (in \\km\\).

Two formulations of C proposed by Miyazaki et al. (1962) and Chen (1994)
are implemented.

Miyazaki et al. (1962) \\C = e^{(-\frac{r}{500} \times \pi)}\\

Chen (1994) \\C = \frac{3 \times R_m^{\frac{3}{2}} \times
r^{\frac{3}{2}}}{R_m^3 + r^3 +R_m^{\frac{3}{2}} \times
r^{\frac{3}{2}}}\\

where, \\R_m\\ is the radius of maximum sustained wind speed (in \\km\\)

The Boose et al. (2004) model, or “HURRECON” model, is a modification of
the Holland (1980) model. In addition to adding asymmetry, this model
treats of water and land differently, using different surface friction
coefficient for each.

\\v_r = F\left(v_m - S \times (1 - \sin(T)) \times \frac{v_h}{2} \right)
\times \sqrt{\left(\frac{R_m}{r}\right)^b \times e^{1 -
\left(\frac{R_m}{r}\right)^b}}\\

with,

\\b = \frac{\rho \times e \times v_m^2}{p\_{oci} - p_c}\\

where, \\v_r\\ is the tangential wind speed (in \\m.s^{-1}\\), \\F\\ is
a scaling parameter for friction (\\1.0\\ in water, \\0.8\\ in land),
\\v_m\\ is the maximum sustained wind speed (in \\m.s^{-1}\\), \\S\\ is
a scaling parameter for asymmetry (usually set to \\1\\), \\T\\ is the
oriented angle (clockwise/counter clockwise in Northern/Southern
Hemisphere) between the forward trajectory of the storm and a radial
line from the eye of the storm to point \$r\$ \\v_h\\ is the storm
velocity (in \\m.s^{-1}\\), \\R_m\\ is the radius of maximum sustained
wind speed (in \\km\\), \\r\\ is the distance to the eye of the storm
(in \\km\\), \\b\\ is the shape parameter, \\\rho = 1.15\\ is the air
density (in \\kg.m^{-3}\\), \\p\_{oci}\\ is the pressure at outermost
closed isobar of the storm (in \\Pa\\), and \\p_c\\ is the pressure at
the centre of the storm (\\pressure\\ in \\Pa\\).

## References

Boose, E. R., Serrano, M. I., & Foster, D. R. (2004). Landscape and
regional impacts of hurricanes in Puerto Rico. Ecological Monographs,
74(2), Article 2. https://doi.org/10.1890/02-4057

Chen, K.-M. (1994). A computation method for typhoon wind field. Tropic
Oceanology, 13(2), 41–48.

Holland, G. J. (1980). An Analytic Model of the Wind and Pressure
Profiles in Hurricanes. Monthly Weather Review, 108(8), 1212–1218.
https://doi.org/10.1175/1520-0493(1980)108\<1212:AAMOTW\>2.0.CO;2

Knapp, K. R., Kruk, M. C., Levinson, D. H., Diamond, H. J., & Neumann,
C. J. (2010). The International Best Track Archive for Climate
Stewardship (IBTrACS). Bulletin of the American Meteorological Society,
91(3), Article 3. https://doi.org/10.1175/2009bams2755.1

Miyazaki, M., Ueno, T., & Unoki, S. (1962). The theoretical
investigations of typhoon surges along the Japanese coast (II).
Oceanographical Magazine, 13(2), 103–117.

Willoughby, H. E., Darling, R. W. R., & Rahn, M. E. (2006). Parametric
Representation of the Primary Hurricane Vortex. Part II: A New Family of
Sectionally Continuous Profiles. Monthly Weather Review, 134(4),
1102–1120. https://doi.org/10.1175/MWR3106.1

## Examples

``` r
# \donttest{
# Creating a stormsDataset
sds <- defStormsDataset()
#> Warning: No basin argument specified. StormR will work as expected
#>              but cannot use basin filtering for speed-up when collecting data
#> === Loading data  ===
#> Open database... /home/runner/work/_temp/Library/StormR/extdata/test_dataset.nc opened
#> Collecting data ...
#> === DONE ===

# Geting storm track data for tropical cyclone Pam (2015) near Vanuatu
pam <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM")
#> === Storms processing ... ===
#> 
#> -> Making buffer: Done
#> -> Searching for PAM storm ...
#>    -> Identifying Storms: Done
#> -> Gathering storm(s) ... 
#> 
#> === DONE with run time 0.4695158 sec ===
#> 
#> SUMMARY:
#> (*) LOI: Vanuatu 
#> (*) Buffer size: 300 km
#> (*) Number of storms: 1 
#>         Name - Tropical season - Scale - Number of observation within buffer:
#>         PAM - 2015 - 6 - 20 
#> 

# Computing maximum sustained wind speed generated by Pam (2015) near Vanuatu
# using default settings
msw.pam <- spatialBehaviour(pam)
#> === spatialBehaviour processing ... ===
#> 
#> Initializing data ... Done
#> 
#> Computation settings:
#>   (*) Temporal resolution: Every 60  minutes
#>   (*) Space resolution: 2.5min 
#>   (*) Method used: Willoughby 
#>   (*) Product(s) to compute: MSW 
#>   (*) Asymmetry used: Chen 
#> 
#> Storm(s):
#>   ( 1 )  PAM 
#> 
#> PAM  ( 1 / 1 )
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |===================                                                   |  26%  |                                                                              |====================                                                  |  28%  |                                                                              |=====================                                                 |  29%  |                                                                              |======================                                                |  31%  |                                                                              |=======================                                               |  32%  |                                                                              |========================                                              |  34%  |                                                                              |=========================                                             |  35%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  38%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================                |  76%  |                                                                              |=======================================================               |  78%  |                                                                              |========================================================              |  79%  |                                                                              |=========================================================             |  81%  |                                                                              |==========================================================            |  82%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  85%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%
#> 
#> === DONE with run time 3.244 sec ===
#> 
#> Output:
#> SpatRaster stack with 1 layers:
#> index - name of layers
#>   1     PAM_MSW 
#> 

# Computing PDI generated by Pam (2015) near Vanuatu using the Holland model without asymmetry
pdi.pam <- spatialBehaviour(pam, method = "Holland", product = "PDI", asymmetry = "None")
#> === spatialBehaviour processing ... ===
#> 
#> Initializing data ... Done
#> 
#> Computation settings:
#>   (*) Temporal resolution: Every 60  minutes
#>   (*) Space resolution: 2.5min 
#>   (*) Method used: Holland 
#>   (*) Product(s) to compute: PDI 
#>   (*) Asymmetry used: None 
#> 
#> Storm(s):
#>   ( 1 )  PAM 
#> 
#> PAM  ( 1 / 1 )
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |===================                                                   |  26%  |                                                                              |====================                                                  |  28%  |                                                                              |=====================                                                 |  29%  |                                                                              |======================                                                |  31%  |                                                                              |=======================                                               |  32%  |                                                                              |========================                                              |  34%  |                                                                              |=========================                                             |  35%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  38%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================                |  76%  |                                                                              |=======================================================               |  78%  |                                                                              |========================================================              |  79%  |                                                                              |=========================================================             |  81%  |                                                                              |==========================================================            |  82%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  85%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%
#> 
#> === DONE with run time 4.321 sec ===
#> 
#> Output:
#> SpatRaster stack with 1 layers:
#> index - name of layers
#>   1     PAM_PDI 
#> 

# Computing duration of exposure to Saffir-Simpson hurricane wind scale threshold values
# during Pam (2015) near Vanuatu using default settings
exp.pam <- spatialBehaviour(pam, product = "Exposure")
#> === spatialBehaviour processing ... ===
#> 
#> Initializing data ... Done
#> 
#> Computation settings:
#>   (*) Temporal resolution: Every 60  minutes
#>   (*) Space resolution: 2.5min 
#>   (*) Method used: Willoughby 
#>   (*) Product(s) to compute: Exposure 
#>   (*) Asymmetry used: Chen 
#> 
#> Storm(s):
#>   ( 1 )  PAM 
#> 
#> PAM  ( 1 / 1 )
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |===================                                                   |  26%  |                                                                              |====================                                                  |  28%  |                                                                              |=====================                                                 |  29%  |                                                                              |======================                                                |  31%  |                                                                              |=======================                                               |  32%  |                                                                              |========================                                              |  34%  |                                                                              |=========================                                             |  35%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  38%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================                |  76%  |                                                                              |=======================================================               |  78%  |                                                                              |========================================================              |  79%  |                                                                              |=========================================================             |  81%  |                                                                              |==========================================================            |  82%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  85%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%
#> 
#> === DONE with run time 9.707 sec ===
#> 
#> Output:
#> SpatRaster stack with 6 layers:
#> index - name of layers
#>   1     PAM_Exposure_18 
#>   2     PAM_Exposure_33 
#>   3     PAM_Exposure_42 
#>   4     PAM_Exposure_49 
#>   5     PAM_Exposure_58 
#>   6     PAM_Exposure_70 
#> 

# Computing wind speed and direction profiles  generated by Pam (2015) near Vanuatu
# using Boose model
prof.pam <- spatialBehaviour(pam, product = "Profiles", method = "Boose")
#> === spatialBehaviour processing ... ===
#> 
#> Initializing data ... Done
#> 
#> Computation settings:
#>   (*) Temporal resolution: Every 60  minutes
#>   (*) Space resolution: 2.5min 
#>   (*) Method used: Boose 
#>   (*) Product(s) to compute: Profiles 
#>   (*) Asymmetry used: None 
#> 
#> Storm(s):
#>   ( 1 )  PAM 
#> 
#> PAM  ( 1 / 1 )
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   9%  |                                                                              |========                                                              |  11%  |                                                                              |=========                                                             |  12%  |                                                                              |==========                                                            |  14%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  18%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |================                                                      |  23%  |                                                                              |==================                                                    |  25%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  32%  |                                                                              |========================                                              |  34%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  38%  |                                                                              |============================                                          |  39%  |                                                                              |=============================                                         |  41%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  46%  |                                                                              |==================================                                    |  48%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  52%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  55%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  61%  |                                                                              |============================================                          |  62%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  66%  |                                                                              |================================================                      |  68%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |==========================================================            |  82%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |================================================================      |  91%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  95%  |                                                                              |====================================================================  |  96%  |                                                                              |===================================================================== |  98%  |                                                                              |======================================================================| 100%
#> 
#> === DONE with run time 33.125 sec ===
#> 
#> Output:
#> SpatRaster stack with 114 layers:
#> index - name of layers
#>   1     PAM_Speed_28 
#>   2     PAM_Speed_28.33 
#>   3     PAM_Speed_28.67 
#>   4     PAM_Speed_29 
#>   5     PAM_Speed_29.33 
#>   6     PAM_Speed_29.67 
#>   7     PAM_Speed_30 
#>   8     PAM_Speed_30.33 
#>   9     PAM_Speed_30.67 
#>   10     PAM_Speed_31 
#>   11     PAM_Speed_31.33 
#>   12     PAM_Speed_31.67 
#>   13     PAM_Speed_32 
#>   14     PAM_Speed_32.33 
#>   15     PAM_Speed_32.67 
#>   16     PAM_Speed_33 
#>   17     PAM_Speed_33.33 
#>   18     PAM_Speed_33.67 
#>   19     PAM_Speed_34 
#>   20     PAM_Speed_34.33 
#>   21     PAM_Speed_34.67 
#>   22     PAM_Speed_35 
#>   23     PAM_Speed_35.33 
#>   24     PAM_Speed_35.67 
#>   25     PAM_Speed_36 
#>   26     PAM_Speed_36.33 
#>   27     PAM_Speed_36.67 
#>   28     PAM_Speed_37 
#>   29     PAM_Speed_37.33 
#>   30     PAM_Speed_37.67 
#>   31     PAM_Speed_38 
#>   32     PAM_Speed_38.33 
#>   33     PAM_Speed_38.67 
#>   34     PAM_Speed_39 
#>   35     PAM_Speed_39.33 
#>   36     PAM_Speed_39.67 
#>   37     PAM_Speed_40 
#>   38     PAM_Speed_40.33 
#>   39     PAM_Speed_40.67 
#>   40     PAM_Speed_41 
#>   41     PAM_Speed_41.33 
#>   42     PAM_Speed_41.67 
#>   43     PAM_Speed_42 
#>   44     PAM_Speed_42.33 
#>   45     PAM_Speed_42.67 
#>   46     PAM_Speed_43 
#>   47     PAM_Speed_43.33 
#>   48     PAM_Speed_43.67 
#>   49     PAM_Speed_44 
#>   50     PAM_Speed_44.33 
#>   51     PAM_Speed_44.67 
#>   52     PAM_Speed_45 
#>   53     PAM_Speed_45.33 
#>   54     PAM_Speed_45.67 
#>   55     PAM_Speed_46 
#>   56     PAM_Speed_46.33 
#>   57     PAM_Speed_46.67 
#>   58     PAM_Direction_28 
#>   59     PAM_Direction_28.33 
#>   60     PAM_Direction_28.67 
#>   61     PAM_Direction_29 
#>   62     PAM_Direction_29.33 
#>   63     PAM_Direction_29.67 
#>   64     PAM_Direction_30 
#>   65     PAM_Direction_30.33 
#>   66     PAM_Direction_30.67 
#>   67     PAM_Direction_31 
#>   68     PAM_Direction_31.33 
#>   69     PAM_Direction_31.67 
#>   70     PAM_Direction_32 
#>   71     PAM_Direction_32.33 
#>   72     PAM_Direction_32.67 
#>   73     PAM_Direction_33 
#>   74     PAM_Direction_33.33 
#>   75     PAM_Direction_33.67 
#>   76     PAM_Direction_34 
#>   77     PAM_Direction_34.33 
#>   78     PAM_Direction_34.67 
#>   79     PAM_Direction_35 
#>   80     PAM_Direction_35.33 
#>   81     PAM_Direction_35.67 
#>   82     PAM_Direction_36 
#>   83     PAM_Direction_36.33 
#>   84     PAM_Direction_36.67 
#>   85     PAM_Direction_37 
#>   86     PAM_Direction_37.33 
#>   87     PAM_Direction_37.67 
#>   88     PAM_Direction_38 
#>   89     PAM_Direction_38.33 
#>   90     PAM_Direction_38.67 
#>   91     PAM_Direction_39 
#>   92     PAM_Direction_39.33 
#>   93     PAM_Direction_39.67 
#>   94     PAM_Direction_40 
#>   95     PAM_Direction_40.33 
#>   96     PAM_Direction_40.67 
#>   97     PAM_Direction_41 
#>   98     PAM_Direction_41.33 
#>   99     PAM_Direction_41.67 
#>   100     PAM_Direction_42 
#>   101     PAM_Direction_42.33 
#>   102     PAM_Direction_42.67 
#>   103     PAM_Direction_43 
#>   104     PAM_Direction_43.33 
#>   105     PAM_Direction_43.67 
#>   106     PAM_Direction_44 
#>   107     PAM_Direction_44.33 
#>   108     PAM_Direction_44.67 
#>   109     PAM_Direction_45 
#>   110     PAM_Direction_45.33 
#>   111     PAM_Direction_45.67 
#>   112     PAM_Direction_46 
#>   113     PAM_Direction_46.33 
#>   114     PAM_Direction_46.67 
#> 
# }
```
