# Package index

## Initialize storms

Functions for initializing storm/stormsList objects

- [`stormsDataset-class`](stormsDataset-class.md)
  [`stormsDataset`](stormsDataset-class.md) : stormsDataset

- [`defStormsDataset()`](defStormsDataset.md) :

  Creating a `stormsDataset` object

- [`storm-class`](storm-class.md) [`storm`](storm-class.md) :

  `storm` object

- [`stormsList-class`](stormsList-class.md)
  [`stormsList`](stormsList-class.md) :

  `stormsList` object

- [`defStormsList()`](defStormsList.md) :

  Creating a `stormsList` object

- [`renameStorms()`](renameStorms.md) :

  Renaming storms to avoid duplicated names in a `stormsList` object

## Getters

Functions for getting data associated with storm/stormsList objects

- [`getStorm()`](getStorm-methods.md) :

  Extracting a `storm`

- [`getNames()`](getNames-methods.md) : Getting the names of the storms

- [`getSeasons()`](getSeasons-methods.md) : Getting cyclonic seasons of
  the storms

- [`getScale()`](getScale-methods.md) : Getting maximum level in the
  wind scale

- [`getNbObs()`](getNbObs-methods.md) : Getting the number of
  observations

- [`getInObs()`](getInObs-methods.md) : Getting the number of the
  observations

- [`getObs()`](getObs-methods.md) : Getting observations

- [`getNbStorms()`](getNbStorms-methods.md) :

  Getting the number of `storm`

- [`getLOI()`](getLOI-methods.md) : Getting the location of interest

- [`getBuffer()`](getBuffer-methods.md) : Getting the buffered location
  of interest

- [`getBufferSize()`](getBufferSize-methods.md) : Getting the buffer
  size

## Compute products

Functions for computing products

- [`spatialBehaviour()`](spatialBehaviour.md) : Computing wind behaviour
  and summary statistics over given areas
- [`temporalBehaviour()`](temporalBehaviour.md) : Computing wind
  behaviour time series and summary statistics at given point locations

## Plot

Functions for plotting tracks and products

- [`plotStorms()`](plotStorms.md) : Plotting storm track data
- [`plotBehaviour()`](plotBehaviour.md) : Plotting spatial wind
  behaviour
- [`plotTemporal()`](plotTemporal.md) : Plotting wind behaviour time
  series and summary statistics at given point locations

## Write

Function for writing data in .GeoTiff or .nc format

- [`writeRast()`](writeRast.md) : Exporting rasters to GeoTIFF or NetCDF
  files

## Data

Examples data provided by this package

- [`eezNC`](eezNC.md) : EEZ of New Caledonia
