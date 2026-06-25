# Removing specific storms from a `stormsList` object

The `removeStorms()` function removes specified storms from a
`stormsList` object based on their names and seasons.

## Usage

``` r
removeStorms(sts, names, seasons)
```

## Arguments

- sts:

  `stormsList` object

- names:

  character vector. Names of storms to remove (in capital letters)

- seasons:

  numeric vector. Seasons of storms to remove (same length as `names`)

## Value

`stormsList` object without the specified storms

## Examples

``` r
if (FALSE) { # \dontrun{
sds <- defStormsDataset(...)
sts <- defStormsList(sds, loi="Vanuatu", seasons=c(1990, 2000))

# Remove specific storms
sts <- removeStorms(sts, names=c("UNNAMED", "BARRY"), seasons=c(1990, 1989))
} # }
```
