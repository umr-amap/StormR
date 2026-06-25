# Keeping only specific storms from a `stormsList` object

The `subsetStorms()` function keeps only specified storms from a
`stormsList` object based on their names and seasons, removing all
others.

## Usage

``` r
subsetStorms(sts, names, seasons)
```

## Arguments

- sts:

  `stormsList` object

- names:

  character vector. Names of storms to keep (in capital letters)

- seasons:

  numeric vector. Seasons of storms to keep (same length as `names`)

## Value

`stormsList` object containing only the specified storms

## Examples

``` r
if (FALSE) { # \dontrun{
sds <- defStormsDataset(...)
sts <- defStormsList(sds, loi="Vanuatu", seasons=c(1990, 2000))

# Keep only specific storms
sts <- subsetStorms(sts, names=c("PAM", "NIRAN"), seasons=c(2015, 2021))
} # }
```
