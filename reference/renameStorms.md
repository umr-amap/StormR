# Renaming storms to avoid duplicated names in a `stormsList` object

The `renameStorms()` function renames all storms from a `stormsList`
that have the same name. It is very common for "UNNAMED" storms for
example. These storms will be renamed "UNNAMED-YEAR-1",
"UNNAMED-YEAR-2", ...

## Usage

``` r
renameStorms(sts)
```

## Arguments

- sts:

  `stormsList` object

## Value

`stormsList` object with no duplicated names of storms

## Examples

``` r
if (FALSE) { # \dontrun{
#Creating a stormsDataset
sds <- defStormsDataset(...)
sts <- defStormsList(...)
getNames(sts)
## "UNNAMED"   "UNNAMED"   "UNNAMED"   "ALLEN"     "CHARLEY"   "DANIELLE"  "JEANNE"    "UNNAMED" 
## "UNNAMED"   "UNNAMED"   "DENNIS"    ...

sts <- renameStorms(sts)
getNames(sts)
## "UNNAMED-1980-1"   "UNNAMED-1980-2"   "UNNAMED-1980-3"   "ALLEN-1980"     "CHARLEY-1980"  ...
## "UNNAMED-1980-6"   "UNNAMED-1980-7"   "DENNIS-1980"    ...
} # }
```
