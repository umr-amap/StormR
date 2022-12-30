

prof_nc = stormBehaviour(sts_nc, format = "profiles", verbose = TRUE)
writeRast(prof_nc[["ERICA_profile93"]])
