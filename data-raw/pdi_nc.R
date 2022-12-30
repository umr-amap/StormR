
pdi_nc = stormBehaviour(sts_nc, time_res = 0.5, method = "Holland80", product = "PDI", verbose = TRUE)
writeRast(pdi_nc)
