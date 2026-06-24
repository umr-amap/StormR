suppressWarnings(sds <- defStormsDataset(verbose = 0))
pam <- defStormsList(sds = sds, loi = "Vanuatu", names = "PAM", verbose = 0)
sts_nc <- defStormsList(sds = sds, loi = "New Caledonia", verbose = 0)
