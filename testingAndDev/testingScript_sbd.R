
library(devtools)
library(roxygen2)

#setwd("C:/Users/kcawley/Documents/GitHub/NEON-utilities")
#install("neonUtilities")
#install_github("NEONScience/NEON-utilities/neonUtilities", force = T)

setwd("C:/Users/kcawley/Documents/GitHub/NEON-stream-discharge")
install("streamQ")
library(streamQ)

dataDirKMC <- "C:/Users/kcawley/Downloads/NEON_discharge-stream-saltbased.zip"
dataDirKMC <- "API"
site <- "KING"

sbdFormatted <- def.format.Q(dataDir = dataDirKMC, site = site)
#write.csv(sbdFormatted, 'C:\\Users\\kcawley\\Documents\\GitHub\\biogeochemistryIPT\\reaeration\\Science Only\\rCodeForRelease\\streamQ\\inst\\extdata\\sbdFormatted.csv', row.names = F, na = '')

sbdDataPlusInjQ <- def.calc.Q.inj(inputFile = sbdFormatted)

sbdDataPlusSlugQ <- def.calc.Q.slug(inputFile = sbdDataPlusInjQ, dataDir = dataDirKMC, pick = T, site = site)
sbdDataPlusSlugQ <- def.calc.Q.slug(inputFile = sbdDataPlusInjQ, dataDir = dataDirKMC, plot = T, site = site)
sbdDataPlusSlugQ <- def.calc.Q.slug(inputFile = sbdDataPlusInjQ, dataDir = dataDirKMC, site = site)

setwd("C:/Users/kcawley/Documents/GitHub/NEON-stream-discharge/streamQ")
#devtools::use_data(sbdFormatted, sbdFormatted, overwrite = T)
document()
devtools::check()
