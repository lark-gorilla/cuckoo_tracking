## 25/05/18
## Downloading, processing and extracting MODIS NDVI data

# Following tutorial:
#https://cran.r-project.org/web/packages/MODIStsp/vignettes/MODIStsp.pdf

#install.packages("gWidgetsRGtk2")
library(gWidgetsRGtk2)

library(devtools)
install_github("lbusett/MODIStsp")

library(MODIStsp)
MODIStsp()
# Thats all you need to use this tool! nice!

