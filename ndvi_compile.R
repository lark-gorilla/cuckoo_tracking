## 25/05/18
## Downloading, processing and extracting MODIS NDVI data

library(MODIS)

#check for GDAL
MODISoptions()

# add GDAL path to list
Sys.setenv(PATH="C:\\Users\\mark.gr.miller\\Documents\\R\\R-3.4.4\\bin\\x64;C:\\ProgramData\\Oracle\\Java\\javapath;C:\\WINDOWS\\system32;C:\\WINDOWS;C:\\WINDOWS\\System32\\Wbem;C:\\WINDOWS\\System32\\WindowsPowerShell\\v1.0\\;C:\\Users\\mark.gr.miller\\AppData\\Local\\Microsoft\\WindowsApps;C:\\Users\\mark.gr.miller\\AppData\\Local\\Programs\\Git\\cmd;C://Program Files//QGIS 2.18//bin")

# recheck
MODISoptions()

ng<-getTile(x='Nigeria')

gd<-runGdal(product='MYD13A2', extent='Nigeria',  begin="2018001", end="2018005")