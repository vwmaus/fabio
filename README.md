# fabio
Forestry and Agriculture Biomass Input-Output Tables

In 2015 we started building physical supply-use and input-output tables covering global agriculture and forestry. The work is fully based on freely available data from FAOSTAT, IEA, EIA and UN Comtrade.

FABIO now covers 191 countries, 116 processes and 130 commodities for the years 1986-2013. The code will be released here in July 2018. An R package is in preparation.

# Depenencies
`FAO_MRIO_4b_footprints` requires non-CRAN rpackage `rhdf5`. It needs to be installed by running
```
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
```

