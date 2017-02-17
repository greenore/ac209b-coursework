## Set Java path if necessary
if (Sys.info()["sysname"] == "Windows" & Sys.getenv("JAVA_HOME") == "") {
  java_path <- "C:/Program Files (x86)/Java/jre7"
  Sys.setenv(JAVA_HOME = java_path)
  cat(paste("Set Java path to", java_path))
}

## CRAN packages
tidyverse_packages <- c("tidyverse", "classInt")
visualization_packages <- c("RColorBrewer", "ggbeeswarm", "pander", "scales",
                            "gridExtra")
modelling_packages <- c("splines", "gam", "boot")
mapping_packages <- c("maptools", "rgeos", "rgdal")

required_packages <- c(tidyverse_packages, visualization_packages, modelling_packages, mapping_packages)
packagesCRAN(required_packages, update = setMissingVar(var_name = "update_package", value = FALSE))

## Clear Workspace
rm(list = ls())
