#installed.packages()
list.of.packages <- c("ggplot2", "stats", "splines", "MASS", "crs", "sem", "stargazer", "AER", "xtable", "fBasics", "mi", "Amelia", "parallel", "Rlab")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


