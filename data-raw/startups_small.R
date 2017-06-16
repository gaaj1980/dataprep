# Code to create R's data archives, loading data from CSV
startupssmall <- read.csv("data-raw/startups.csv")
devtools::use_data(startupssmall, overwrite = TRUE)

startupsbig <- rbind(startupssmall,startupssmall)
for (i in seq(1:15)) {
  startupsbig <- rbind(startupsbig, startupsbig)  
}
devtools::use_data(startupsbig, overwrite = TRUE)

startupshuge <- startupsbig
for (i in seq(1:9)) {
  startupsaux <- startupsbig
  names(startupsaux) <- paste0(names(startupsaux),"_",i)
  startupshuge <- cbind(startupshuge, startupsaux)  
}
devtools::use_data(startupshuge, overwrite = TRUE)
