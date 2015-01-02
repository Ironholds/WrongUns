library(WMUtils)
library(lubridate)
library(cryptohash)
library(rgeoip)
library(data.table)

bots <- c("/wiki/FSArchiver","/wiki/Additive_white_Gaussian_noise")
humans <- c("/wiki/Ariana_Grande","/wiki/Fidel_Castro")
plausibly_bots <- c("/wiki/Surfers_Paradise_Meter_Maids","/wiki/Fugio_Cent")