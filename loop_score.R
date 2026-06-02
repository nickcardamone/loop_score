library(plotrix)
library(pracma)
library(tidyverse)
library(plotKML)
require(dplyr)
require(data.table)
require(ggforce)
library(geohashTools)
require(trackeR)
setwd("C:\\Users\\Nick\\Downloads\\")

df <- trackeR::readGPX("The_Catalina_Wine_Mixer_5_Miler_4th.gpx")

df <- trackeRdata(df, sport = "running")

df <- smoother(df)
p <- as.data.frame(df[[1]])

loop <- p[, 1:2]
require(HistDAWass)
loop_s <- DouglasPeucker(data.matrix(loop), 0.000005)
#loop = loop[seq(1, nrow(loop), 6), ]


colnames(loop_s)[1:2] <- c("lat", "lon")
loop <- as.data.frame(loop_s)
#output <- pracma::circlefit(loop$lon, loop$lat)

circlefit_manual <- function(xp, yp, fast = TRUE) {
  if (!is.vector(xp, mode="numeric") || !is.vector(yp, mode="numeric"))
    stop("Arguments 'xp' and 'yp' must be numeric vectors.")
  if (length(xp) != length(yp))
    stop("Vectors 'xp' and 'yp' must be of the same length.")
  if (!fast)
    warning("Option 'fast' is deprecated and will not be used!",
            call. = FALSE, immediate. = TRUE)
  
  n  <- length(xp)
  p <- qr.solve(cbind(xp, yp, 1), matrix(xp^2 + yp^2, ncol = 1))
  v <- c(p[1]/2, p[2]/2, sqrt((p[1]^2 + p[2]^2)/4 + p[3]))
  
  rms <- sqrt(sum((sqrt((xp-v[1])^2 + (yp-v[2])^2) - v[3])^2)/n)
  rse <- sum((sqrt((xp-v[1])^2 + (yp-v[2])^2) - v[3])^2)/sum((sqrt((xp-v[1])^2 + (yp-v[2])^2) - (sum(v[3])/n))^2)
  rrms <- sqrt((sum((sqrt((xp-v[1])^2 + (yp-v[2])^2) - v[3])^2)/n)/ sum((v[3])^2))
  
  
  cat("RM Sq error:", rms, "\n")
  cat("Relative Sq error:", rse, "\n")
  cat("Relative RM Sq error:", rrms, "\n")
  data.frame(v) %>% transpose()
}

out <- circlefit_manual(loop$lon, loop$lat)


loop <- loop %>% mutate(geohash = gh_encode(lat, lon, precision = 7L))

# Strava repots lon and lat to the 5th decimal place. Which is precision down to the square meter. 
# Concatenate the absolute value of lat and lon to make a location ID. Multiply?
loop <- loop %>% mutate(Streak = if_else(geohash == lag(geohash), T, F), count = sequence(rle(as.character(Streak))$lengths),
              rlid = rleid(Streak))

## PURITY
loop$rlid <- as.factor(loop$rlid)
loop_pure <- loop %>% group_by(rlid) %>% dplyr::summarise(point = first(geohash))
loop_pure <- loop_pure[-1,]

nrow(data.frame(unique(loop_pure$point)))/nrow(loop_pure)

## Mark duplicates
loop_pure$dup <- c(duplicated(loop_pure$point, fromLast = TRUE)  | duplicated(loop_pure$point))

loop2 <- left_join(loop, loop_pure, "rlid")

loop2 <- loop2[-1, ]

require(plotly)

p <- ggplot(loop2, aes(x = lon, y = lat, color = as.factor(dup), size = as.factor(dup))) + geom_jitter() + guides(color = guide_legend(reverse=T)) + ggforce::geom_circle(aes(x0 = out$V1, y0 = out$V2, r = out$V3), n = 50, inherit.aes = F)
#p <- ggplot(loop, aes(x = lon, y = lat)) + geom_jitter()
ggplotly(p)

