## ----setup-patterns, include=FALSE-----------------------------------------------------------------
library(tidyverse)
library(raster)
library(spatstat)
library(stars)
library(tmap)
library(sf)
library(sp)


## ---- echo = FALSE, fig.width=10, fig.height=5, dpi=300--------------------------------------------
lambda = function(x,y) { 500 * (y^2 + x) }

U = rpoispp(lambda, win=square(1))

par(mfrow = c(1,2))
plot(U, pch = 20)
plot(density(U))


## ---- echo=FALSE-----------------------------------------------------------------------------------
knitr::include_url('https://tsamsonov.github.io/r-geo-course/slides/16-PointPatterns_slides.html#1', height = '500px')

