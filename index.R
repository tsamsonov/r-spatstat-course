## ----setup-index, include=FALSE--------------------------------------------------------------------
library(methods)
library(knitr)
library(DT)
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE)
knitr::opts_knit$set(global.par = TRUE)
knitr::knit_hooks$set(webgl = 'hook_webgl')

write_bib(c("ggplot2", "gganimate", "lubridate"), file = "packages.bib")

