## --------------------------------------------------------------------------------------------------
library(sf)
library(RColorBrewer)
library(readxl)
library(tidyverse)
library(tmap)
library(GWmodel)


## ---- fig.cap="Весовая функция", echo=FALSE--------------------------------------------------------
knitr::include_graphics("images/gwr_wlocal.png")


## ---- fig.cap="Фиксированная весовая функция", echo=FALSE------------------------------------------
knitr::include_graphics("images/wfixed.png")


## ---- fig.cap="Адаптивная весовая функция", echo=FALSE---------------------------------------------
knitr::include_graphics("images/wadaptive.png")


## --------------------------------------------------------------------------------------------------
realest = read_delim(url('https://www.jefftk.com/apartment_prices/apts-1542637382.txt'),
                 delim = ' ',
                 col_names = c('price', 'rooms', 'id', 'lon', 'lat')) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  st_transform(3395)

# tmap_mode('view')
tm_shape(realest) +
  tm_bubbles(col = 'price',
             size = 'rooms',
             style = 'fixed',
             breaks = c(0, 1000, 2000, 3000, 4000, 5000, 10000, max(realest$price)),
             scale = 0.25,
             palette = colorRampPalette(c('steelblue4', 'orange', 'darkred'))(7),
             alpha = 0.8) +
  tm_view(symbol.size.fixed = TRUE)


## ---- eval=FALSE-----------------------------------------------------------------------------------
## samples = realest %>% dplyr::sample_n(1000) %>% as('Spatial')
## 
## (gwr_res = gwr.basic(price ~ rooms, data = samples, bw = 1000, kernel = 'gaussian'))
## 
## tm_shape(gwr_res$SDF) +
##   tm_bubbles(col = 'rooms', # это не количество комнат, а коэффициент регрессии
##              style = 'quantile',
##              scale = 0.3,
##              palette = 'Reds',
##              alpha = 0.5) +
##   tm_view(symbol.size.fixed = TRUE)


## ---- echo=FALSE-----------------------------------------------------------------------------------
knitr::include_url('https://tsamsonov.github.io/r-geo-course/slides/15-SpatialRegression_slides.html#1', height = '500px')

