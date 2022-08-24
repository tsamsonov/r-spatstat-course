## Пусть $X_1,\ldots, X_n$ случайные величины, а $Y_1 = \sum\limits_{i=1}^n a_i X_i,\; Y_2 = \sum\limits_{j=1}^m b_j X_j$ — их две произвольные линейные комбинации. Тогда:

## 

## $$\Cov[Y_1,Y_2] = \sum\limits_{i=1}^n\sum\limits_{j=1}^m a_i b_j \Cov[X_i,X_j].$$


## ---- fig.height=3, dpi=300, fig.align='center'----------------------------------------------------
n = 60
a = 40
h = 0:n

tab = tibble::tibble(
  h = 0:60,
  gamma = c(3 * (0:(a-1)) / (2 * a) - 0.5 * (0:(a-1) / a)^3, rep(1, n-a+1))
)

ggplot() +
  geom_line(tab, mapping = aes(h, gamma), size = 1, color = 'steelblue') +
  geom_vline(xintercept = a, color = 'orangered') +
  annotate("text", x = a + 3, y = 0.5625, label = paste("a =", a), color = 'orangered') + 
  theme_bw()


## ---- fig.height=3, dpi=300, fig.align='center'----------------------------------------------------
tab = tibble::tibble(
  h = h,
  gamma = 1 - exp(-3*h/a)
)

pl = ggplot() +
  geom_line(tab, mapping = aes(h, gamma), size = 1, color = 'steelblue') +
  geom_vline(xintercept = a, color = 'orangered') +
  annotate("text", x = a + 3, y = 0.5625, label = paste("a =", a), color = 'orangered') + 
  theme_bw()

(pl)


## ---- fig.height=3, dpi=300, fig.align='center'----------------------------------------------------
tab = tibble::tibble(
  h = h,
  gamma = 1 - exp(-3*h^2/a^2)
)

pl = ggplot() +
  geom_line(tab, mapping = aes(h, gamma), size = 1, color = 'steelblue') +
  geom_vline(xintercept = a, color = 'orangered') +
  annotate("text", x = a + 3, y = 0.5625, label = paste("a =", a), color = 'orangered') + 
  theme_bw()

(pl)


## ---- fig.height=3, dpi=300, fig.align='center'----------------------------------------------------
tab = tibble::tibble(
  h = h,
  gamma = h^1.5
)

pl = ggplot() +
  geom_line(tab, mapping = aes(h, gamma), size = 1, color = 'steelblue') +
  theme_bw()

(pl)


## ----sam, fig.height=3, fig.align='center', dpi=300------------------------------------------------
tab = tibble::tibble(
  gamma = rep(1, n+1),
  h = h
)

ggplot() +
  geom_line(tab, mapping = aes(h, gamma), size = 1, color = 'steelblue') +
  geom_point(data = data.frame(x = 0, y = 1), mapping = aes(x, y), shape=21,
             colour = 'steelblue', fill = 'white', size = 3, stroke = 1.5) +
  annotate('point', x = 0, y = 0, color = 'steelblue', size = 4) +
  theme_bw()


## ---- fig.height=3.5, dpi=300, fig.align='center'--------------------------------------------------
options(scipen = 999)

cities = st_read("data/Italy_Cities.gpkg")

rainfall = read_table2("data/Rainfall.dat") %>% 
  st_as_sf(coords = c('x', 'y'), 
           crs = st_crs(cities),
           remove = FALSE)

hscat(rain_24~1, data = rainfall, 1000 * c(0, 10, 20, 50, 100), pch = 19)


## ---- fig.height=4, dpi=300------------------------------------------------------------------------
varcl = variogram(rain_24~1, data=rainfall, cutoff = 150000, cloud=TRUE)

ggplot(varcl) +
  geom_point(aes(dist, gamma), alpha = 0.5, size = 2, color = 'steelblue') +
  ylab('semivariance') +
  theme_bw()


## ---- fig.height=3.5, dpi=300----------------------------------------------------------------------
width = 10000
intervals = width * 0:15

vargr = variogram(rain_24~1, data=rainfall, cutoff = 150000, width = width)


## ---- echo = FALSE---------------------------------------------------------------------------------
ggplot() +
  geom_bin2d(varcl, mapping = aes(dist, gamma), binwidth = c(width, 3000)) +
  geom_point(varcl, mapping = aes(dist, gamma), alpha = 0.5, size = 2, color = 'steelblue') +
  geom_vline(xintercept = intervals, size = 0.25) +
  geom_vline(xintercept = 1e4 * 6:7, size = 1, color = 'white') +
  geom_label(aes(x = 30000, y = 2700, label = "лаг [60 000, 70 000)"), label.padding = unit(0.35, "lines"),
             color = 'forestgreen', fill = 'white', size = 4.5, alpha = 0.8) + 
  geom_line(vargr, mapping = aes(dist, gamma)) +
  geom_point(vargr, mapping = aes(dist, gamma, size = np)) +
  scale_fill_continuous(low = 'bisque', high = 'coral3') +
  scale_size(range = c(1, 3)) +
  coord_cartesian(ylim = c(0, 3000)) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()


## ---- fig.height=3, dpi=300------------------------------------------------------------------------
ggplot() +
  geom_line(vargr, mapping = aes(dist, gamma)) +
  geom_point(vargr, mapping = aes(dist, gamma, size = np)) +
  scale_size(range = c(1, 5)) +
  theme_bw()


## ---- fig.height=3.5, dpi=300----------------------------------------------------------------------
varcl = varcl %>% 
  mutate(sqgamma = sqrt(gamma),
         lag = cut(dist, breaks = intervals, labels = 0.001 * (intervals[-1] - 0.5*width)))

ggplot(varcl) +
  geom_boxplot(aes(lag, sqrt(gamma)), outlier.alpha = 0.1)


## ---- fig.height=5, dpi=300------------------------------------------------------------------------
varmp = variogram(rain_24~1, data=rainfall, cutoff = 150000, width = width, map = TRUE)[['map']]


## ---- echo = FALSE---------------------------------------------------------------------------------
dims = unname(varmp@grid@cells.dim)
offs = varmp@grid@cellcentre.offset

m = varmp@data %>% pull(var1) %>% matrix(dims)
dim(m) = c(dx = dims[1], dy = dims[2])

varstars = st_as_stars(gamma = m)

attr(varstars, "dimensions")[[1]]$offset = offs[1]
attr(varstars, "dimensions")[[2]]$offset = -offs[2]

attr(varstars, "dimensions")[[1]]$delta = width
attr(varstars, "dimensions")[[2]]$delta = -width

ggplot() +
  geom_stars(data = varstars) +
  scale_fill_continuous(type = "viridis") +
  coord_equal() +
  theme_bw() +
  theme(axis.text = element_text(size=18),
        legend.text =  element_text(size=14),
        legend.title =  element_text(size=18),
        axis.title = element_text(size=18,face="bold"))


## ---- echo = FALSE, fig.height=3, dpi=300----------------------------------------------------------
nugget = 15
sill = 215
lag = 1000
a = 120000
cutoff = 150000

h0 = lag * 0:(a/lag)
h1 = lag * (a/lag + 1):(cutoff/lag) 

tab1 = tibble::tibble(
  h = c(h0, h1),
  gamma = c(nugget + (sill - nugget) * (3 * h0 / (2 * a) - 0.5 * (h0 / a)^3), rep(sill, length(h1))),
  fit = 'manual'
)

vargr = variogram(rain_24~1, data=rainfall, cutoff = 150000, width = width)

ggplot() +
  geom_vline(xintercept = a, color = 'orangered') +
  annotate("text", x = a + 1.5 * width, y = 0.5 * sill, label = paste("a =", a), color = 'orangered') + 
  geom_hline(yintercept = sill, color = 'orangered') +
  annotate("text", x = 0.5 * a, y = sill * 1.05, label = paste("sill =", sill), color = 'orangered') + 
  geom_line(vargr, mapping = aes(dist, gamma)) +
  geom_point(vargr, mapping = aes(dist, gamma), size = 2) +
  scale_size(range = c(1, 5)) +
  geom_line(tab1, mapping = aes(h, gamma), size = 1, color = 'steelblue') +
  geom_point(data.frame(h = 0, gamma = nugget), mapping = aes(h, gamma), size = 3, color = 'orangered') +
  annotate("text", x = 1.5 * width, y = nugget, label = paste("nugget =", nugget), color = 'orangered') + 
  xlab('lag') + ylab('gamma') +
  ggtitle('Сферическая модель') +
  theme_bw()


## ---- fig.height=4, dpi=300------------------------------------------------------------------------
varmd = fit.variogram(vargr, model = vgm(psill = 215, model = 'Sph', range = 120000, nugget = 15))

h0 = lag * 0:(varmd[2, 'range']/lag)
h1 = lag * (varmd[2, 'range']/lag + 1):(cutoff/lag) 

tab2 = tibble::tibble(
  h = c(h0, h1),
  gamma = c(varmd[1, 'psill'] + (varmd[2, 'psill'] * (3 * h0 / (2 * varmd[2, 'range']) - 0.5 * (h0 / varmd[2, 'range'])^3)), rep(varmd[1, 'psill'] + varmd[2, 'psill'], length(h1))),
  fit = 'automatic'
)

tab = bind_rows(tab1, tab2)

ggplot() +
  geom_line(vargr, mapping = aes(dist, gamma)) +
  geom_point(vargr, mapping = aes(dist, gamma), size = 2) +
  scale_size(range = c(1, 5)) +
  geom_line(tab, mapping = aes(h, gamma, color = fit), size = 1) +
  xlab('lag') + ylab('gamma') +
  ggtitle('Сферическая модель') +
  theme_bw()


## ---- warning=FALSE, dpi = 300, fig.height = 4-----------------------------------------------------
box = st_bbox(rainfall)
envelope = box[c(1,3,2,4)]

px_grid = st_as_stars(box, dx = 2000, dy = 2000)

ggplot() + 
  geom_sf(data = rainfall, color = 'red') +
  geom_sf(data = st_as_sf(px_grid), size = 0.5, fill = NA)


## --------------------------------------------------------------------------------------------------
plot(vargr, model = varmd)
plot(varmp)


## --------------------------------------------------------------------------------------------------
(px_grid = krige(rain_24~1, rainfall, px_grid, model = varmd))


## ---- warning=FALSE, dpi = 300, fig.height=4-------------------------------------------------------
rain_colors = colorRampPalette(c("white", "dodgerblue", "dodgerblue4"))
rain_levels = seq(0,80,by=10)
rain_ncolors = length(rain_levels)-1

err_colors = colorRampPalette(c("white", "coral", "violetred"))
err_levels = seq(0, 180, by = 20)
err_ncolors = length(err_levels) - 1

cont = st_contour(px_grid['var1.pred'], breaks = rain_levels, contour_lines = TRUE)
conterr = st_contour(px_grid['var1.var'], breaks = err_levels, contour_lines = TRUE)

ggplot() +
  geom_stars(data = cut(px_grid['var1.pred'], breaks = rain_levels)) +
  scale_fill_manual(name = 'мм',
                    values = rain_colors(rain_ncolors),
                    labels = paste(rain_levels[-rain_ncolors-1], '-', rain_levels[-1]),
                    drop = FALSE) +
  coord_sf(crs = st_crs(rainfall)) +
  geom_sf(data = cont, color = 'black', size = 0.2) +
  geom_sf(data = rainfall, color = 'black', size = 0.3) +
  theme_bw()

ggplot() +
  geom_stars(data = cut(px_grid['var1.var'], breaks = err_levels)) +
  scale_fill_manual(name = 'мм',
                    values = err_colors(err_ncolors),
                    labels = paste(err_levels[-err_ncolors-1], '-', err_levels[-1]),
                    drop = FALSE) +
  coord_sf(crs = st_crs(rainfall)) +
  geom_sf(data = conterr, color = 'black', size = 0.2) +
  geom_sf(data = rainfall, color = 'black', size = 0.3) +
  theme_bw()


## ---- warning=FALSE, dpi = 300, fig.height=4-------------------------------------------------------
cvl = krige.cv(rain_24~1, rainfall, varmd) %>% 
  st_as_sf() %>% 
  mutate(sterr = residual / sqrt(var1.var))

head(cvl %>% st_set_geometry(NULL), 10)


## ---- message=FALSE, warning=FALSE, dpi = 300, fig.height=4----------------------------------------
ggplot(cvl, aes(x = sterr)) +
  geom_histogram(aes(y = stat(density)), fill = 'grey', color = 'black', size = 0.1) +
  geom_density(fill = 'olivedrab', alpha = 0.5) +
  theme_bw()


## ---- message=FALSE, warning=FALSE, dpi = 300, fig.height=4----------------------------------------
ggplot(cvl, aes(x = var1.pred, sterr)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm') +
  theme_bw()

cor.test(~ sterr + var1.pred, data = cvl)


## ---- message=FALSE, warning=FALSE, dpi = 300, fig.height=4----------------------------------------
ggplot(cvl, aes(x = var1.pred, observed)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm') +
  theme_bw()

# Диагностика модели линейной регрессии
summary(lm(observed ~ var1.pred, cvl))


## ---- warning=FALSE, dpi = 300, fig.height=3, fig.width=7------------------------------------------
library(akima)

coords = st_coordinates(rainfall)
coords_grid = st_coordinates(px_grid)

px_grid = px_grid %>% 
  mutate(sterr = interpp(x = coords[,1],
                         y = coords[,2],
                         z = cvl$sterr, 
                         xo = coords_grid[,1],
                         yo = coords_grid[,2],
                         linear = FALSE,
                         extrap = TRUE)$z)

sterr_levels = seq(-8,8,2)
sterr_ncolors = length(sterr_levels)-1
sterr_colors = colorRampPalette(c('blue', 'white', 'red'))

sterrcont = st_contour(px_grid['sterr'], breaks = sterr_levels, contour_lines = TRUE)

ggplot() +
  geom_stars(data = cut(px_grid['sterr'], breaks = sterr_levels)) +
  scale_fill_manual(name = 'мм',
                    values = sterr_colors(sterr_ncolors),
                    labels = paste(sterr_levels[-sterr_ncolors-1], '-', sterr_levels[-1]),
                    drop = FALSE) +
  coord_sf(crs = st_crs(rainfall)) +
  geom_sf(data = sterrcont, color = 'black', size = 0.2) +
  geom_sf(data = rainfall, color = 'black', size = 0.3) +
  theme_bw()


## ---- echo=FALSE-----------------------------------------------------------------------------------
knitr::include_url('https://tsamsonov.github.io/r-geo-course/slides/14-Geostatistics_slides.html#1', height = '500px')

