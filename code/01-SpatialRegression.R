## ---- fig.cap = 'Процент домохозяйств, находящихся во владении', echo=FALSE------------------------
knitr::include_graphics('images/tyne_ownerocc.png')


## ---- fig.cap = 'Уровень безработицы', echo=FALSE--------------------------------------------------
knitr::include_graphics('images/tyne_unempl.png')


## ---- fig.cap = 'Зависимость процента домохозяйств во владении от уровня безработицы', echo=FALSE----
knitr::include_graphics('images/tyne_regr.png')


## ---- fig.cap = 'Остатки линейной регрессии', echo=FALSE-------------------------------------------
knitr::include_graphics('images/tyne_resid.png')


## --------------------------------------------------------------------------------------------------
library(sf)
library(spdep)  # оценка соседства, построение матрицы весов, индексы автокорреляции
library(spatialreg) # пространственная регрессия
library(lattice)
library(RANN)
library(RColorBrewer)
library(readxl)
library(tidyverse)
library(tmap)
library(GWmodel)

options(scipen = 999)

reg_sf = st_read('data/Kirov.gpkg')
reg = st_geometry(reg_sf)

par(mar = c(1, 1, 1, 1))
plot(reg, border = "gray50")


## --------------------------------------------------------------------------------------------------
nb_queen = poly2nb(reg) # Соседство по правилу ферзя
nb_queen  # посмотрим сводную информацию
class(nb_queen)  # проверим тип объекта


## ---- message = FALSE, results = "hide", collapse=TRUE---------------------------------------------
coords = reg %>% 
  st_centroid() %>% 
  st_coordinates()

# Теперь рисуем граф:
plot(reg, border = "gray50")
plot(nb_queen, coords, pch = 19, cex = 0.5, add = TRUE)
title(main = "Соседи по смежности (правило ферзя)")


## --------------------------------------------------------------------------------------------------
nb_rook = poly2nb(reg, queen = FALSE) # Соседство по правилу ладьи

plot(reg, border = "grey70")
plot(nb_rook, coords, pch = 19, cex = 0.5, add = TRUE)
title(main = "Соседи по смежности (правило ладьи)")


## --------------------------------------------------------------------------------------------------
nb_tin = tri2nb(coords)

plot(reg, border = "grey70")
plot(nb_tin, coords, pch = 19, cex = 0.5, add = TRUE)
title(main = "Соседи по триангуляции Делоне")


## --------------------------------------------------------------------------------------------------
nb_tin = soi.graph(nb_tin, coords) %>% graph2nb()

plot(reg, border = "grey70")
plot(nb_tin, coords, pch = 19, cex = 0.5, add = TRUE)
title(main = "Соседи по сфере влияния")


## --------------------------------------------------------------------------------------------------
nb_gab = gabrielneigh(coords) %>% graph2nb()
 
plot(reg, border = "grey70")
plot(nb_gab, coords, pch = 19, cex = 0.5, add = TRUE)
title(main = "Соседи по графу Гэбриела")


## --------------------------------------------------------------------------------------------------
nb_rel = relativeneigh(coords) %>% graph2nb()

plot(reg, border = "grey70")
plot(nb_rel, coords, pch = 19, cex = 0.5, add = TRUE)
title(main = "Относительные соседи по графу")


## --------------------------------------------------------------------------------------------------
par(mfrow = c(2,2),
    mar = c(1,1,1,1))
for (i in 1:4){
  nb_knn = knearneigh(coords, k = i) %>% knn2nb()
  
  plot(reg, border = "grey70")
  plot(nb_knn, coords, pch = 19, cex = 0.5, add = TRUE)
  title(main = paste("Ближайшие соседи (k = ", i, ")", sep = ''))
}


## --------------------------------------------------------------------------------------------------
par(mfrow = c(2,2),
    mar = c(1,1,1,1))
for (d in 5:8) {
  dnearnei = dnearneigh(coords, d1 = 0, d2 = 10000 * d)
  
  plot(reg, border = "grey70")
  plot(dnearnei, coords, pch = 19, cex = 0.5, add = TRUE)
  title(main = paste("Ближайшие соседи (d <=", 10000 * d, ")", sep = ''))
}


## --------------------------------------------------------------------------------------------------
Wbin = nb2listw(nb_queen, style = "B")
Wbin  # посмотрим, что за объект получается на выходе (listw)


## --------------------------------------------------------------------------------------------------
Wbin$neighbours
Wbin$weights


## --------------------------------------------------------------------------------------------------
M = listw2mat(Wbin)
levelplot(M, main = "Матрица весов (бинарная)")


## --------------------------------------------------------------------------------------------------
Wstand = nb2listw(nb_queen, style = "W")
M = listw2mat(Wstand)

ramp = colorRampPalette(c("white","red"))
levels = 1 / 1:10  # шкала 1, 0.5, 0.33, 0.25 ... 0.1
levelplot(M, 
          main="Матрица весов (нормированная)", 
          at = levels, 
          col.regions=ramp(10))


## --------------------------------------------------------------------------------------------------
# Ближайшие соседи (k = 1)
nb_knn = knearneigh(coords, k = 1) %>% knn2nb()

Wstand = nb2listw(nb_knn, style = "B")
M = listw2mat(Wstand)
levelplot(M, 
          main = "Матрица весов (нормированная)", 
          at = levels, 
          col.regions = ramp(10))


## ---- fig.width = 8, fig.height=8------------------------------------------------------------------
mun_src = reg_sf

# Чтение таблицы со статистикой
tab = read_xlsx("data/Kirov.xlsx", 1)

# Соединение таблиц
mun = mun_src %>% 
  left_join(tab, by = c("OBJECTID" = "N")) %>% 
  pivot_longer(cols = 22:31,
               names_to = 'month',
               values_to = 'nsick') %>% 
  mutate(month = ordered(month, levels = c('Январь', 'Февраль', 'Март', 
                                           'Апрель', 'Май', 'Июнь', 
                                           'Июль', 'Август', 'Сентябрь', 
                                           'Октябрь', 'Ноябрь', 'Декабрь'))) %>% 
  st_set_geometry('geometry')

# Построение серии карт
ramp = colorRampPalette(c("white", "orange", "red"))
levels = seq(0, 10000, 1000)
nclasses = length(levels) - 1

ggplot() +
  geom_sf(mun, mapping = aes(geometry = geometry, 
                             fill = cut(nsick, levels))) +
  scale_fill_manual(values = ramp(nclasses),
                    labels = paste(levels[-nclasses-1], '-', levels[-1]),
                    guide = guide_legend(reverse = TRUE),
                    drop = FALSE) +
  facet_wrap(~month)


## --------------------------------------------------------------------------------------------------
# Определение соседства (правило ферзя)
nb_queen = poly2nb(mun_src)

# Визиуализация графа соседства
coords = st_centroid(mun_src) %>% st_coordinates()
plot(mun_src %>% st_geometry(), border = "darkgray")
plot(nb_queen, coords, pch = 19, cex = 0.5, add = TRUE)
title(main = "Соседи по смежности (правило ферзя)")

# Вычисление весов (нормированная матрица)
W = nb2listw(nb_queen)

# Визуализация матрицы весов
M = listw2mat(W)

ramp2 = colorRampPalette(c("white","red"))
levels2 = 1 / 1:10 # шкала 1, 0.5, 0.33, 0.25 ... 0.1
levelplot(M, 
          main = "Матрица весов (нормированная)", 
          at = levels2, 
          col.regions = ramp2(10))


## --------------------------------------------------------------------------------------------------
# Выбираем данные за февраль
feb = mun %>% 
  dplyr::filter(month == 'Февраль')

# Вычисление индекса (тест) Морана
moran.test(feb$nsick, W)


## --------------------------------------------------------------------------------------------------
(sim = moran.mc(feb$nsick, listw = W, nsim = 10000))

# Построим гистограмму по вычисленным индексам:
hist(sim$res,
     freq = TRUE,
     breaks = 20, 
     xlim = c(-1,1),
     main = "Перестановочный тест Морана", 
     xlab = "Случайный индекс Морана",
     ylab = "Частота появления",
     col = "steelblue")

# Нанесем фактическое значение
abline(v = sim$statistic, col = "red")


## --------------------------------------------------------------------------------------------------
moran.plot(feb$nsick, W)


## --------------------------------------------------------------------------------------------------
model = lagsarlm(nsick ~ 1, data = feb, listw = W)
model


## ---- fig.height=3.5-------------------------------------------------------------------------------
# Извлекаем результаты пространственной авторегрессии
feb_spreg = feb |>  
  mutate(fitted = model$fitted.values,
         residual = model$residuals) |>  
  pivot_longer(all_of(c("nsick", "fitted", "residual")), # TODO: cannot find sick as usual!
               names_to = 'type',
               values_to = 'value') |>  
  st_set_geometry('geometry')

# Построение серии карт
ramp = colorRampPalette(c('steelblue3', 'white', 'orange', 'violetred'))
levels = seq(-3000, 10000, 1000)
nclasses = length(levels) - 1

# Сравниваем исходные данные, модельные и остатки
ggplot() +
  geom_sf(feb_spreg, mapping = aes(geometry = geometry, fill = cut(value, levels))) +
  scale_fill_manual(values = ramp(nclasses),
                    labels = paste(levels[-nclasses-1], '-', levels[-1]),
                    guide = guide_legend(reverse = TRUE),
                    drop = FALSE) +
  facet_wrap(~type)


## ---- echo=FALSE-----------------------------------------------------------------------------------
knitr::include_url('https://tsamsonov.github.io/r-geo-course/slides/15-SpatialRegression_slides.html#1', height = '500px')

