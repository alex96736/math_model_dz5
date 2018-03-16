# Esly russkie bukvy ne otobrajautsa: File -> Reopen with encoding... UTF-8

# Используйте UTF-8 как кодировку по умолчанию!
# Установить кодировку в RStudio: Tools -> Global Options -> General, 
#  Default text encoding: UTF-8

# ..............................................................................
# Математическое моделирование: Практика 5
#   Кросс-валидация и бутстреп
#      * как оценить точность модели методом перекрёстной выборки;    
#      * методом проверочной выборки;    
#      * методом перекрёстной проверки по отдельным наблюдениям (LOOCV);   
#      * методом k-кратной перекрёстной проверки;   
#      * как применять бутстреп для оценки точности статистического параметра 
#        и оценок параметров модели   
# ..............................................................................

library('ISLR')              # набор данных Auto
library('GGally')            # матричные графики
library('boot')              # расчёт ошибки с кросс-валидацией

my.seed <- 1

# Перекрёстная проверка --------------------------------------------------------

# Пример на данных по автомобилям: Auto {ISLR}
?Auto
head(Auto)
str(Auto)

# графики разброса
ggpairs(Auto[, -9])
# только mpg ~ horsepower
plot(Auto$horsepower, Auto$mpg,
     xlab = 'horsepower', ylab = 'mpg', pch = 21,
     col = rgb(0, 0, 1, alpha = 0.4), bg = rgb(0, 0, 1, alpha = 0.4))

# Метод проверочной выборки ====================================================

n <- nrow(Auto)

# доля обучающей выборки
train.percent <- 0.5

# выбрать наблюдения в обучающую выборку
set.seed(my.seed)
inTrain <- sample(n, n * train.percent)

plot(Auto$horsepower[inTrain], Auto$mpg[inTrain],
     xlab = 'horsepower', ylab = 'mpg', pch = 21,
     col = rgb(0, 0, 1, alpha = 0.4), bg = rgb(0, 0, 1, alpha = 0.4))
points(Auto$horsepower[-inTrain], Auto$mpg[-inTrain],
       pch = 21, col = rgb(1, 0, 0, alpha = 0.4), bg = rgb(1, 0, 0, alpha = 0.4))
legend('topright', 
       pch = c(16, 16), col = c('blue', 'red'), legend = c('test', 'train'))


# Линейная модель ##############################################################

# присоединить таблицу с данными: названия стоблцов будут доступны напрямую
attach(Auto)
# подгонка линейной модели на обучающей выборке
fit.lm.1 <- lm(mpg ~ horsepower, 
               subset = inTrain)
# считаем MSE на тестовой выборке
mean((mpg[-inTrain] - predict(fit.lm.1,
                              Auto[-inTrain, ]))^2)

# отсоединить таблицу с данными
detach(Auto)


# Квадратичная модель ##########################################################

# присоединить таблицу с данными: названия стоблцов будут доступны напрямую
attach(Auto)
# подгонка линейной модели на обучающей выборке
fit.lm.2 <- lm(mpg ~ poly(horsepower, 2), 
               subset = inTrain)
# считаем MSE на тестовой выборке
mean((mpg[-inTrain] - predict(fit.lm.2,
                              Auto[-inTrain, ]))^2)

# отсоединить таблицу с данными
detach(Auto)


# Кубическая модель ############################################################

attach(Auto)

# подгонка линейной модели на обучающей выборке
fit.lm.3 <- lm(mpg ~ poly(horsepower, 3), 
               subset = inTrain)
# считаем MSE на тестовой выборке
mean((mpg[-inTrain] - predict(fit.lm.3,
                              Auto[-inTrain, ]))^2)

# отсоединить таблицу с данными
detach(Auto)


# Перекрёстная проверка по отдельным наблюдениям (LOOCV) =======================

# подгонка линейной модели на обучающей выборке
fit.glm <- glm(mpg ~ horsepower, data = Auto)
# считаем LOOCV-ошибку
cv.err <- cv.glm(Auto, fit.glm)
# результат: первое число -- по формуле LOOCV-ошибки,
#  второе -- с поправкой на смещение
cv.err$delta[1]

# оценим точность полиномиальных моделей, меняя степень
# вектор с LOOCV-ошибками
cv.err.loocv <- rep(0, 5)
names(cv.err.loocv) <- 1:5
# цикл по степеням полиномов
for (i in 1:5){
  fit.glm <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.err.loocv[i] <- cv.glm(Auto, fit.glm)$delta[1]
}
# результат
cv.err.loocv


# k-кратная перекрёстная проверка ==============================================

# оценим точность полиномиальных моделей, меняя степень
# вектор с ошибками по 10-кратной кросс-валидации
cv.err.k.fold <- rep(0, 5)
names(cv.err.k.fold) <- 1:5
# цикл по степеням полиномов
for (i in 1:5){
  fit.glm <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.err.k.fold[i] <- cv.glm(Auto, fit.glm,
                             K = 10)$delta[1]
}
# результат
cv.err.k.fold

# Для сравнения напомним результаты расчёта MSE методом проверочной выборки:
err.test

# Бутстреп ---------------------------------------------------------------------

# Оценивание точности статистического параметра --------------------------------

# Пример с инвестиционным портфелем из двух активов: Portfolio {ISLR}

?Portfolio
head(Portfolio)
str(Portfolio)

# функция для вычисления искомого параметра
alpha.fn <- function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2*cov(X, Y))
}

# рассчитать alpha по всем 100 наблюдениям
alpha.fn(Portfolio, 1:100)

# создать бутстреп-выборку и повторно вычислить alpha
set.seed(my.seed)
alpha.fn(Portfolio, sample(100, 100, replace = T))


# теперь -- многократное повторение предыдущей операции
boot(Portfolio, alpha.fn, R = 1000)



# Оценивание точности линейной регрессионной модели ----------------------------

# оценить стандартные ошибки параметров модели 
#  mpg = beta_0 + beta_1 * horsepower с помощью бутстрепа,
#  сравнить с оценками ошибок по МНК

# функция для расчёта коэффициентов ПЛР по выборке из данных
boot.fn <- function(data, index){
  coef(lm(mpg ~ horsepower, data = data, subset = index))
}
boot.fn(Auto, 1:n)
    
# пример применения функции к бутстреп-выборке
set.seed(my.seed)
boot.fn(Auto, sample(n, n, replace = T))


# применяем функцию boot для вычисления стандартных ошибок параметров
#  (1000 выборок с повторами)
boot(Auto, boot.fn, 1000)


# сравним с МНК
attach(Auto)
summary(lm(mpg ~ horsepower))$coef
detach(Auto)

# оценки отличаются из-за того, что МНК -- параметрический метод с допущениями

# вычислим оценки параметров квадратичной модели регрессии
boot.fn.2 <- function(data, index){
  coef(lm(mpg ~ poly(horsepower, 2), data = data, subset = index))
}
# применим функцию к 1000 бутсреп-выборкам
set.seed(my.seed)
boot(Auto, boot.fn, 1000)