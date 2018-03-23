library('ISLR')              # набор данных Auto
library('GGally')            # матричные графики
library('boot')              # расчёт ошибки с кросс-валидацией

my.seed <- 100

# Пример на данных по автомобилям: Auto {ISLR}
?Auto
data(Auto)
head(Auto)
str(Auto)

Auto <- Auto[, c(1:5)]

str(Auto)

# графики разброса
ggpairs(Auto)

# общее число наблюдений
n <- nrow(Auto)

# доля обучающей выборки
train.percent <- 0.5

# выбрать наблюдения в обучающую выборку
set.seed(my.seed)
inTrain <- sample(n, n * train.percent)

# присоединить таблицу с данными: названия стоблцов будут доступны напрямую
attach(Auto)
# подгонка линейной модели на обучающей выборке
fit.lm.1 <- lm(mpg ~ weight + displacement + horsepower + cylinders, 
               subset = inTrain)
# считаем MSE на тестовой выборке
mean((mpg[-inTrain] - predict(fit.lm.1,
                              Auto[-inTrain, ]))^2)
# отсоединить таблицу с данными
detach(Auto)

# подгонка линейной модели на обучающей выборке
fit.glm <- glm(mpg ~ weight + displacement + horsepower + cylinders, data = Auto)
# считаем LOOCV-ошибку
cv.err <- cv.glm(Auto, fit.glm)
# результат: первое число -- по формуле LOOCV-ошибки,
#  второе -- с поправкой на смещение
cv.err$delta[1]
# вектор с LOOCV-ошибками
cv.err.loocv <- rep(0, 5)
names(cv.err.loocv) <- 1:5
# цикл по степеням полиномов
for (i in 1:5){
  fit.glm <- glm(mpg ~ poly(weight, i) + poly(displacement, i) + poly(horsepower, i) + cylinders, data = Auto)
  cv.err.loocv[i] <- cv.glm(Auto, fit.glm)$delta[1]
}
# результат
cv.err.loocv
# оценим точность полиномиальных моделей, меняя степень
# вектор с ошибками по 10-кратной кросс-валидации
cv.err.k.fold1 <- rep(0, 5)
names(cv.err.k.fold1) <- 1:5
# цикл по степеням полиномов
for (i in 1:5){
  fit.glm <- glm(mpg ~ poly(weight, i) + poly(displacement, i) + poly(horsepower, i) + cylinders, data = Auto)
  cv.err.k.fold1[i] <- cv.glm(Auto, fit.glm,
                             K = 5)$delta[1]
}
# результат
cv.err.k.fold1

cv.err.k.fold2 <- rep(0, 5)
names(cv.err.k.fold2) <- 1:5
# цикл по степеням полиномов
for (i in 1:5){
  fit.glm <- glm(mpg ~ poly(weight, i) + poly(displacement, i) + poly(horsepower, i) + cylinders, data = Auto)
  cv.err.k.fold2[i] <- cv.glm(Auto, fit.glm,
                              K = 10)$delta[1]
}
# результат
cv.err.k.fold2


str(Auto)

# Оценивание точности линейной регрессионной модели ----------------------------

# оценить стандартные ошибки параметров модели 
#  mpg = beta_0 + beta_1 * horsepower с помощью бутстрепа,
#  сравнить с оценками ошибок по МНК
# функция для расчёта коэффициентов ПЛР по выборке из данных
boot.fn <- function(data, index){
  coef(lm(mpg ~ weight + displacement + horsepower + cylinders, data = data, subset = index))
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
summary(lm(mpg ~ weight + displacement + horsepower + cylinders))$coef

detach(Auto)


# присоединить таблицу с данными: названия стоблцов будут доступны напрямую
attach(Auto)
# подгонка линейной модели на обучающей выборке
fit.lm.1 <- lm(mpg ~ weight + displacement + horsepower, 
               subset = inTrain)
# считаем MSE на тестовой выборке
mean((mpg[-inTrain] - predict(fit.lm.1,
                                Auto[-inTrain, ]))^2)
# отсоединить таблицу с данными
detach(Auto)

# подгонка линейной модели на обучающей выборке
fit.glm <- glm(mpg ~ weight + displacement + horsepower, data = Auto)
# считаем LOOCV-ошибку
cv.err <- cv.glm(Auto, fit.glm)
# результат: первое число -- по формуле LOOCV-ошибки,
#  второе -- с поправкой на смещение
cv.err$delta[1]
# вектор с LOOCV-ошибками
cv.err.loocv <- rep(0, 5)
names(cv.err.loocv) <- 1:5
# цикл по степеням полиномов
for (i in 1:5){
  fit.glm <- glm(mpg ~ poly(weight, i) + poly(displacement, i) + poly(horsepower, i), data = Auto)
  cv.err.loocv[i] <- cv.glm(Auto, fit.glm)$delta[1]
}
# результат
cv.err.loocv
# оценим точность полиномиальных моделей, меняя степень
# вектор с ошибками по 10-кратной кросс-валидации
cv.err.k.fold1 <- rep(0, 5)
names(cv.err.k.fold1) <- 1:5
# цикл по степеням полиномов
for (i in 1:5){
  fit.glm <- glm(mpg ~ poly(weight, i) + poly(displacement, i) + poly(horsepower, i), data = Auto)
  cv.err.k.fold1[i] <- cv.glm(Auto, fit.glm,
                              K = 5)$delta[1]
}
# результат
cv.err.k.fold1

cv.err.k.fold2 <- rep(0, 5)
names(cv.err.k.fold2) <- 1:5
# цикл по степеням полиномов
for (i in 1:5){
  fit.glm <- glm(mpg ~ poly(weight, i) + poly(displacement, i) + poly(horsepower, i), data = Auto)
  cv.err.k.fold2[i] <- cv.glm(Auto, fit.glm,
                              K = 10)$delta[1]
}
# результат
cv.err.k.fold2


str(Auto)


# Оценивание точности линейной регрессионной модели ----------------------------

# оценить стандартные ошибки параметров модели 
#  mpg = beta_0 + beta_1 * horsepower с помощью бутстрепа,
#  сравнить с оценками ошибок по МНК

# функция для расчёта коэффициентов ПЛР по выборке из данных
boot.fn <- function(data, index){
  coef(lm(mpg ~ weight + displacement + horsepower, data = data, subset = index))
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
summary(lm(mpg ~ weight + displacement + horsepower))$coef

detach(Auto)

