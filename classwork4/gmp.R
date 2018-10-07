#Загрузите данные в датафрейм. Адрес: github    https://raw???путь_к_файлу_найдите_сами???/data/gmp.dat 
gmp <- read.table("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2018/master/data/gmp.dat", skip = 1)
names(gmp) <- c("ID", "MSA", "gmp", "pcgmp")
gmp$pop <- gmp$gmp/gmp$pcgmp
# Функция, высчитывающая коэффициент alpha для модели Y=y0*N^alpha (источник статьи https://arxiv.org/pdf/1102.4101.pdf)
# Работает на основе критерия наименьших квадратов
# Входные параметры
# a - примерная оценка, коэффициент, который требуется более точно установить
# y0 - коэффициент y0 для заданной модели
# response - влияемый компонент
# predictor - влияющий компонент
# maximum.iterations - ограничение, чтобы избежать зацикливания функции
# deriv - вычисляемая производная
# deriv.step - шаг дифференцирования
# step.scale - шаг приближения
# stopping.deriv - позволяет выйти из цикла, когда deriv становится меньше этого параметра
# Выходные параметры:
# $a - полученный коэффициент
# $iterations - количество выполненных итераций
# $converged - был ли произведен выход из цикла или было достигнуто максимальное количество шагов в цикле
estimate.scaling.exponent <- function(a, y0=6611, response=gmp$pcgmp,
                                        predictor = gmp$pop, maximum.iterations=100, deriv.step = 1/100,
                                        step.scale = 1e-12, stopping.deriv = 1/100) {
  # mse - коэффициент наименьших квадратов
  # а - аргумент для вычисления коэффициента
  mse <- function(a) { mean((response - y0*predictor^a)^2) }
  for (iteration in 1:maximum.iterations) {
    deriv <- (mse(a+deriv.step) - mse(a))/deriv.step
    a <- a - step.scale*deriv
    if (stopping.deriv >= abs(deriv) ) { break(); }
  }
  stopifnot(iteration > 10)
  fit <- list(a=a,iterations=iteration,
              converged=(iteration < maximum.iterations))
  return(fit)
}
#Пример вызова с начальным занчением a
a <- estimate.scaling.exponent(0.15)

#С помошью полученного коэффициента постройте кривую (функция curve) зависимости
curve(6611*x^a$a, xlab = "Население, человек", ylab = "Доход на душу населения, $ на душу населения в год", from = min(gmp$pop), to=max(gmp$pop))

#Удалите точку из набора исходных данных случайным образом, как изменилось статистическая оценка коэффициента a?
rnd <- runif(1 ,min = 1, max = max(gmp$ID))
gmp.onedel <- gmp
gmp.onedel <- gmp.onedel[-rnd,]
b <- estimate.scaling.exponent(0.15, response = gmp.onedel$pcgmp, predictor = gmp.onedel$pop)
b$a - a$a
# Коэффициент поменялся не сильно, уменьшился на 0.00005 при случайном значении rnd = 40

#Запустите оценку несколько раз с разных стартовых точек. Как изменилось значение a?
estimate.scaling.exponent(1) # -4701782057 за 1 итерацию 
estimate.scaling.exponent(0.5) # -990.2312 за 2 итерации
estimate.scaling.exponent(0.3) # -2.850634 за 2 итерации
estimate.scaling.exponent(0.25, maximum.iterations = 10000) # 0.1211533 за 5607 итераций
estimate.scaling.exponent(0.2) # 0.1211533 за 70 итераций
estimate.scaling.exponent(0.1211533) # 0.1211533 за 28 итерации
estimate.scaling.exponent(0.12) # 0.1211533 за 54 итерации
estimate.scaling.exponent(0.10) # 0.1211533 за 61 итерацию
estimate.scaling.exponent(0.05) # 0.1211533 за 69 итераций
estimate.scaling.exponent(0) # 0.1211533 за 78 итераций
estimate.scaling.exponent(-0.1, maximum.iterations = 1000) # 0.1211533 за 117 итераций 
estimate.scaling.exponent(-0.5, maximum.iterations = 10000) # 0.1211533 за 7459 итераций
estimate.scaling.exponent(-1, maximum.iterations = 1000000) # -0.9705958 за 1000000 итераций 
estimate.scaling.exponent(-10) # -10 за 1 итерацию
# Таким образом, чем ближе передан начальный коэффициент а к итоговому, тем меньшее количество итераций требуется на его расчет.
# При передаче слишком большого значения функция выдает некорректный результат и производит 1-2 итерации
# для избежания ошибок в функцию была добавлена строка stopifnot(iteration > 10), которая выдаст ошибку при некорректной работе
