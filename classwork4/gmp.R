#Загрузите данные в датафрейм. Адрес: github    https://raw???путь_к_файлу_найдите_сами???/data/gmp.dat 
gmp <- read.table("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2018/master/data/gmp.dat", skip = 1)
names(gmp) <- c("ID", "MSA", "gmp", "pcgmp")
gmp$pop <- gmp$gmp/gmp$pcgmp

estimate.scaling.exponent <- function(a, y0=6611, response=gmp$pcgmp,
                                        predictor = gmp$pop, maximum.iterations=100, deriv.step = 1/100,
                                        step.scale = 1e-12, stopping.deriv = 1/100) {
  mse <- function(a) { mean((response - y0*predictor^a)^2) }
  for (iteration in 1:maximum.iterations) {
    deriv <- (mse(a+deriv.step) - mse(a))/deriv.step
    a <- a - step.scale*deriv
    if (stopping.deriv >= abs(deriv) ) { break(); }
  }
  fit <- list(a=a,iterations=iteration,
              converged=(iteration < maximum.iterations))
  return(fit)
}
#Пример вызова с начальным занчением a
estimate.scaling.exponent(0.15)

#С помошью полученного коэффициента постройте кривую (функция curve) зависимости

#Удалите точку из набора исходных данных случайным образом, как изменилось статистическая оценка коэффициента a?

#Запустите оценку несколько раз с разных стартовых точек. Как изменилось значение a?

