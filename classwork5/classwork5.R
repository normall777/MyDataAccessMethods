#Модифицируйте код из предыдущей лекции (функцию estimate.scaling.exponent), чтобы он возвращал список a и y0

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
  fit <- list(a=a, y0 = y0)
  return(fit)
}

estimate.scaling.exponent(0.15)

#Напишите рекурсивные функции факториала и фибоначчи
factor <- function(n){
  stopifnot(n>=0)
  if (n <= 1) {
    return(1)
  }
  return(factor(n-1)*n)
}

factor(5)

fib <- function(n){
  stopifnot(n>0)
  if (n<=2){
    return(1)
  }
  return(fib(n-1)+fib(n-2))
}

fib(10)
