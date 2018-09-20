1234+4567

29-45

325/25

56*12

11*11

111*111

1111111*1111111

options(digits=14)

options(width=40)
5:32

1:10

(1:10)+3

(1:10)-3

(1:10)*3

(1:10)^2

(1:10)^3

3:4

31 %% 7

31 %/% 7

7*4 + 3

x <-c(4, 1, 8, 9)
y <-c(6, 2, 4, 3)

plot(x,y);
lines(x, y)

x <- 1:10; y <- x^2;
plot(x,y)

plot(x,y);
lines(x,y)

learn <-c("stats" = 15, "math"= 10,"programming" = 30, "attempts" = 45)
pie(learn)

barplot(learn)

Z <- rnorm(1000)# 1000 standard normal random variates
hist(Z, prob = TRUE, main = "Гистограмма относительной частоты",
     sub = "Плотность распределения")
curve(dnorm(x), from = -3, to = 3, add = TRUE, col = "blue")

#---------
11111111*11111111
11111111*1111111?.
#Error in `?`(11111111 * 1111111, .) : 
#нет документации на тип ‘12345677654321’ и раздел ‘.’ (или ошибка в обработке помощи)
#Ошибка в постановке вопросительного знака и точки

a <- c(3,7,12,15,20)
b <- c(2,5,8,11,15)
S <- a*b

plot(a,S);
lines(a,S)

plot(b,S);
lines(b,S)

plot(a,b)

vasya <- c("Математика" = 40, "Английский"=40, "Физическая культура"=10, "Программирование"=150)
pie(vasya)

drinks <- rnorm(5, mean = 450, sd = 4)
drinks

drinks > 455

drinks <- rnorm(10000, mean = 450, sd = 4)
spent <- sum(drinks>455)
spent
#1014/10000 ~ 10%