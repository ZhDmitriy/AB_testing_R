#Загрузим данные по проведенному AB-тестированию 
data_ab_testing <- read.csv(file.choose())
#Посмотрим на наши данные
View(data_ab_testing)

#Уберем пропущенные значения
data_not_null <- na.omit(data_ab_testing)
#Снова посмотрим на данные 
View(data_not_null)

#Запишем значения конверсии на текущем варианте сайта 
A_test_old <- data_not_null$OLD
A_test_old[1:5]

#Запишем значения конверсии на новом варианте дизайна сайта
B_test_new <- data_not_null$NEW
B_test_new[1:5]

#Посчитаем значения конверсий на двух сайтах 
p_old <- sum(A_test_old) / length(A_test_old)
p_new <- sum(B_test_new) / length(B_test_new)
p_new - p_old #Посмотрим на разницу в конверсиях

N = 1500 

differences <- rep(NA, N) #куда запишем значения разниц

for (i in 1:N){
  s1 <- sample(A_test, replace = TRUE)
  s2 <- sample(B_test, replace = TRUE)
  p1 <- sum(s1)/length(s1)
  p2 <- sum(s2)/length(s2)
  p_diff <- p2 - p1
  differences[i] <- p_diff
}

differences[1:10]

hist(differences, 
     main="Распределение вероятностей", 
     xlab = "Вероятность",
     ylab = "Частота появления",
     col='ivory')

sum(differences >= 0.6325) / N

sum(differences >= 0.6325) / N < 0.05
