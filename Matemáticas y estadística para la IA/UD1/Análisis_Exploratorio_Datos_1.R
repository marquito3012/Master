#Análisis Exploratorio en R

install.packages("AER")
library(AER)

data("CPS1985")
print(CPS1985)

str(CPS1985)

head(CPS1985)

tail(CPS1985)

summary(CPS1985)

attach(CPS1985)
summary(wage)
summary(age)

mean(wage)
mean(age)
median(wage)
min(age)
max(wage)

var(wage)
sd(wage)

hist(wage, freq = FALSE)

hist(log(wage), freq = FALSE)
lines(density(log(wage)), col = 1)
lines(density(log(wage)), col = 20)
lines(density(log(wage)), col = 50)

#En variables categóricas
summary(occupation)
table(occupation)

tab <- table(occupation)
barplot(tab)
pie(tab)

#Relación entre 2 variables categorica: género y ocupación
xtabs(~ gender + occupation, data = CPS1985)
plot(gender ~ occupation, data = CPS1985)

xtabs(~ gender + married, data = CPS1985)
plot(gender ~ married, data = CPS1985)

#Relación entre 2 variables númericas: Salario y Educación
cor(wage, education)

#Correlación de Pearson
cor(log(wage), education)

#Correlación de Spearman
cor(log(wage), education, method = "spearman")

plot(log(wage) ~ education)

#En variables donde una es categorica y la otra númerica: Género y Salario
tapply(log(wage), gender, mean)
tapply(log(wage), gender, summary)

plot((log(wage) ~ gender))
