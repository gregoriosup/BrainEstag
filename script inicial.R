###pacotes###
library("readxl")
install.packages("pls")
library(pls)
install.packages("car")
library(car)
install.packages("leaps")
library(leaps)

###dados####

dados <- read_excel("Dados Fenofibrato.xlsx", sheet = "Sheet7")
str(dados)

####modelo####

mod <- lm(F2 ~ lactose+pvp+desin_int,data=dados)
summary(mod)

vif(mod)
#pressupostos

shapiro.test(residuals(mod))
plot(mod)

# Criar um data frame com combina??es de vari?veis
variaveis <- expand.grid(
  desin_int = seq(5, 21, by = 1),
  pvp = seq(1, 4, by = 0.5),
  lactose = seq(20,40,by=5)
)

# Prever F2 para todas as combina??es
variaveis$F2_previsto <- predict(mod, newdata = variaveis)

# Encontrar a combina??o que maximiza F2
linha_maxima <- variaveis[which.max(variaveis$F2_previsto), ]

# Exibir os resultados
print("Valores ?timos que maximizam F2:")
print(linha_maxima)


