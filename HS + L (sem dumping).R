###pacotes###


###dados####

dados <- read.table("clipboard", header = TRUE, sep = "\t")

dados$F2 <- as.numeric(gsub(",", ".", dados$F2))
dados$desin_ext <- as.numeric(gsub(",", ".", dados$desin_ext))
dados$lactose <- as.numeric(gsub(",", ".", dados$lactose))
dados$pvp <- as.numeric(gsub(",", ".", dados$pvp))
dados$desin_int <- as.numeric(gsub(",", ".", dados$desin_int))


####modelo####

mod <- lm(F2 ~ desin_int+pvp+granulacao,data=dados)

summary(mod)

shapiro.test(residuals(mod))
plot(mod)

# Criar um data frame com combinações de variáveis
variaveis <- expand.grid(
  desin_int = seq(5, 30, by = 1),
  pvp = seq(1, 10, by = 0.5),
  #lactose = seq(20,40,by=1),
  granulacao = c("H", "L")
)

# Prever F2 para todas as combinações
variaveis$F2_previsto <- predict(mod, newdata = variaveis)

# Encontrar a combinação que maximiza F2
linha_maxima <- variaveis[which.max(variaveis$F2_previsto), ]

# Exibir os resultados
print("Valores ótimos que maximizam F2:")
print(linha_maxima)