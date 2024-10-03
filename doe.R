####pacotes####
install.packages("FrF2")
install.packages("DoE.base")
install.packages("rsm")

# Carregar pacotes
library(FrF2)
library(DoE.base)
library(rsm)
library(lmtest)
#####fatorial completo 2²####

# Criar um delineamento fatorial completo 2^2####
design <- FrF2(nruns = 4, nfactors = 2, factor.names = c("lactose", "crospovidona"))
str(design)

#adicionar ponto central####

ponto_central <- data.frame(lactose=as.factor(0), crospovidona=as.factor(0))
replicata <- data.frame(lactose=as.factor(1), crospovidona=as.factor(1))
design_pc <- rbind(design,ponto_central, replicata)
str(design_pc)

design_pc$ponto_central <- ifelse(design_pc$lactose == 0 & design_pc$crospovidona == 0, 1, 0)



#adicionar resposta####

resposta <- c(64.21, 40.09,53.53,42.06, 49.18, 63.08)
design_pc$resposta <- resposta

#criacao modelo####

mod <- lm(resposta ~ lactose * crospovidona + ponto_central, data = design_pc)
summary(mod)

anova <- aov(mod)
summary(anova)

#pressupostos####

shapiro.test(mod$residuals) #normalidade

par(mfrow=c(2,2))
plot(mod)
par(mfrow=c(1,1))

bptest(mod) #homocedasticidade

#interação dos fatores####
interaction.plot(design_pc$lactose, design_pc$crospovidona, design_pc$resposta)



