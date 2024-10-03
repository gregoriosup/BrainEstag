####pacotes#####
install.packages("FrF2")
install.packages("DoE.base")
install.packages("rsm")
install.packages("ggpubr")

# Carregar pacotes#####
library(FrF2)
library(DoE.base)
library(rsm)
library(lmtest)
library(ggplot2)
library(ggpubr)

#####fatorial completo 2²####

design_pc <- read_excel("Dados Fenofibrato.xlsx", sheet = "doe")

mod_decod <- lm(resposta ~ lactose * crospovidona, data = design_pc) #decodificado
mod_cod <- lm(resposta ~ f1 * f2, data = design_pc) #codificado


summary(mod_cod)
anova <- aov(mod_cod)
summary(anova)

#pressupostos####

shapiro.test(mod_cod$residuals) #normalidade

par(mfrow=c(2,2))
plot(mod_cod)
par(mfrow=c(1,1))

bptest(mod_cod) #homocedasticidade

#interação dos fatores####

p1<- ggline(data = design_pc,
       x="lactose", y="resposta",
       group = 1,
       add=c("mean", "jitter"),
       color="blue")+theme_bw()

p2<- ggline(data = design_pc,
       x="crospovidona", y="resposta",
       group = 1,
       add=c("mean", "jitter"),
       color="blue")+theme_bw()

p12<- ggline(data = design_pc,
       x="lactose", y="resposta",
       group = "crospovidona",
       add=c("mean", "jitter"),
       color="crospovidona")+theme_bw()




#grafico de contorno####

x_levels <- expand.grid(lactose=seq(24,36, length=100),
                        crospovidona = seq(10,20, length=100))


y_pred <- predict(mod_decod, newdata = x_levels)

dados_graf_contorno <- cbind(x_levels,y_pred)


g_contorno <- ggplot(data = dados_graf_contorno,
                     mapping = aes(x=lactose, y= crospovidona, z=y_pred, fill=y_pred))+
              geom_tile()+
              scale_fill_distiller(palette = "RdYlGn",
                                   direction = 1)+
              geom_contour(color="navy")+
              coord_equal()+theme_bw()
                                  