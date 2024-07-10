####Planejamento simplex-centróide: teoria e implementação no R, YOUTUBE
library(mixexp)

#montagem planejamento
plan <- SCD(3)

plan <- rbind(plan, plan[7,],plan[7,],plan[7,])

resp <- c(60,76,88,80,72,83,75,77,73,77)

plan$resp <- resp

DesignPoints(plan)

#analise
analise <- MixModel(frame = plan,
                    response ='resp',
                    mixcomps = c('x1','x2','x3'),
                    model = 4)

summary(analise)

#graficos
grafsuperficie <- ModelPlot(model = analise,
          dimensions = list(x1='x1',x2='x2',x3='x3'),
          contour = T,
          fill = T,
          axislabs = c('k100m', 'xtend', 'k200'),
          color.palette = cm.colors,
          colorkey = T)

grafefeito <- ModelEff(nfac = 3,
                       mod = 4,
                       dir = 2,
                       nproc = 0,
                       ufunc = analise)

#pressupostos

shapiro.test(analise$residuals) #normalidade acima de 0,05




