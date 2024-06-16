####planejamento####

install.packages("mixexp")
library(mixexp)


plan <- SLD(3,2)

plan <- rbind(plan,plan,plan)

y <- c(28.56,21.73,26.38,33.71,24.22,22.93,
      29.58,20.98,25.9,32.98,23.98,21.79,
      29.26,21.23,26.65,34,23.15,22.17)

plan$resp <- y
plan

####analise####

mod <- MixModel(frame = plan,
                  response = 'resp',
                  mixcomps = c('x1','x2','x3'),
                  model = 2)

summary(mod)


ModelPlot(model = mod,
          dimensions = list(x1='x1',x2='x2',x3='x3'),
          contour = T,
          fill = T,
          axislabs = c('sisal', 'justa', 'coco'),
          color.palette = cm.colors,
          colorkey = T)
