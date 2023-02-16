library(tidyverse)


temp <- tempfile()
download.file('https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_poblacion_csv.zip', temp)
poblacion <- read.csv(unz(temp, 'poblacion.csv'))


poblacion[,c(5,7,9,25,28:31,46,48)] <- sapply(poblacion[,c(5,7,9,25,28:31,46,48)],
       function(x) 2-x)
mean(poblacion$sexo)
# 0.489131

poblacion$entidad <- floor(poblacion$folioviv/100000000)

entidades <- c('ags', 'bc', 'bcs', 'camp',
               'coah', 'col', 'chia', 'chih', 
               'cdmx', 'dur', 'gto', 'gue', 'hgo',
               'jal', 'mex', 'mich', 'mor', 'nay',
               'nl', 'oax', 'pue', 'que', 'qroo',
               'slp', 'sin', 'son', 'tab', 'tamp',
               'tlax', 'ver', 'yuc', 'zac')

poblacion$ent[poblacion$entidad %in% 1:32] <- entidades[match(poblacion$entidad, 1:32)]


poblacion[,51:56] <- sapply(poblacion[,51:56],
       function(x) ifelse(x > 2,
              abs(((x %% 5)) - 1) + 2,
              x))


# Descripción de los hogares

df <- poblacion %>%
  group_by(folioviv) %>%
  summarise(n = n())
summary(df$n)

poblacion$disc <- ifelse(poblacion$disc1 == 8, 0, 1)


table(poblacion$disc1) %>%
  prop.table() %>%
  as.data.frame() %>%
  subset(Var1 %in% c(1:7)) %>%
  ggplot(aes(x = Var1))+
  geom_bar(aes(y = Freq), stat = 'identity')+
  scale_x_discrete(labels = c('Caminar, moverse, subir o bajar',
                            'Ver, aún usando lentes',
                            'Hablar, comunicarse o conversar',
                            'Oír, aún usando aparato auditivo',
                            'Vestirse, bañarse o comer',
                            'Poner atención o aprender cosas sencillas',
                            'Tiene alguna limitación mental'))+
  coord_flip()

mean(poblacion$disc)

df <- poblacion %>%
  group_by(folioviv, disc) %>%
  summarise()
mean(df$disc)