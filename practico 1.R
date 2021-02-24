install.packages("skimr")
library(skimr)
library(tidyverse)
library(janitor)
install.packages("hrbrthemes")
library(hrbrthemes)
library(lubridate)
install.packages("summarytools")
library(summarytools)
library(dplyr)
install.packages("ggsci")
library(ggsci)
library(ggplot2)

delitos_2019 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2019.csv")

delitos_2018 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2018.csv")

delitos_2017 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2017.csv")

df1<-read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2019.csv")
df2<-read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2018.csv")
df3<- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2017.csv")

delitos<-bind_rows(df1, df2, df3 %>% mutate(franja_horaria=as.double(franja_horaria)))

# 5.Cuantas filas tiene cada dataset? Y cuantas columnas?
dim (df1)
#Tiene 117661 filas y 10 columnas
dim (df2)
#Tiene 123733 filas y 10 columnas
dim (df3)
#Tiene 120564 filas y 10 columnas
dim(delitos)
#Tiene 361958  filas y 10 columnas

## 6. Que tipo de datos contiene el dataset? 
glimpse(delitos)
#id:      <dbl>  valores numéricos con puntos decimales 
# fecha  <date>
#franja_horaria       <dbl>
#tipo_delito          <chr> caracter
# cantidad_registrada <dbl> 
# comuna  <dbl>
#barrio   <chr>
# lat     <dbl>
# long    <dbl>

#Cuantos valores faltantes se registran en cada variable? 
sum(is.na(delitos$franja_horaria))
#43

sum(is.na(delitos$comuna ))
#7738
sum(is.na(delitos$lat ))
#7738
sum(is.na(delitos$long ))
#7738

### 8. Que sucede con la variable cantidad registrada? Explora los valores unicos, 
#cuales son los valores mas frecuentes y saca conclusiones al respecto. Puede que 
#tengas que buscar sobre tablas de frecuencia.  

unique(delitos$cantidad_registrada)
freq(delitos$cantidad_registrada)
# son delitos unicos que solo pasaron una vez, en el 99,99 %

## 9. Cual es la relacion entre tipo de delito y subtipo de delito? Describir. Puede que 
#tengas que buscar sobre tablas de contingencia


delitos %>% 
  select(tipo_delito, subtipo_delito) %>% 
  table()
#                       subtipo_delito
#tipo_delito             Doloso Hurto Automotor Robo Automotor Siniestro Vial
#Homicidio                364               0              0            365
#Hurto (sin violencia)      0           16573              0              0
#Lesiones                   0               0              0          25320
#Robo (con violencia)       0               0           7766              0

#En una tabla podemos analizar que en la base de Datos obtenemos cuatro tipos de Delitos,
#los cuales son Homicidios, Hurto (sin violencia), Lesiones y Hurto (con violencia). 
#Por su parte los subtipos de delitos son Doloso, Hurto automotor, Robo automotor y
#Siniestro Vial. La relación que podemos hacer en base a la tabla, son distitas tendencias.
#La primera es que en los tipos de delitos los subtipos dolosos y siniestros viales son los 
#que absorven a la mayoria de los homicidios causalmente registrados. POr su parte el Hurto 
#(sin violencia) esta casi totalmente relacionado al hurto automotor, dato con el cual no estamos
#totalmente de acuerdo, dado que hurto, es el hecho en el cual no opera ningun tipo de forzamiento
#sobre la cosa y/o victima.Respecto al Robo (con violencia) analizamos que 7766 casos, fueron
#de robo automotor. dato que considero que dberia ser reabordado, con Hurto (sin violencia) puede
#ocurri el robo sin violencia y que en la confeccion de la base de datos, hayan incluido robos 
#sin violencia, junto con hurto. El hurto es siempre sin violencia.
#Finalmente tenemos que la gran mayoria de los delitos de lesiones, ocurrieron en siniestros viales.

## 10. Hace el grafico pertinente para mostrar los tipos de delitos existentes y sus 
#frecuencias. No olvides incluir titulo, nombres a los ejes y colores.  

tab1 <- delitos %>% 
  group_by(tipo_delito) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = round(n/sum(n) *100, 2))

plot1 <- ggplot(data = tab1, aes(x = tipo_delito, y = perc))+
  geom_bar(stat = "identity", fill = "maroon", width = 0.7)+
  geom_text(aes(label = perc), size = 5, hjust = 0.5, vjust = -0.25)+
  theme_minimal()+
  labs(title = "Tipos de Delitos Existentes y sus Frecuencias",
       x = "",y = "Porcentaje",
       caption = "Analisis de Juan y Vero")+
  ylim(c(0,55))

plot(plot1, main = "Diagrama de barras")



## 11. Hace el grafico pertinente para mostrar como se distribuye la variable 
#franja horaria. No olvides incluir titulo, nombres a los ejes y colores.  

tab2 <- delitos %>% 
  group_by(franja_horaria) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = round(n/sum(n) *100, 2))

plot2 <- ggplot(data = tab2, aes(x = franja_horaria, y = perc))+
  geom_bar(stat = "identity", fill = "maroon", width = 0.7)+
  geom_text(aes(label = perc), size = 5, hjust = 0.5, vjust = -0.25)+
  theme_minimal()+
  labs(title = "Distribucion de la Variable Franja Horaria",
       x = "Hora",y = "Porcentaje",
       caption = "Analisis de Juan y Vero")+
  ylim(c(0,55))


plot(plot2, main = "Diagrama de barras")

## 12. Incorporaremos al grafico anterior una segmentacion por tipo de 
#delito y un filtro para quedarnos con los delitos que hayan ocurrido 
#especialmente en Puerto Madero. 

df5<- delitos %>%
  filter(barrio== "Puerto Madero") %>%
           select(tipo_delito)

tab3 <- df5 %>% 
  group_by(tipo_delito) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = round(n/sum(n) *100, 2))

plot3 <- ggplot(data = tab3, aes(x = tipo_delito, y = perc))+
  geom_bar(stat = "identity", fill = "maroon", width = 0.7)+
  geom_text(aes(label = perc), size = 5, hjust = 0.5, vjust = -0.25)+
  theme_minimal()+
  labs(title = "Tipos de Delito en Puerto Madero",
       x = "Hora",y = "Porcentaje",
       caption = "Analisis de Juan y Vero")+
  ylim(c(0,55))

plot(plot3, main = "Diagrama de barras")
