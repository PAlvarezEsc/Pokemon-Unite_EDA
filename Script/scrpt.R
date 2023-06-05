############### Proyecto Pokemon Unite Season 11 ##############

##### 1. Resumen de la empresa ##### 
#nPokemon Unite es un nuevo y famoso videojuego creado para dispositivos móviles y consola Nintendo Switch. El juego se hizo popular instantáneamente después de su lanzamiento inicial en Nintendo Switch, pero creció aún más después de su lanzamiento en dispositivos móviles e incluso ganó el premio al juego del año en Play Store en la región de EE. UU. y varios otros premios en diferentes categorías en varias otras regiones en el Premios anuales de Google Play 2021.

# El juego, con sus virtudes y limitaciones ha captado un publico objetivo bastante variado, acaparando gamers con diferentes grados de experiencia y habilidad. Esto ha generado gran controversia en el mundo gamer, debido a que los gamers más avezados, han tenido que lidiar con el desafío de interactuar con gamers (la mayoria niños pequeños fanaticos de la serie pokemon) con habilidades técnicas menores, generando grandes dificultades en su rendimiento personal en las partidas.

# Junto a esta problemática, se ha criticado a los creadores y desarrolladores de Pokemon Unite, por la poca claridad sobre los aspectos del juego claves, en los cuales, los fanáticos deben cuidar y mantener atención para mejorar sus curvas de desarrollo e ascenso, sobretodo, en partidas rankeadas.

# En el presente proyecto se realiza un analisis exploratorio del desempeño en una temporada de Ranked de Pokemon United en diferentes momentos del dia, diferentes momentos de la semana, utilizando diferentes tipos de personajes y rols dentro del juego, analizando así los aspectos más importantes del juego, victorias, derrotas y rendimientos.

##### 2.- Objetivo ######
#2.1. Objetivos generales.

#Por lo tanto, los objetivos generales del presente proyecto son:
  
# Identificar los aspectos fundamentales del juego para mejorar el desempeño dentro de las partidas.

# Identificar las mejores alternativas horarias para jugar y mantener un buen desempeño en las partidas.

##### 3.- Preparación ######

# Establecer la relación entre el rendimiento en una partida y las diferentes acciones de juego (Kills, Asistencias y Puntos convertidos).

# Analizar que Roles de juegos son más convenientes de jugar y que carriles de juegos pueden favorecer más influencia en una partida.

# Identificar los personajes (Pokemon) que más aportan victorias y que favorezcan un mejor rendimiento en las partidas.

# Identificar que momentos de la semana y del día son más factibles para tener un mejor rendimiento en partidas rankeadas.

##### 4.- Procesamiento ######



# 4.1 carga de los paquetes necesarios

# El dataset untilizado en este proyecto corresponde a una base de datos creada en una hoja de cálculo Excel, 
# considerando una temporada completa de ranked de Pokemon Unite (32 días).


#   El uso de Tidyverse permite facilitar el trabajo estadístico y la generación de trabajos reproducibles. 

library(tidyverse)
library(readxl)

# 4.2 Importando los datos

dataset <- read_excel("dataset.xlsx")
View(dataset)  

# 4,3 Transformacion y limpieza de datos

 # la verificaion previa si hay datos duplicadoso perdidos se hizo manualmente a traves de Excel.

# Agregare una columna de dia de la semana para verificar que dia de la semana es más eefectivo jugar

dataset$Fecha <- format(as.Date(dataset$Fecha), "%A")
view(dataset)

##### 5.- Análisis ######

 # Una vez procesados los datos, hago un analisis de los datos obtenidos realizadon un resumen de los mismos:

summary(dataset$`Puntos Marcados`)
summary(dataset$Kills)
summary(dataset$Asistencias)
summary(dataset$Nivel)
summary(dataset$Valoracion)


# Ordenar los datos segun dia

dataset$Fecha <- ordered(dataset$Fecha, 
                                   levels=c("domingo", "martes", "miércoles", "jueves", "viernes", "sábado"))

#  obtencion de promedio por dia de la semana

AVG_por_dia<- dataset %>% 
  group_by(Fecha) %>%  
  summarise(`Puntos Marcados` = mean(`Puntos Marcados`), 
            Kills = mean(Kills),
            Asistencias = mean(Asistencias),
            Nivel = mean(Nivel),
            Valoracion = mean(Valoracion)) %>%
  arrange(Fecha)

head(AVG_por_dia)

#  obtencion de promedio por momento del dia

AVG_por_momento<- dataset %>%
  group_by(`Momento del dia`) %>%
  summarise(`Puntos Marcados` = mean(`Puntos Marcados`), 
            Kills = mean(Kills),
            Asistencias = mean(Asistencias),
            Nivel = mean(Nivel),
            Valoracion = mean(Valoracion)) %>%
  arrange(`Momento del dia`)

head(AVG_por_momento)

#  obtencion de promedio por momento de la semana

AVG_semana<- dataset %>%
  group_by(`Dia de la Semana`) %>%
  summarise(`Puntos Marcados` = mean(`Puntos Marcados`), 
            Kills = mean(Kills),
            Asistencias = mean(Asistencias),
            Nivel = mean(Nivel),
            Valoracion = mean(Valoracion)) %>%
  arrange(`Dia de la Semana`)
head(AVG_semana)

# obtencion de promedio por rol

AVG_rol<- dataset %>%
  group_by(Rol) %>%
  summarise(`Puntos Marcados` = mean(`Puntos Marcados`), 
            Kills = mean(Kills),
            Asistencias = mean(Asistencias),
            Nivel = mean(Nivel),
            Valoracion = mean(Valoracion)) %>%
  arrange(Rol)
head(AVG_rol)

# obtencion de promedio  por carril

AVG_carril<- dataset %>%
  group_by(Carril) %>%
  summarise(`Puntos Marcados` = mean(`Puntos Marcados`), 
            Kills = mean(Kills),
            Asistencias = mean(Asistencias),
            Nivel = mean(Nivel),
            Valoracion = mean(Valoracion)) %>%
  arrange(Carril)
head(AVG_carril)

# obtencion de promedios por Pokemon

AVG_pokemon<- dataset %>%
  group_by(Pokemon) %>%
  summarise(`Puntos Marcados` = mean(`Puntos Marcados`), 
            Kills = mean(Kills),
            Asistencias = mean(Asistencias),
            Nivel = mean(Nivel),
            Valoracion = mean(Valoracion)) %>%
  arrange(Pokemon)
head(AVG_pokemon)

# Obtencion de promedios en juego por Equipos

AVG_Acompañante<- dataset %>%
  group_by(Acompañante) %>%
  summarise(`Puntos Marcados` = mean(`Puntos Marcados`), 
            Kills = mean(Kills),
            Asistencias = mean(Asistencias),
            Nivel = mean(Nivel),
            Valoracion = mean(Valoracion)) %>%
  arrange(Acompañante)
head(AVG_Acompañante)

##### 6.- Visualizacion ######

#    Ahora que analicé los datos, es hora de visualizarlos para responder las siguientes preguntas 
# y demostrar mis hallazgos:


## ¿Hay relacion entre los puntos marcados y la valoracion obtenida en cada partida?

dataset %>% 
  group_by(Pokemon) %>%  
  summarise(Average_Puntos_Marcados = mean(`Puntos Marcados`),
            Average_Valoracion = mean(Valoracion)) %>% 
  arrange(Pokemon) %>%
  ggplot(aes(x = Average_Puntos_Marcados , y = Average_Valoracion )) + 
  labs(title="Puntos Marcados y Valoración", x = "Puntos Marcados", y = "Valoración") +
  geom_line(color="purple") +
  theme_classic()

## ¿Hay relacion entre los kills y la valoracion obtenida en cada partida?

dataset %>% 
  group_by(Pokemon) %>%  
  summarise(Average_kills = mean(Kills),
            Average_Valoracion = mean(Valoracion)) %>% 
  arrange(Pokemon) %>%
  ggplot(aes(x = Average_kills , y = Average_Valoracion )) + 
  labs(title=" Kills y Valoración", x = "Kills", y = "Valoración") +
  geom_line(color="red") + 
  theme_classic()

## ¿Hay relacion entre los Asistencias y la valoracion obtenida en cada partida?

dataset %>% 
  group_by(Pokemon) %>%  
  summarise(Average_Asistencias = mean(Asistencias),
            Average_Valoracion = mean(Valoracion)) %>% 
  arrange(Pokemon) %>%
  ggplot(aes(x = Average_Asistencias , y = Average_Valoracion )) + 
  labs(title="Asistencias y Valoración", x = "Asistencias" , y = "Valoración") +
  geom_line(color="orange") +
  theme_classic()

# ¿En que dia de la semana hay mejor rendimiento en las partidas?

dataset %>% 
  group_by(Fecha) %>%  
  summarise(Valoración = mean(Valoracion)) %>% 
  arrange(Fecha) %>%
  ggplot(aes(x = Fecha , y = Valoración, fill = Valoración)) + 
  labs(title="Rendimiento por día de la semana", x = NULL , y = "Valoración") +
  geom_col()+
  scale_fill_gradient(low="purple", high="orange")+
  theme_classic()

# ¿En que momento del dia hay mejor rendimiento en las partidas?

dataset %>% 
  group_by(`Momento del dia`) %>%  
  summarise(Valoración = mean(Valoracion)) %>% 
  arrange(`Momento del dia`) %>%
  ggplot(aes(x = `Momento del dia` , y = Valoración, fill = Valoración)) + 
  labs(title="Valoracion por Momento del dia", x = NULL, y = "Valoración") +
  geom_col() +
  scale_fill_gradient(low="purple", high="orange") +
  theme_classic()

# ¿En que momento de la semana hay mejor rendimiento en las partidas?

dataset %>% 
  group_by(`Dia de la Semana`) %>%  
  summarise(Valoración = mean(Valoracion)) %>% 
  arrange(`Dia de la Semana`) %>%
  ggplot(aes(x = `Dia de la Semana` , y = Valoración, fill = Valoración)) + 
  labs(title="Valoracion por Momento de la semana", x = NULL, y = "Valoración") +
  geom_col() + 
  scale_fill_gradient(low="purple", high="orange")+
  theme_classic()

# ¿ Qué pokemones tienen mejor rendimiento?

dataset %>% 
  group_by(Pokemon) %>%  
  summarise(Valoración= mean(Valoracion)) %>% 
  arrange(Pokemon) %>%
  ggplot(aes(x = Pokemon , y = Valoración, fill = Valoración)) + 
  labs(title="Valoracion por Pokemon", x = NULL, y = "Valoración") +
  geom_col() + 
  scale_fill_gradient(low="purple", high="orange")+
  theme_classic()

# ¿En que rol me desempreño mejor?

dataset %>% 
  group_by(Rol) %>%  
  summarise(Valoración = mean(Valoracion)) %>% 
  arrange(Rol) %>%
  ggplot(aes(x = Rol , y = Valoración, fill = Valoración)) + 
  labs(title="Valoración por Rol", x = NULL) +
  geom_col() + 
  scale_fill_gradient(low="purple", high="orange")+
  theme_classic()

# 1.- Victorias por temporada

ggplot(dataset, aes(x=reorder(Resultado, Resultado, function(x)-length(x)))) +
  geom_bar(fill= c("purple", "orange")) +  
  labs( title = " Resutados de la temporada", x='Resultado', 
                                                 y = " Conteo") +
  theme_classic()

# Resultados por pokemon

ggplot(dataset, aes(dataset$Pokemon, ..count..)) + 
  geom_bar(aes(fill = dataset$Resultado), position = "dodge") +
  labs( title = " Número de Victorias por pPokemon", x = NULL,
        y = " Conteo") +
  scale_fill_discrete(name = NULL) +
  theme_classic()  
  
# 2.- Resultados por Momento del día

ggplot(dataset, aes(dataset$`Momento del dia`, ..count..)) + 
  geom_bar(aes(fill = dataset$Resultado), position = "dodge") +
  labs( title = " Número de Victorias por momento del dia", x = NULL,
        y = " Conteo") +
  scale_fill_discrete(name = NULL) +
  theme_classic()  
  
# 3.- Victorias por  momento de la semana

ggplot(dataset, aes(dataset$`Dia de la Semana`, ..count..)) + 
  geom_bar(aes(fill = dataset$Resultado), position = "dodge") + 
  labs( title = " Número de Victorias por momento de la semana", x = NULL,
        y = " Conteo") +
  scale_fill_discrete(name = NULL) +
  theme_classic()  

# 4.- Victorias por Rol

ggplot(dataset, aes(dataset$Rol, ..count..)) + 
  geom_bar(aes(fill = dataset$Resultado), position = "dodge") + 
  labs( title = " Número de Victorias por rol", x = NULL,
        y = " Conteo") +
  scale_fill_discrete(name = NULL) +
  theme_classic()  

# 5.- Victorias por Carril

ggplot(dataset, aes(dataset$Carril, ..count..)) + 
  geom_bar(aes(fill = dataset$Resultado), position = "dodge") + 
  labs( title = " Número de Victorias por momento Carril", x = NULL,
        y = " Conteo") +
  scale_fill_discrete(name = NULL) +
  theme_classic()  


##### 7.- Conclusiones ######
