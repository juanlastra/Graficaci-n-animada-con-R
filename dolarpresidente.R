
# grafico 


## obtener datos de la web

# paquetes necesarios

library(rvest)
library(tidyverse)
library(gganimate)
library(gifski)

### Obtener datos de la web

## año 2022
urla22 <- "https://www.dolar-colombia.com/mes/2022-08"
urls22 <- "https://www.dolar-colombia.com/mes/2022-09"
urlo22 <- "https://www.dolar-colombia.com/mes/2022-10"
urln22 <- "https://www.dolar-colombia.com/mes/2022-11"

## año 2018

urla18 <- "https://www.dolar-colombia.com/mes/2018-08"
urls18 <- "https://www.dolar-colombia.com/mes/2018-09"
urlo18 <- "https://www.dolar-colombia.com/mes/2018-10"
urln18 <- "https://www.dolar-colombia.com/mes/2018-11"

## vector con todas las urls

urls <- c(urln22, urlo22, urls22, urla22, 
          urln18, urlo18, urls18, urla18)


## crear data.frame vacio 

datos <- data.frame(X1 = c(),
                    X2 = c())


for(j in urls){
  archivo <- read_html(j)
  archivo <- html_nodes(archivo, "table")
  
  ## guardar los archivo
  tabla1 <- html_table(archivo[[1]])
  tabla2 <- html_table(archivo[[2]])
  
  ## unirlos 
  tabla <- rbind(tabla1, tabla2)
  
  ## unirlo a datos
  
  datos <- rbind(datos, tabla)
  
  print(paste0("Tabla: ", j, " Cargada"))
}


## crear los diferentes datos

datos$mes <- c(rep("2022-11", 30), rep("2022-10", 30),
               rep("2022-09", 30), rep("2022-08", 30),
               rep("2022-11", 30), rep("2022-10", 30),
               rep("2022-09", 30), rep("2022-08", 30))


# dia 
datos$dia <- c(30:1, 31:2, 30:1, 31:2,30:1, 
               31:2, 30:1, 31:2)

#fecha 

datos$fecha <- as.Date(paste0(datos$mes,"-",datos$dia),
                       format = "%Y-%m-%d")


## limpiar el valor del tipo de cambio

datos$X2 <- str_replace(datos$X2, "^[:print:]", "")

# elimar la coma

datos$X2 <- gsub(",", "", datos$X2)

# eliminar decimales
datos$X2 <- round(as.numeric(datos$X2), 0)


## colocar variable de presidente

datos$presidente <- c(rep("Petro", 120), 
                      rep("Duque", 120))


# ordenar los datos por fecha
datos <- datos[order(240:1),]



## obtener la variación 
datos$var <- (datos$X2 - lag(datos$X2)) / lag(datos$X2)

datos$var <- round(datos$var, 4)

## correción de primera observacion p. petro

datos[121, 7] <- NA



## variación acumulada

datos$varsum <- c(NA, cumsum(datos$var[2:120]),NA,
                  cumsum(datos$var[122:240]))


## grafico de variación acumulada
grafico <- ggplot(datos, aes(datos$fecha, datos$varsum,
                  col = datos$presidente)) +
  geom_line(size = 1.5) +
  theme_linedraw() +
  labs(title = "Variación Acumulada TRM",
       subtitle = "Primeros 4 meses de gobierno",
       x = "MES", y = "V. TRM",
       col = "") +
  theme(plot.title = element_text(size = 30, hjust = 0.5,
                                  colour = "blue"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        strip.background = element_rect(fill = "white"),
        legend.text = element_text(size = 20))+
  transition_reveal(datos$fecha)




animate(grafico,fps = 15, width = 900, height = 600)



