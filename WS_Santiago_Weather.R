library(dplyr)
library(rvest) # paquete para web scraping
library(stringr)
library(ggplot2)
library(ggridges)

guardar_todo <- data.frame()
for (a in 1970:2021){
  
  for (m in 1:12){
    
    # Definir URL de donde se sacará la información (esta irá variando por los valores del loop)
    url <- paste0("https://climatologia.meteochile.gob.cl/application/mensuales/temperaturaMediaMensual/180005/", a, "/", m)
    
    df <- read_html(url) %>% # leer url
      # detectar la información a extraer
      rvest::html_nodes("td:nth-child(8)") %>% 
      rvest::html_text() %>% # extraer texto
      as_tibble() %>% 
      #slice(-c(1:3)) %>% # sacar filas no necesarias
      mutate(value = str_squish(value), # sacar espacios blancos
             sacar = ifelse(str_detect(value, ":"), 1, 0)) %>% # definir filas para sacar
      filter(sacar == 0, # sacar fila definida anteriormente
             value != ".") %>% # y filas que nos irven
      mutate(año = a, mes = m, .before = value) %>% # agregar columnas de año y mes
      select(-sacar) # eliminar columna innecesaria
    
    # almacenar información de cada loop
    guardar_todo <- bind_rows(guardar_todo, df)
    
    # para hacer seguimiento al loop
    print(a, m)
  }

}

# Transformar a tibble
guardar_todo <- guardar_todo %>% as_tibble()

# Ajustar datos
datos_grafico <- guardar_todo %>% 
  mutate(año_mes = case_when(
    nchar(mes) == 2 ~ as.character(mes),
    TRUE ~ paste0("0",mes))) %>% 
  mutate(value = as.numeric(value)) 

# Gráfico de distribución de temperaturas máximas 1970-2021 por mes
datos_grafico %>% 
  filter(año %in% c(1980, 2000, 2020)) %>% 
  ggplot(aes(x = value, y = año_mes, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.02) +
  scale_fill_viridis_c(name = "Temp. [C]", option = "C") +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  facet_wrap(vars(año), nrow = 3)



