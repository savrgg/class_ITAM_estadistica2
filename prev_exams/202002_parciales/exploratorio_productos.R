library(tidyverse)
library(arrow)
library(lubridate)
library(readxl)

cal <- 
  readxl::read_xlsx("abraxas/pedido_urgente/calendario_bimbo.xlsx") %>% 
  set_names(tolower(names(.))) %>% 
  mutate(fecha = as.Date(fecha))

data <- read_parquet("abraxas/pedido_urgente/all_data.parquet") %>% 
  set_names(tolower(names(.)))

cat_productos <- 
  read_csv("abraxas/pedido_urgente/cat_productos.csv") %>% 
  filter(Bandera == 1) %>% 
  select(Producto_DwId, Producto, Sublinea, Linea, Categoria) %>% 
  set_names("producto", "producto_name", "sublinea_name", "linea_name", "categoria_name")
  

# 835,375
xx <- data %>% 
  left_join(cal) %>% 
  filter(agencia_nombre %in% c("CALLAO", "INDEPENDENCIA")) %>% 
  group_by(year, sem, agencia, agencia_nombre, ruta, producto, marca, categoria, linea) %>% 
  summarise(venta_piezas = sum(venta_piezas)) %>% 
  ungroup %>% 
  mutate(year_sem = paste0(year, str_pad(sem, side = "left", pad = "0", width = 2))) %>% 
  left_join(cat_productos, by = c("producto"="producto")) %>% 
  mutate(linea_sublinea = paste0(linea, "-", sublinea_name))

xx %>%
  group_by(categoria_name, agencia_nombre, year_sem) %>% 
  summarise(venta_piezas = sum(venta_piezas)) %>% 
  ggplot(aes(x = year_sem, y = venta_piezas/1000, fill = categoria_name))+
  geom_bar(stat = "identity")+
  facet_grid(agencia_nombre~.)+
  theme_minimal()+
  labs(y = "Venta en miles de piezas", x = "Semana", fill = "", 
       title = "Venta de Piezas para Agencias Callao e Independencia",
       caption = "Retrieved data from: MC1",
       subtitle = "Datos por categoría")+
  theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 6),
        legend.position = "bottom")+
  scale_fill_manual(values = c("darkorange4", "darkolivegreen4", "dodgerblue4", "darkorchid4", "darkgoldenrod4", "darkcyan", "deeppink4"))



gg_categoria <- function(categoria, colors){
  xx %>%
    filter(categoria_name == categoria_par) %>% 
    group_by(categoria_name, linea_sublinea, agencia_nombre, year_sem) %>% 
    summarise(venta_piezas = sum(venta_piezas)) %>% 
    ggplot(aes(x = year_sem, y = venta_piezas/1000, fill = linea_sublinea))+
    geom_bar(stat = "identity")+
    facet_grid(categoria_name~agencia_nombre, scales = "free_y")+
    theme_minimal()+
    labs(y = "Venta en miles de piezas", x = "Semana", fill = "", 
         title = "Venta de Piezas para Agencias Callao e Independencia",
         caption = "Retrieved data from: MC1",
         subtitle = "Datos por categoría")+
    theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust=1),
          axis.text = element_text(size = 6),
          legend.position = "bottom")+
    scale_fill_manual(values = colors_par)
}


ggcategoria("Confiteria", colors_par = )
(xx %>% filter(categoria_name == "Confiteria")) %>% select(linea_sublinea) %>% unique
(xx %>% filter(categoria_name == "Galletas")) %>% select(linea, sublinea_name) %>% unique
(xx %>% filter(categoria_name == "Pan Dulce")) %>% select(linea, sublinea_name) %>% unique
(xx %>% filter(categoria_name == "Panes")) %>% select(linea, sublinea_name) %>% unique
(xx %>% filter(categoria_name == "Pasteles")) %>% select(linea, sublinea_name) %>% unique
(xx %>% filter(categoria_name == "Promociones")) %>% select(linea, sublinea_name) %>% unique
(xx %>% filter(categoria_name == "Tortillas")) %>% select(linea, sublinea_name) %>% unique


  
  group_by(categoria_name, linea_name, agencia_nombre, year_sem) %>% 
  summarise(venta_piezas = sum(venta_piezas)) %>% 
  ggplot(aes(x = year_sem, y = venta_piezas/1000, fill = linea_name))+
  geom_bar(stat = "identity")+
  facet_grid(categoria_name~agencia_nombre, scales = "free_y")+
  theme_minimal()+
  labs(y = "Venta en miles de piezas", x = "Semana", fill = "", 
       title = "Venta de Piezas para Agencias Callao e Independencia",
       caption = "Retrieved data from: MC1",
       subtitle = "Datos por categoría")+
  theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 6),
        legend.position = "bottom")+
  scale_fill_manual(values = colors_par)
gg_categoria("Confiteria", c("darkorange1", "darkorange4"))






