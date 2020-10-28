library(readxl)
library(tidyverse)

xx <- readxl::read_excel("Desktop/90_PERS_estadistica/900_PERS_calificaciones/99_estadistica2.xlsx")
nombre <- xx$`CVE. UNICA`

parcial_1 <- xx$`Parcial 1 (20%)   26 Febrero`
parcial_2 <- xx$`Parcial 2 adj`
parcial_3 <- xx$`Parcial 3`
final <- xx$Final
tarea1 <- xx$`Tarea 1        24 Febrero`
tarea2 <- xx$`Tarea 2`
tarea3 <- xx$`Tarea 3`


Tareas <- 
  data.frame(
    cu = nombre, 
    tarea1 = tarea1, 
    tarea2 = tarea2, 
    tarea3 = tarea3) %>% filter(!is.na(cu)) %>% 
  gather(tarea, calificación, -cu) %>% 
  mutate(baja = if_else(cu %in% c("00155559",
                                  "00156059",
                                  "00157354"), "baja", ""))

plot1 <- 
Tareas %>% 
  ggplot(aes(x = cu, y = calificación, , fill = tarea)) +
  geom_bar(stat = "identity", position = "stack")+
  geom_text(aes(label = calificación), size = 3, position = position_stack(vjust = 0.5), color = "gray20") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  labs(x = "Clave única", y = "Tareas (15 puntos)", title = "Calificaciones Tareas (15%)",
       subtitle = "Fechas de entrega: 24 Febrero, 10 abril y 6 mayo", fill = "",
       caption = "Se toma como correcto si el procedimiento es correcto aunque no tenga correcto resultado")+
  geom_hline(yintercept = 15, color = "red") +
  geom_hline(yintercept = 3.63*3, color = "blue", show.legend = "mean", linetype = 2) +
  geom_line() +
  scale_fill_brewer(palette = "Greens") 




Examenes <- 
  data.frame(
    cu = nombre, 
    parcial1 = parcial_1, 
    parcial2 = parcial_2, 
    parcial3 = parcial_3,
    final = final) %>% filter(!is.na(cu)) %>% 
  gather(parcial, calificación, -cu) %>% 
  mutate(baja = if_else(cu %in% c("00155559",
                                  "00156059",
                                  "00157354"), "baja", ""))

plot2 <- 
Examenes %>% 
  ggplot(aes(x = cu, y = calificación, , fill = parcial)) +
  facet_wrap(~parcial)+
  geom_bar(stat = "identity", position = "stack")+
  geom_text(aes(label = calificación), size = 3, angle = 90, position = position_stack(vjust = 0.5), color = "gray20") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  labs(x = "Clave única", y = "Examenes (85 puntos)", title = "Calificaciones Examenes (85%)",
       subtitle = "Fechas de examen: 26 Febrero, 13 abril, 6 mayo y 6 junio", fill = "")+
  geom_hline(yintercept = 100, color = "red") +
  geom_line() +
  scale_fill_brewer(palette = "Greens") 


ggsave(filename = "calificaciones_tareas.png",plot = plot1, width = 8, height = 5)
ggsave(filename = "calificaciones_examenes.png",plot = plot2, width = 8, height = 5)

