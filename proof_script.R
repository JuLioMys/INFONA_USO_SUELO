library(sf)
library(leaflet)
library(tidyverse, warn.conflicts = FALSE)
library(DT)

# read data

# shapes de limites
lup_data<- st_read("data/lup.gpkg",
                   layer= "lup_limit")


# explore the data
names(lup_data)
glimpse(lup_data)

# Look for duplicate entries
## for variable put_id
lup_data %>%
  st_drop_geometry() %>% 
  group_by(put_id) %>% 
  summarise(conteo = n()) %>% 
  arrange(desc(conteo)) 

# filter data by put_id
put_filtered <- lup_data |> filter(put_id == "PUT0029")


# Interactive map with data put_id

leaflet(data =  put_filtered)|>
  addProviderTiles(providers$CartoDB.Positron)|>
  addPolygons(color = "#d35400", 
              stroke =TRUE,
              weight = 5, 
              smoothFactor = 0.2,
              opacity = 3.0,
              fillColor = "transparent",
              fillOpacity = 0.5,
              popup = ~paste(
                "Numero de PUT:", put_filtered$put_id, "<br>",
                "Periodo:", put_filtered$anho_capa,"<br>",
                "Estado:", put_filtered$estado,"<br>",
                "Area (ha):", put_filtered$sup_sig,"<br>",
                "Distrito:", put_filtered$distrito)
  )
 

  # plot a table
  DT::datatable(put_filtered %>% 
                  select("Numero de PUT" = "put_id", "Periodo" ="anho_capa", 
                         "Finca" = "finca", "Padron"= "padron",
                         "Area (ha)"= "sup_sig", "Distrito:"= "distrito") %>% 
                  st_drop_geometry(), options = list(pagelength = 10), rownames = FALSE)

  
# customize table
 # DT:::DT2BSClass('display')
  #DT:::DT2BSClass(c('compact', 'cell-border'))
  
  put_filtered %>% 
    select("Numero de PUT" = "put_id", "Periodo" ="anho_capa", 
           "Finca" = "finca", "Padron" = "padron",
           "Area (ha)" = "sup_sig", "Distrito:"= "distrito") %>%
    st_drop_geometry() %>% 
    DT::datatable(options = list(pagelength = 10, 
                                 #autoWidth = TRUE, 
                                 #dom= "t" #display the table, and nothing else
                                 dom= "ft" # display the search box and table
                                 ), 
                  rownames = FALSE,
                  #style = "bootstrap", 
                  #class = "table-bordered"
                  #class = "table-hover"
                  #caption = 'Table 1: This is a simple caption for the table.'
                  )  
                 
  
  # shapes de uso de suelo
  
cad_data<- st_read("data/lup.gpkg",
                   layer= "lup")

names(cad_data)
glimpse(cad_data)
# ver categorias de uso de suelo
unique(cad_data$categoria_ant)
unique(cad_data$categoria)
unique(cad_data$grupo)

# filter data by put_id
put_filtered_cad <- cad_data |> filter(put_id == "PUT1428")



#
leaflet(data =  put_filtered_cad)|>
  addProviderTiles(providers$CartoDB.Positron)|>
  addPolygons(
    color = "#229954", 
    stroke =TRUE,
    weight = 3, 
    smoothFactor = 0.2,
    opacity = 3.0,
    fillColor = "transparent",
    fillOpacity = 0.5,
    highlightOptions = highlightOptions(color = "red", weight = 5,
                                        bringToFront = TRUE),
    popup = ~paste(
      "Numero de PUT:", put_filtered_cad$put_id, "<br>",
      "Categ. Anter.:", put_filtered_cad$categoria_ant, "<br>",
      "Grupo:", put_filtered_cad$grupo,"<br>",
      "Estado:", put_filtered_cad$estado,"<br>",
      "Area (ha):", put_filtered_cad$sup_sig)) 



# ggplot based on uso de suelo
put_filtered_cad %>% 
  select(put_id, grupo, sup_sig, categoria_ant, categoria) %>% 
  st_drop_geometry() %>% 
  group_by(grupo) %>% 
  summarise(sum_uso = sum(sup_sig, na.rm = TRUE)) %>%
  pivot_wider(names_from = grupo, 
              values_from = sum_uso) %>% 
  pivot_longer(c("AREA_AUTORIZADA", "BOSQUES", "OTRAS_COBERTURAS"), 
               names_to = "categorias", values_to = "sum_uso") %>% 
  mutate(porcentaje = round(sum_uso / sum(sum_uso)*100)) %>% 
  ggplot(aes(x=2, y=porcentaje, fill=categorias)) +
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label = porcentaje),
            position=position_stack(vjust=0.5), color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange","gray"))+
  theme_void()+
  labs(title= "Uso de Suelo") +
  xlim(0.5, 2.5)

