
server <- function(input, output, session) {
  # filter by put_id ----
  put_filtered <- reactive({
    cad_data |>  
      filter(put_id == input$code)
  })
  # render map by put_id ----
  #### option A. Map base filtered info regenerated at every search
  output$map <- renderLeaflet({
    leaflet(data =  put_filtered())%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addPolygons(color = "#229954", 
                  stroke =TRUE,
                  weight = 3, 
                  smoothFactor = 0.2,
                  opacity = 3.0,
                  fillColor = "transparent",
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "red", weight = 5,
                                                      bringToFront = TRUE),
                  popup = ~paste(
                    "Numero de PUT:", put_filtered()$put_id, "<br>",
                    "Categ. Anter.:", put_filtered()$categoria_ant, "<br>",
                    "Grupo:", put_filtered()$grupo,"<br>",
                    "Estado:", put_filtered()$estado,"<br>",
                    "Area (ha):", put_filtered()$sup_sig)
      )
  }) # End of plot of map
  #### option B. Update the map without re-creating the map form scratch
  #1. Render the initial static map
#output$map <- renderLeaflet({
#      leaflet(data =  bn_data) %>%
#    fitBounds(lng1 = -61.21584, lat1 = -20.62128, lng2 = -57.99799, lat2= -19.36932) %>% 
#        addProviderTiles(providers$CartoDB.Positron)
#
#    }) # 
# 2. The dynamic content along the leafletProxy
#observe(leafletProxy("map", data = estab_filtered()) %>% 
#  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
#              opacity = 1.0, fillOpacity = 0.5,
#              fillColor = "#49f3e1", stroke = TRUE,
#              highlightOptions = highlightOptions(color = "red", weight = 5,
#                                                  bringToFront = TRUE), 
#              popup = ~paste(
#                      "Cod. Establecimiento", estab_filtered()$cod_establ, "<br>",
#                      "CAT", estab_filtered()$cat,"<br>",
#                      "Sit. Legal:", estab_filtered()$sit_legal,"<br>",
#                      "Sit. Ambiental:", estab_filtered()$sit_ambien)
#              )
#)

  # render a table 
  output$table <- DT::renderDataTable(
    put_filtered() %>% 
      select("Numero de PUT" = "put_id", "Periodo" = "anho_capa", 
             "Categ. Anterior"= "categoria_ant", "Grupo:"= "grupo",
             "Area (ha)" = "sup_sig") %>% 
      st_drop_geometry() %>% 
    DT::datatable(options = list(pagelength = 10 
                                 #dom= "t" 
                                 ), 
                  rownames = FALSE
                  )
    )
  
  # render plot for areas selected by year ----
  
  output$plot_area <- renderPlot({
    
    put_filtered() %>% 
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
    
  })
 
  
  # render a markdown report
  output$download_report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed)
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data = put_filtered(), 
                     map = input$map, 
                     table = input$table
                     )
      
      rmarkdown::render(tempReport, 
                        #output_format = "html_document",
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

