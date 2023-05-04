library(shiny)

fluidPage(
  theme = bslib::bs_theme(bootswatch = "morph"),
  titlePanel("Reporte INFONA"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      # input -----
      textInput("code", "Codigo de PUT", value = ifelse(is.null(cad_data$put_id), "PUT0001", cad_data$put_id)),
      # button for download report
      downloadButton("download_report", "Guardar reporte")
    ),
    mainPanel(
      width = 8,
      fluidRow(
        column(
          width = 12,
          br(),
          div(
            style = 'overflow-x: scroll; height: auto;',
          # output 1 map ----
          leafletOutput(outputId = "map")
          )
        )
      ),
      
      fluidRow(
        # output 2 table ----
        DT::dataTableOutput("table")
      ),
      fluidRow(
        # output 3. Plot of percentage of areas  ----
    plotOutput(outputId = "plot_area")
      )
      
    )
  )
)
