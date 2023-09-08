dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  skin = "black",
  dashboardBody(
    img(src = "r4v.png", height = 80),
    tabsetPanel(
      tabPanel(
        title = "English",
        br(),
        p("Updated by September 2023, please send any comments to the Regional platform IM team", style = "color: #fff; background-color: #672D53"),
        column(
          4,
          shinydashboard::box(
            id = "box_2",
            title = "Please copy paste only the activities here (without header)",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 26,
            status = "primary",
            import_copypaste_ui("myid", title = "Paste only the activities rows.")
          )
        ),
        column(
          4,
          shinydashboard::box(
            id = "box_3",
            title = "Summary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            status = "warning",
            p("Number or Activities"),
            h2(textOutput("Number_of_Activities")),
            p("Number of errors"),
            h2(textOutput("Number_of_Errors")),
            p("Download the report"),
            downloadButton("report", "Download Data Quality report", style = "color: #fff; background-color: #672D53")
          )
        ),
        fluidRow(
          column(
            12,
            shinydashboard::box(
              id = "box_9",
              title = "Preview data",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              status = "primary",
              DT::dataTableOutput("error", width = "100%")
            )
          )
        ),
        fluidRow(
          column(
            1,
            shinydashboard::box(
              id = "box_15",
              title = "Control",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              status = "primary",
              tags$b("Imported data:"),
              verbatimTextOutput(outputId = "status"),
              verbatimTextOutput(outputId = "data")
            )
          )
        )
      ),
      tabPanel(
        title = "Espanol",
        br(),
        p("Actualizada a septiembre 2023, por favor, enviar sus comentarios al equipo regional de Manejo de informacion", style = "color: #fff; background-color: #672D53"),
        column(
          4,
          shinydashboard::box(
            id = "box_2",
            title = "Por favor, copie y pegue la tabla completa (SIN EL ENCABEZADO)",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 26,
            status = "primary",
            import_copypaste_ui("myidESP", title = "Pegue la tabla completa sin el encabezado aquí")
          )
        ),
        column(
          4,
          shinydashboard::box(
            id = "box_3",
            title = "Resumen",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            status = "warning",
            p("Número de actividades"),
            h2(textOutput("Number_of_ActivitiesESP")),
            p("Número de errores"),
            h2(textOutput("Number_of_ErrorsESP")),
            p("Descargar el reporte"),
            downloadButton("reportESP", "Descargar el reporte de calidad", style = "color: #fff; background-color: #672D53")
          )
        ),
        fluidRow(
          column(
            12,
            shinydashboard::box(
              id = "box_9",
              title = "Visualizar los datos",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              status = "primary",
              DT::dataTableOutput("errorESP")
            )
          )
        ),
        fluidRow(
          column(
            1,
            shinydashboard::box(
              id = "box_15",
              title = "Control",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              status = "primary",
              tags$b("Imported data:"),
              verbatimTextOutput(outputId = "statusESP"),
              verbatimTextOutput(outputId = "dataESP")
            )
          )
        )
      )
    )
  )
)