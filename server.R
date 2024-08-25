#add upload capacity to shiny to 30MB

options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {
  
  ## Declaring Variables
  Data <- reactiveVal()
  DataESP <- reactiveVal()


  #ENGLISH ENGLISH
  
  imported <- import_copypaste_server("myid", btn_show_data = TRUE,
                                      show_data_in = "modal")
  
  output$status <- renderPrint({
    imported$status() })
  
    output$data <- renderPrint({
      source("R/Subimission2324QualityCheck.R")
     Data(ErrorSub2324EN(imported$data()))
     showNotification("Data Processing Complete",duration = 10, type = "error")
     
     output$Number_of_Activities <- renderText({nrow(Data())})
    output$Number_of_Errors <- renderText({sum(!is.na(Data()$Review))})
      showNotification("Successful",duration = 10, type = "error")
      
    })

  ## Data Preview
  output$error <- DT::renderDataTable({Data()},extensions = c("Buttons"), options = list(
    dom = 'lfrtip', 
    paging = TRUE,
    ordering = TRUE,
    lengthChange = TRUE,
    pageLength = 20,
    scrollX = TRUE,
    autoWidth = TRUE,
    rownames = TRUE

  ))

  ## Download Pre-Cleaned
  output$report <- downloadHandler(
    filename = function() {
      paste("Error_report", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(Data(), file)
    }
  )

  #ESPANOL ESPANOL ESPANOL ESPANOL ESPANOL

  imported2 <- import_copypaste_server("myidESP", , btn_show_data = TRUE,
                                       show_data_in = "modal")
  output$status <- renderPrint({
    imported2$status() })
  output$dataESP <- renderPrint({
    source("R/Subimission2324QualityCheck.R")
    DataESP(ErrorSub2324SP(imported2$data()))
    showNotification("Data Processing Complete",duration = 10, type = "error")

    output$Number_of_ActivitiesESP <- renderText({nrow(DataESP())})
    output$Number_of_ErrorsESP <- renderText({sum(!is.na(DataESP()$Review))})
    showNotification("Successful",duration = 10, type = "error")

  })

  ## Data Preview
  output$errorESP <- DT::renderDataTable({DataESP()},extensions = c("Buttons"), options = list(
    dom = 'lfrtip',
    paging = TRUE,
    ordering = TRUE,
    lengthChange = TRUE,
    pageLength = 20,
    scrollX = TRUE,
    autowidth = TRUE,
    rownames = TRUE
  ))

  ## Download Pre-Cleaned
  output$reportESP <- downloadHandler(
    filename = function() {
      paste("Reporte_Errores", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(DataESP(), file)
    }
  )



  
  
})





