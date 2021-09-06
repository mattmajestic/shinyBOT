library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(stringr)
library(shinycssloaders)
library(magrittr)

chat_dictionary <- data.frame("Question_String" = c("Course","Tree","Progress"),
                              "Answer_String" = c("Please take Survey First","Select Data Table Row","Look at Your Spots of Improvement"))

ui <- fluidRow(
  useShinydashboard(),
  shinydashboard::box("Shiny Chat Bot",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                          uiOutput("message_ui"),
                          textAreaInput("chat_question",""),
                          actionButton("chat_submit","Send Question"))
)

server <- function(input,output,session){
  
  rv <- reactiveValues(message_count = 0,
                       messages = as.character(),
                       message_times = as.character(),
                       user = as.character())
  
  observeEvent(input$chat_submit,{
    rv$message_count <- rv$message_count + 1
    rv$messages <- append(input$chat_question,rv$messages)
    rv$messages %>% print()
    rv$user <- "Matt"
    rv$message_count %>% print()
    rv$message_times <- append(rv$message_times,Sys.time() %>% as.character())
    
    rv$chat_dictionary <-  chat_dictionary %>%
      dplyr::filter(str_detect(Question_String,input$chat_question)) %>%
      dplyr::select(Answer_String)
    rv$chat_dictionary %>% print()
    
    if(nrow(rv$chat_dictionary) == 0){
      rv$chat_dictionary_response <- "Did Not Find in Dictionary.  Try searching again"
    }
      rv$chat_dictionary_response <- rv$chat_dictionary %>% as.character()
    
    rv$message_tags <- tagList()
      if(rv$message_count == 0){
        "Don't run" %>% print()
      }else{
          for(message in 1:rv$message_count) {
            temp_tag <- 
              userMessages(status = "success",width = 12,
              userMessage(
              author = rv$user,
              date = rv$message_times[message],
              # image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
              type = "sent",
              rv$messages[message]
            )
             # userMessage(
             #   author = "Chat Bot",
             #   date = rv$message_times[message],
             #   # image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
             #   type = "received",
             #   "Let me check our dictionary"
             # )
            )
            
            temp_tag <- append(temp_tag,
            userMessage(
              author = "Chat Bot",
              date = rv$message_times[message],
              # image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
              type = "received",
              rv$chat_dictionary_response
            ))
            rv$message_tags <- append(rv$message_tags,temp_tag) 
          }
      }
    
    output$message_ui <- renderUI({
        box(
          width = 12,
          title = "",
          solidheader = TRUE,
          status = "primary",
        rv$message_tags
      )
    })
    
    updateTextAreaInput(session,"chat_question",value = "")
      
  })
  
}

shinyApp(ui,server)

