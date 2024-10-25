library(shiny)
library(bslib)
library(shinychat)

ui <- page_fillable(
  chat_ui("chat")
)

server <- function(input, output, session) {
  chat_append("chat", "Hello there. Let's chat!")

  observeEvent(input$chat_user_input, {
    chat_append("chat", paste0("You said \"", input$chat_user_input, "\""))
  })
}

shinyApp(ui, server)

?elmer::content_image_plot()
