library(shiny)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(base64enc)

api_key <- Sys.getenv("GROQ_API_KEY")

ui <- fluidPage(
  titlePanel("LLM Text Summarizer"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("default_behavior", "Default behavior:", "summarize in one sentence", width = "100%"),
      fileInput("upload_txt", "Upload TXT file (Optional)"),
      selectInput("dropdown_model", "Select model:", 
                  choices = c("deepseek-r1-distill-llama-70b", "llama-3.3-70b-versatile", "mixtral-8x7b-32768")),
      actionButton("send_button", "Send")
    ),
    mainPanel(
      textAreaInput("input", "Input Text", "", width = "100%", height = "200px"),
      textAreaInput("output", "Output Text", "", width = "100%", height = "200px"),
      downloadButton("download_txt", "Download Modified TXT")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$upload_txt, {
    req(input$upload_txt)
    ext <- tools::file_ext(input$upload_txt$name)
    if (ext != "txt") return(NULL)
    
    file_content <- readLines(input$upload_txt$datapath, warn = FALSE)
    updateTextAreaInput(session, "input", value = paste(file_content, collapse = "\n"))
  })
  
  observeEvent(input$send_button, {
    req(input$input)
    
    response <- tryCatch({
      res <- POST(
        "https://api.groq.com/openai/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", api_key)),
        content_type_json(),
        body = toJSON(list(
          model = input$dropdown_model,
          messages = list(
            list(role = "system", content = input$default_behavior),
            list(role = "user", content = input$input)
          )
        ), auto_unbox = TRUE),
        encode = "json"
      )
      content(res, as = "parsed")$choices[[1]]$message$content
    }, error = function(e) paste("LLM Error:", e$message))
    
    updateTextAreaInput(session, "output", value = response)
  })
  
  output$download_txt <- downloadHandler(
    filename = "modified_file.txt",
    content = function(file) {
      writeLines(input$output, file)
    }
  )
}

shinyApp(ui, server)
