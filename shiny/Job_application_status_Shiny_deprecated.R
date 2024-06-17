library(shiny)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggalluvial)

# Load data 
{
  FILE <- "./Job application records_20240616.xlsm"
  COLUMNS_TO_BE_USED <- c("Phone chat", "Interview 1st")
  # 
  # # base R 
  # df <- readxl::read_excel(FILE, sheet = "Records")
  # # Convert value in columns "Phone chat" and "Interview 1st" to TRUE/FALSE
  # df[COLUMNS_TO_BE_USED] <- lapply(df[COLUMNS_TO_BE_USED], function (x) {!is.na(x)})
  # 
  # df_freq <- data.frame(table(df[c("Job Board", COLUMNS_TO_BE_USED, "Status")]))
  # df_freq <- df_freq[df_freq$Freq!=0, ]
  # 
  # df_freq$Phone.chat <- as.logical(df_freq$Phone.chat)
  # df_freq$Interview.1st <- as.logical(df_freq$Interview.1st)
  # 
  # interview_status <- vector(mode = "list", length = nrow(df_freq))
  # for (i in 1:nrow(df_freq)) {
  #   state <- if (df_freq[i, "Phone.chat"] & df_freq[i, "Interview.1st"]) {"Phone chat and interview"}
  #   else if (df_freq[i, "Phone.chat"] & !df_freq[i, "Interview.1st"]) {"Phone chat"}
  #   else if (!df_freq[i, "Phone.chat"] & df_freq[i, "Interview.1st"]) {"Interview"}
  #   else if (!df_freq[i, "Phone.chat"] & !df_freq[i, "Interview.1st"]) {"None"}
  #   
  #   interview_status[[i]] <- state
  # }
  # df_freq$interview_status <- factor(unlist(interview_status), levels=c("Phone chat and interview", "Phone chat", "Interview", "None"))
  # 
  # 
  # # tidyverse
  # df <- readxl::read_excel(FILE, sheet = "Records") %>%
  #   mutate(across(all_of(COLUMNS_TO_BE_USED), ~ !is.na(.x))) %>%
  #   select(`Job Board`, all_of(COLUMNS_TO_BE_USED), `Status`) %>%
  #   group_by(`Job Board`, across(all_of(COLUMNS_TO_BE_USED)), `Status`) %>%
  #   summarise(Freq = n())
  #   
  # df_freq <- df %>%
  #   mutate(interview_status = case_when(
  #     `Phone chat` & `Interview 1st` ~ "Phone chat and interview",
  #     `Phone chat` & !`Interview 1st` ~ "Phone chat",
  #     !`Phone chat` & `Interview 1st` ~ "Interview",
  #     !`Phone chat` & !`Interview 1st` ~ "None"
  #   )) %>%
  #   mutate(interview_status = factor(interview_status,
  #                                    levels = c("Phone chat and interview", "Phone chat", "Interview", "None")))
  # 
  # 
  # data.table
  library(data.table)
  df <- as.data.table(readxl::read_excel(FILE, sheet = "Records"))
  df[, (COLUMNS_TO_BE_USED) := lapply(.SD, function(x) {!is.na(x)}), .SDcols = COLUMNS_TO_BE_USED]
  
  df <- df[, .SD, .SDcols = c("Job Board", COLUMNS_TO_BE_USED, "Status")]
  
  df_freq <- df[, .N, by = c("Job Board", COLUMNS_TO_BE_USED, "Status")]
  setnames(df_freq, "N", "Freq")
  
  df_freq[, interview_status := fcase(
    `Phone chat` & `Interview 1st`, "Phone chat and interview",
    `Phone chat` & !`Interview 1st`, "Phone chat",
    !`Phone chat` & `Interview 1st` , "Interview",
    !`Phone chat` & !`Interview 1st` , "None"
  )]
  # END
}

# Shiny App
# Define UI for app
ui <- fluidPage(
  titlePanel("Job Applcation Status"),
  
  sidebarPanel(
    checkboxGroupInput(
      inputId = "checkGroup",
      label = "What application status to see",
      choices = c("Accepted", "Chatting", "Pending", "Rejected", "Declined", 
                  "Ghosted", "Not Submitted", "In System"),
      selected = "Accepted"
      )
  ),
  
  mainPanel(
    textOutput(outputId = "text"),
    plotOutput(outputId = "plot", height = "600px")  
  )
  
)

# Define server logic
server <- function(input, output) {
  # Plot with ggalluvial
  # Reference: https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html
  {
    # Reactive expression to filter data based on input$checkGroup
    filtered_data <- reactive({
      df_freq %>%
        filter(Status %in% input$checkGroup)
    })
    
    number_of_job_application <- reactive({
      df_freq %>%
        filter(Status %in% input$checkGroup) %>%
        summarise(sum(Freq))
    })
    
    # Render output text
    # output$text <- renderText(output_text())
    
    output$text <- renderText({
      paste("Selected application status:", paste(input$checkGroup, collapse = ", "), 
            " ( n = ", number_of_job_application(), ")")
    })
    
    # Render output plot
    output$plot <- renderPlot(
      ggplot(filtered_data(), aes(y = `Freq`, axis1 = `Job Board`, axis2 = `Status`)) +
        geom_alluvium(aes(fill = `Status`), width = 1/8) +
        geom_stratum(alpha = 0.5, width = 1/8) +
        geom_label(stat = "stratum", aes(label = after_stat(stratum)), label.size = 0.1) +
        scale_x_discrete(limits = c("Job Board", "Status"), expand = c(.05, .05)) +
        # scale_fill_manual(values = c("Interview" = "#3F752B", "Phone chat" = "#E2AC76",
        #                              "Phone chat and interview" = "#81B0E4", "None" = "grey")) +
        facet_wrap(~interview_status, scales = "free") +
        scale_fill_brewer(type = "qual", palette = "Set3") +
        ggtitle("Job Applications Alluvial Diagram", paste0(format(Sys.Date(), "%d/%m/%Y"), ", n=", number_of_job_application()))
    )
    
    # END
  }
}

# Run the application
shinyApp(ui = ui, server = server)