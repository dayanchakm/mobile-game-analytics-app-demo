library(shiny)
library(plotly)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(xgboost)
library(dplyr)
library(jsonlite)
library(knitr)
library(shinydashboard)
library(bslib)
library(DT)

dt <- read.csv("data/data.csv", stringsAsFactors = TRUE)
test_data_churn <- read.csv("data/test_data_churn7.csv")
data_vp <- read.csv("data/data_vp.csv")

dt$user_first_engagement <- as.Date(as.POSIXct(dt$user_first_engagement, origin="1970-01-01"))
col_names <- read.csv("data/col_names.csv")
dt$churn7 <- as.factor(dt$churn7)
dt$churn14 <- as.factor(dt$churn14)
col_names <- col_names$x[-1]
dt_retention <- dt[dt$churn7 == 0, ]
dt_churn <- dt[dt$churn7 == 1, ]

load(file = "models/churn7_xgb.Rdata")
load(file = "models/churn14_xgb.Rdata")
load(file = "models/vp_xgb.Rdata")

load(file = "preds/churn7_xgb_preds.RData")
load(file = "preds/churn14_xgb_preds.RData")
load(file = "preds/vp_xgb_preds.RData")

load(file = "models/churn7_dtree.Rdata")
load(file = "models/churn14_dtree.Rdata")
load(file = "models/dtree_vp.Rdata")

preds <- list("Churn 7" = xgb_preds_churn7,
              "Churn 14" = xgb_preds_churn14,
              "VP" = xgb_preds_vp)
models <- list("Churn 7" = dtree_churn7,
              "Churn 14" = dtree_churn14,
              "VP" = dtree_vp)

ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage( theme = shinythemes::shinytheme("paper"),
    title = img(
      src = "logo.png",
      height = "180%",
      width = "100%",
      style = "position:relative;bottom:8px;"
    ),
    id = "nav",
    tabPanel(
      "Analytics",
      div(
        shinybrowser::detect(),
        class = "outer",
        tags$head(includeCSS("styles.css")),
        uiOutput("frame")
      )
    ),
    tabPanel(title = "Temp",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 selectInput(
                   inputId = "var1",
                   label = "x:",
                   choices = col_names
                 ),
                 selectInput(
                   inputId = "var2",
                   label = "y:",
                   choices = col_names
                 ),
                 selectInput(
                   inputId = "dist",
                   label = "Distribution:",
                   choices = col_names
                 )
               ),
               mainPanel(fluidRow(
                 column(6, plotlyOutput("plot")),
                 column(6, plotlyOutput("hist"), )
               ),
               br(),
               fluidRow(
                 column(6, plotlyOutput("hist_retention")),
                 column(6, plotlyOutput("hist_churn"))
               ), )
             )),
    ##########################################################################
    ################## MODEL ################################################
    ########################################################################
    tabPanel(title = "Models",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 selectInput(
                   inputId = "model",
                   label = "Select model:",
                   choices = c(
                     "Churn 7",
                     "Churn 14",
                     "VP"
                   )
                 ),
                 textInput(inputId = "ad_price", label = "Average Income per User per Ad:"),
                 dateRangeInput(
                   inputId = "date_range",
                   label = "Select Date Range for prediction:",
                   start = "2018-09-03",
                   end = "2018-09-03",
                   min = min(dt$user_first_engagement),
                   max = max(dt$user_first_engagement)
                 ),
                 actionButton(inputId = "predict", label = "Predict", 
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
               ),
               mainPanel(conditionalPanel(
                 condition = "input['predict'] > 0",
                 fluidRow(
                   column(6, fluidRow(align = "center", tags$style(".small-box {border-radius:5px;box-shadow: rgba(0, 0, 0, 0.1) 0px 4px 12px;}"),
                                      valueBoxOutput("churn_val"), valueBoxOutput("churn_val2")),
                          fluidRow(align = "center", valueBoxOutput("min_income"), valueBoxOutput("max_income")), br(),
                          DTOutput("metrics")),
                   column(6, fluidRow(textOutput("metric_table"), align = "center"), br(), imageOutput("model_plot"), tags$style("#metric_table{font-size: 20px;font-style: italic;}"))
                 ),
                 fluidRow(
                   
                     h5("Export Predictions:"), br(),
                     column(2, actionButton(inputId = "export_churned", label = "Churned users")),
                     column(2, actionButton(inputId = "export_retai\ned", label = "Retained users")),
                     column(2, actionButton(inputId = "export_full", label = "Full"))
                     
                   )
                 )
               )
             ))
    
  )
)

server <- function(input, output, session) {
  
  output$frame <- renderUI({
    tags$iframe(
      src = "https://lookerstudio.google.com/embed/reporting/cb87a79b-a167-47fb-93af-98c2831d0920/page/SfILD",
      width = "100%",
      height = shinybrowser::get_height() - 50
    )
  })
  x <- reactive(input$var1)
  y <- reactive(input$var2)
  dist <- reactive(input$dist)
  output$plot <- renderPlotly({
    if (is.factor(dt[, x()]) & is.factor(dt[, y()])) {
      g <- ggplot(data = dt, aes(x = dt[, x()], fill = dt[, y()])) +
        geom_bar(position = "fill") +
        ylab("Proportion") + xlab(x()) + labs(fill = y()) +
        ggtitle(paste("Barplot of", x(), "vs", y())) +
        theme_classic()
    } else if (is.factor(dt[, x()])) {
      g <-
        ggplot(data = dt, aes(x = dt[, x()],
                              y = dt[, y()])) +
        geom_boxplot() + xlab(x()) + ylab(y()) +
        ggtitle(paste("Boxplot of", x(), "vs", y())) +
        theme_classic()
    } else if (is.factor(dt[, y()])) {
      g <-
        ggplot(data = dt, aes(
          x = dt[, x()],
          y = dt[, y()],
          fill = dt[, y()]
        )) +
        geom_boxplot() + xlab(x()) + ylab(y()) + labs(fill = y()) +
        ggtitle(paste("Boxplot of", x(), "vs", y()))
    } else {
      g <- ggplot(data = dt, aes(x = dt[, x()], y = dt[, y()])) +
        geom_point() + geom_smooth(method = "lm") + xlab(x()) + ylab(y()) +
        ggtitle(paste("Scatter plot of", x(), "vs", y())) +
        theme_classic()
    }
    ggplotly(g)
  })
  output$hist <- renderPlotly({
    if (is.factor(dt[, dist()])) {
      ggplot(data = dt, aes(x = dt[, dist()])) +
        geom_histogram(
          stat = "count",
          colour = "black",
          fill = "white",
          size = 1
        ) +
        ylab("Frequency") + xlab(dist()) +
        ggtitle(paste("Distribution of", dist())) +
        theme_classic() + theme(axis.text.x = element_text(
          angle = 20,
          vjust = 0.5,
          hjust = 1
        ))
    } else {
      ggplot(data = dt, aes(x = dt[, dist()])) +
        geom_histogram(
          aes(y = ..density..),
          colour = 1,
          fill = "white",
          size = 1
        ) +
        geom_density(color = 2) +
        ylab("Frequency") + xlab(dist()) +
        ggtitle(paste("Distribution of", dist())) +
        theme_classic()
    }
  })
  output$hist_retention <- renderPlotly({
    if (is.factor(dt_retention[, dist()])) {
      ggplot(data = dt_retention, aes(x = dt_retention[, dist()])) +
        geom_histogram(
          stat = "count",
          colour = "black",
          fill = "white",
          size = 1
        ) +
        ylab("Frequency") + xlab(dist()) +
        ggtitle(paste("Distribution of", dist(), "for Retention group")) +
        theme_classic() + theme(axis.text.x = element_text(
          angle = 20,
          vjust = 0.5,
          hjust = 1
        ))
    } else {
      ggplot(data = dt_retention, aes(x = dt_retention[, dist()])) +
        geom_histogram(
          aes(y = ..density..),
          colour = 1,
          fill = "white",
          size = 1
        ) +
        geom_density(color = 2) +
        ylab("Frequency") + xlab(dist()) +
        ggtitle(paste("Distribution of", dist(), "for Retention group")) +
        theme_classic()
    }
  })
  output$hist_churn <- renderPlotly({
    if (is.factor(dt_churn[, dist()])) {
      ggplot(data = dt_churn, aes(x = dt_churn[, dist()])) +
        geom_histogram(
          stat = "count",
          colour = "black",
          fill = "white",
          size = 1
        ) +
        ylab("Frequency") + xlab(dist()) +
        ggtitle(paste("Distribution of", dist(), "for Churned group")) +
        theme_classic() + theme(axis.text.x = element_text(
          angle = 20,
          vjust = 0.5,
          hjust = 1
        ))
    } else {
      ggplot(data = dt_churn, aes(x = dt_churn[, dist()])) +
        geom_histogram(
          aes(y = ..density..),
          colour = 1,
          fill = "white",
          size = 1
        ) +
        geom_density(color = 2) +
        ylab("Frequency") + xlab(dist()) +
        ggtitle(paste("Distribution of", dist(), "for Churned group")) +
        theme_classic()
    }
  })
  
  ##############################################################################
  #################### MODELS TAB #############################################
  #############################################################################
  
  output$churn_val <- renderValueBox({
    req(input$predict)
    isolate(
      if (input$model == "VP") {
        valueBox(paste0(round((sum(preds_export()[,"preds"] == 1)/length(preds_export()[,"preds"]))*100, digits = 1),"%"),h4("VP"))
      } else {
        valueBox(paste0(round((sum(preds_export()[,"preds"] == 1)/length(preds_export()[,"preds"]))*100, digits = 1),"%"),h4("Churn"))
      })
    
  })
  output$churn_val2 <- renderValueBox({
    req(input$predict)
    isolate(
      if (input$model == "VP") {
        valueBox(paste0(round((sum(preds_export()[,"preds"] == 0)/length(preds_export()[,"preds"]))*100, digits = 1),"%"),h4("NON-VP"))
      } else {
        valueBox(paste0(round((sum(preds_export()[,"preds"] == 0)/length(preds_export()[,"preds"]))*100, digits = 1),"%"),h4("Retain"))
      })
  })
  output$min_income <- renderValueBox({
    req(input$predict)
    isolate(
      valueBox(value = h5(floor(sum(preds_export()[,"preds"] == 0)*cm()$byClass['Sensitivity']*as.numeric(input$ad_price)), "$ per ad"), h5("Expected income"))
    )
      })
  output$max_income <- renderValueBox({
    req(input$predict)
    isolate(
      valueBox(value = h5(floor(length(preds_export()[,"preds"])*cm()$overall['Accuracy']*as.numeric(input$ad_price)), "$ per ad"), h5("Possible income"))
    )
  })
  
  
  output$metrics <- renderDT({
    input$predict
    isolate(preds_export()
    # if (input$model == "VP") {
    #   confusionMatrix(data = as.factor(preds[[input$model]]), reference = as.factor(data_vp$vp))
    # } else if (input$model == "Churn 7") {
    #   confusionMatrix(data = as.factor(preds[[input$model]]), reference = as.factor(test_data_churn$churn7))
    # } else {
    #   confusionMatrix(data = as.factor(preds[[input$model]]), reference = as.factor(test_data_churn$churn14))
    # }
    )
  }, options = list(pageLength = 5))
  output$model_plot <-
    renderImage({
      # if (model() == "Churn 7 (Decision Tree)") {
      #   rpart.plot(dtree)
      # } else if (model() == "Churn 7 (XGBoost)") {
      #   xgb_table <- xgb.importance(model = xgb$finalModel)
      #   xgb.ggplot.importance(xgb_table)
      # }
      input$predict
      isolate({
        if (input$model == "VP") {
          list(src = "Rplot.png",contentType = 'image/png')
        } else if (input$model == "Churn 7") {
          list(src = "Rplot01.png",contentType = 'image/png')
        } else {
          list(src = "Rplot02.png",contentType = 'image/png')
        }
        
        # xgb_table <- xgb.importance(model = models[[input$model]]$finalModel)
        # xgb.ggplot.importance(xgb_table)
        # rpart.plot(models[[input$model]], type = 3,fallen.leaves = FALSE, branch.lty = 3,box.palette = "BuGn",clip.right.labs = FALSE, branch = .3, under = TRUE,shadow.col = "gray")
      })
    })
  
  cm <- reactive({
    if (input$model == "VP") {
      confusionMatrix(data = as.factor(preds[["VP"]]), reference = as.factor(data_vp$vp))
    } else if (input$model == "Churn 7") {
      confusionMatrix(data = as.factor(preds[["Churn 7"]]), reference = as.factor(test_data_churn$churn7))
    } else {
      confusionMatrix(data = as.factor(preds[["Churn 14"]]), reference = as.factor(test_data_churn$churn14))
    }})
  
  # Get confusion matrix metrics
  # metrics <- data.frame(
  #   Accuracy = paste0(round(cm()$overall['Accuracy'] * 100, digits = 1), "%"),
  #   Sensitivity = cm()$byClass['Sensitivity'],
  #   Specificity = cm()$byClass['Specificity']
  # )
  
  # Output confusion matrix table
  output$metric_table <- renderText({
    input$predict
    isolate(paste0("Accuracy: ", round(cm()$overall['Accuracy'] * 100, digits = 1), "%"))
  })
  
  
  
  preds_export <- reactive({
    if (input$model == "Churn 7") {
      filtered_data <- dt[dt$user_first_engagement >= input$date_range[1] & dt$user_first_engagement <= input$date_range[2],]
      filtered_data$country <- as.integer(factor(filtered_data$country))
      filtered_data$language <- as.integer(factor(filtered_data$language))
      p <-predict(xgb_churn7, filtered_data)
      data.frame("user" = filtered_data[, "user_pseudo_id"], "date" = filtered_data$user_first_engagement, "preds" = p)
    } else if (input$model == "VP"){
      filtered_data <- dt[dt$user_first_engagement >= input$date_range[1] & dt$user_first_engagement <= input$date_range[2],]
      filtered_data$country <- as.integer(factor(filtered_data$country))
      filtered_data$language <- as.integer(factor(filtered_data$language))
      p <-predict(xgb_vp, filtered_data)
      data.frame("user" = filtered_data[, "user_pseudo_id"], "date" = filtered_data$user_first_engagement, "preds" = p)
    } else {
      filtered_data <- dt[dt$user_first_engagement >= input$date_range[1] & dt$user_first_engagement <= input$date_range[2],]
      filtered_data$country <- as.integer(factor(filtered_data$country))
      filtered_data$language <- as.integer(factor(filtered_data$language))
      p <-predict(xgb_churn14, filtered_data)
      data.frame("user" = filtered_data[, "user_pseudo_id"], "date" = filtered_data$user_first_engagement, "preds" = p)
    }
  })
  
  observeEvent(input$predict, {
    if (input$model == "VP") {
      updateActionButton(session, "export_churned", label = "VP Users")
      updateActionButton(session, "export_retained", label = "Non-vp Users")
    } else {
      updateActionButton(session, "export_churned", label = "Churned Users")
      updateActionButton(session, "export_retained", label = "Retained Users")
    }
  })
  
  observeEvent(input$export_churned, {
    showModal(modalDialog(
      title = "Download Results for Churned Users",
      footer = tagList(
        downloadButton(outputId = "downloadCSV_churned", label = "CSV"),
        downloadButton(outputId = "downloadJSON_churned", label = "JSON")
      ),
      easyClose = TRUE,
    ))
  })
  
  observeEvent(input$export_retained, {
    showModal(modalDialog(
      title = "Download Results for Retained Users",
      footer = tagList(
        downloadButton("downloadCSV_retained", "CSV"),
        downloadButton("downloadJSON_retained", "JSON")
      ),
      easyClose = TRUE,
    ))
  })

  observeEvent(input$export_full, {
    showModal(modalDialog(
      title = "Download Full Predicted Data",
      footer = tagList(
        downloadButton("downloadCSV_full", "CSV"),
        downloadButton("downloadJSON_full", "JSON")
      ),
      easyClose = TRUE,
    ))
  })
  output$downloadCSV_churned <- downloadHandler(
    filename = function() {
      paste("export-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      f <- preds_export()
      f <- f[f$preds == 1,]
      write.csv(f, file, row.names = FALSE)
    }
  )
  
  output$downloadJSON_churned <- downloadHandler(
    filename = function() {
      paste("export-", Sys.Date(), ".json", sep="")
    },
    content = function(file) {
      f <- preds_export()
      f <- f[f$preds == 1,]
      writeLines(toJSON(f), file)
    }
  )
  
  output$downloadCSV_retained <- downloadHandler(
    filename = function() {
      paste("retained-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      f <- preds_export()
      f <- f[f$preds == 0,]
      write.csv(f, file, row.names = FALSE)
    }
  )

  output$downloadJSON_retained <- downloadHandler(
    filename = function() {
      paste("retained-", Sys.Date(), ".json", sep="")
    },
    content = function(file) {
      f <- preds_export()
      f <- f[f$preds == 0,]
      writeLines(toJSON(f), file)
    }
  )

  output$downloadCSV_full <- downloadHandler(
    filename = function() {
      paste("full-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preds_export(), file, row.names = FALSE)
    }
  )

  output$downloadJSON_full <- downloadHandler(
    filename = function() {
      paste("full-", Sys.Date(), ".json", sep="")
    },
    content = function(file) {
      writeLines(toJSON(preds_export()), file)
    }
  )
  
}

shinyApp(ui, server)
