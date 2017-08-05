explore = function(data) {

  ggplot2::theme_set(theme_minimal())

  shiny::shinyApp(
    ui = navbarPage("Data Explorer",
                    tabPanel("Raw data",
                             DT::dataTableOutput("table")),
                    tabPanel("1D Summary statistics",
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("selVar", "Select variable",
                                             choices=colnames(data)),
                                 checkboxGroupInput('summStat',
                                                    label = 'Summary statistics',
                                                    choices = list("Mean" = 'mean',
                                                                   "Median" = 'median',
                                                                   "Standard deviation" = 'sd',
                                                                   "Summary" = 'summary'),
                                                    selected = c('mean','median','sd','summary'))
                               ),
                               mainPanel(
                                 verbatimTextOutput("OneDsummary")
                               )
                             )
                             ),
                    tabPanel("2D Summary statistics",
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("selVar1", "Select variable/category 1",
                                             choices=colnames(data)),
                                 selectInput("selVar2", "Select variable 2",
                                             choices=colnames(data)),
                                 checkboxGroupInput('TwoDStat',
                                                    label = 'Summary statistics',
                                                    choices = list("Table" = 'table',
                                                                   "Correlation" = 'cor',
                                                                   "Aggregate" = 'agg'),
                                                    selected = c('table'))
                               ),
                               mainPanel(
                                 verbatimTextOutput("TwoDsummary")
                               )
                             )
                    ),
                    navbarMenu("Plots",
                          tabPanel("1D Plots",
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("selVar", "Select variable",
                                             choices=colnames(data)),
                                 radioButtons("plotType1D", "Plot type",
                                              c("Histogram"="hist",
                                                "Kernel Density"="dens",
                                                "Bar chart" = "bar")
                                 ),
                                 uiOutput("ui1D")
                               ),
                               mainPanel(
                                 plotOutput("OneDplot")
                               )
                             )
                    ),
                    tabPanel("2D Plots",
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("selVar1", "Select variable/category 1",
                                             choices=colnames(data)),
                                 selectInput("selVar2", "Select variable 2",
                                             choices=colnames(data)),
                                 radioButtons("plotType2D", "Plot type",
                                              c("Scatter"="scat",
                                                "Box plot"="box")
                                 ),
                                 uiOutput("ui2D")
                               ),
                               mainPanel(
                                 ggiraphOutput("TwoDplot")
                               )
                             )
                    )))
    ,
    server = function(input, output) {

      output$OneDsummary <- renderPrint({
        cat('Statistics for', input$selVar, '\n\n')
        if('mean' %in% input$summStat) {
          cat('mean:',mean(data[,input$selVar]), '\n\n')
        }
        if('median' %in% input$summStat) {
          cat('median:',median(data[,input$selVar]), '\n\n')
        }
        if('sd' %in% input$summStat) {
          cat('sd:',sd(data[,input$selVar]), '\n\n')
        }
        if('summary' %in% input$summStat) {
          cat('summary:\n')
          print(summary(data[,input$selVar]))
        }
      })

      output$TwoDsummary <- renderPrint({
        if('table' %in% input$TwoDStat) {
          cat('Table of', input$selVar1, 'vs', input$selVar2, '\n')
          print(table(data[,input$selVar1], data[,input$selVar2]))
        }
        if('cor' %in% input$TwoDStat) {
          cat('Correlation between', input$selVar1, 'and', input$selVar2, '\n')
          print(cor(data[,input$selVar1], data[,input$selVar2]))
        }
        if('agg' %in% input$TwoDStat) {
          print(aggregate(data[,input$selVar2],
                          by = list(data[,input$selVar1]),
                          mean))
        }
      })

      output$OneDplot <- renderPlot({
        if(input$plotType1D == 'hist') {
          ggplot(data, aes_string(x = input$selVar,
                                  fill = '..count..')) +
            geom_histogram(bins = input$nBins) +
            theme(legend.position = 'None')
        } else if(input$plotType1D == 'dens') {
          ggplot(data, aes_string(x = input$selVar)) +
            geom_density(bw = input$bw,
                         fill = "blue",
                         alpha = 0.5) +
            theme(legend.position = 'None')
        } else if(input$plotType1D == 'bar') {
          ggplot(data, aes_string(x = input$selVar,
                                  fill = input$selVar)) +
            geom_bar() +
            theme(legend.position = 'None')
        }
      })

      output$TwoDplot <- renderggiraph({
        if(input$plotType2D == 'scat') {
          p = ggplot(data, aes_string(x = input$selVar1,
                                  y = input$selVar2,
                                  tooltip = 1:nrow(data))) +
            geom_point_interactive()
          ggiraph(code = print(p))
        } else if(input$plotType2D == 'box') {
          p2 = ggplot(data, aes_string(x = input$selVar1,
                                  y = input$selVar2,
                                  fill = input$selVar1)) +
            geom_boxplot_interactive() +
            theme(legend.position = 'None')
          ggiraph(code = print(p2))
        }
      })

      output$ui1D <- renderUI({
        switch(input$plotType1D,
               "hist" = sliderInput("nBins", "Number of bins",
                                      min = 10, max = 50, value = 30),
               "dens" = sliderInput("bw", "Bandwidth",
                                    min = 0,
                                    max = ceiling(2 * bw.nrd0(data[,input$selVar])),
                                    value = bw.nrd0(data[,input$selVar]))
        )
      })

      output$table <- DT::renderDataTable(DT::datatable({
        data
      },
      filter = 'top',
      options = list(pageLength = 25,
                     searchHighlight = TRUE,
                     autoWidth = TRUE,
                     class = 'cell-border stripe')))
    }
  )
}
