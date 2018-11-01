library(shiny)
library(ggplot2)
library(plyr)
incidence <- read.csv("iodb10.18.csv") #read in dataset
incidence <- subset(incidence, incidence$SPECIES == "Human" | incidence$SPECIES == " ") #subset only human data
incidence <- subset(incidence, incidence$RESULT_TYPE == "PERCENTAGE_POSITIVE")   #only taking incidence data  
incidence <- incidence[which(incidence$NUMERIC_RESULT != ""),]   #take out numeric result blanks 
write.csv(incidence,'incidence2.csv')#doing this because when you sub in R, it's not a vector so the code wont work but if you write it again, its a vector
incidence <- read.csv("incidence2.csv") #read in dataset
piecount <- count(incidence$COMPOUND_MODALITY)
piecount <- piecount[which(piecount$x != ""),]   #take out blanks



ui <- 
  
  
  
  fluidPage(
    
  # Give the page a title
  titlePanel("Modality: Click on the colored square box in the legend to create plot of incidence for selected modality. Scroll down to see plot."),
    
  fluidRow(
    column(width = 4,
           plotOutput("plot", width = "250%", height = "600px",
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = clickOpts(id = "plot_click"),
                      brush = brushOpts(
                        id = "plot_brush"
                      )
           )
    )
  ),
  
  
    
  fluidRow(  
    
    # sidebarLayout(
    
    #  sidebarPanel(
    #column(width = 6,
    #    h4("Points near click"),
    # verbatimTextOutput("click_info"),
    #)
    #)
    # uiOutput("first"),
    
    mainPanel(
      plotOutput("plot1", width = "150%", height = "400px",
                 # Equivalent to: click = clickOpts(id = "plot_click")
                 click = clickOpts(id = "plot1_click")#,
                 #brush = brushOpts(
                 # id = "plot_brush"
      )
    )
  ),
  
  fluidRow(  
    
    # sidebarLayout(
    
    #  sidebarPanel(
    #column(width = 6,
    #    h4("Points near click"),
    # verbatimTextOutput("click_info"),
    #)
    #)
    # uiOutput("first"),
    
    mainPanel(
      plotOutput("plot2", width = "150%", height = "400px"#,
                 # Equivalent to: click = clickOpts(id = "plot_click")
                 #click = clickOpts(id = "plot1_click")#,
                 #brush = brushOpts(
                 # id = "plot_brush"
      )
    )
  )#,
  
  
 # mainPanel(
  #  plotOutput("phonePlot")
  #  )),
    
    
 # fluidRow(
   # column(width = 6,
         # h4("Points near click"),
          # verbatimTextOutput("click_info"),
           
           #column(width = 6,
              #    h4("Brushed points"),
             #     verbatimTextOutput("brush_info")
           #)
    #)
  #)
)


server <- function(input, output) {
  output$plot <- renderPlot({
    proportions <- piecount$freq/sum(piecount$freq)
    percentages <- round(proportions*100)
  
    
    pie(piecount$freq,
        labels = paste0(piecount$x, '(', percentages, "%)") ,
        main=paste0('Modality of Compounds in Database, (n=', length(unique(incidence$COMPOUND_NUMBER)), ')'), col = rainbow(length(piecount$freq)))
   
    legend("topright", c("Biologic", "Drug Conjugate", "Fusion Protein", "Monoclonal or Fragment Antibody", "Nanobody", "Other Protein Therapeutic", "Peptide", "Protein"), cex = 0.8, fill = rainbow(8))
    
  })
     
      output$plot1 <- renderPlot({
        
        if (is.null(input$plot_click)) 
        
            return("")
        
        else if ((input$plot_click$y < .89) & (input$plot_click$y > .85) & (input$plot_click$x < .63) & (input$plot_click$x > .60))
        
          {output$plot1 <- renderPlot({
          
          incidence <- incidence[which(incidence$COMPOUND_MODALITY == "Monoclonal or Fragment Antibody"), ] #Select modality
          
          output$first <- renderUI({
            
            
            selectInput(inputId = "ASSAY_TYPE", #defined what compound was by this and choices
                        label = "Assay Type:",
                        choices = incidence$ASSAY_TYPE, 
                        selected = NULL)
            
          })
          
          
          ggplot(incidence, aes(COMPOUND_NUMBER, NUMERIC_RESULT)) + geom_point()+
            theme_minimal() + labs(title = incidence$COMPOUND_MODALITY) + 
            ylab("Incidence Percent") +
            xlab("Compound Number")
          
        })
        
        }
        
      
        
        else if((input$plot_click$y < .66) & (input$plot_click$y > .63) & (input$plot_click$x < .63) & (input$plot_click$x > .59))
        
        {output$plot1 <- renderPlot({
          
          incidence <- incidence[which(incidence$COMPOUND_MODALITY == "Protein"), ] #Select modality
          
          output$first <- renderUI({
            
            
            selectInput(inputId = "ASSAY_TYPE", #defined what compound was by this and choices
                        label = "Assay Type:",
                        choices = incidence$ASSAY_TYPE, 
                        selected = NULL)
            
          })
          
          
          ggplot(incidence, aes(COMPOUND_NUMBER, NUMERIC_RESULT)) + geom_point()+
            theme_minimal() + labs(title = incidence$COMPOUND_MODALITY) + 
            ylab("Incidence Percent") +
            xlab("Compound Number")
          
        
          
        })
        
        }
        
        
        
        else if((input$plot_click$y < .66) & (input$plot_click$y > .63) & (input$plot_click$x < .63) & (input$plot_click$x > .59))
          
        {output$plot1 <- renderPlot({
          
          incidence <- incidence[which(incidence$COMPOUND_MODALITY == "Biologic"), ] #Select modality
          
          output$first <- renderUI({
            
            
            selectInput(inputId = "ASSAY_TYPE", #defined what compound was by this and choices
                        label = "Assay Type:",
                        choices = incidence$ASSAY_TYPE, 
                        selected = NULL)
            
          })
          
          
          ggplot(incidence, aes(COMPOUND_NUMBER, NUMERIC_RESULT)) + geom_point()+
            theme_minimal() + labs(title = incidence$COMPOUND_MODALITY) + 
            ylab("Incidence Percent") +
            xlab("Compound Number")
          
          
          
        })
        
        }
        
        
        else if((input$plot_click$y < .932) & (input$plot_click$y > .905) & (input$plot_click$x < .63) & (input$plot_click$x > .59))
          
        {output$plot1 <- renderPlot({
          
          incidence <- incidence[which(incidence$COMPOUND_MODALITY == "Fusion Protein"), ] #Select modality
          
          output$first <- renderUI({
            
            
            selectInput(inputId = "ASSAY_TYPE", #defined what compound was by this and choices
                        label = "Assay Type:",
                        choices = incidence$ASSAY_TYPE, 
                        selected = NULL)
            
          })
          
          
          ggplot(incidence, aes(COMPOUND_NUMBER, NUMERIC_RESULT)) + geom_point()+
            theme_minimal() + labs(title = incidence$COMPOUND_MODALITY) + 
            ylab("Incidence Percent") +
            xlab("Compound Number")
          
          
          
        })
        
        }
        
       
        else if((input$plot_click$y < .979) & (input$plot_click$y > .951) & (input$plot_click$x < .63) & (input$plot_click$x > .59))
          
        {output$plot1 <- renderPlot({
          
          incidence <- incidence[which(incidence$COMPOUND_MODALITY == "Drug Conjugate"), ] #Select modality
          
          output$first <- renderUI({
            
            
            selectInput(inputId = "ASSAY_TYPE", #defined what compound was by this and choices
                        label = "Assay Type:",
                        choices = incidence$ASSAY_TYPE, 
                        selected = NULL)
            
          })
          
          
          ggplot(incidence, aes(COMPOUND_NUMBER, NUMERIC_RESULT)) + geom_point()+
            theme_minimal() + labs(title = incidence$COMPOUND_MODALITY) + 
            ylab("Incidence Percent") +
            xlab("Compound Number")
          
          
          
        })
        
        }
        
        
        else if((input$plot_click$y < 1.03) & (input$plot_click$y > 1.01) & (input$plot_click$x < .63) & (input$plot_click$x > .59))
          
        {output$plot1 <- renderPlot({
          
          incidence <- incidence[which(incidence$COMPOUND_MODALITY == "Biologic"), ] #Select modality
          
          output$first <- renderUI({
            
            
            selectInput(inputId = "ASSAY_TYPE", #defined what compound was by this and choices
                        label = "Assay Type:",
                        choices = incidence$ASSAY_TYPE, 
                        selected = NULL)
            
          })
          
          
          ggplot(incidence, aes(COMPOUND_NUMBER, NUMERIC_RESULT)) + geom_point()+
            theme_minimal() + labs(title = incidence$COMPOUND_MODALITY) + 
            ylab("Incidence Percent") +
            xlab("Compound Number")
          
          
          
        })
        
        }
        
        
        else if((input$plot_click$y < .826) & (input$plot_click$y > .794) & (input$plot_click$x < .63) & (input$plot_click$x > .59))
          
        {output$plot1 <- renderPlot({
          
          incidence <- incidence[which(incidence$COMPOUND_MODALITY == "Nanobody"), ] #Select modality
          
          output$first <- renderUI({
            
            
            selectInput(inputId = "ASSAY_TYPE", #defined what compound was by this and choices
                        label = "Assay Type:",
                        choices = incidence$ASSAY_TYPE, 
                        selected = NULL)
            
          })
          
          
          ggplot(incidence, aes(COMPOUND_NUMBER, NUMERIC_RESULT)) + geom_point()+
            theme_minimal() + labs(title = incidence$COMPOUND_MODALITY) + 
            ylab("Incidence Percent") +
            xlab("Compound Number")
          
          
          
        })
        
        }
        
        
        else if((input$plot_click$y < .761) & (input$plot_click$y > .738) & (input$plot_click$x < .63) & (input$plot_click$x > .59))
          
        {output$plot1 <- renderPlot({
          
          incidence <- incidence[which(incidence$COMPOUND_MODALITY == "Other Protein Therapeutic"), ] #Select modality
          
          output$first <- renderUI({
            
            
            selectInput(inputId = "ASSAY_TYPE", #defined what compound was by this and choices
                        label = "Assay Type:",
                        choices = incidence$ASSAY_TYPE, 
                        selected = NULL)
            
          })
          
          
          ggplot(incidence, aes(COMPOUND_NUMBER, NUMERIC_RESULT)) + geom_point()+
            theme_minimal() + labs(title = incidence$COMPOUND_MODALITY) + 
            ylab("Incidence Percent") +
            xlab("Compound Number")
          
          
          
        })
        
        }
        
        
        
        else if((input$plot_click$y < .72) & (input$plot_click$y > .69) & (input$plot_click$x < .63) & (input$plot_click$x > .59))
          
        {output$plot1 <- renderPlot({
          
          incidence <- incidence[which(incidence$COMPOUND_MODALITY == "Peptide"), ] #Select modality
          
          output$first <- renderUI({
            
            
            selectInput(inputId = "ASSAY_TYPE", #defined what compound was by this and choices
                        label = "Assay Type:",
                        choices = incidence$ASSAY_TYPE, 
                        selected = NULL)
            
          })
          
          
          ggplot(incidence, aes(COMPOUND_NUMBER, NUMERIC_RESULT)) + geom_point()+
            theme_minimal() + labs(title = incidence$COMPOUND_MODALITY) + 
            ylab("Incidence Percent") +
            xlab("Compound Number")
          
          
          
        })
        
        }
        
        else{
          
          return("")
          }
      })
        
        
        
      output$plot2 <- renderPlot({
        
        incidence <- read.csv("iodb10.18.csv") #read in dataset
        incidence <- subset(incidence, incidence$SPECIES == "Human" | incidence$SPECIES == " ") #subset only human data
        incidence <- subset(incidence, incidence$RESULT_TYPE == "PERCENTAGE_POSITIVE")   #only taking incidence data  
        incidence <- incidence[which(incidence$NUMERIC_RESULT != ""),]   #take out numeric result blanks 
        write.csv(incidence,'incidence2.csv')#doing this because when you sub in R, it's not a vector so the code wont work but if you write it again, its a vector
        incidence <- read.csv("incidence2.csv") #read in dataset
        
        
        if (is.null(input$plot1_click$x)) return()
        
        
        else{
          
          keeprows <- round(input$plot1_click$x) == as.numeric(incidence$COMPOUND_NUMBER)
          head(incidence[keeprows, ], 10)
        }
      })
    
    
    
  
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
    
  })
  
  output$brush_info <- renderPrint({
    cat("input$plot_brush:\n")
    str(input$plot_brush)
  })
  
  
}

shinyApp(ui, server)



