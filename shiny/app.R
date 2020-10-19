library("shiny")
library("breastrisk")

## Risk calc
fn.calcdr <- function(age, bmi, density, risk){
    drs<-1.0
    
#    temp<-.C("denresid", as.integer(age), as.double(bmi), as.double(density), as.double(drs))
 #   mydenr <- temp[[4]]

  #  myrevised10y <- (1-exp(log(1-risk/100)*mydenr)) *100

    mybrm <- brm(age, bmi, density, risk)
    
    myout <- paste0("Revised 10y risk: ", round(mybrm@adjrisk,1), "% \n [DRS: ", round(mybrm@denresid,2), "]")
    
    return(myout)
    }


ui <- pageWithSidebar(

  # App title ----
  headerPanel("Density risk modification calculator"),

  # Sidebar panel for inputs ----
  sidebarPanel(


          numericInput("inage", 
                        h3("Age"), 
                        value = 50),

      
          numericInput("inbmi", 
                        h3("BMI"), 
                        value = 25),

#          numericInput("indensity", 
#                        h3("BI-RADS density"), 
#                        value = 2),

	  radioButtons("indensity", h3("BI-RADS density"),
                        choices = list("a. Fatty" = 1, "b. Scattered" = 2,
                                       "c. Hetero" = 3, "d. Dense" = 4),selected = 3),
      
          numericInput("tenyrisk", 
                        h3("10y risk (%)"), 
                        value = 3.1)


  ),

  # Main panel for displaying outputs ----
  mainPanel(
      
      h3(textOutput("drs"))

  )
)
# Define server logic to plot various variables against mpg ----
server <- function(input, output) {



    output$drs <- renderText({
	validate(
		need(input$inage >=40 & input$inage <=75, "Error - Age must be between 40y and 75y"),
		need(input$inbmi >=15 & input$inbmi < 100, "Error- Check BMI (15-99)"),
		need(input$tenyrisk>0 & input$tenyrisk <100, "Error - 10y risk must be a percentage (0-100)"))

        fn.calcdr(input$inage,input$inbmi,input$indensity, input$tenyrisk)
  })
}

shinyApp(ui, server)
