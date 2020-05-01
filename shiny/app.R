#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


ui <- fluidPage(

    # Application title
    titlePanel("Two sample t-test with P-value"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            p(strong("Sample A:")),

            sliderInput("na", "Sample size",
                        min =1, max = 500, 50),

            numericInput("meana", "Mean", 50),

            numericInput("sda", "Standard Diviation", 10),

            p(br()),


            p(strong("Sample B:")),

            sliderInput("nb", "Sample size",
                        min =1, max = 500, 50),

            numericInput("meanb", "Mean", 50),

            numericInput("sdb", "Standard Diviation", 10),

            p(br()),

            strong('Option:'),

            checkboxInput("paired", "The two samples are dependent/paired", TRUE)
        ),

        # Show a plot of the generated t-distribution
        mainPanel(

            h3("Two sample t-test"),
            verbatimTextOutput("ttest.out"),

        )
    )
)

server <- function(input, output) {

    ttest <- reactive({
        na <- input$na
        meana <- input$meana
        sda <- input$sda
        nb <- input$nb
        meanb <- input$meanb
        sdb <- input$sdb

        if (input$paired) {
            df1 <- na+nb-2
            v1 <- ((na-1)*sda^2+(nb-1)*sdb^2)/df1
            tstat1 <- round((meana-meanb)/sqrt(v1*(1/na+1/nb)),3)
            diff <- round((meana - meanb), 3)
            P1 <- 2 * pt(-abs(tstat1), df1)

            cat("Dependent t-test (variances are equal)", "\n",
                "H0: The two samples have same means", "\n",
                " t-value =", tstat1, ",", "degree of freedom =", df1, ",", "p-value =", P1, "\n")

        } else {
            stderrx <- sqrt(sda^2/na)
            stderry <- sqrt(sdb^2/nb)
            stderr <- sqrt(stderrx^2 + stderry^2)
            df2 <- round(stderr^4/(stderrx^4/(na - 1) + stderry^4/(nb - 1)),3)
            tstat2 <- round((meana - meanb)/stderr,3)
            P2 <- 2 * pt(-abs(tstat2), df2)

            cat("Independent t-test (unequal variances)", "\n",
                "H0: The two samples have same means", "\n",
                " t-value =", tstat2, ",", "degree of freedom =", df2, ",", "p-value =", P2, "\n")
        }
    })


    output$ttest.out <- renderPrint({
        ttest()
    })

}


# Run the application
shinyApp(ui = ui, server = server)
