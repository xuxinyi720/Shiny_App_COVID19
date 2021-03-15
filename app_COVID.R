#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#install.packages('tidytuesdayR')
library(shiny)
library(tidyverse)
library(tidytuesdayR)


### Get data ###
COVID_data<- 
    opendatatoronto::search_packages("COVID-19 Cases in Toronto")%>%
    opendatatoronto::list_package_resources()%>%
    filter(name=="COVID19 cases")%>%
    select(id)%>%
    opendatatoronto::get_resource()
### Split date ###
COVID_data<- 
    COVID_data %>%
    dplyr::mutate(year = lubridate::year(COVID_data$`Reported Date`), 
                  month = lubridate::month(COVID_data$`Reported Date`), 
                  day = lubridate::day(COVID_data$`Reported Date`))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Case in Toronto,by December 2020"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("ms",
                        "Months:",
                        min = 1,
                        max = 12,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
     

        # draw the line
        data_2020%>%
            filter(data_2020$month > input$ms)%>%
            ggplot( aes(x=month, y=Total))+
            geom_line( 
                color="red")+
            labs(x = "Months",
                 y = "Number of Cases")+
            theme(
                  axis.text.x=element_text(margin = margin(1, unit = "cm"),siz=12,hjust = 0.5, vjust =1),
                  title =element_text(size=16, face='bold'))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
