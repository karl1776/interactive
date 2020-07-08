library(shiny) 
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr) 

Surveillance <- read.csv("/data/interactive.csv", as.is=T)
Surveillance <- data.frame( 
    Date = seq(as.Date("2017/1/1"), as.Date("2017/12/31"), "days"),
    zoom = as.numeric(sample(1:100,365,replace=T)), chat                 =as.numeric(sample(1:100,365,replace=T)))

Surveillance <- Surveillance %>% mutate(
    Week = format(Date, "%Y-%m-%U"))

ui <- fluidPage(
    dateRangeInput("daterange", "Choose the date",
                   start = min(Surveillance$Date),
                   end = max(Surveillance$Date),
                   min = min(Surveillance$Date),
                   max = max(Surveillance$Date),
                   separator = " - ", format = "dd/mm/yy",        
                   startview = 'Week', language = 'en', weekstart = 1),  
    
    selectInput(inputId = 'Diseases',
                label='Transaction Type',
                choices=c('zoom','chat'),
                selected='zoom'),
    
    plotOutput("barplot"))


server <- function(input, output) {
    
    dateRangeInput<-reactive({
        dataset <- subset(Surveillance, Date >= input$daterange[1] & Date <=         input$daterange[2])
        dataset = switch(input$Diseases,
                         "zoom" =         Surveillance[,setdiff(colnames(Surveillance),'chat')],
                         "chat" =         Surveillance[,setdiff(colnames(Surveillance),'zoom')])  
        print(head(dataset))
        return(dataset)
    })
    
    output$barplot <-renderPlot({
        
        if(is.null(dateRangeInput())) return()
        
        ggplot(data=dateRangeInput(), aes_string(x="Week",y=input$Diseases))          + 
            stat_summary(fun = sum, geom =         "bar",colour="red",fill="red") +
            geom_bar(stat="identity") + 
            labs(title=input$Diseases, y ="Number") +
            theme_classic() + 
            theme(plot.title = element_text(hjust = .5))        
    })        
    
}
shinyApp (ui = ui, server = server)

