library(shiny)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
library(ggrepel)


setwd("~/Rprace/university rank/Shiny")
cwurData <- read.csv("cwurData.csv")
educationExpenditure <- read.csv("education_expenditure_supplementary_data.csv")
educationalAttainment <- read.csv("educational_attainment_supplementary_data.csv")
schoolCountry <- read.csv("school_and_country_table.csv")
shanghaiData <- read.csv("shanghaiData.csv")
timesData <- read.csv("timesData.csv")
all <- read.csv("all.csv")
all <- select(all, 1, 3)
all$name <- plyr::revalue(all$name, c("United States of America"="USA",
                                       "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                                       "Czechia"= "Czech Republic",
                                       "Slovakia"="Slovak Republic",
                                       "Taiwan, Province of China" = "Taiwan",
                                       "Russian Federation" = "Russia",
                                      "Korea (Republic of)" = "South Korea",
                                      "Iran (Islamic Republic of)" = "Iran"))
                                  
                                        
                                  

cwurData <- select(cwurData,-11)
cwurData <- left_join(cwurData, all, by=c("country" = "name"))
cwurData$alpha.3 <- as.character(cwurData$alpha.3)  
cwurData <- filter(cwurData, country != "Hong Kong")
cwurData <- filter(cwurData, country != "Singapore")

WorldCountry <- rgdal::readOGR("countries.geo.json")


ui <- tagList(
  navbarPage(
    title = "3rd shiny app", theme=shinytheme("cerulean"),
    tabPanel("CWUR Rank",
             sidebarPanel(
               selectInput("rok",
                           label = "Choose a year",
                           choices = unique(as.character(cwurData$year))),
               radioButtons("paleta",
                            label = "Choose your colors",
                            choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3)),
               sliderInput("ilosc",
                           label = "Number of schools", min=3, max=10, value=3)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Rank of universities",
                          plotOutput("wykres1")),
                 tabPanel("Rank of countries",
                          plotOutput("wykres2"))
               )
             )),
    tabPanel("Correlations",
             sidebarPanel(
               selectInput("zmienna1",
                           label="First variable",
                           choices=colnames(cwurData)[5:11]),
               selectInput("zmienna2",
                           label="Second variable",
                           choices=colnames(cwurData)[5:11]),
               selectInput("szkoly",
                           label="Choose an Institution ",
                           choices = unique(cwurData$institution),
                           multiple = TRUE,
                           selectize = TRUE),
               selectInput("rok2",
                           label = "Choose a year",
                           choices = unique(as.character(cwurData$year))),
               checkboxInput("smooth",
                             label="Add regression line",
                             value=FALSE),
               checkboxInput("se",
                             label="Display confidence interval",
                             value=FALSE),
               selectInput("kolor",
                           label="Choose a color of the span",
                           choices = c("blue","green","yellow",
                                       "purple","brown")),
               sliderInput("slider2",
                           label = "Trimm the plot (x)",
                           min = 0, max = 110, value = c(0,110))
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("correlation",
                          plotOutput("wykres3"))
               )
             )),
    tabPanel("Map",
             sidebarPanel(
               selectInput("rokii",
                           label="Choose a year",
                           choices = unique(as.character(cwurData$year)))
               
             ),
             mainPanel("Leaflet",
                       leafletOutput("wykres4")))
)
)


server <- function(input, output) {
  
  output$wykres1 <- renderPlot({
    
    c <- cwurData %>% group_by(year) %>% select(institution, year, world_rank) %>% filter(world_rank %in% c(1:10))
    
    kolory1 <- c(rep("lightgreen",7), "#CD7F32", "grey", "gold")
    kolory2 <- c(rep("khaki",7), "brown", "yellow", "gold")
    kolory3 <- c(rep("salmon",7), "pink", "blue", "purple")
    names(kolory1) <- c("10","9","8","7","6","5","4","3","2","1")
    names(kolory2) <- c("10","9","8","7","6","5","4","3","2","1")
    names(kolory3) <- c("10","9","8","7","6","5","4","3","2","1")

    
    if (input$paleta == 1) {
      c %>% filter(year == input$rok) %>% 
        filter(world_rank %in% c(1:input$ilosc)) %>%
        ggplot(aes(x=reorder(institution,-world_rank), y=world_rank)) +
        geom_col(color="black",aes(fill=factor(world_rank))) +
        scale_fill_manual(aes(name=world_rank), values=kolory1) +
        coord_flip()  +
        guides(fill=FALSE) +
        ggtitle(paste("Ranking szkol na rok", input$rok)) +
        xlab("Uniwersytet") +
        ylab("Pozycja w rankingu") + 
        scale_y_continuous(breaks = seq(1,10,1)) +
        theme(plot.title = element_text(hjust=0.5))
    } else if (input$paleta == 2) {
      
      c %>% filter(year == input$rok) %>% 
        filter(world_rank %in% c(1:input$ilosc)) %>%
        ggplot(aes(x=reorder(institution,-world_rank), y=world_rank)) +
        geom_col(color="black",aes(fill=factor(world_rank))) +
        scale_fill_manual(aes(name=world_rank), values=kolory2) +
        coord_flip()  +
        guides(fill=FALSE) +
        ggtitle(paste("Ranking szkol na rok", input$rok)) +
        xlab("Uniwersytet") +
        ylab("Pozycja w rankingu") + 
        scale_y_continuous(breaks = seq(1,10,1)) +
        theme(plot.title = element_text(hjust=0.5))
    } else if (input$paleta ==3) {
      
      c %>% filter(year == input$rok) %>% 
        filter(world_rank %in% c(1:input$ilosc)) %>%
        ggplot(aes(x=reorder(institution,-world_rank), y=world_rank)) +
        geom_col(color="black",aes(fill=factor(world_rank))) +
        scale_fill_manual(aes(name=world_rank), values=kolory3) +
        coord_flip()  +
        guides(fill=FALSE) +
        ggtitle(paste("Ranking szkol na rok", input$rok)) +
        xlab("Uniwersytet") +
        ylab("Pozycja w rankingu") + 
        scale_y_continuous(breaks = seq(1,10,1)) +
        theme(plot.title = element_text(hjust=0.5))
    }
    
    
  })
  
  output$wykres2 <- renderPlot({
    
    kolory1 <- c(rep("lightgreen",7), "#CD7F32", "grey", "gold")
    kolory2 <- c(rep("khaki",7), "brown", "yellow", "gold")
    kolory3 <- c(rep("salmon",7), "pink", "blue", "purple")
    names(kolory1) <- c("10","9","8","7","6","5","4","3","2","1")
    names(kolory2) <- c("10","9","8","7","6","5","4","3","2","1")
    names(kolory3) <- c("10","9","8","7","6","5","4","3","2","1")
    
    index <- c(1:10)
    ddd <- cwurData %>% group_by(country,year) %>% 
      filter(year == input$rok) %>%
      summarise(suma = n()) %>% arrange(desc(suma)) 
    ddd <- ddd[1:10,]
    
    ddd$index <- c(1:10)
    ddd <- ddd %>% filter(index %in% c(1:input$ilosc))
    
    if (input$paleta == 1) {
        ggplot(data= ddd, aes(x=reorder(country,-index), y=suma)) +
        geom_col(color="black",aes(fill=factor(index))) +
        scale_fill_manual(aes(name=index), values=kolory1) + 
        guides(fill=FALSE) +
        coord_flip() +
        scale_y_continuous(breaks=seq(0,250,10)) +
        theme_bw() +
        ggtitle("Ranking top 10 krajow") +
        xlab("Kraj") + ylab("Liczba uniwersytetow")+
        theme(plot.title = element_text(hjust=0.5))
    } else if (input$paleta == 2) {
      
      ggplot(data= ddd, aes(x=reorder(country,-index), y=suma)) +
        geom_col(color="black",aes(fill=factor(index))) +
        scale_fill_manual(aes(name=index), values=kolory2) + 
        guides(fill=FALSE) +
        coord_flip() +
        scale_y_continuous(breaks=seq(0,250,10)) +
        theme_bw() +
        ggtitle("Ranking top 10 krajow") +
        xlab("Kraj") + ylab("Liczba uniwersytetow")+
        theme(plot.title = element_text(hjust=0.5))
    } else if (input$paleta ==3) {
      
      ggplot(data= ddd, aes(x=reorder(country,-index), y=suma)) +
        geom_col(color="black",aes(fill=factor(index))) +
        scale_fill_manual(aes(name=index), values=kolory3) + 
        guides(fill=FALSE) +
        coord_flip() +
        scale_y_continuous(breaks=seq(0,250,10)) +
        theme_bw() +
        ggtitle("Ranking top 10 krajow") +
        xlab("Kraj") + ylab("Liczba uniwersytetow")+
        theme(plot.title = element_text(hjust=0.5))
    }
    
  })
  
  output$wykres3 <- renderPlot({
    
    g <- cwurData %>% filter(year == input$rok2) 
    
    wektor_szkol <- input$szkoly
      
    ggplot(data=g,aes_string(x=input$zmienna1, y=input$zmienna2)) +
      geom_point(alpha=0.8, size=ifelse(g$institution %in% wektor_szkol, 4, 1.5),
                 color=ifelse(g$institution %in% wektor_szkol,brewer.pal(9,"Set1"),"black")) +
      geom_text_repel(data=subset(g, institution %in% wektor_szkol),aes(label=institution), size = 3,
                      segment.size = 0.2) + 
      scale_x_continuous(limits = input$slider2) + 
      theme(axis.text.x= element_text(angle=45, hjust=1)) %>%
      {if (input$smooth) (geom_smooth(se = ifelse(input$se, T, F),
                                      method="loess",color=input$kolor,span=2)) else NULL}

  })
  
  output$wykres4 <- renderLeaflet({
    
    gg <- cwurData %>% group_by(year,alpha.3) %>% summarise(suma = n()) %>%
      arrange(alpha.3)
    
    w <- filter(gg,year==input$rokii)
    
    data_Map <- WorldCountry[WorldCountry$id %in% gg$alpha.3, ]
    data_Map1 <- WorldCountry[WorldCountry$id %in% w$alpha.3, ]
    
    Map <- leaflet(data_Map) %>% addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') 
    pal <- colorBin("YlOrRd", domain = w$suma,bins=20)
    Map %>% addPolygons(data=data_Map1, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 2,
                        fillColor = ~pal(w$suma),
                        label = ~paste0(w$alpha.3, " : ", w$suma),
                        highlight = highlightOptions(
                          weight = 5,
                          color = "#666",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE),
                        labelOptions = labelOptions(
                          style = list("font-weight" = "normal", padding = "3px 8px"),
                          textsize = "15px",
                          direction = "auto")) 
    
  })
    
}

shinyApp(ui = ui, server = server)

