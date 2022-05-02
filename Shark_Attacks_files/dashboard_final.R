# Load packages -----------------------------------------------------
##################### loading libraries ##############################
p_needed <- c('shiny','tidyverse','dplyr','leaflet','ggplot2','shinythemes','plotly',
              "shinyWidgets","mapview",'shinydashboard','hrbrthemes','RColorBrewer',"dashboardthemes")


packages <- rownames(installed.packages())

p_to_install <- p_needed[!(p_needed %in% packages)]


if(length(p_to_install) >0){
  install.packages(p_to_install)
}

library(shiny)
library(tidyverse)
library(dplyr)
library(leaflet)
library(ggplot2)
library(shinythemes)
library(plotly)
library(shinyWidgets)
#mapview is used to save leaflet map to png
library(mapview)

library(shinydashboard)
library(hrbrthemes)
library(RColorBrewer)
library(dashboardthemes)



########################################################################
df <- read_csv("data/sharks.csv")
names(df)[13] <- 'Fatal'

year_span <-c("All year",1968:2018)

countries <- c("All place","USA","BRAZIL","AUSTRALIA","ENGLAND","COSTA RICA","MALDIVES","SOUTH AFRICA",
               "THAILAND","BAHAMAS","ECUADOR","MALAYSIA","CUBA","SPAIN","JAPAN","EGYPT","ST HELENA, British overseas territory",
               "MEXICO","REUNION","UNITED KINGDOM","UNITED ARAB?EMIRATES","PHILIPPINES",
               "INDONESIA","NEW CALEDONIA","CHINA","COLUMBIA","NEW ZEALAND","FRENCH POLYNESIA",
               "FIJI","MOZAMBIQUE","MAURITIUS","KIRIBATI","ISRAEL","FRANCE","JAMAICA","NIGERIA",
               "TONGA","SCOTLAND","TAIWAN","DOMINICAN REPUBLIC","KENYA","PAPUA NEW GUINEA",
               "RUSSIA","SEYCHELLES","TURKS & CAICOS","SOUTH KOREA","MALTA","VIETNAM","MADAGASCAR",
               "PANAMA","GUAM","CROATIA","ST.?MARTIN","GRAND CAYMAN","VANUATU","URUGUAY",
               "VENEZUELA","INDIA","CANADA","OKINAWA","MARSHALL ISLANDS","HONG KONG",
               "CHILE","SOMALIA","EL SALVADOR","ITALY","PORTUGAL","SOUTH CHINA SEA","WESTERN SAMOA",
               "BRITISH ISLES","TURKEY","MICRONESIA","PALAU","GRENADA")

shark_species <- c("All species","Angel shark","Blacktip shark","Blue shark",
                   "Bronze whaler shark","Bull shark","Caribbean reef shark","Carpet shark",
                   "Copper shark","Dogfish shark","Dusky shark","Galapagos shark","Grey nurse shark",
                   "Grey reef shark", "Hammerhead shark", "Juvenile shark","Lemon shark","Mako shark",
                   "Oceanic whitetip shark","Porbeagle shark", "Raggedtooth shark","Reef shark",
                   "Sand shark", "Sevengill shark", "Spinner shark", "Thresher shark","Tiger shark",
                   "White shark","Wobbegong shark","Zambesi shark","Unknown")

activities <- c("All activity","Paddle and Boogie boarding","Standing", "Surfing" ,"Swimming",
                "Fishing", "Walking","Diving","Snorkeling","Kayaking", "Floating", "Playing in the water")

#########################################################################
ui <- dashboardPage(
  dashboardHeader(title = h2("Sharks Attacks Analysis")),
  dashboardSidebar(
      sidebarMenu(
        menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
        menuItem("Map Interaction", tabName = "map", icon = icon("th")),
        menuItem("Time", tabName = "time", icon = icon("th")),
        menuItem("Frequencies", tabName = "freq", icon = icon("th")),
        menuItem("Shark Species Info", tabName = "info", icon = icon("th"))
      )
  ),
  dashboardBody(
    tabItems(
      ####################### Introduction#######################################
      tabItem(tabName = "introduction",
                      fluidRow(
                        align = "center",
                        strong("Shark Attack Analysis"),
                        style = "font-size:70px;"
                      ),
                      fluidRow(
                        align = "center",
                        "By: Xiaomeng Zhou, Lenka Raslova, and Sean Oberer",
                        style = "font-size:40px;"
                      ),
                      hr(),
                      fluidRow(
                        HTML('<center><img src="shark-meme.jpeg"></center>')
                      )
              ),
      #########################################################################################
      tabItem(tabName = "map",

              sidebarLayout(
                sidebarPanel(
                  
                  # select year 
                  selectInput(inputId = "year", label = h4(strong("Year")),
                              choices = year_span,
                              selected = "All year"),
                  hr(),
                  
                  # select country/place
                  pickerInput(inputId = "country", label = h4(strong("Country/Place")),
                              choices = countries,selected = "All place",
                              options = list(`live-search`=TRUE)),
                  hr(),
                  
                  # select shark species 
                  pickerInput(inputId = "species", label = h4(strong("Select shark species")),
                              choices =  shark_species, selected = "All species",
                              options = list(`live-search`=TRUE)),
                   hr(),
                  
                  ## select activity
                  # selectInput(inputId = "activity", label = "Activity when attack happened",
                  #             choices = activities,
                  #             selected = "All Kinds")
                  pickerInput(inputId = "activity", label = h4(strong("Activity when attack happened")),
                              choices = activities,selected = "All activity",
                              options = list(`live-search`=TRUE)),
                  hr(),
                  #add action button to reshesh the app
                  actionButton(inputId = "Refresh", 
                               label = h3(strong("Refresh Map"))),
                  br(),
                  br(),
                  br(),
                  hr(),
                  #create a checkbox for whether or not show the table
                  checkboxInput(inputId = "showtable", label = h4(strong("Show Data Table")),
                                value = FALSE),
                  ## add button to download data 
                  actionButton(inputId = "download", 
                               label = strong("Download as csv"))
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  #leafletOutput(outputId ="map"),
                  uiOutput("map"),
                  fluidRow(valueBoxOutput("Total"), valueBoxOutput("Survival"), valueBoxOutput("Death")),
                  DT::dataTableOutput(outputId = "filteredtable"),
                )
              )
      ),
      #############################################################################################
      # tab for Frenquecies
      tabItem(tabName = "freq",
                  ## slider input for setting the max number of words in the word cloud
              fluidRow(
                box(sliderInput(inputId = "num_attacks", label = "Number of shark attacks",
                            min = 1, max = 1300, value = 10, step = 1)),
                box(sliderInput(inputId = "word_size", label = "Font Size",
                                min = 1, max = 30, value = 10, step = 1))
              ),
              fluidRow(
                box(title = h2(strong("Number of Attacks by Activities")), plotOutput(outputId = "Attack_by_activity")),
                box(title = h2(strong("Number of Attacks by Species")), plotOutput(outputId = "Attack_by_species")))
              
      ),
      
      ############################################################################################
      tabItem(tabName = "time",
              fluidRow(
                box(title = "Shark Attacks During The Time","This tab is giving you basic
                    information about shark attacks during a specific time period.", br(), br(),
                    "You can select a shark specie from more than 20 species.",br(), br(),
                    "The time period of shark attacks is 50 years. You can specify a desired range of years."),
               
                 box(title = "Specify Shark Specie and Year Span:",
                    pickerInput("species3", label = "Select a shark:",
                                choices = shark_species,
                                options = list(`live-search` = TRUE)),
                    
                    sliderInput("range", label = h5("Choose Range of Years:"), min = 1968, 
                                max = 2018,  value = c(1968, 2018),sep = " ")
                    
                )),
              
              fluidRow(
                box(title = "Attacks in Specific Year Span", plotOutput(outputId = "year")),
                box(title = "Attacks by Months", plotOutput(outputId = "month")))
      ),
      ############################################################################################
      tabItem(tabName = "info",
              sidebarLayout(
                sidebarPanel(
                  h2("Information about most common species that attack"),
                  ## seperate sections ---------
                  hr(),
                  ## add shark information
                  # White shark
                  br(),
                  # White shark & Tiger shark
                  fluidRow(
                    column(6, p(a(h4("White Shark"), href = "https://en.wikipedia.org/wiki/Great_white_shark"))),
                    column(6, p(a(h4("Tiger Shark"), href = "https://en.wikipedia.org/wiki/Tiger_shark")))
                  ),
                  fluidRow(
                    column(5,img(src='White shark.png',alight = "left", height = 100, width = 200)),
                    column(5,img(src='Tiger shark.png', alight = "left",height = 100, width = 200))
                  ),
                  
                  # Bull shark & Blacktip shark
                  fluidRow(hr()),
                  fluidRow(
                    column(6, p(a(h4("Bull Shark"), href = "https://en.wikipedia.org/wiki/Bull_shark"))),
                    column(6, p(a(h4("Blacktip shark"), href = "https://en.wikipedia.org/wiki/Blacktip_shark")))
                  ),
                  fluidRow(
                    column(5,img(src='Bull shark.jpg', alight = "left",height = 100, width = 200)),
                    column(5,img(src='Blacktip shark.png', alight = "left",height = 100, width = 200))
                  ),
                  
                  #Bronze Whaler shark & Raggedtooth shark
                  fluidRow(hr()),
                  fluidRow(
                    column(6, p(a(h4("Bronze Whaler shark"), href = "https://en.wikipedia.org/wiki/Copper_shark"))),
                    column(6, p(a(h4("Raggedtooth shark"), href = "https://en.wikipedia.org/wiki/Sand_tiger_shark")))
                  ),
                  fluidRow(
                    column(5,img(src='Bronze Whaler shark.jpg',alight = "right", height = 100, width = 200)),
                    column(5, img(src='Raggedtooth shark.jpeg',alight = "right", height = 100, width = 200))
                  ),
                  #Grey nurse shark & Mako shark
                  fluidRow(hr()),
                  fluidRow(
                    column(6, p(a(h4("Grey nurse shark"), href = "https://en.wikipedia.org/wiki/Grey_nurse_shark_conservation"))),
                    column(6, p(a(h4("Mako shark"), href = "https://en.wikipedia.org/wiki/Shortfin_mako_shark")))
                  ),
                  fluidRow(
                    column(5,img(src='Grey nurse shark.jpg', alight = "right",height = 100, width = 200)),
                    column(5,img(src='Mako shark.png',alight = "right", height = 100, width = 200))
                  ),
                  
                ),
                
                
                mainPanel(
                  
                  hr(),

                  p(h2("Learn Quick Tips:"),a(h2("How to minimize the chance of being bitten by a shark"), 
                                              href = "https://www.floridamuseum.ufl.edu/shark-attacks/reduce-risk/quick-tips/")),
                  
                  tags$iframe(
                      src="https://www.youtube.com/embed/X-K4PRz-eJs",
                             # allowfullscreen = "allowfullscreen",
                              allowfullscreen = NA,
                              width = 750, height = 450)
                )
              )
          ) 
              
    )  #parencheses for TabItems()
  ) # parencheses for dashboardbody()
) #parencheses for ui()

#######################################################################################################
server <- function(input, output) {
 
 ############################## Tab"Map Interaction"########################################
   ###################For Input part ###########################
  conditional <- function(condition, success) {
    if (condition) success else TRUE
  }
  
  shark_subset <- reactive({
    df %>%
      filter(
        conditional(input$year != "All year", Year == input$year),
        conditional(input$country != "All place", Country == input$country),
        conditional(input$species != "All species", Species == input$species),
        conditional(input$activity != "All activity",Activity == input$activity)
      )
  })
  
  
  DataForMap <- eventReactive(
    #df,
    input$Refresh,
    {
      FullDataSign = (input$year == "All year")&
        (input$country == "All place")&
        (input$species == "All species")&
        (input$activity == "All activity")


      if (FullDataSign)
      {
        df
        
      }
      else
      {
        shark_subset()
      }

    }
  )
  
  ####################output #############################
  
  output$map <- renderUI({
    leafletOutput("default_plot")
  })
  
  output$default_plot <- renderLeaflet({
    
    pal <- colorFactor(palette = c("blue", "red"),domain = c("N", "Y"))
    # Fatal = c("Y","N")
    leaflet(df) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        #radius = ~ifelse(Type == "Provoked", 10, 5),
        color = ~pal(Fatal),
        label = ~Species,
        popup = ~paste("Activity: ", df$Activity,", Gender: ", df$Sex,", Age: ", df$Age),
        #~Activity,
        radius = c(6),
        #color = ifelse(Fatal =="Y","red","blue"),
        stroke = FALSE, fillOpacity = 0.5
      )
  })
  
  ### Click on Refresh map button
  observeEvent(input$Refresh, {
    output$map <- renderUI({
      leafletOutput("dynamic_plot")
    })
    
  output$dynamic_plot <- renderLeaflet({
    
    if(nrow(DataForMap()!=0 )){
    
    pal <- colorFactor(palette = c("blue", "red"),domain = c("N", "Y"))
    # Fatal = c("Y","N")
    leaflet(DataForMap()) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        #radius = ~ifelse(Type == "Provoked", 10, 5),
        color = ~pal(Fatal),
        label = ~Species,
        popup = ~paste("Activity: ", df$Activity,", Gender: ", df$Sex,", Age: ", df$Age),
        radius = c(6),
        #color = ifelse(Fatal =="Y","red","blue"),
        stroke = FALSE, fillOpacity = 0.5
      )
    }
    else{
      leaflet() %>% addTiles()
    }
  })
  
})
  
  
  output$Total <- renderValueBox({
    valueBox(
      paste0(count(shark_subset()[, 'Species'])), "# of Attacks", icon = icon("globe", lib = "glyphicon"),
      color = "aqua"
    )
  })
  
  output$Survival <- renderValueBox({
    
    b <- count(shark_subset()[, c('Species', "Fatal")] %>%
                 filter(Fatal == "N") %>%
                 select(Species))
    
    valueBox(
      paste0(round((b/count(shark_subset()[, 'Species']))*100,0), " %"), "Survival Rate", icon = icon("heart", lib = "glyphicon"),
      color = "lime"
    )
  })
  
  output$Death <- renderValueBox({
    a <- count(shark_subset()[, c('Species', "Fatal")] %>%
                 filter(Fatal == "Y") %>%
                 select(Species))
    
    valueBox(
      paste0(a), "# of Fatal Attacks", icon = icon("user", lib = "glyphicon"),
      color = "maroon"
    )
  })
  
  
  output$filteredtable <- DT::renderDataTable({
    
    if(input$showtable)
      
      DT::datatable(shark_subset(), rownames = FALSE)
    
  })
  
  ## download data as csv when the button is clicked
  observeEvent(
    input$download, {
      file_path <- glue::glue("data/Shark_attack_{input$year}_{input$country}_{input$species}_{input$activity}.csv")
      write_csv(shark_subset(), file_path)
      ## thinking to add another botton: download full dataset,
      ## another one: to download filter data based on the user selection, 
      ##dataset should be update according
    })
  
  ####################################For Tab "Frequencies"#############################################
  ## For Tab"Frenquencies"
  output$Attack_by_species <- renderPlot({
    
    barchart <- df %>%
      #filter(Species !="Unknown") %>%
      group_by(Species) %>%
      mutate(count = n()) %>%
      filter(count > input$num_attacks) %>%
      ggplot(aes(fill = Fatal,x = reorder(Species,count)))+
      geom_bar(stat="count")+
      labs(x = "", y = "count")+
      scale_fill_manual(values = c("blue","red" ))+
      coord_flip()+
      theme(text = element_text(size = input$word_size))
    
    barchart 
  })
  
  output$Attack_by_activity <- renderPlot({
    
    barchart <- df %>%
      #filter(Species !="Unknown") %>%
      group_by(Activity) %>%
      mutate(count = n()) %>%
      filter(count > input$num_attacks) %>%
      ggplot(aes(fill = Fatal,x = reorder(Activity,count)))+
      geom_bar(stat="count")+
      labs(x = "", y = "Number of Attacks")+
      scale_fill_manual(values = c("blue","red" ))+
      coord_flip() +
      theme(text = element_text(size = input$word_size))
    
    barchart 
  })
  
  ##########################################For Tab "Time"################################################
    #################input part############
  DataForTime <-  reactive({
    df %>%
      filter(
            conditional(input$species3 != "All species", Species == input$species3),
            (Year >= input$range[1] & Year <= input$range[2])) 
  
  })
  
  #################  output part###########
  output$year <- renderPlot({
    
     lineforyear <- DataForTime() %>%   
         group_by(Year) %>%
         mutate(count = n()) %>% 
         ggplot() + 
         aes(x=Year, y=count)+
         geom_line(color="black")+
         geom_point(shape=21, color="black", fill="#69b3a2", size=3)+
         labs(x = "Years", y = "# of Attacks")
         #+theme_ipsum()
    
     lineforyear
  })
  
  output$month <- renderPlot({
    
    barformonth <- DataForTime() %>%
      group_by(Month) %>%
      summarise(count2=n()) %>%
      mutate(Month = recode(Month, Sep = 'September', Jan = "January",  Feb ="February",Mar = "March", Apr = "April", Jun = "June", Jul= "July", Aug = "August", Oct = "October", Nov = "November", Dec = "December"))%>%
      ggplot(aes(x = reorder(Month,count2),count2,fill = count2))+
      geom_bar(stat="identity")+
      labs(x = "", y = "Number of Attacks")+
      scale_fill_distiller("Number of Attacks",palette = "Spectral")+
      coord_flip()
    
    barformonth 
  })
  
  ################################For Tab "Shark Species info"##########################
  

  
  
  
  ######################################################################################################
  
} # curly sign for server()

shinyApp(ui, server)
