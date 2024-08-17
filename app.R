library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tigris)
library(rsconnect)# Load your GeoJSON data
survey_data <- st_read("www/Entrance_to_Faregate.geojson")# Ensure data is transformed and clean
survey_data <- st_transform(survey_data, crs =4326)
survey_data$ADA_Fare_A[is.na(survey_data$ADA_Fare_A)]<-"No"
survey_data$Wide_Aisle[is.na(survey_data$Wide_Aisle)]<-"No"
survey_data$Wide_Aisle[survey_data$Wide_Aisle =="<Null>"]<-"No"
survey_data$Wide_Aisle[survey_data$Wide_Aisle =="1"]<-"Yes"
survey_data <- survey_data %>% filter(!is.na(Borough))# Generate line symbols for the popups
line_symbols <- sapply(strsplit(survey_data$Line_s_,","),function(lines){
  paste0(sapply(lines,function(line){
    color <-switch(trimws(line),"1"="red","2"="red","3"="red","B"="orange","D"="orange","F"="orange","M"="orange","N"="yellow","Q"="yellow","R"="yellow","J"="brown","Z"="brown","L"="grey","G"="lightgreen","7"="purple","A"="blue","C"="blue","E"="blue","default"="black")
    text_color <- ifelse(line %in%c("N","Q","R"),"black","white")
    paste0("<div style='display:inline-block; width:25px; height:25px; background-color:", color,"; color:", text_color,"; border-radius:50%; text-align:center; line-height:25px; font-weight:bold;'>", line,"</div>")}), collapse =" ")})# Add popups without the Learn More button
survey_data <- survey_data %>%
  mutate(
    Line_symbols = line_symbols,
    popup_info = paste0("<strong style='color:navy;'>Station Name: </strong>","<span style='color:navy; font-weight:bold;'>", Station_Na,"</span><br>","<strong style='color:navy;'>Line(s): </strong>", Line_symbols,"<br>","<strong style='color:navy;'>ADA Fare Access System: </strong>","<span style='color:navy; font-weight:bold;'>", ADA_Fare_A,"</span><br>","<strong style='color:navy;'>Wide-Aisle Fare Gate: </strong>","<span style='color:navy; font-weight:bold;'>", Wide_Aisle,"</span><br>","<strong style='color:navy;'>Total Walking Distance: </strong>","<span style='color:navy; font-weight:bold;'>", Flat_Walki," ft</span><br>","<strong style='color:navy;'>Tactile Guiding Strips: </strong>","<span style='color:navy; font-weight:bold;'>", Tactile_Gu,"</span><br>"))# Load NYC Borough boundaries and Census Tracts
boroughs <- tigris::counties(state ="NY", cb =TRUE)%>%
  filter(NAME %in%c("Bronx","Kings","New York","Queens","Richmond"))%>%
  st_transform(4326)

tracts <- tigris::tracts(state ="NY", cb =TRUE)%>%
  filter(COUNTYFP %in% boroughs$COUNTYFP)%>%
  st_transform(4326)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body, html { height: 100%; margin: 0; }
    .container-fluid { padding: 0; }
    .sidebar { padding-left: 0.5in; }
    .selectize-control.multi .selectize-input > div { background: #EEE; border-radius: 3px; padding: 3px 5px; margin: 2px 0; }
    .selectize-control.multi .selectize-input > div .remove { color: #CCC; cursor: pointer; }
    strong { color: navy; font-weight: bold; }
    .compact-legend .leaflet-control {
      width: 100px !important;
      font-size: 10px;
    }
    .counter-button {
      background-color: #0056b3;
      color: white;
      padding: 10px;
      border-radius: 5px;
      font-weight: bold;
      font-size: 16px;
      text-align: center;
      display: inline-block;
      margin: 5px;
    }
  "))),
  div(class="container-fluid text-center",
      div(class="counter-button", textOutput("total_entrances")),
      div(class="counter-button", textOutput("total_inaccessible")),
      div(class="counter-button", textOutput("total_lines"))),
  titlePanel(title = div("Click on the entrance points on the map to view details", style ="font-size: 16px; text-align: center;")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("walking","Total Walking Distance to Fare Gates (in ft):",min=min(survey_data$Flat_Walki, na.rm =TRUE),max=max(survey_data$Flat_Walki, na.rm =TRUE),
                  value =range(survey_data$Flat_Walki, na.rm =TRUE), step =50),
      sliderInput("steps","Total Number of Stair-steps till the Fare Gates (in ft):",min=min(survey_data$Step_Total, na.rm =TRUE),max=max(survey_data$Step_Total, na.rm =TRUE),
                  value =range(survey_data$Step_Total, na.rm =TRUE), step =5),
      selectInput("station_name","Select Station", choices =c("All", unique(survey_data$Station_Na))),
      selectInput("borough","Select Borough", choices =c("All", unique(survey_data$Borough))),
      selectInput("line","Select Line", choices =c("All", unique(unlist(strsplit(survey_data$Line_s_,","))))),
      selectInput("ada_gate","ADA Fare Gate Access", choices =c("All", unique(survey_data$ADA_Fare_A))),
      selectInput("wide_aisle","Wide Aisle Gate", choices =c("All", unique(survey_data$Wide_Aisle))),
      selectInput("station_booth","Station Booth", choices =c("All", unique(survey_data$Station_Bo)))),
    mainPanel(
      leafletOutput("map", width ="100%", height ="100vh"))))

server <-function(input, output, session){
  
  filteredData <- reactive({
    data <- survey_data
    
    if(input$station_name !="All"){
      data <- data[data$Station_Na == input$station_name,]}
    if(input$borough !="All"){
      data <- data[data$Borough == input$borough,]}
    if(input$line !="All"){
      data <- data[grepl(input$line, data$Line_s_),]}
    if(input$ada_gate !="All"){
      data <- data[data$ADA_Fare_A == input$ada_gate,]}
    if(input$wide_aisle !="All"){
      data <- data[data$Wide_Aisle == input$wide_aisle,]}
    if(input$station_booth !="All"){
      data <- data[data$Station_Bo == input$station_booth,]}
    
    data <- data[data$Flat_Walki >= input$walking[1]& data$Flat_Walki <= input$walking[2],]
    data <- data[data$Step_Total >= input$steps[1]& data$Step_Total <= input$steps[2],]
    data
  })
  
  output$total_inaccessible <- renderText({
    paste(length(unique(survey_data$Station_Na[survey_data$ADA_Fare_A =="No"])),"Inaccessible Stations")})
  
  output$total_entrances <- renderText({
    paste(nrow(survey_data),"Subway Entrances")})
  
  output$total_lines <- renderText({
    paste(length(unique(unlist(strsplit(survey_data$Line_s_,",")))),"Subway Lines")})
  
  output$map <- renderLeaflet({
    leaflet(data = filteredData())%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      setView(lng =-73.9000342, lat =40.730010, zoom =11)%>%
      addPolygons(data = boroughs, color ="grey", weight =3, fillOpacity =0, group ="Borough Boundaries")%>%
      addPolygons(data = tracts, color ="grey", weight =0.50, fillOpacity =0, group ="Census Tracts")%>%
      addCircleMarkers(~x,~y,
                       radius =5,
                       stroke =TRUE,
                       color ="black",
                       weight =2,
                       fillOpacity =0.8,
                       group ="Subway Stations",
                       popup =~popup_info,
                       fillColor =~colorNumeric(palette ="RdPu", domain = survey_data$Step_Total)(Step_Total))%>%
      addLayersControl(
        overlayGroups =c("Subway Stations","Borough Boundaries","Census Tracts"),
        options = layersControlOptions(collapsed =FALSE))%>%
      addLegend("bottomright",
                pal = colorNumeric(palette ="RdPu", domain = survey_data$Step_Total),
                values = survey_data$Step_Total,
                title ="Number of Steps to<br>Reach Fare Gates",
                opacity =1,
                className ="compact-legend")})
  
  observeEvent(input$map_marker_click,{
    click <- input$map_marker_click
    leafletProxy("map")%>%
      setView(lng = click$lng, lat = click$lat, zoom =14)})}

shinyApp(ui, server)
