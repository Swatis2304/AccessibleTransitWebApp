# Load required libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(rsconnect)
library(tigris)# Load your GeoJSON data
survey_data <- st_read("ccc/Entrance_to_Faregate.geojson")# Data preparation
survey_data <- st_transform(survey_data, crs =4326)
survey_data$ADA_Fare_A[is.na(survey_data$ADA_Fare_A)]<-"No"
survey_data$Wide_Aisle[is.na(survey_data$Wide_Aisle)]<-"No"
survey_data$Wide_Aisle[survey_data$Wide_Aisle =="<Null>"]<-"No"
survey_data$Wide_Aisle[survey_data$Wide_Aisle =="1"]<-"Yes"
survey_data <- survey_data %>% filter(!is.na(Borough))# UI
ui <- dashboardPage(
  dashboardHeader(title ="AccessibALL NYC Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName ="map_tab", icon = icon("map")),
      menuItem("Accessibility Metrics", tabName ="metrics_tab", icon = icon("chart-bar")),
      menuItem("Comparisons", tabName ="comparison_tab", icon = icon("balance-scale")))),
  dashboardBody(
    tabItems(
      tabItem(tabName ="map_tab",
              fluidRow(
                box(width =12, leafletOutput("map", height =600)))),
      tabItem(tabName ="metrics_tab",
              fluidRow(
                box(width =6, plotlyOutput("ada_bar_chart")),
                box(width =6, plotlyOutput("stairs_pie_chart"))),
              fluidRow(
                box(width =6, plotlyOutput("tactile_pie_chart")),
                box(width =6, plotlyOutput("turns_bar_chart")))),
      tabItem(tabName ="comparison_tab",
              fluidRow(
                box(width =12, plotlyOutput("borough_comparison_chart")),
                box(width =12, plotlyOutput("line_comparison_chart")))))))# Server
server <-function(input, output, session){# Leaflet Map
  output$map <- renderLeaflet({
    leaflet(data = survey_data)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      setView(lng =-73.9000342, lat =40.730010, zoom =11)%>%
      addCircleMarkers(~st_coordinates(geometry)[,1],~st_coordinates(geometry)[,2],
                       radius =5,
                       stroke =TRUE,
                       color ="black",
                       weight =2,
                       fillOpacity =0.8,
                       group ="Subway Stations",
                       popup =~paste0("<strong>Station Name: </strong>", Station_Na,"<br>","<strong>ADA Fare Access: </strong>", ADA_Fare_A,"<br>","<strong>Wide-Aisle Gate: </strong>", Wide_Aisle,"<br>","<strong>Total Walking Distance: </strong>", Flat_Walki," ft","<br>","<strong>Steps: </strong>", Step_Total))%>%
      addLayersControl(
        overlayGroups =c("Subway Stations"),
        options = layersControlOptions(collapsed =FALSE))})# Bar Chart for ADA Compliance
  output$ada_bar_chart <- renderPlotly({
    ada_data <- survey_data %>% 
      count(ADA_Fare_A)%>%
      mutate(percentage =round(n /sum(n)*100,2))
    
    plot_ly(ada_data, x =~ADA_Fare_A, y =~percentage, type ='bar')%>%
      layout(title ="Percentage of Stations with ADA-Compliant Fare Gates",
             xaxis =list(title =""),
             yaxis =list(title ="Percentage"))})# Pie Chart for Stations with More Than 25 Stairs
  output$stairs_pie_chart <- renderPlotly({
    stairs_data <- survey_data %>%
      mutate(more_than_25_stairs = ifelse(Step_Total >25,"More than 25","25 or less"))%>%
      count(more_than_25_stairs)
    
    plot_ly(stairs_data, labels =~more_than_25_stairs, values =~n, type ='pie')%>%
      layout(title ="Stations with More Than 25 Stairs")})# Pie Chart for Stations Lacking Tactile Strips
  output$tactile_pie_chart <- renderPlotly({
    tactile_data <- survey_data %>%
      mutate(has_tactile = ifelse(Tactile_Gu =="Yes","Has Tactile Strips","Lacks Tactile Strips"))%>%
      count(has_tactile)
    
    plot_ly(tactile_data, labels =~has_tactile, values =~n, type ='pie')%>%
      layout(title ="Stations Lacking Tactile Strips")})# Bar Chart for Stations with More Than 2 Turns
  output$turns_bar_chart <- renderPlotly({
    turns_data <- survey_data %>%
      mutate(more_than_2_turns = ifelse(Turns >2,"More than 2","2 or less"))%>%
      count(more_than_2_turns)
    
    plot_ly(turns_data, x =~more_than_2_turns, y =~n, type ='bar')%>%
      layout(title ="Stations with More Than 2 Turns",
             xaxis =list(title =""),
             yaxis =list(title ="Number of Stations"))})# Comparison Bar Chart for Boroughs
  output$borough_comparison_chart <- renderPlotly({
    comparison_data <- survey_data %>%
      group_by(Borough)%>%
      summarize(
        ada_compliance =round(sum(ADA_Fare_A =="Yes")/ n()*100,2),
        avg_stairs =round(mean(Step_Total, na.rm =TRUE),2),
        avg_turns =round(mean(Turns, na.rm =TRUE),2))
    
    plot_ly(comparison_data, x =~Borough, y =~ada_compliance, type ='bar', name ='ADA Compliance (%)')%>%
      add_trace(y =~avg_stairs, name ='Average Stairs')%>%
      add_trace(y =~avg_turns, name ='Average Turns')%>%
      layout(title ="Comparison of Accessibility Features by Borough",
             barmode ='group',
             yaxis =list(title ="Percentage / Average"))})# Comparison Bar Chart for Subway Lines
  output$line_comparison_chart <- renderPlotly({
    line_comparison_data <- survey_data %>%
      group_by(Line_s_)%>%
      summarize(
        avg_steps =round(mean(Step_Total, na.rm =TRUE),2),
        avg_flat_walking_distance =round(mean(Flat_Walki, na.rm =TRUE),2),
        avg_flights =round(mean(Flights, na.rm =TRUE),2))%>%
      gather(key ="Metric", value ="Average", avg_steps, avg_flat_walking_distance, avg_flights)
    
    plot_ly(line_comparison_data, x =~Line_s_, y =~Average, color =~Metric, type ='bar')%>%
      layout(title ="Comparison of Accessibility Features by Subway Line",
             xaxis =list(title ="Subway Line"),
             yaxis =list(title ="Average"))})}# Run the application 
shinyApp(ui = ui, server = server)

