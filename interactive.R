library(leaflet)
library(shiny)
library(tidyverse)
library(RColorBrewer)
job = read_csv('job_with_lat_long.csv')%>%
  filter(!is.na(latitude)) %>%
  filter(longitude < -74.0060 + 1) %>%
  filter(longitude > -74.0060 - 1) %>%
  filter(latitude < 40.7128 + 0.8) %>%
  filter(latitude > 40.7128 - 0.8)

job$longitude = map_dbl(job$longitude, function(x) jitter(x, factor = 0.0001))
job$latitude = map_dbl(job$latitude, function(x) jitter(x, factor = 0.0001))

df_popular = read_csv('popular_cate.csv')
job$Full.Time.Part.Time.indicator[job$Full.Time.Part.Time.indicator == 'F'] = 'Full time'

job$Full.Time.Part.Time.indicator[job$Full.Time.Part.Time.indicator == 'P'] = 'Part time'

job = job %>%
  select(-'Job.Category')
df_popular_test = df_popular[,-3]

job = unique(merge(df_popular_test, job, by = 'Job.ID')) %>%
  rename(Category = Job.Category)

high_salary_geocodes = job %>%
  filter(Annual_salary > mean(Annual_salary))
low_salary_geocodes = job %>%
  filter(Annual_salary < mean(Annual_salary))

#add custom color column
high_salary_geocodes$color = rep('red', nrow(high_salary_geocodes))
low_salary_geocodes$color = rep('blue', nrow(low_salary_geocodes))

job = rbind(high_salary_geocodes, low_salary_geocodes) %>%
  filter(!is.na(Annual_salary))

ui <- shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, left = 50,
                selectInput(inputId = "Full_part_time","Choose full time or part time:",
                            c("All", "Full time", "Part time")
                ),
                selectInput(inputId = "chosen_cate","Choose the category that you want:",
                            c("All", job$Category)
                ),
                selectInput(inputId = "chosen_agency","Choose the agency that you want:",
                            c("All", job$Agency)
                ),
                sliderInput("Annual_salary", "Slide bar to choose the range of salary:", 
                            20000, 220000,
                            value = range(20000:220000), step = 10000
                )
  )
)
)




server <- function(input, output, session) {
  
  filteredData <- reactive({
    temp <- job
    if(input$Full_part_time !='All'){
      temp <- temp[temp$Full.Time.Part.Time.indicator == input$Full_part_time, ]
    }
    if (input$chosen_cate !='All') {
      temp <- temp[temp$Category == input$chosen_cate, ]
    }
    if (input$chosen_agency !='All') {
      temp <- temp[temp$Agency == input$chosen_agency, ]
    }
    temp[temp$Annual_salary >= input$Annual_salary[1] & temp$Annual_salary <= input$Annual_salary[2], ]
  })
  
  color = rev(brewer.pal(100, "RdYlBu"))
  
  pal <- colorNumeric(
    palette = color,
    domain = job$Annual_salary
  )
  
  output$map <- renderLeaflet({
    leaflet(job) %>% setView(lat = 40.7128, lng = -74.0060, zoom = 11) %>%
      addTiles() %>% 
      addCircleMarkers(data = filteredData(),  
                       lng = ~longitude, 
                       lat = ~latitude, 
                       radius = 4, 
                       color = ~pal(Annual_salary),
                       clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                       popup = ~paste("<strong>Angency</strong>", Agency,
                                      "<br /><strong>Job Title:</strong>", Business.Title,
                                      "<br /><strong>Annual Salary:</strong>","$",Annual_salary,
                                      "<br /><strong>Category:</strong>",Category)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~Annual_salary,
                #colors = c('blue', 'red'),
                #labels = c( 'Low salary', 'High salary'),
                title = "Salary",
                opacity = 1
      )
  })
}

shinyApp(ui, server)
