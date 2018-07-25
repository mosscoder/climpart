ui <- fluidPage(
  useShinyalert(),
  sidebarLayout(
    
    sidebarPanel(width=2,
                 
                 fluidRow(
                   img(src="https://storage.googleapis.com/seedmapper_dat/usgs.log.png", height=81, width=180),
                   
                   selectInput("boundSelect", label = ("How would you like to define your area of interest?"), 
                               choices = list("Lat/Long Slider Bars" = "slider", 
                                              "Spatial Polygon" = "poly"),
                               selected = "slider"),
                   
                   conditionalPanel( condition = "input.boundSelect == 'poly'",             
                                     fileInput("boundFile2", 
                                               label = "Upload spatial polygon files (.shp, .shx, .prj, and .dbf) compressed into a .zip format:")),
                   
                   
                   conditionalPanel(condition = "input.boundSelect == 'slider'",
                                    sliderInput("lat.range",
                                                label = "Latitude Extent", 
                                                min = 7, max = 83, step = 0.1,
                                                value=c(32,40))),
                   
                   conditionalPanel(condition = "input.boundSelect == 'slider'",  
                                    sliderInput("lon.range",
                                                label = "Longitude Extent", 
                                                min =-168, max = -52, step = 0.1,
                                                value=c(-115,-105))),
                   
                   sliderInput("cluster.num", label = "Number of climate partitions:", 
                               ticks = F, value=5, min = 1, max = 50, step = 1),
                   HTML(paste0('<button data-toggle="collapse" class="btn btn-default" id="wtButton" 
                               data-target="#demo"><img src="https://github.com/mosscoder/climpartUploadTest/blob/master/scale.png?raw=true" height = 20, width 20/><span>Weight Variables</span></button>')),
                   tags$style(type = "text/css", "#wtButton {width: 190px; display: inline-block; padding: 5px; margin: 2px;}"),
                   
                   tags$div(id = 'demo',  class="collapse out",
                            sliderInput("wtMAT", label = "Mean Annual Temperature", 
                                        ticks = F, value=1, min = 0, max = 1, step = .01),
                            sliderInput("wtDiurnal", label = "Diurnal Range", 
                                        ticks = F, value=1, min = 0, max = 1, step = .01),
                            sliderInput("wtTSeason", label = "Temperature Seasonality", 
                                        ticks = F, value=1, min = 0, max = 1, step = .01),
                            sliderInput("wtTWet", label = "Temperature Wettest Qtr.",
                                        ticks = F, value=1, min = 0, max = 1, step = .01),
                            sliderInput("wtMAP", label = "Mean Annual Precipitation",
                                        ticks = F, value=1, min = 0, max = 1, step = .01),
                            sliderInput("wtPSeason", label = "Precipitation Seasonality",
                                        ticks = F, value=1, min = 0, max = 1, step = .01),
                            sliderInput("wtPWarm", label = "Precipitation Warmest Qtr.",
                                        ticks = F, value=1, min = 0, max = 1, step = .01)        
                   ),
                   
                   actionButton("goButton", label=HTML("<b>Generate Partitions</b>")),
                   
                   downloadButton('downloadData', 'Download Data', style = ' width: 190px; margin: 2px;'),
                   
                   helpText("Click above to download underlying rasters and summary data. Note that clicking will open a new tab."),
                   
                   actionButton('moreInfo', label = HTML("<b>Contact Info & Disclaimer</b>")),
                   
                   tags$style(type = "text/css", "#goButton{background-color: #18B66A; color: #fff; border-color: #ffffff; width: 190px;margin: 2px;}
                              #goButton:hover{background-color: #1EE285;}
                              #moreInfo{background-color: #008CBA; color: #fff;border-color: #ffffff; width: 190px;margin: 2px; padding:5px}
                              #moreInfo:hover{background-color: #00B1ED;}")
                   )), 
    
    mainPanel(width = 10,
              
              tabsetPanel(id = "tabs",
                          tabPanel("Map", id="map", leafletOutput("leaf",width="100%", height = "700px") %>% withSpinner(size = 3)),
                          tabPanel("Climate Center Data", dataTableOutput("centerTable")),
                          tabPanel("Within-Assignment Distributions", id="box", 
                                   fluidRow(
                                     column(2,selectInput("ggVar1", label = ("Variable 1"), 
                                                          choices = list("MAT" = "MAT",
                                                                         "DiurnalRange" = "DiurnalRange",
                                                                         "TSeasonality" = "TSeasonality",
                                                                         "TWettestQtr" = "TWettestQtr",
                                                                         "MAP" = "MAP",
                                                                         "PSeasonality" = "PSeasonality",
                                                                         "PWarmestQtr" = "PWarmestQtr"),
                                                          selected = "MAT")),
                                     column(2,selectInput("ggVar2", label = ("Variable 2"), 
                                                          choices = list("MAT" = "MAT",
                                                                         "DiurnalRange" = "DiurnalRange",
                                                                         "TSeasonality" = "TSeasonality",
                                                                         "TWettestQtr" = "TWettestQtr",
                                                                         "MAP" = "MAP",
                                                                         "PSeasonality" = "PSeasonality",
                                                                         "PWarmestQtr" = "PWarmestQtr"),
                                                          selected = "MAP"))),
                                   plotOutput("boxPlot", height=650) %>% withSpinner( size = 3)),
                          tabPanel("Background and Use", id="background", textOutput('instruct')),
                          tabPanel("Run Offline", id="offline",
                                   tags$iframe(src='https://rawgit.com/mosscoder/climpart/master/offlineInstructions.html',
                                               width = "100%", height = "1000px", style="border:0"))
              ))
                   )
  )