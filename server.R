packrat::on()
library(jose)

# Darshan - auth0|5e7ddc6a947abf0cce44671e
# Tom - auth0|598d85147819dd249c98ffd8
# Beth - auth0|5e7de3f2c74faa0cfcdecaf5
# Mark - auth0|5e735e40708a2d0c715a85f5

shinyServer(function(input, output, session) {
  
  # Get the username
  js$getCookie()
  
  output$username = renderPrint({
    paste('You are logged in as:', 
          username_from_jwt(input$authorization_cookie))
  })
  
  # Show upload tab if the user is authorised
  observe({
    userID <- as.character(username_from_jwt(input$authorization_cookie))
    # if(userID == 'auth0|598d85147819dd249c98ffd8'){
      shinyjs::show(selector = '#nav li a[data-value="Upload Files"]')
    # }
  })
  
  addResourcePath('datatiles',file.path(data_path,'tiles'))
  click <- reactiveValues(click_marker = NULL, click_start = NULL, click_end = NULL, link = NULL)
  maps <- reactiveValues(satellite = TRUE, roadmap = TRUE)
  points <- reactiveValues(monkey = NULL, tick = NULL, human = NULL, humanTY = NULL, unknowns = NULL,
                           hospitals = NULL, health = NULL, forest = NULL, tourism = NULL)
  buffers <- reactiveValues(monkey = NULL, tick = NULL, human = NULL, unknowns = NULL, point = NULL)
  legends <- reactiveValues(monkey = NULL, tick = NULL, human = NULL, hospitals = NULL)
  polygons <- reactiveValues(villages = NULL, taluks = NULL)
  riskFactors <- reactiveValues(options = NULL)
  loaded_social <- reactiveValues(health = NULL, forest = NULL, tourism = NULL)

  # Create the base map - this never changes
  output$India_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 82, lat = 22, zoom = 5) %>%
      addMapPane('sat', zIndex = 410) %>%
      addMapPane('bm', zIndex = 420) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       layerId = 'satT',
                       options = c(providerTileOptions(opacity = 1),
                                   tileOptions(maxZoom = 19, maxNativeZoom = 17),
                                   pathOptions(pane = 'sat')),
                       group = 'basemap') %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       layerId = 'basemapT',
                       options = c(providerTileOptions(opacity = .4),
                                   tileOptions(maxZoom = 19), pathOptions(pane = 'sat')),
                       group = 'basemap') %>%
      addMeasure(position = 'bottomleft',
                 primaryLengthUnit = "kilometers",
                 primaryAreaUnit = "sqmeters") %>%
      onRender(
        "function(el,x){
                    this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange('hover_coordinates', coord)
                    });
                    this.on('mouseout', function(e) {
                        Shiny.onInputChange('hover_coordinates', null)
                    })
                }"
      )# %>%
      #addMapPane('drw', zIndex = 425) %>%
      #addDrawToolbar(targetGroup = "draw", singleFeature = TRUE,
      #               rectangleOptions = F, polygonOptions = c(zindex = 450),
      #               polylineOptions = F, 
      #               markerOptions = F, 
      #               editOptions = editToolbarOptions(selectedPathOptions = c(selectedPathOptions())), 
      #               circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(clickable = T))) %>%
      #addLayersControl(overlayGroups = c("draw"),
      #                 options = layersControlOptions(collapsed = FALSE)) %>%
      #addStyleEditor()
  })
  
  # Create the mouse coordinates
  output$mouseCoords <- renderText({
    if(is.null(input$hover_coordinates)) {
      "Lat: <br>Lng: "
    } else {
      paste0("Lat: ", round(input$hover_coordinates[1],3), 
             "<br>Lng: ", round(input$hover_coordinates[2],3))
    }
  })
  
  observeEvent(input$openRiskFactors, {
    updateCollapse(session, id = 'layers', open = 'Risk Factors')
  })
  
  observeEvent(input$openPastYears, {
    updateCollapse(session, id = 'layers', 'Past Human Cases')
  })
  
  observeEvent(input$openThisYear, {
    updateCollapse(session, id = 'layers', 'This Year Cases')
  })
  
  observeEvent(input$openPRMaps, {
    updateCollapse(session, id = 'layers', 'Predictive Risk Maps')
  })
  
  observeEvent(input$openSocialData, {
    updateCollapse(session, id = 'layers', 'Health, Environment and Tourism')
  })
  
  observeEvent(input$openDistrictChoice, {
    updateCollapse(session, id = 'layers', 'District Selector')
  })
  
  observeEvent(input$openHazardDistrict, {
    updateCollapse(session, id = 'layers', 'District Risk Factors')
  })
  
  observeEvent(input$infoBox, {
    toggle('popupInfo')
  })
  
  observeEvent(input$surveillance, {
    updateCollapse(session, id = 'popupInfo', 'Surveillance Protocols')
  })

  observeEvent(input$roadmap, {
    maps$roadmap <- !maps$roadmap
    basemap(leafletProxy('India_map'), satellite = maps$satellite, roadmap = maps$roadmap, tiles = tiles)
  })
  
  observeEvent(input$satellite, {
    maps$satellite <- !maps$satellite
    basemap(leafletProxy('India_map'), satellite = maps$satellite, roadmap = maps$roadmap, tiles = tiles)
  })

  observeEvent(input$India_map_marker_click, {
    # Add a 100m buffer to the map at the click point
    pointbuf <- create_buffer(input$India_map_marker_click)
    leafletProxy('India_map', data = pointbuf) %>%
      clearGroup(group = 'point_buffer')  %>%
      addMapPane('pointBuff',zIndex = 410) %>%
      addPolygons(
        group = 'point_buffer',
        color = '#071376',
        stroke = TRUE,
        fillOpacity = 0,
        opacity = 0.3,
        weight = 5,
        options = pathOptions(pane = 'pointBuff'))
    
    # Generate the first and last click points for google maps
    click$click_marker <- input$India_map_marker_click
    if(is.null(click$click_start)){
      click$click_start <- c(click$click_marker$lat,click$click_marker$lng)
    } else if(is.null(click$click_end)){
      click$click_end <- c(click$click_marker$lat,click$click_marker$lng)
    } else {
      click$click_start <- click$click_end
      click$click_end <- c(click$click_marker$lat,click$click_marker$lng)
    }

    if(!is.null(click$click_end)){
      google <- a(img(src = 'icons/googleMaps.png', height = '50', width = '50'),
                  href = paste0("https://www.google.com/maps/dir/'",
                                click$click_start[1], ',',click$click_start[2], "'/'",
                                click$click_end[1],   ',',click$click_end[2],   "'/"),
                  target = '_blank')

      leafletProxy('India_map') %>%
        clearGroup(group = c('starts','ends')) %>%
        addMarkers(lat = click$click_end[1], lng = click$click_end[2], group = 'ends') %>%
        addMapPane('circle',zIndex = 490) %>%
        addCircleMarkers(lat = click$click_start[1], lng = click$click_start[2],
                         color = 'black', group = 'starts',
                         options = pathOptions(pane = 'circle'))
      
    } else {
      google <- ''
      leafletProxy('India_map') %>%
        clearGroup(group = c('starts','ends')) %>%
        addCircleMarkers(lat = click$click_start[1], lng = click$click_start[2],
                         color = 'black', group = 'starts')
    }
    output$google <- renderUI({
      tagList('', google)
    })

    
  })

  # India - raster layers ----
  observe({
    if(input$rasterchoice == '<none>'){
      leafletProxy('India_map', session) %>%
        clearGroup(group = 'forest_tiles')
    } else {
      forest <- paste0('datatiles/risk_tiles/forest_cover/',str_replace_all(input$rasterchoice,' ','_'),'/{z}/{x}/{y}.png')
      native <- file.path(data_path,'tiles/risk_tiles/forest_cover',str_replace_all(input$rasterchoice,' ','_')) %>%
        list.dirs(recursive = FALSE, full.names = FALSE) %>% as.numeric() %>% max()
      leafletProxy('India_map', session) %>%
        clearGroup(group = 'forest_tiles') %>%
        addMapPane('ft',zIndex = 480) %>%
        addTiles(urlTemplate = forest,
                 options = c(tileOptions(opacity = 0.8,
                                         maxZoom = 19,
                                         maxNativeZoom = native),
                             pathOptions(pane = 'ft')),
                 group = 'forest_tiles')
    }
  })
  
  # India - raster layers ----
  observe({
    if(input$farmingCover == '<none>'){
      output$farming_risk_info <- renderUI({''})
      leafletProxy('India_map', session) %>%
        clearGroup(group = 'farming_tiles') %>% 
        removeControl('farming_tiles')
    } else {
      farming <- paste0('datatiles/risk_tiles/farming/',str_replace_all(input$farmingCover,' ','_'),'/{z}/{x}/{y}.png')
      pal <- read.csv(file.path(data_path,'tiles/risk_tiles/farming',str_replace_all(input$farmingCover,' ','_'),'pal.csv'),stringsAsFactors = FALSE)
      output$farming_risk_info <- renderUI({
        readLines(file.path(data_path,'tiles/risk_tiles/farming',str_replace_all(input$farmingCover,' ','_'),'info.txt'), warn = FALSE) %>%
          paste0(sep = '<br/>') %>% HTML()
      })
      native <- file.path(data_path,'tiles/risk_tiles/farming',str_replace_all(input$farmingCover,' ','_')) %>%
        list.dirs(recursive = FALSE, full.names = FALSE) %>% as.numeric() %>% max()
      leafletProxy('India_map', session) %>%
        clearGroup(group = 'farming_tiles') %>%
        addMapPane('ft',zIndex = 460) %>%
        addTiles(urlTemplate = farming,
                 options = c(tileOptions(opacity = 0.8,
                                         maxZoom = 19,
                                         maxNativeZoom = native),
                             pathOptions(pane = 'ft')),
                 group = 'farming_tiles') %>%
        addLegend(pal = colorFactor(pal$colour, levels = pal$value),
                  values = pal$value, 
                  title = input$farmingCover, 
                  position = 'bottomleft',
                  layerId = 'farming_tiles',
                  labFormat = labelFormat(digits = 4,
                                          big.mark = ''))
    }
  })
  
  observe({
    if(input$elevation){
      elevation <- paste0('datatiles/risk_tiles/Elevation/{z}/{x}/{y}.png')
      pal <- read.csv(file.path(data_path,'tiles/risk_tiles/Elevation/pal.csv'),stringsAsFactors = FALSE)
      native <- file.path(data_path,'tiles/risk_tiles/Elevation') %>%
        list.dirs(recursive = FALSE, full.names = FALSE) %>% as.numeric() %>% max()
      output$elevation_info <- renderUI({
        readLines(file.path(data_path,'tiles/risk_tiles/Elevation/info.txt'), warn = FALSE) %>%
          paste0(sep = '<br/>') %>% HTML()
      })
      leafletProxy('India_map', session) %>%
        clearGroup(group = 'elevation') %>%
        addMapPane('ele',zIndex = 460) %>%
        addTiles(urlTemplate = elevation,
                 options = c(tileOptions(opacity = 0.8,
                                         maxZoom = 19,
                                         maxNativeZoom = native),
                             pathOptions(pane = 'ele')),
                 group = 'elevation') %>%
        addLegend(pal = colorFactor(pal$colour, levels = pal$value),
                  values = pal$value, 
                  title = 'Elevation (m)', 
                  position = 'bottomleft',
                  layerId = 'elevation',
                  labFormat = labelFormat(digits = 4,
                                          big.mark = ''))
    } else {
      output$elevation_info <- renderUI({''})
      leafletProxy('India_map', session) %>%
        clearGroup(group = 'elevation') %>%
        removeControl('elevation')
    }
  })

  # India - raster explanation ----
  output$risk_info <- renderUI({
    
    if(input$rasterchoice != '<none>'){
      readLines(file.path(data_path,'tiles/risk_tiles/forest_cover',
                          str_replace_all(input$rasterchoice,' ','_'),'info.txt'), warn = FALSE) %>%
        paste0(sep = '<br/>') %>% HTML()
    } else {
      ''
    }
    
  })
  
  # India - predicted risk raster  ----
  observe({
    if(!input$PredictedRiskIndia){
      output$predicted_risk_info <- renderUI({''})
      leafletProxy('India_map', session) %>%
        clearGroup(group = 'predicted_risk2') %>% 
        removeControl('predicted_risk2')
    } else {
      
      pal <- read.csv(file.path(data_path,'tiles/predicted_risk2/risk_tiles/pal.csv'),stringsAsFactors = FALSE)
      output$predicted_risk_info <- renderUI({
        readLines(file.path(data_path,'tiles/predicted_risk2/risk_tiles/info.txt'), warn = FALSE) %>%
          paste0(sep = '<br/>') %>% HTML()
      })
      native <- file.path(data_path,'tiles/predicted_risk2/risk_tiles') %>%
        list.dirs(recursive = FALSE, full.names = FALSE) %>% as.numeric() %>% max()

      leafletProxy('India_map', session) %>%
        addMapPane('pr',zIndex = 460) %>%
        addTiles(urlTemplate = 'datatiles/predicted_risk2/risk_tiles/{z}/{x}/{y}.png',
                 options = c(tileOptions(opacity = 0.8,
                                         maxZoom = 19,
                                         maxNativeZoom = native),
                             pathOptions(pane = 'pr')),
                 group = 'predicted_risk2') %>%
        addLegend(pal = colorFactor(pal$colour, levels = pal$value),
                  values = pal$value, 
                  title = 'KFD Predicted Risk', 
                  position = 'bottomleft',
                  layerId = 'predicted_risk2',
                  labFormat = labelFormat(digits = 4,
                                          big.mark = ''))
      
    }
  })
  
  # India - predicted risk raster  ----
  observe({
    if(!input$PredictedRiskIndia2020){
      output$predicted_risk_info2020 <- renderUI({''})
      leafletProxy('India_map', session) %>%
        clearGroup(group = 'predicted_risk2020') %>% 
        removeControl('predicted_risk2020')
    } else {
      
      pal <- read.csv(file.path(data_path,'tiles/predicted_risk_2020/risk_tiles/pal.csv'),stringsAsFactors = FALSE)
      output$predicted_risk_info2020 <- renderUI({
        readLines(file.path(data_path,'tiles/predicted_risk_2020/risk_tiles/info.txt'), warn = FALSE) %>%
          paste0(sep = '<br/>') %>% HTML()
      })
      native <- file.path(data_path,'tiles/predicted_risk_2020/risk_tiles') %>%
        list.dirs(recursive = FALSE, full.names = FALSE) %>% as.numeric() %>% max()
      
      leafletProxy('India_map', session) %>%
        addMapPane('pr',zIndex = 460) %>%
        addTiles(urlTemplate = 'datatiles/predicted_risk_2020/risk_tiles/{z}/{x}/{y}.png',
                 options = c(tileOptions(opacity = 0.8,
                                         maxZoom = 19,
                                         maxNativeZoom = native),
                             pathOptions(pane = 'pr')),
                 group = 'predicted_risk2020') %>%
        addLegend(pal = colorFactor(pal$colour, levels = pal$value),
                  values = pal$value, 
                  title = 'Unvalidated KFD Predicted Risk', 
                  position = 'bottomleft',
                  layerId = 'predicted_risk2020',
                  labFormat = labelFormat(digits = 4,
                                          big.mark = ''))
      
    }
  })
  
  observeEvent(input$clearAll, {
    updateCheckboxGroupInput(session, 'HumanCases','',inline = TRUE, choices = c(2014:(globalyears-1)))
    updateCheckboxGroupInput(session, 'TickCases','',inline = TRUE, choices = c(2014:(globalyears-1)))
    updateCheckboxGroupInput(session, 'MonkeyCases','',inline = TRUE, choices = c(2014:(globalyears-1)))
    updateCheckboxInput(session, 'casesOn', label = 'Show Cases', value = FALSE)
    updateCheckboxInput(session, 'HumanfiveKM', label = 'Show 5km buffer', value = FALSE)
    updateCheckboxInput(session, 'TickfiveKM', label = 'Show 5km buffer', value = FALSE)
    updateCheckboxInput(session, 'MonkeyfiveKM', label = 'Show 5km buffer', value = FALSE)
    updateCheckboxGroupInput(session, 'TickCasesTY','',inline = TRUE, choices = c(globalyears))
    updateCheckboxGroupInput(session, 'MonkeyCasesTY','',inline = TRUE, choices = c(globalyears))
    updateCheckboxInput(session, 'HumanfiveKMTY', label = 'Show 5km buffer', value = FALSE)
    updateCheckboxInput(session, 'TickfiveKMTY', label = 'Show 5km buffer', value = FALSE)
    updateCheckboxInput(session, 'MonkeyfiveKMTY', label = 'Show 5km buffer', value = FALSE)
    updateSelectInput(session, 'rasterchoice', label = '', choices = c('<none>', risk_options))
    updateSelectInput(session, 'farmingCover', label = '', choices = c('<none>', farming_options))
    updateCheckboxInput(session, 'elevation', label = 'Show Elevation Map', value = FALSE)
    updateCheckboxInput(session, 'PredictedRiskIndia', 'Show Predictive Risk Map', value = FALSE)
    updateCheckboxInput(session, 'villages', 'Show village/town outlines', value = FALSE)
    updateCheckboxInput(session, 'popdensity', 'Colour localities by population density', value = FALSE)
    updateSelectInput(session, 'forestchoice', '',
      choices = ifelse(is.null(riskFactors$options),c('<none>', district_options),c('<none>', riskFactors$options)))
    updateCheckboxGroupInput(session, 'SocialDataHealth', label = '',
                             inline = TRUE, choices = social_choices$health, selected = NULL)
    updateCheckboxGroupInput(session, 'SocialDataForest', label = '',
                             inline = TRUE, choices = social_choices$forest, selected = NULL)
    updateCheckboxGroupInput(session, 'SocialDataTourism', label = '',
                             inline = TRUE, choices = social_choices$tourism, selected = NULL)
  })
  
  observe({
    if(input$hideAll == 0) return(NULL)
    else if(input$hideAll%%2==1) {
      shinyjs::hide('layers')
      shinyjs::hide('controls')
      shinyjs::hide('layerstwo')
      shinyjs::hide('indiaMenu')
      shinyjs::hide('districtMenu')
      shinyjs::hide('mapsonoff')
      shinyjs::hide('infoBox')
      shinyjs::hide('clearAll')
      shinyjs::hide('backupSatellite')
      shinyjs::hide('backupBasemap')
    } else {
      shinyjs::show('layers')
      shinyjs::show('controls')
      shinyjs::show('layerstwo')
      shinyjs::show('indiaMenu')
      shinyjs::show('districtMenu')
      shinyjs::show('mapsonoff')
      shinyjs::show('infoBox')
      shinyjs::show('clearAll')
      shinyjs::show('backupSatellite')
      shinyjs::show('backupBasemap')
    }
  })
  
  # Add backup OSM basemap in case of no internet connection
  observe({
    if(input$backupSatellite == 0) return(NULL)
    else if(input$backupSatellite%%2==0) {
      leafletProxy('India_map') %>%
        clearGroup(group = 'backupSat')
    } else {
      native <- file.path(data_path,'tiles/osm') %>%
        list.dirs(recursive = FALSE, full.names = FALSE) %>% as.numeric() %>% max()
      leafletProxy('India_map', session) %>%
        addMapPane('backupSat',zIndex = 425) %>%
        addTiles(urlTemplate = 'datatiles/worldimagery/{z}/{x}/{y}.png',
                 options = c(tileOptions(opacity = 1,
                                         maxZoom = 19,
                                         maxNativeZoom = native),
                             pathOptions(pane = 'backupSat')),
                 group = 'backupSat')
    }
  })
  
  # Add backup satellite basemap in case of no internet connection
  observe({
    if(input$backupBasemap == 0) return(NULL)
    else if(input$backupBasemap%%2==0) {
      leafletProxy('India_map') %>%
        clearGroup(group = 'backupBM')
    } else {
      if(input$backupSatellite%%2==0){
        opa <- 1
      } else {
        opa <- .4
      }
      native <- file.path(data_path,'tiles/osm') %>%
        list.dirs(recursive = FALSE, full.names = FALSE) %>% as.numeric() %>% max()
      leafletProxy('India_map', session) %>%
        clearGroup(group = 'backupBM') %>%
        addMapPane('backupBM',zIndex = 430) %>%
        addTiles(urlTemplate = 'datatiles/osm/{z}/{x}/{y}.png',
                 options = c(tileOptions(opacity = opa,
                                         maxZoom = 19,
                                         maxNativeZoom = native),
                             pathOptions(pane = 'backupBM')),
                 group = 'backupBM')
    }
  })
  
  observe({
    if(input$selectallHC == 0) return(NULL)
    else if(input$selectallHC%%2==0){
      updateCheckboxGroupInput(session, 'HumanCases','',inline = TRUE, choices = c(2014:(globalyears-1)))
    } else {
      updateCheckboxGroupInput(session, 'HumanCases','',inline = TRUE,
                               choices = c(2014:(globalyears-1)), selected = c(2014:(globalyears-1)))
    }
  })
  
  observe({
    if(input$selectallTC == 0) return(NULL)
    else if(input$selectallTC%%2==0){
      updateCheckboxGroupInput(session, 'TickCases','',inline = TRUE, choices = c(2014:(globalyears-1)))
    } else {
      updateCheckboxGroupInput(session, 'TickCases','',inline = TRUE,
                               choices = c(2014:(globalyears-1)), selected = c(2014:(globalyears-1)))
    }
  })
  
  observe({
    if(input$selectallMC == 0) return(NULL)
    else if(input$selectallMC%%2==0){
      updateCheckboxGroupInput(session, 'MonkeyCases','',inline = TRUE, choices = c(2014:(globalyears-1)))
    } else {
      updateCheckboxGroupInput(session, 'MonkeyCases','',inline = TRUE,
                               choices = c(2014:(globalyears-1)), selected = c(2014:(globalyears-1)))
    }
  })
  
  observeEvent(input$subsetdata, {
    if(input$subsetdata){
      updateCheckboxInput(session = session, inputId = 'subsetdatataluk',
                          label = 'Subset data to taluk',
                          value = FALSE)

      if(!is.null(polygons$taluks)&any(polygons$taluks$district==input$DistrictChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$district==input$DistrictChoice,]
        subset <- 'District'
        
      } else {
        subset <- 'Unsubset'
        dTaluks <- NULL
      }
    } else if(!input$subsetdatataluk){
      subset <- 'Unsubset'
      dTaluks <- NULL
    } else {
      if(!is.null(polygons$taluks)&any(polygons$taluks$district==input$DistrictChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$tahsil==input$TalukChoice,]
        subset <- 'Taluk'
      } else {
        subset <- 'Unsubset'
        dTaluks <- NULL
      }
      
    }

    loaded_social$health <-
      gadd_social_points(input$SocialDataHealth, 'Health',
                         loaded_social$health, subset = subset, taluks = dTaluks)
    loaded_social$forest <-
      gadd_social_points(input$SocialDataForest, 'Forest',
                         loaded_social$forest, subset = subset, taluks = dTaluks)
    loaded_social$tourism <-
      gadd_social_points(input$SocialDataTourism, 'Tourism',
                         loaded_social$tourism, subset = subset, taluks = dTaluks)
  })
  
  observeEvent(input$subsetdatataluk, {
    if(input$subsetdatataluk){
      updateCheckboxInput(session = session, inputId = 'subsetdata',
                          label = 'Subset data to district',
                          value = FALSE)
      
      if(!is.null(polygons$taluks)&any(polygons$taluks$tahsil==input$TalukChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$tahsil==input$TalukChoice,]
        subset <- 'Taluk'
      } else {
        subset <- 'Unsubset'
        dTaluks <- NULL
      }
    } else if(!input$subsetdata){
      subset <- 'Unsubset'
      dTaluks <- NULL
    } else {
      if(!is.null(polygons$taluks)&any(polygons$taluks$district==input$DistrictChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$district==input$DistrictChoice,]
        subset <- 'District'
      } else {
        subset <- 'Unsubset'
        dTaluks <- NULL
      }
    }
    
    loaded_social$health <-
      gadd_social_points(input$SocialDataHealth, 'Health',
                        loaded_social$health, subset = subset, taluks = dTaluks)
    loaded_social$forest <-
      gadd_social_points(input$SocialDataForest, 'Forest',
                        loaded_social$forest, subset = subset, taluks = dTaluks)
    loaded_social$tourism <-
      gadd_social_points(input$SocialDataTourism, 'Tourism',
                        loaded_social$tourism, subset = subset, taluks = dTaluks)
  })
  
  observeEvent(input$subsetdatataluk, {
    
    if(input$subsetdatataluk){
      if(!is.null(polygons$taluks)&
           any(polygons$taluks$tahsil==input$TalukChoice)&
           !is.null(points$human)){
        dTaluks <- polygons$taluks[polygons$taluks$tahsil==input$TalukChoice,]
        points_human <- points_in_buff(points$human, dTaluks)
      } else {
        points_human <- NULL
      }
    } else if(!input$subsetdata&!is.null(points$human)){
      points_human <- points$human
    } else {
      points_human <- NULL
    }
    
    if(!is.null(points_human)){
      hSTable <- human_subset(as.data.frame(points_human))
      output$hSTable <- renderUI({HTML(hSTable)})
    }
  })
  
  observeEvent(input$subsetdata, {
    
    if(input$subsetdata){
      if(!is.null(polygons$taluks)&
         any(polygons$taluks$district==input$DistrictChoice)&
         !is.null(points$human)){
        dTaluks <- polygons$taluks[polygons$taluks$district==input$DistrictChoice,]
        points_human <- points_in_buff(points$human, dTaluks)
      } else {
        points_human <- NULL
      }
    } else if(!input$subsetdatataluk&!is.null(points$human)){
      points_human <- points$human
    } else {
      points_human <- NULL
    }

    if(!is.null(points_human)){
      hSTable <- human_subset(as.data.frame(points_human))
      output$hSTable <- renderUI({HTML(hSTable)})
    }
  })
  
  observeEvent(input$TalukChoice, {
    if(input$subsetdata){
      if(!is.null(polygons$taluks)&
         any(polygons$taluks$district==input$DistrictChoice)&
         !is.null(points$human)){
        dTaluks <- polygons$taluks[polygons$taluks$district==input$DistrictChoice,]
        points_human <- points_in_buff(points$human, dTaluks)
      } else {
        points_human <- NULL
      }
    } else if(input$subsetdatataluk){
      if(!is.null(polygons$taluks)&
         any(polygons$taluks$tahsil==input$TalukChoice)&
         !is.null(points$human)){
        dTaluks <- polygons$taluks[polygons$taluks$tahsil==input$TalukChoice,]
        points_human <- points_in_buff(points$human, dTaluks)
      } else {
        points_human <- NULL
      }
    } else if(!is.null(points$human)){
      points_human <- points$human
    } else {
      points_human <- NULL
    }
    
    if(!is.null(points_human)){
      hSTable <- human_subset(as.data.frame(points_human))
      output$hSTable <- renderUI({HTML(hSTable)})
    }
  })
  
  observe({
    years <- input$HumanCases

    # Read in the data
    if(is.null(points$human)){
      points$human <- readRDS(file.path(data_path, 'cases/human_cases/human_pts.rds'))
      legends$human <- list(c("Human Cases<br>"),
                            lapply(2014:(globalyears-1), FUN = function(i){
                              paste0("<img src=icons/",'human',i,'.png',
                                     "\nstyle='width:20px;height:20px;'> ",i,"<br/>")
                            }) %>% unlist() %>% paste0(collapse = ''),
                            paste0("<img src=icons/",'human',globalyears,'.png',
                                   "\nstyle='width:20px;height:20px;'> ",globalyears,"<br/>"))
    }
    
    if(is.null(years)&is.null(input$HumanCasesTY)){
      leafletProxy('India_map') %>%
        clearGroup(group = 'human_cases')  %>%
        removeControl(layerId = 'human_points_legend') %>%
        clearGroup(group = 'human_buffers')
      #output$casesonscreenall <- renderText({''})
      return()
    } else if(is.null(years)){
      html_legend <- legends$human[c(1,3)] %>% unlist() %>% paste0(collapse = '')
      leafletProxy('India_map') %>%
        clearGroup(group = 'human_cases') %>%
        addControl(layerId = 'human_points_legend',
                   html = html_legend,
                   position = 'bottomleft') %>%
        clearGroup(group = 'human_buffers')
      #output$casesonscreenall <- renderText({''})
      return()
    }
    human_points <- points$human[points$human$Year %in% years,]

    if(input$subsetdata){
      if(!is.null(polygons$taluks)&any(polygons$taluks$district==input$DistrictChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$district==input$DistrictChoice,]
        if(nrow(human_points)>0){
          human_points <- points_in_buff(human_points, dTaluks)
        }
      }
    }
    
    if(input$subsetdatataluk){
      if(!is.null(polygons$taluks)&any(polygons$taluks$tahsil==input$TalukChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$tahsil==input$TalukChoice,]
        if(nrow(human_points)>0){
          human_points <- points_in_buff(human_points, dTaluks)
        }
      }
    }
    
    humanIcon <- makeIcon(iconUrl = paste0('icons/',basename(human_points$icon)),
                          iconWidth = 22, iconHeight = 22, iconAnchorX = 11, iconAnchorY = 11)
    
    if(is.null(input$HumanCasesTY)){
      html_legend <- legends$human[1:2] %>% unlist() %>% paste0(collapse = '')
    } else {
      html_legend <- legends$human %>% unlist() %>% paste0(collapse = '')
    }
    
    leafletProxy('India_map', data = human_points) %>%
      clearGroup(group = 'human_cases')  %>%
      addMarkers(
        group = 'human_cases',
        icon = humanIcon,
        popup = create_popout_human(human_points)) %>%
      addControl(layerId = 'human_points_legend', html = html_legend, position = 'bottomleft')
    #output$casesonscreenall <- renderText({
    #  paste0('Number of Cases previous years: ',nrow(human_points))
    #})
    
    if(input$HumanfiveKM&(nrow(human_points)>0)){
      buffers <- human_points %>%
        spTransform(CRS("+proj=utm +zone=43 +datum=WGS84 +ellps=WGS84 +units=m")) %>%
        gBuffer(byid = TRUE, width = 5000) %>%
        spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      
      leafletProxy('India_map', data = buffers) %>%
        clearGroup(group = 'human_buffers')  %>%
        addMapPane('humBuff',zIndex = 410) %>%
        addPolygons(
          group = 'human_buffers',
          color = 'black',
          stroke = TRUE,
          fillOpacity = 0,
          opacity = 0.5,
          weight = 1,
          options = pathOptions(pane = 'humBuff'))
    } else {
      leafletProxy('India_map') %>%
        clearGroup(group = 'human_buffers')
    }
  })
  
  observe({
    if(input$selectall == 0) return(NULL)
    else if(input$selectall%%2==0){
      updateDateRangeInput(session,
                           "casesDates",
                           "Dates:",
                           start = as.Date('2020-12-31',"%Y-%m-%d"),
                           end = as.Date('2020-12-31',"%Y-%m-%d"),
                           min = as.Date("2020-01-01","%Y-%m-%d"),
                           max = as.Date("2020-12-31","%Y-%m-%d"))
    } else {
      updateDateRangeInput(session,
                           "casesDates",
                           "Dates:",
                           start = as.Date('2019-11-01',"%Y-%m-%d"),
                           end = as.Date('2020-12-31',"%Y-%m-%d"),
                           min = as.Date("2019-11-01","%Y-%m-%d"),
                           max = as.Date("2020-12-31","%Y-%m-%d"))
    }
  })
  
  observe({
    months <- input$casesDates

    # Read in the data
    if(is.null(points$human)){
      points$human <- readRDS(file.path(data_path, 'cases/human_cases/human_pts.rds'))
      legends$human <- list(c("Human Cases<br>"),
                            lapply(2014:(globalyears-1), FUN = function(i){
                              paste0("<img src=icons/",'human',i,'.png',
                                     "\nstyle='width:20px;height:20px;'> ",i,"<br/>")
                            }) %>% unlist() %>% paste0(collapse = ''),
                            paste0("<img src=icons/",'human',globalyears,'.png',
                                   "\nstyle='width:20px;height:20px;'> ",globalyears,"<br/>"))
    }
    if(is.null(points$humanTY)){
      points$humanTY <- points$human[points$human$Year == globalyears,]
      points$humanTY <- points$humanTY[!is.na(points$humanTY$Date.of.Onset),]
    }

    human_points <- points$humanTY[points$humanTY$Date.of.Onset <= max(months) &
                                   points$humanTY$Date.of.Onset >= min(months),]
    
    if(input$subsetdata){
      if(!is.null(polygons$taluks)&any(polygons$taluks$district==input$DistrictChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$district==input$DistrictChoice,]
        if(nrow(human_points)>0){
          human_points <- points_in_buff(human_points, dTaluks)
        }
      }
    }
    
    if(input$subsetdatataluk){
      if(!is.null(polygons$taluks)&any(polygons$taluks$tahsil==input$TalukChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$tahsil==input$TalukChoice,]
        if(nrow(human_points)>0){
          human_points <- points_in_buff(human_points, dTaluks)
        }
      }
    }
    
    humanIcon <- makeIcon(iconUrl = paste0('icons/',basename(human_points$icon)),
                          iconWidth = 22, iconHeight = 22, iconAnchorX = 11, iconAnchorY = 11)
    
    if(is.null(input$HumanCases)){
      html_legend <- legends$human[c(1,3)] %>% unlist() %>% paste0(collapse = '')
    } else {
      html_legend <- legends$human %>% unlist() %>% paste0(collapse = '')
    }
      
    if((!input$casesOn)&(is.null(input$HumanCases))){
      leafletProxy('India_map', data = human_points) %>%
        clearGroup(group = 'human_casesTY') %>%
        removeControl(layerId = 'human_points_legend') %>%
        clearGroup(group = 'human_buffersTY')
      #output$casesonscreen <- renderText({''})
    } else if(!input$casesOn){
      leafletProxy('India_map') %>%
        clearGroup(group = 'human_casesTY') %>%
        clearGroup(group = 'human_buffersTY')
      #output$casesonscreen <- renderText({''})
    } else if(nrow(human_points)==0){
      leafletProxy('India_map') %>%
        clearGroup(group = 'human_casesTY') %>%
        clearGroup(group = 'human_buffersTY')
      #output$casesonscreen <- renderText({'Number of Cases this year: 0'})
    } else {
      leafletProxy('India_map', data = human_points) %>%
        clearGroup(group = 'human_casesTY')  %>%
        addMarkers(
          group = 'human_casesTY',
          icon = humanIcon,
          popup = create_popout_human(human_points)) %>%
        addControl(layerId = 'human_points_legend', html = html_legend, position = 'bottomleft')
      #output$casesonscreen <- renderText({
      #  paste0('Number of Cases this year: ',nrow(human_points))
      #})
      
      if(input$HumanfiveKMTY&(nrow(human_points)>0)){
        buffers <- human_points %>%
          spTransform(CRS("+proj=utm +zone=43 +datum=WGS84 +ellps=WGS84 +units=m")) %>%
          gBuffer(byid = TRUE, width = 5000) %>%
          spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
        
        leafletProxy('India_map', data = buffers) %>%
          clearGroup(group = 'human_buffersTY')  %>%
          addMapPane('humBuff',zIndex = 410) %>%
          addPolygons(
            group = 'human_buffersTY',
            color = 'black',
            stroke = TRUE,
            fillOpacity = 0,
            opacity = 0.5,
            weight = 1,
            options = pathOptions(pane = 'humBuff'))
      } else {
        leafletProxy('India_map') %>%
          clearGroup(group = 'human_buffersTY')
      }
    }
  })
  
  # India - Point layers - Tick ----
  observe({
    
    year <- input$TickCases
    # Create the map
    if(is.null(year)){
      leafletProxy('India_map') %>%
        clearGroup(group = 'tick_cases')  %>%
        removeControl(layerId = 'tick_points_legend') %>%
        clearGroup(group = 'tick_buffers')
      return()
    }
    
    if(is.null(points$tick)){
      points$tick <- readRDS(file.path(data_path, 'cases/tick_cases/tick_pts.rds'))
    }
    # get the points needed
    tick_points <- points$tick[points$tick$year %in% year,]
    
    if(input$subsetdata){
      if(!is.null(polygons$taluks)&any(polygons$taluks$district==input$DistrictChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$district==input$DistrictChoice,]
        if(nrow(tick_points)>0){
          tick_points <- points_in_buff(tick_points, dTaluks)
        }
      }
    }
    
    if(input$subsetdatataluk){
      if(!is.null(polygons$taluks)&any(polygons$taluks$tahsil==input$TalukChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$tahsil==input$TalukChoice,]
        if(nrow(tick_points)>0){
          tick_points <- points_in_buff(tick_points, dTaluks)
        }
      }
    }

    tickIcon <- makeIcon(iconUrl = paste0('icons/',basename(tick_points$icon)),
                         iconWidth = 30, iconHeight = 30, iconAnchorX = 15, iconAnchorY = 15)
    
    html_legend <- paste0("Tick Positives - Past Years<br>",
                          (lapply(2014:(globalyears-1), FUN = function(i){
                            paste0("<img src=icons/",'tick',i,'.png',
                                   "\nstyle='width:20px;height:20px;'> ",i,"<br/>")
                          }) %>% unlist() %>% paste0(collapse = '')))
    
    leafletProxy('India_map', data = tick_points) %>%
      clearGroup(group = 'tick_cases')  %>%
      addMarkers(
        group = 'tick_cases',
        icon = tickIcon,
        popup = create_popout_tick(tick_points)) %>%
      addControl(layerId = 'tick_points_legend', html = html_legend, position = 'bottomleft')
    
    if(input$TickfiveKM&(nrow(tick_points)>0)){
      buffers <- tick_points %>%
        spTransform(CRS("+proj=utm +zone=43 +datum=WGS84 +ellps=WGS84 +units=m")) %>%
        gBuffer(byid = TRUE, width = 5000) %>%
        spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      
      leafletProxy('India_map', data = buffers) %>%
        clearGroup(group = 'tick_buffers')  %>%
        addMapPane('tickBuff',zIndex = 410) %>%
        addPolygons(
          group = 'tick_buffers',
          color = 'black',
          stroke = TRUE,
          fillOpacity = 0,
          opacity = 0.5,
          weight = 1,
          options = pathOptions(pane = 'tickBuff'))
    } else {
      leafletProxy('India_map') %>%
        clearGroup(group = 'tick_buffers')
    }
    
  })
  
  # India - Point layers - Tick ----
  observe({
    
    year <- input$TickCasesTY
    # Create the map
    if(is.null(year)){
      leafletProxy('India_map') %>%
        clearGroup(group = 'tick_casesTY')  %>%
        removeControl(layerId = 'tick_points_legendTY') %>%
        clearGroup(group = 'tick_buffers')
      return()
    }

    if(is.null(points$tick)){
      points$tick <- readRDS(file.path(data_path, 'cases/tick_cases/tick_pts.rds'))
    }
    tick_points <- points$tick[points$tick$year %in% year,]

    if(input$subsetdata){
      if(!is.null(polygons$taluks)&any(polygons$taluks$district==input$DistrictChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$district==input$DistrictChoice,]
        if(nrow(tick_points)>0){
          tick_points <- points_in_buff(tick_points, dTaluks)
        }
      }
    }
    
    if(input$subsetdatataluk){
      if(!is.null(polygons$taluks)&any(polygons$taluks$tahsil==input$TalukChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$tahsil==input$TalukChoice,]
        if(nrow(tick_points)>0){
          tick_points <- points_in_buff(tick_points, dTaluks)
        }
      }
    }
    
    tickIcon <- makeIcon(iconUrl = paste0('icons/',basename(tick_points$icon)),
                         iconWidth = 30, iconHeight = 30, iconAnchorX = 15, iconAnchorY = 15)
    
    html_legend <- paste0("Tick Cases - This Year<br>",
                          "<img src=icons/",'tick',globalyears,'.png',
                          "\nstyle='width:20px;height:20px;'> ",globalyears,"<br/>")

    leafletProxy('India_map', data = tick_points) %>%
      clearGroup(group = 'tick_casesTY')  %>%
      addMarkers(
        group = 'tick_casesTY',
        icon = tickIcon,
        popup = create_popout_tick(tick_points)) %>%
      addControl(layerId = 'tick_points_legendTY', html = html_legend, position = 'bottomleft')
    
    if(input$TickfiveKMTY&(nrow(tick_points)>0)){
      buffers <- tick_points %>%
        spTransform(CRS("+proj=utm +zone=43 +datum=WGS84 +ellps=WGS84 +units=m")) %>%
        gBuffer(byid = TRUE, width = 5000) %>%
        spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      
      leafletProxy('India_map', data = buffers) %>%
        clearGroup(group = 'tick_buffersTY')  %>%
        addMapPane('tickBuff',zIndex = 410) %>%
        addPolygons(
          group = 'tick_buffersTY',
          color = 'black',
          stroke = TRUE,
          fillOpacity = 0,
          opacity = 0.5,
          weight = 1,
          options = pathOptions(pane = 'tickBuff'))
    } else {
      leafletProxy('India_map') %>%
        clearGroup(group = 'tick_buffersTY')
    }
  })
  
  # India - Point layers - Monkey ----
  observe({
    
    year <- input$MonkeyCases
    # Create the map
    if(is.null(year)){
      leafletProxy('India_map') %>%
        clearGroup(group = 'monkey_cases')  %>%
        removeControl(layerId = 'monkey_points_legend') %>%
        clearGroup(group = 'monkey_buffers')
      return()
    }

    if(is.null(points$monkey)){
      points$monkey <- readRDS(file.path(data_path,'cases/monkey_cases/monkey_pts.rds'))
    }
    monkey_points <- points$monkey[points$monkey$year %in% year,]
    
    if(input$subsetdata){
      if(!is.null(polygons$taluks)&any(polygons$taluks$district==input$DistrictChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$district==input$DistrictChoice,]
        if(nrow(monkey_points)>0){
          monkey_points <- points_in_buff(monkey_points, dTaluks)
        }
      }
    }
    
    if(input$subsetdatataluk){
      if(!is.null(polygons$taluks)&any(polygons$taluks$tahsil==input$TalukChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$tahsil==input$TalukChoice,]
        if(nrow(monkey_points)>0){
          monkey_points <- points_in_buff(monkey_points, dTaluks)
        }
      }
    }
    
    monkeyIcon <- makeIcon(iconUrl = paste0('icons/',basename(monkey_points$icon)),
                           iconWidth = 28, iconHeight = 28, iconAnchorX = 14, iconAnchorY = 14)
    
    html_legend <- paste0("Monkey Positives - Past Years<br>",
                          (lapply(2014:(globalyears-1), FUN = function(i){
                            paste0("<img src=icons/",'monkey',i,'.png',
                                   "\nstyle='width:20px;height:20px;'> ",i,"<br/>")
                          }) %>% unlist() %>% paste0(collapse = '')))

    leafletProxy('India_map', data = monkey_points) %>%
      clearGroup(group = 'monkey_cases')  %>%
      addMarkers(
        group = 'monkey_cases',
        icon = monkeyIcon,
        popup = create_popout_monkey(monkey_points)) %>%
      addControl(layerId = 'monkey_points_legend', html = html_legend, position = 'bottomleft')
    
    if(input$MonkeyfiveKM&(nrow(monkey_points)>0)){
      buffers <- monkey_points %>%
        spTransform(CRS("+proj=utm +zone=43 +datum=WGS84 +ellps=WGS84 +units=m")) %>%
        gBuffer(byid = TRUE, width = 5000) %>%
        spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      
      leafletProxy('India_map', data = buffers) %>%
        clearGroup(group = 'monkey_buffers')  %>%
        addMapPane('monkeyBuff',zIndex = 410) %>%
        addPolygons(
          group = 'monkey_buffers',
          color = 'black',
          stroke = TRUE,
          fillOpacity = 0,
          opacity = 0.5,
          weight = 1,
          options = pathOptions(pane = 'monkeyBuff'))
    } else {
      leafletProxy('India_map') %>%
        clearGroup(group = 'monkey_buffers')
    }
  })
  
  # India - Point layers - Monkey ----
  observe({
    
    year <- input$MonkeyCasesTY
    # Create the map
    if(is.null(year)){
      leafletProxy('India_map') %>%
        clearGroup(group = 'monkey_casesTY')  %>%
        removeControl(layerId = 'monkey_points_legendTY') %>%
        clearGroup(group = 'monkey_buffersTY')
      return()
    }

    if(is.null(points$monkey)){
      points$monkey <- readRDS(file.path(data_path,'cases/monkey_cases/monkey_pts.rds'))
    }
    monkey_points <- points$monkey[points$monkey$year %in% year,]

    if(input$subsetdata){
      if(!is.null(polygons$taluks)&any(polygons$taluks$district==input$DistrictChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$district==input$DistrictChoice,]
        if(nrow(monkey_points)>0){
          monkey_points <- points_in_buff(monkey_points, dTaluks)
        }
      }
    }
    
    if(input$subsetdatataluk){
      if(!is.null(polygons$taluks)&any(polygons$taluks$tahsil==input$TalukChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$tahsil==input$TalukChoice,]
        if(nrow(monkey_points)>0){
          monkey_points <- points_in_buff(monkey_points, dTaluks)
        }
      }
    }
    
    monkeyIcon <- makeIcon(iconUrl = paste0('icons/',basename(monkey_points$icon)),
                           iconWidth = 28, iconHeight = 28, iconAnchorX = 14, iconAnchorY = 14)
    html_legend <- paste0("Monkey Cases - This Year<br>",
                          "<img src=icons/",'monkey',globalyears,'.png',
                          "\nstyle='width:20px;height:20px;'> ",globalyears,"<br/>")

    leafletProxy('India_map', data = monkey_points) %>%
      clearGroup(group = 'monkey_casesTY')  %>%
      addMarkers(
        group = 'monkey_casesTY',
        icon = monkeyIcon,
        popup = create_popout_monkey(monkey_points)) %>%
      addControl(layerId = 'monkey_points_legendTY', html = html_legend, position = 'bottomleft')
    
    if(input$MonkeyfiveKMTY&(nrow(monkey_points)>0)){
      buffers <- monkey_points %>%
        spTransform(CRS("+proj=utm +zone=43 +datum=WGS84 +ellps=WGS84 +units=m")) %>%
        gBuffer(byid = TRUE, width = 5000) %>%
        spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      
      leafletProxy('India_map', data = buffers) %>%
        clearGroup(group = 'monkey_buffersTY')  %>%
        addMapPane('monkeyBuff',zIndex = 410) %>%
        addPolygons(
          group = 'monkey_buffersTY',
          color = 'black',
          stroke = TRUE,
          fillOpacity = 0,
          opacity = 0.5,
          weight = 1,
          options = pathOptions(pane = 'monkeyBuff'))
    } else {
      leafletProxy('India_map') %>%
        clearGroup(group = 'monkey_buffersTY')
    }
  })
  
  observe({
    social_health <- input$SocialDataHealth

    if(input$subsetdata){
      if(!is.null(polygons$taluks)&any(polygons$taluks$district==input$DistrictChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$district==input$DistrictChoice,]
        subset <- 'District'
      }
    } else if(input$subsetdatataluk){
      if(!is.null(polygons$taluks)&any(polygons$taluks$tahsil==input$TalukChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$tahsil==input$TalukChoice,]
        subset <- 'Taluk'
      }
    } else {
      subset <- NA
      dTaluks = NULL
    }
    
    loaded_social$health <- gadd_social_points(social_health, 'Health',
                                              loaded_social$health,
                                              subset = subset, taluks = dTaluks)
  })
  
  observe({
    social_forest <- input$SocialDataForest
    
    if(input$subsetdata){
      if(!is.null(polygons$taluks)&any(polygons$taluks$district==input$DistrictChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$district==input$DistrictChoice,]
        subset <- 'District'
      }
    } else if(input$subsetdatataluk){
      if(!is.null(polygons$taluks)&any(polygons$taluks$tahsil==input$TalukChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$tahsil==input$TalukChoice,]
        subset <- 'Taluk'
      }
    } else {
      subset <- NA
      dTaluks = NULL
    }
    
    loaded_social$forest <- gadd_social_points(social_forest, 'Forest',
                                               loaded_social$forest,
                                               subset = subset, taluks = dTaluks)
  })
  
  observe({
    social_tourism <- input$SocialDataTourism
    
    if(input$subsetdata){
      if(!is.null(polygons$taluks)&any(polygons$taluks$district==input$DistrictChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$district==input$DistrictChoice,]
        subset <- 'District'
      }
    } else if(input$subsetdatataluk){
      if(!is.null(polygons$taluks)&any(polygons$taluks$tahsil==input$TalukChoice)){
        dTaluks <- polygons$taluks[polygons$taluks$tahsil==input$TalukChoice,]
        subset <- 'Taluk'
      }
    } else {
      subset <- NA
      dTaluks = NULL
    }
    
    loaded_social$tourism <- gadd_social_points(social_tourism, 'Tourism',
                                                loaded_social$tourism,
                                                subset = subset, taluks = dTaluks)
  })
  
  # State level - create district menu ----
  observe({
    sCh <- input$StateChoice
    updateSelectInput(session, 'DistrictChoice', label = '',
                      choices = states$district[sCh==states$state],
                      selected = states$district[sCh==states$state][1])
    polygons$taluks <- readRDS(file.path(data_path,'districts',paste0(sCh,'.rds')))
  })
  
  # District level - zoom to district and add polygons, find available villages and rasters
  observe({
    dCh <- input$DistrictChoice
    updateSelectInput(session, 'TalukChoice', label = '',
                      choices = taluk_lookup$tahsil[dCh==taluk_lookup$district],
                      selected = taluk_lookup$tahsil[dCh==taluk_lookup$district][1])
    if(!is.null(polygons$taluks)&any(polygons$taluks$district==dCh)){
      output$taluk_info <- renderText({''})
      dTaluks <- polygons$taluks[polygons$taluks$district==dCh,]
      extent <- extent(bbox(dTaluks))
      if(input$showtaluks){
        leafletProxy('India_map', data = dTaluks) %>%
          clearGroup(group = 'taluks') %>%
          addMapPane('taluks',zIndex = 490) %>%
          addPolygons(
            group = 'taluks',
            color = '#a103fc',
            stroke = TRUE,
            fillOpacity = 0,
            opacity = 0.5,
            weight = 1,
            popup = create_popout_taluk(dTaluks),
            options = pathOptions(pane = 'taluks'))%>%
          clearGroup(group = 'forest_district_tiles') %>%
          removeControl('forest_district_tiles')
      } else {
        leafletProxy('India_map') %>%
          clearGroup(group = 'taluks')
      }
      if(input$zoomto){
        leafletProxy('India_map') %>%
          setView(lng = extent[2],
                  lat = mean(extent[3:4]),
                  zoom = 9)
      } else {
        output$taluk_info <-
          renderText({''})
      }
    }
    riskFactors$options <- list.dirs(file.path(data_path,'tiles/district_tiles',dCh),
                         recursive = FALSE, full.names = FALSE) %>% str_replace_all('_',' ')
    updateSelectInput(session, 'forestchoice', label = '',
                      choices = c('<none>', riskFactors$options), selected = '<none>')

  })
  
  observe({
    if(!file.exists(paste0(data_path,'/villages/',input$DistrictChoice,'_villages.rds'))){
      output$warning <- renderText({'No village file available for this district'})
      leafletProxy('India_map') %>%
        clearGroup(group = 'villages')
    } else {
      output$warning <- renderText({''})
      if(input$villages){
        polygons$villages <- readRDS(paste0(data_path,'/villages/',input$DistrictChoice,'_villages.rds'))
        if(input$popdensity){
          colRange <- colorNumeric(palette = 'Greys',
                                   domain = c(min(log(polygons$villages$pop_km2[polygons$villages$pop_km2>0])),
                                              max(log(quantile(polygons$villages$pop_km2,.95)))))
          pal <- function(x,y){
            if(x==0){
              '#FFFFFF'
            } else if(x>=y){
              '#000000'
            } else {
              colRange(log(x))
            }
          }
          cols <- lapply(polygons$villages$pop_km2, pal, quantile(polygons$villages$pop_km2,.95)) %>% unlist()
          opa <- .5
        } else {
          cols <- NULL
          opa <- 0
        }
        leafletProxy('India_map', data = polygons$villages) %>%
          clearGroup(group = 'villages')  %>%
          addMapPane('villages',zIndex = 490) %>%
          addPolygons(
            group = 'villages',
            color = 'black',
            fillColor = cols,
            stroke = TRUE,
            fillOpacity = opa,
            smoothFactor = .5,
            opacity = .5,
            weight = 1,
            popup = create_popout_village(polygons$villages),
            options = pathOptions(pane = 'villages')) %>%
            removeControl('villages')
        if(input$popdensity){
          colValue <- c(0,(quantile(polygons$villages$pop_km2,.95)*c(.05,.1,.2,.4,1)) %>% round(0))
          leafletProxy('India_map') %>%
            addLegend(pal = colorFactor(c(lapply(colValue, pal, quantile(polygons$villages$pop_km2,.95)) %>% unlist()),
                                        levels = c(paste0(c('     ','    ','   ','  ',' ','>'),colValue %>% round(0)))),
                      values = c(paste0(c('     ','    ','   ','  ',' ','>'),c(colValue %>% round(0)))), 
                      title = 'Population Density', 
                      position = 'bottomleft',
                      layerId = 'villages',
                      labFormat = labelFormat(digits = 4,
                                              big.mark = ''))
        }
      } else {
        leafletProxy('India_map') %>%
          clearGroup(group = 'villages') %>%
          removeControl('villages')
      }
      if(input$popdensity&!input$villages){
        updateCheckboxInput(session, 'villages', 'Show locality outlines', value = TRUE)
      }
    }
  })
  
  # District level - cover raster layers ----
  observe({
    dCh <- input$DistrictChoice
    riskFactors$options <- list.dirs(file.path(data_path,'tiles/district_tiles',dCh),
                         recursive = FALSE, full.names = FALSE) %>% str_replace_all('_',' ')
    if(length(riskFactors$options)==0|(!(input$forestchoice %in% riskFactors$options))){
      updateSelectInput(session, 'forestchoice', label = '',
                        choices = c('<none>', riskFactors$options), selected = '<none>')
      output$district_info <- renderText({''})
      leafletProxy('India_map') %>%
        clearGroup(group = 'forest_district_tiles') %>%
        removeControl('forest_district_tiles')
    }
    if(input$forestchoice %in% riskFactors$options){
      native <- file.path(data_path,'tiles/district_tiles',input$DistrictChoice, input$forestchoice %>% str_replace_all(' ','_')) %>%
                  list.dirs(recursive = FALSE, full.names = FALSE) %>% as.numeric() %>% max()
      tiles <- paste0('datatiles/district_tiles/',tolower(input$DistrictChoice),'/',
                      input$forestchoice %>% str_replace_all(' ','_'),'/{z}/{x}/{y}.png')
      output$district_info <- renderUI({
        readLines(file.path(data_path,'tiles/district_tiles',input$DistrictChoice, input$forestchoice %>% str_replace_all(' ','_'),'info.txt'), warn = FALSE) %>%
          paste0(sep = '<br/>') %>% HTML()
      })
      leafletProxy('India_map', session) %>%
        clearGroup(group = 'forest_district_tiles') %>%
        removeControl('forest_district_tiles') %>%
        addMapPane('fdt',zIndex = 480) %>%
        addTiles(urlTemplate = tiles,
                 options = c(tileOptions(opacity = .8, maxZoom = 19, maxNativeZoom = native),
                             pathOptions(pane = 'fdt')),
                 group = 'forest_district_tiles')
      if(file.exists(file.path(data_path,'tiles/district_tiles',input$DistrictChoice, input$forestchoice %>% str_replace_all(' ','_'),'pal.csv'))){
        pal <- read.csv(file.path(data_path,'tiles/district_tiles',input$DistrictChoice, input$forestchoice %>% str_replace_all(' ','_'),'pal.csv'), stringsAsFactors = FALSE)
        leafletProxy('India_map', session) %>%
          addLegend(pal = colorFactor(pal$colour, levels = pal$value),
                    values = pal$value, 
                    title = input$forestchoice, 
                    position = 'bottomleft',
                    layerId = 'forest_district_tiles',
                    labFormat = labelFormat(digits = 4,
                                            big.mark = ''))
      }
    }
  })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  observeEvent(input$uploadCSV, {
    if(input$uploadKey == 'Darshan'){
      if(input$file1Type=='human'){
        if(grepl('HUMAN',input$file1$name)){
          files <- list.files(file.path(data_path,'case_csvs'), pattern = 'HUMAN', full.names = TRUE)
        } else {
          output$uploadReturn <- renderText({'Human data selected, but HUMAN not in filename'})
          return()
        }
      } else {
        if(grepl('TICK',input$file1$name)){
          files <- list.files(file.path(data_path,'case_csvs'), pattern = 'TICK', full.names = TRUE)
        } else {
          output$uploadReturn <- renderText({'Monkey and tick data selected, but TICK not in filename'})
          return()
        }
      }
      file.copy(files,file.path(data_path,'case_csvs','backups'))
      unlink(files)
      file.copy(input$file1$datapath, file.path(data_path,'case_csvs'))
      file.rename(file.path(data_path,'case_csvs',basename(input$file1$datapath)),
                  file.path(data_path,'case_csvs',input$file1$name))
      output$uploadReturn <- renderText({'Correct Key, data uploaded'})
      if(input$file1Type=='human'){
        humanlist <- human_cases()
        points$human <- humanlist$cases
        buffers$human <- humanlist$buffers
        points$humanTY <- points$human[points$human$Year == globalyears,]
        points$humanTY <- points$humanTY[!is.na(points$humanTY$Date.of.Onset),]
        buffers$humanTY <- buffers$human[buffers$human$Year == globalyears,]
        buffers$humanTY <- buffers$humanTY[!is.na(buffers$humanTY$Date.of.Onset),]
      } else {
        ticklist <- tick_monkey_cases()
        points$ticks <- ticklist$tickpoints
        points$monkey <- ticklist$monkeypoints
        buffers$tick <- ticklist$buffers_tick
        buffers$monkey <- ticklist$buffers_monkey
      }
    } else {
      output$uploadReturn <- renderText({'Incorrect Key, no data uploaded'})
    }
  })
})
