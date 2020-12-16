# Create package list, install all required, and load them up

package.list <- c('shiny','leaflet','raster','colorspace','shinyjs','RColorBrewer','rgdal','curl', 'V8',
                  'shinyBS','rgeos','stringr','htmltools','shinyWidgets', 'tidyr',
                  'leaflet.extras','sf','pbapply','htmlwidgets','dplyr')
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages[1])
success <- lapply(package.list, require, character.only = TRUE)

globalyears <- format(Sys.Date(), "%Y") %>% as.numeric()

# Set data directory
data_path <- 'data'
if(dir.exists("P:/NEC06136_KFD_in_India/Workfiles/WP3b/shiny_app/MFRexplorer")){
  setwd("P:/NEC06136_KFD_in_India/Workfiles/WP3b/shiny_app/MFRexplorer")
  if(Sys.info()['user'] %in% c('tomaug','beth')){
    data_path <- 'P:/NEC06136_KFD_in_India/Workfiles/WP3b/shiny_app/MFRexplorer/data'
  }
}

if(!file.exists(file.path(data_path,'additional','allIndiaElev.rds'))){
  allIndia <-
    SpatialPointsDataFrame(coords = data.frame(Longitude = c(68,68,90,90,68),
                                               Latitude = c(5,26,26,5,5)),
                           data = data.frame(points = c(1:5)),
                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  allIndiaElev <- get_elev_raster(locations = allIndia,
                                  prj = sp::proj4string(allIndia),
                                  z=8)
  saveRDS(allIndiaElev,file.path(data_path,'additional','allIndiaElev.rds'))
}

jsCode <- 'shinyjs.winprint = function(){
window.print();
}'

if(!file.exists(file.path(data_path,'additional/hospitals.rds'))){
  hospital_pts <- read.csv(file.path(data_path, 'additional/PHC Coordinates.csv'))
  hospital_pts <-
    SpatialPointsDataFrame(coords = hospital_pts %>% select(Longitude, Latitude),
                           data = hospital_pts %>% select(-Latitude, -Longitude),
                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  hospital_pts %>% saveRDS(file.path(data_path,'additional/hospitals.rds'), version = 2)
}

basemap <- function(map = NULL, latlng = NULL, zoom = 5, satellite = TRUE, roadmap = TRUE, tiles = NULL){
  if(is.null(map)){
    map <- leaflet()
  }
  if(satellite){
    map <- map %>%
      addMapPane('sat', zIndex = 410) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       layerId = 'satT',
                       options = c(providerTileOptions(opacity = 1),
                                   tileOptions(maxZoom = 19, maxNativeZoom = 17),
                                   pathOptions(pane = 'sat')),
                       group = 'basemap')
  } else {
    map <- map %>% removeTiles('satT')
  }
  
  if(satellite&roadmap){
    # User has asked to see the roadmap AND satellite, so turn down opacity on roadmap
    map <- map %>%
      addMapPane('bm', zIndex = 420) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       layerId = 'basemapT',
                       options = c(providerTileOptions(opacity = .4),
                                   tileOptions(maxZoom = 19),
                                   pathOptions(pane = 'bm')),
                       group = 'basemap')
  } else if(roadmap){
    # User has asked for roadmap and not satellite
    map <- map %>%
      addMapPane('bm', zIndex = 420) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       layerId = 'basemapT',
                       options = c(providerTileOptions(opacity = 1),
                                   tileOptions(maxZoom = 19),
                                   pathOptions(pane = 'bm')),
                       group = 'basemap')
  } else {
    # User has asked for no roadmap
    map <- map %>% removeTiles('basemapT')
  }

  if(!is.null(latlng)) map <- map %>% setView(lng = latlng[2], lat = latlng[1], zoom = zoom)
}

yearsList <- function(years){
  yL <- as.list(years)
  names(yL) <- years %>% as.character()
  yL
}

create_popout_human <- function(point_DF){
  
  paste('<b>Summary of cases and deaths each year</b><br>',
        '<b>within 100m of this site</b><br>',
        point_DF$table, '<br>',
        '<b>Details of most recent case</b><br>',
        '<b>Locality:</b>', point_DF$Locality, '<br>',
        '<b>PHC:</b>', point_DF$PHC, '<br>',
        '<b>State:</b>', point_DF$State, '<br>',
        '<b>District:</b>', point_DF$District, '<br>',
        '<b>Taluk:</b>', point_DF$Taluk, '<br>',
        '<b>Age:</b>', point_DF$Age, '<br>',
        '<b>Sex:</b>', point_DF$Sex, '<br>',
        '<b>Date of onset:</b>', point_DF$Date.of.Onset, '<br>',
        '<b>Year:</b>', point_DF$Year, '<br>',
        '<b>Real-time RT PCR:</b>', point_DF$Real.Time.RT.PCR, '<br>',
        '<b>IgM Elisa:</b>', point_DF$IgM.Elisa)
}

create_buffer <- function(click_point){
  pointbuf <- 
    SpatialPointsDataFrame(coords = data.frame(Longitude = click_point$lng,
                                               Latitude = click_point$lat),
                           data = data.frame(Point = 'Click'),
                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  pointbuf <- pointbuf %>%
    spTransform(CRS("+proj=utm +zone=43 +datum=WGS84 +ellps=WGS84 +units=m")) %>%
    gBuffer(byid = TRUE, width = 100) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  pointbuf
}

within_buffer <- function(pointbuf, points){
  within_buff <- pointbuf@polygons[[1]]@Polygons[[1]]@coords %>% data.frame()
  within_buff <-
    SpatialPolygons(list(Polygons(list(Polygon(hole = FALSE, within_buff)), ID=1)),
                    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  within_buff <- over(points, within_buff) %>% data.frame()
  if(any(!is.na(within_buff[,1]))){
    within_buff <- points[!is.na(within_buff[,1]),] %>% data.frame() %>%
      select(Year) %>% group_by(Year) %>% summarise(Cases = n())
    within_buff$Year <- within_buff$Year %>% round(0) %>% as.character()
  } else {
    within_buff <- NULL
  }
  within_buff
}

points_in_buff <- function(human_points, dTaluks){
  within_buff <- over(human_points, dTaluks) %>% data.frame()
  if(any(!is.na(within_buff[,1]))){
    human_points <- human_points[!is.na(within_buff[,1]),]
  } else {
    human_points <- human_points[0,]
  }
  human_points
}

create_popout_tick <- function(point_DF){
  
  paste('<b>Locality:</b>', point_DF$Place.Name, '<br>',
        '<b>Taluk:</b>', point_DF$Taluk, '<br>',
        '<b>Date:</b>', point_DF$Date, '<br>',
        '<b>Host:</b>', point_DF$Host, '<br>')
  
}

create_popout_monkey <- function(point_DF){
  
  paste('<b>Locality:</b>', point_DF$Place.Name, '<br>',
        '<b>Taluk:</b>', point_DF$Taluk, '<br>',
        '<b>Date:</b>', point_DF$Date, '<br>',
        '<b>Host:</b>', point_DF$Host, '<br>')
  
}

create_popout_village <- function(poly_DF){
  
  paste('<b>Locality:</b>', poly_DF$NAME, '<br>',
        '<b>District:</b>', poly_DF$DISTRICT, '<br>',
        '<b>Population:</b>', poly_DF$POPULATION, '<br>',
        '<b>Area:</b>', poly_DF$Area, '<br>',
        '<b>Pop Density:</b>', poly_DF$pop_km2, '<br>',
        '<b>Cattle:</b>', poly_DF$cbcattle, '<br>')
  
}

create_popout_hospital <- function(point_DF){
  
  paste('<b>Name:</b>', point_DF$Name, '<br>',
        '<b>Location:</b>', point_DF$Location, '<br>')
  
}

create_popout_taluk <- function(poly_DF){
  
  paste('<b>Taluk:</b>', poly_DF$tahsil, '<br>',
        '<b>District:</b>', poly_DF$district, '<br>',
        '<b>Population:</b>', poly_DF$tot_p, '<br>')
  
}

create_popout_social <- function(point_DF){
  
  paste(point_DF[[2]], '<br>')

}

# State lookup table to find all districts in each state
states <- read.csv(file.path(data_path,'additional/district_lookup.csv'), stringsAsFactors = FALSE)
taluk_lookup <- readRDS(file.path(data_path,'districts/taluk_lookup.rds'))

# Source KGI data
rdsKGI <- list.files(file.path(data_path,'KGIdata'), pattern = '\\.rds') %>% str_replace_all('\\.rds','')
social_data <- read.csv(file.path(data_path,'KGIdata/KGIXML.csv'))

gadd_social_points <- function(inputs, types, loaded_social, subset = NA, taluks = NULL){
  for(socials in inputs){
    if((!(socials %in% loaded_social))|(!(is.na(subset)))){
      row_social <- social_data %>% filter(name == socials)
      Icon <- makeIcon(iconUrl = paste0('icons/KGI/',row_social$subcat,'.png'),
                       iconWidth = 24, iconHeight = 30, iconAnchorX = 12, iconAnchorY = 30)
      html_legend <- paste0("<img src=icons/KGI/",row_social$subcat,".png",
                            "\nstyle='width:18px;height:24px;'> ",socials,"<br/>")
      row_social <- social_data %>% filter(name == socials)
      file_name <- paste0(row_social$category,'-',row_social$subcat,'.rds')
      socials_points <- readRDS(file.path(data_path,'KGIdata',file_name))
      if(!is.na(subset)){
        if(!is.null(taluks)){
          #taluks <- readRDS(file.path(data_path,'districts',paste0('KARNATAKA','.rds')))
          #taluks <- taluks[taluks$district=='SHIMOGA',]
          socials_points <- points_in_buff(socials_points, taluks)
        }
      }
      
      leafletProxy('India_map', data = socials_points) %>%
        clearGroup(group = socials) %>%
        removeControl(paste0(socials,'_legend')) %>%
        addMapPane('tour',zIndex = 480) %>%
        addMarkers(
          group = socials,
          icon = Icon,
          options = pathOptions(pane = 'tour'),
          popup = create_popout_social(socials_points)) %>%
        addControl(layerId = paste0(socials,'_legend'), html = html_legend, position = 'bottomleft')
    }
  }

  name_social <- social_data %>% filter(type == types) %>% pull(name)
  for(socials in name_social[!(name_social %in% inputs)]){ 
    row_social <- social_data %>% filter(name == socials)
    leafletProxy('India_map') %>%
      clearGroup(group = socials) %>%
      removeControl(paste0(socials,'_legend'))
  }
  return(inputs)
}

social_choices <- list(
  social_data %>%
    filter(subcat %in%
             (rdsKGI[grepl('HealthAssets|AHVSAssets',rdsKGI)] %>%
                str_replace_all('GetNearby[A-z]+Assets-',''))) %>%
    pull(name),
  social_data %>%
    filter(subcat %in%
             (rdsKGI[grepl('TourismAssets',rdsKGI)] %>%
                str_replace_all('GetTourismAssets-',''))) %>%
    pull(name),
  social_data %>%
    filter(subcat %in%
             (rdsKGI[grepl('ForestAssets',rdsKGI)] %>%
                str_replace_all('GetNearbyForestAssets-',''))) %>%
    pull(name))
names(social_choices) <- c('health','tourism','forest')

human_subset <- function(human){
  if(nrow(human)==0){
    hSTable <-
      '<table style="width:100%"><tr><th>Year</th><th>Cases</th><th>Deaths</th></tr></table>'
  } else {
    human$Death <- ifelse(human$Death=='death',1,0)
    human <- human %>% group_by(Year) %>%
      summarize(Cases = n(), Deaths = sum(Death)) %>% arrange(Year)
    hSTable <-
      lapply(1:nrow(human), FUN = function(rown){
          paste0('<tr><th>',human$Year[rown],'</th>',
                 '<th>',human$Cases[rown],'</th>',
                 '<th>',human$Deaths[rown],'</th></tr>')
        }) %>% unlist() %>% paste0(collapse = '')
    hSTable <-
      paste0('<table style="width:100%"><tr><th>Year</th><th>Cases</th><th>Deaths</th></tr>',
             hSTable,'</table>')
  }
  hSTable
}

# This gets the username on datalabs
username_from_jwt <- function(input) {
  result = tryCatch({
    # This is the public key for DataLabs auth service, it's important to verify
    # this signature to ensure users aren't generating their own identity
    pubkey <- '-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAzetfcgaITaJWpKd9SW83
OJiOsy6tyB56NHwJ5MjX6NfzHCJqPRtouQ3WCgTu21aSrZxlxHvvIZwIPGPKxTzN
DOIyQ+hNtaBhn4PjcgjpGsgpRx/9dWqCr2HP1iXnTh39kBjbdB4ngJXvVnZ7KITm
hJdePM9Cb8jSd3wioaoL4pslCGkipfZPjoWc1D8u83ZotE/p1laip6A0LO3JNSgO
gMbgMkdUMLMu9FCxLTrvpBIib0hR39MWcdN9USVJmT3F3yagvFJ6pVOjNwRbpHVc
KzL/aKOnNqCnvWLswQiyxDt/XyVclTy+gm7cI5ThvzpO7t+FWYNnqxn00eUW0+nU
LQIDAQAB
-----END PUBLIC KEY-----'
    # Retrieve the cookie and validate signature
    result <- jwt_decode_sig(input, pubkey=pubkey)
    text <- paste(result['sub'])
  }, error = function(error_condition) {
    text <- 'N/A'
  })
  return(result)
}