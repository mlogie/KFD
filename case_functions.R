# Create any new tick, monkey and human data
tick_monkey_cases <- function(){
  files <- list.files(file.path(data_path,'case_csvs'), pattern = 'TICK',full.names = TRUE)
  dates <- file.info(files)$mtime
  currentData <- files[which(dates == max(dates))]
  tick_monkey_pts <- read.csv(currentData)
  tick_monkey_pts$icon <- paste0(tolower(tick_monkey_pts$Host),
                                 tick_monkey_pts$year,
                                 '.png')
  tick_monkey_pts <-
    SpatialPointsDataFrame(coords = tick_monkey_pts %>% filter(!is.na(tick_monkey_pts$Longitude)) %>% select(Longitude, Latitude),
                           data = tick_monkey_pts %>% filter(!is.na(tick_monkey_pts$Longitude)) %>% select(-Latitude, -Longitude),
                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  buffers <- tick_monkey_pts %>%
    spTransform(CRS("+proj=utm +zone=43 +datum=WGS84 +ellps=WGS84 +units=m")) %>%
    gBuffer(byid = TRUE, width = 5000) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  tick_points <- tick_monkey_pts[tick_monkey_pts$Host == 'Tick',]
  tick_points %>%
    saveRDS(file.path(data_path,'cases/tick_cases/tick_pts.rds'), version = 2)
  monkey_points <- tick_monkey_pts[tick_monkey_pts$Host == 'Monkey',]
  monkey_points %>%
    saveRDS(file.path(data_path,'cases/monkey_cases/monkey_pts.rds'), version = 2)
  buffers_tick <- buffers[buffers$Host == 'Tick',]
  buffers_tick %>%
    saveRDS(file.path(data_path,'cases/tick_cases/buffers.rds'), version = 2)
  buffers_monkey <- buffers[buffers$Host == 'Monkey',]
  buffers_monkey %>%
    saveRDS(file.path(data_path,'cases/monkey_cases/buffers.rds'), version = 2)
  
  return(list(tickpoints = tick_points, monkeypoints = monkey_points,
              buffers_tick = buffers_tick, buffers_monkey = buffers_monkey))
}

human_cases <- function(){
  filesKar <- list.files(file.path(data_path,'case_csvs'), pattern = 'HUMAN',full.names = TRUE)
  filesAll <- list.files(file.path(data_path,'case_csvs'), pattern = 'Coord_regional_models',full.names = TRUE)
  datesKar <- file.info(filesKar)$mtime
  datesAll <- file.info(filesKar)$mtime
  currentDataKar <- filesKar[which(datesKar == max(datesKar))]
  currentDataAll <- filesAll[which(datesAll == max(datesAll))]
  human_ptsKar <- read.csv(currentDataKar)
  human_ptsAll <- read.csv(currentDataAll)
  human_ptsKar$Date.of.Onset <- human_ptsKar$Date.of.Onset %>% as.Date('%d/%m/%Y')
  yearnames <- str_extract(names(human_ptsAll),'[0-9]{4}') %>% .[!is.na(.)]
  human_ptsAll <- gather(human_ptsAll, Year, cases, paste0('X',yearnames)) %>% filter(cases != 0)
  human_ptsAll$Year <- substr(human_ptsAll$Year,2,5) %>% as.numeric()
  human_ptsKar <- human_ptsKar %>% select(Locality, PHC, District, Taluk, Age, Sex, Date.of.Onset,
                                          Real.Time.RT.PCR, IgM.Elisa, onset_month, Longitude, Latitude,
                                          Year, Death)
  human_ptsKar$onset_month <- substr(as.character(human_ptsKar$Date.of.Onset),6,7)
  yearTmp <- as.numeric(substr(as.character(human_ptsKar$Date.of.Onset),1,4))
  human_ptsKar$Year[!is.na(yearTmp)] <- yearTmp[!is.na(yearTmp)]
  human_ptsKar$Year[human_ptsKar$onset_month %in% c('10','11','12')] <-
    human_ptsKar$Year[human_ptsKar$onset_month %in% c('10','11','12')] + 1
  human_ptsKar$State <- 'Karnataka'
  human_ptsKar <- data.frame(lapply(human_ptsKar, as.character), stringsAsFactors=FALSE)
  human_ptsAll <- data.frame(Locality = human_ptsAll$Place.name, PHC = '', District = '',
                             Taluk = human_ptsAll$Taluk, Age = '', Sex = '', Date.of.Onset = '',
                             Real.Time.RT.PCR = '', IgM.Elisa = '', onset_month = '',
                             Longitude = human_ptsAll$Longitude, Latitude = human_ptsAll$Latitude,
                             Year = human_ptsAll$Year, State = human_ptsAll$State, Death = 'unknown',
                             stringsAsFactors = FALSE)
  human_ptsKar$Latitude <- as.numeric(human_ptsKar$Latitude)
  human_ptsKar$Longitude <- as.numeric(human_ptsKar$Longitude)
  human_ptsKar$Locality <- as.character(human_ptsKar$Locality)
  human_ptsKar$Taluk <- as.character(human_ptsKar$Taluk)
  human_ptsKar$State <- as.character(human_ptsKar$State)
  human_ptsKar$Year <- as.numeric(human_ptsKar$Year)
  human_ptsAll$Latitude <- as.numeric(human_ptsAll$Latitude)
  human_ptsAll$Longitude <- as.numeric(human_ptsAll$Longitude)
  human_ptsAll$Locality <- as.character(human_ptsAll$Locality)
  human_ptsAll$Taluk <- as.character(human_ptsAll$Taluk)
  human_ptsAll$State <- as.character(human_ptsAll$State)
  human_pts <- bind_rows(human_ptsKar, human_ptsAll)
  human_pts <- human_pts[!is.na(human_pts$Year),]
  human_pts$Death <- tolower(human_pts$Death)
  human_pts$onset_month <- as.numeric(human_pts$onset_month)
  human_pts <- human_pts %>% filter(!is.na(human_pts$Latitude)|!is.na(human_pts$Year))
  # Remove duplicated rows from new table
  human_pts <- human_pts %>% 
    filter(!(duplicated(human_pts %>% select(Latitude, Longitude, Year, Date.of.Onset))&
               ((nchar(human_pts$Date.of.Onset)==0)&(nchar(human_pts$Sex)==0))))
  human_pts$icon <- paste0('human', human_pts$Year, '.png')
  human_pts <- human_pts %>% filter(Longitude > 60 & Longitude < 95) %>% filter(Latitude > 0 & Latitude < 50)
  human_pts$ID <- 1:nrow(human_pts)
  human_pts <-
    SpatialPointsDataFrame(coords = human_pts %>% filter(!is.na(human_pts$Longitude)) %>% select(Longitude, Latitude),
                           data = human_pts %>% filter(!is.na(human_pts$Longitude)) %>% select(-Latitude, -Longitude),
                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  buffers <- human_pts %>%
    spTransform(CRS("+proj=utm +zone=43 +datum=WGS84 +ellps=WGS84 +units=m")) %>%
    gBuffer(byid = TRUE, width = 5000) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  buffers100 <- human_pts %>%
    spTransform(CRS("+proj=utm +zone=43 +datum=WGS84 +ellps=WGS84 +units=m")) %>%
    gBuffer(byid = TRUE, width = 100) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  within_buffers <- pblapply(1:length(buffers100), FUN = function(i){
    within_buff <- buffers100@polygons[[i]]@Polygons[[1]]@coords %>% data.frame()
    within_buff <-
      SpatialPolygons(list(Polygons(list(Polygon(hole = FALSE, within_buff)), ID=1)),
                      proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    within_buff <- over(human_pts, within_buff) %>% data.frame()
    if(any(!is.na(within_buff[,1]))){
      within_buff <- human_pts[!is.na(within_buff[,1]),] %>% data.frame() %>%
        select(Year, Death)
      within_buff$Death[within_buff$Death!='death'] <- 0
      within_buff$Death[within_buff$Death=='death'] <- 1
      within_buff$Death <- as.numeric(within_buff$Death)
      within_buff <- within_buff %>% group_by(Year) %>%
        summarise(Cases = n(), Deaths = sum(Death)) %>% data.frame()
    } else {
      within_buff <- NULL
    }
    within_buff
  })

  human_pts$table <-
    lapply(human_pts@data$ID, FUN = function(ID){
      lapply(1:nrow(within_buffers[[ID]]), FUN = function(rown){
        paste0('<tr><th>',within_buffers[[ID]]$Year[rown],'</th>',
               '<th>',within_buffers[[ID]]$Cases[rown],'</th>',
               '<th>',within_buffers[[ID]]$Deaths[rown],'</th></tr>')
      }) %>% unlist() %>% paste0(collapse = '')
    }) %>% unlist()
  human_pts$table <-
    paste0('<table style="width:100%"><tr><th>Year</th><th>Cases</th><th>Deaths</th></tr>',
           human_pts$table,'</table>')
  
  within_buffers %>% saveRDS(file.path(data_path,'cases/human_cases/within_buffers.rds'), version = 2)
  human_pts %>% saveRDS(file.path(data_path,'cases/human_cases/human_pts.rds'), version = 2)
  buffers %>% saveRDS(file.path(data_path,'cases/human_cases/buffers.rds'), version = 2)
  return(list(cases = human_pts, buffers = buffers))
}
