# Set up default starting options
risk_options <- list.dirs(file.path(data_path,'tiles/risk_tiles/forest_cover'),
                          full.names = FALSE, recursive = FALSE) %>%
  str_replace_all('_',' ')
farming_options <- list.dirs(file.path(data_path,'tiles/risk_tiles/farming'),
                             full.names = FALSE, recursive = FALSE) %>%
  str_replace_all('_',' ')
district_options <- list.dirs(file.path(data_path,'tiles/district_tiles/SHIMOGA'),
                              recursive = FALSE, full.names = FALSE) %>%
  str_replace_all('_',' ')

# Set up social KGI data
social_data <- read.csv(file.path(data_path,'KGIdata/KGIXML.csv'), stringsAsFactors = FALSE)
rdsKGI <- list.files(file.path(data_path,'KGIdata'), pattern = '\\.rds') %>% str_replace_all('\\.rds','')
csvKGI <- list.files(file.path(data_path,'KGIdata'), pattern = '^KGIcleaned.+\\csv$')
to_create <- csvKGI[!(csvKGI %>% str_replace_all('\\.csv','') %>% str_replace_all('^KGIcleaned_','') %in% rdsKGI)]
tmp <- lapply(to_create, FUN = function(file){
  csv <- read.csv(file.path(data_path,'KGIdata',file), stringsAsFactors = FALSE)
  pts <- SpatialPointsDataFrame(coords = csv %>% filter(!is.na(csv$LONG)) %>% select(LONG, LAT),
                                data = csv %>% filter(!is.na(csv$LONG)) %>% select(-LAT, -LONG),
                                proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  pts %>% saveRDS(file.path(data_path,'KGIdata',
                            file %>% str_replace_all('csv$','rds') %>% str_replace_all('^KGIcleaned_','')),
                  version = 2)
})
rdsKGI <- list.files(file.path(data_path,'KGIdata'), pattern = '\\.rds') %>% str_replace_all('\\.rds','')
