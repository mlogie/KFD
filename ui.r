source('functions.R')
source('case_functions.R')
source('setup_functions.R')
library('htmlwidgets')

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

jsCode <- 'shinyjs.getCookie = function() {
   var auth_cookie = Cookies.get(\'authorization\'); 
   Shiny.onInputChange("authorization_cookie", auth_cookie);
}' 

shinyUI(
  # Hide the upload tab by default
  navbarPage('Kyasanur Forest Disease (KFD) Risk Explorer', id='nav',
    tabPanel('India map',
      div(class='outer', tags$head(
        includeCSS('styles.css'),
        useShinyjs(),
        extendShinyjs(text = jsCode),
        tags$script(src="js.cookie.min.js"),
          tags$style(
            HTML(
              ".checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
               }
               .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
               }
               #nav li a[data-value = 'Upload Files']{
                    display: none;
               }
               #mouseCoords {opacity : 0.65; border: 3px solid #FFFFFF; background: #FFFFFF}"
            )
          )),
        leafletOutput('India_map', width='100%', height='100%'),
        fixedPanel(id = 'mouseCoords', bottom = 7, left = 70,
                   htmlOutput("mouseCoords")),
        #fixedPanel(id = 'print', top = 112, left = 70, useShinyjs(),
        #           bsButton('hideAll', label = 'Hide All Menus', style = 'primary', size = 'small') %>%
        #             popify(title = '', placement = 'right', content = 'Click to hide/unhide all menus',
        #                    trigger = 'hover',  options = list(container = 'body'))),
        fixedPanel(id = 'mapsonoff', top = 100, left = 50,
                   fluidRow(column(12,
                     bsButton(inputId = 'satellite', label = 'Satellite', style = 'success',
                              icon = icon('globe-asia', 'fa-2x')) %>%
                       popify(title = '', placement = 'bottom',
                              content = 'Toggle on/off satellite base map',
                              trigger = 'hover',  options = list(container = 'body')),
                     bsButton(inputId = 'roadmap', label = 'Basemap', style = 'success',
                              icon = icon('map', 'fa-2x')) %>%
                       popify(title = '', content = 'Toggle on/off road and towns base map',
                              placement = 'right', trigger = 'hover',
                              options = list(container = 'body'))))),
        fixedPanel(id = 'backupmaps', top = 150, left = 50,
                   fluidRow(column(12,
                     bsButton('backupSatellite', label = 'Backup Sat', style = 'warning', size = 'small') %>%
                     popify(title = '', placement = 'bottom',
                            content = 'Click to add backup satellite images in the case of no internet connection',
                            trigger = 'hover',  options = list(container = 'body')),
                     bsButton('backupBasemap', label = 'Backup Map', style = 'warning', size = 'small') %>%
                       popify(title = '', placement = 'bottom',
                              content = 'Click to add backup basemap in the case of no internet connection',
                              trigger = 'hover',  options = list(container = 'body'))))),
        fixedPanel(id = 'google', top = 240, left = 40,
                   fluidRow(column(12,
                     uiOutput('google') %>%
                       popify(title = '', content = 'Show google directions between two selected points',
                              placement = 'right', trigger = 'hover',  options = list(container = 'body'))))),
        useShinyjs(),
        fixedPanel(id = 'topLevelButtons', top = 60, left = 50,
          fluidRow(column(12,
          bsButton('hideAll', label = 'Hide/Show Menus', style = 'primary', size = 'small') %>%
            popify(title = '', placement = 'right', content = 'Click to hide/unhide all menus',
                   trigger = 'hover',  options = list(container = 'body')),
          bsButton('clearAll', label = 'Clear All Data', style = 'danger', size = 'small') %>%
            popify(title = '', placement = 'bottom', content = 'Click to clear all selected data',
                   trigger = 'hover',  options = list(container = 'body'))))),
        absolutePanel(id = 'popupInfo', class = 'panel panel-default', fixed = TRUE,
                      draggable = TRUE, top = 120, left = 110, right = 'auto', bottom =  'auto',
                      width = 360, height = 0,
                      bsCollapse(id = 'puInfo', open = '',
                                 bsCollapsePanel('KFD Burdens','Here is some text on KFD burdens',
                                                 style = 'warning'),
                                 bsCollapsePanel('Who gets KFD?','Here is some text on who gets KFD',
                                                 style = 'warning'),
                                 bsCollapsePanel('Risk Factors', 'Here is some text on risky activites, season and habitats',
                                                 style = 'warning'),
                                 bsCollapsePanel('Management for KFD','Here is some text on KFD management',
                                                 style = 'warning'),
                                 bsCollapsePanel('Surveillance Protocols', 'Here is some text on surveillance protocols',
                                                 style = 'warning'))) %>%
          hidden(),
        absolutePanel(
          id = 'controls', class = 'panel panel-default', fixed = TRUE,
          draggable = TRUE, top = 60, left = 'auto', right = 20, bottom =  'auto',
          width = 300, height = '90%', style = 'overflow-y: scroll;',
          bsCollapse(id = 'layers', open = 'This Year Human Cases', multiple = TRUE,
            bsCollapsePanel('Past Human Cases', style = 'success',
              h4('Past Human Cases'),
              h5(paste0('Please note that the KFDExplorer works in KFD "outbreak years" rather ',
                        'than "calendar years" where the transmission season goes from 1st ',
                        'November - 31st May each year. For example, the 2019 tick box will ',
                        'bring up data for the 2018/2019 outbreak year, spanning from 1st ',
                        'November 2018 to 31st May 2019')),
              checkboxGroupInput('HumanCases', label = NULL,
                                 inline = TRUE,
                                 choices = yearsList(2014:(globalyears-1)),
                                 selected = NULL),
              actionLink('selectallHC','Select/Deselect All'),
              checkboxInput('HumanfiveKM', label = 'Show 5km buffer', value = FALSE),
              bsButton('HCBox', label = NULL, icon = icon('question'), style = 'info', size = 'small') %>%
                 popify(title = '', placement = 'bottom', 
                 content = 'Management in KFD-affected areas, both vaccination and awareness raising, occurs within 5km zone around human cases, ticks and monkeys in the current and preceding 4 years',
                 trigger = 'hover',  options = list(container = 'body'))),
            bsCollapsePanel('Past Tick/Monkey Positives', style = 'success',
              h4('Past Tick Positives'),
              checkboxGroupInput('TickCases', label = NULL,
                                 inline = TRUE ,
                                 choices = yearsList(2014:(globalyears-1)),
                                 selected = NULL),
              actionLink('selectallTC','Select/Deselect All'),
              checkboxInput('TickfiveKM',
                            label = 'Show 5km buffer',
                            value = FALSE),
              h4('Past Monkey Positives'),
              checkboxGroupInput('MonkeyCases', label = NULL,
                                 inline = TRUE ,
                                 choices = yearsList(2014:(globalyears-1)),
                                 selected = NULL),
              actionLink('selectallMC','Select/Deselect All'),
              checkboxInput('MonkeyfiveKM',
                            label = 'Show 5km buffer',
                            value = FALSE)),
            bsCollapsePanel('This Year Human Cases', style = 'success',
              h4('This Year Human Cases, by Month'),
              dateRangeInput("casesDates",
                             "Dates:",
                             start = as.Date('2020-11-01',"%Y-%m-%d"),
                             end = as.Date('2021-04-01',"%Y-%m-%d"),
                             min = as.Date("2020-11-01","%Y-%m-%d"),
                             max = as.Date("2021-12-31","%Y-%m-%d")),
              actionLink('selectall','Fully expand/contract date range'),
              checkboxInput('casesOn', label = 'Show Cases', value = TRUE),
              checkboxInput('HumanfiveKMTY',
                            label = 'Show 5km buffer',
                            value = FALSE)),
            bsCollapsePanel('This Year Tick/Monkey Positives', style = 'success',
              h4('This Year Tick Positives'),
              checkboxGroupInput('TickCasesTY', label = NULL,
                                 inline = TRUE ,
                                 choices = yearsList(globalyears),
                                 selected = NULL),
              checkboxInput('TickfiveKMTY',
                            label = 'Show 5km buffer',
                            value = FALSE),
              h4('This Year Monkey Positives'),
              checkboxGroupInput('MonkeyCasesTY', label = NULL,
                                 inline = TRUE ,
                                 choices = yearsList(globalyears),
                                 selected = NULL),
              checkboxInput('MonkeyfiveKMTY',
                            label = 'Show 5km buffer',
                            value = FALSE)),
            bsCollapsePanel('Risk Maps and Risk Factors', style = 'success',
              h4('Predictive Risk Maps'),
              checkboxInput('PredictedRiskIndia',
                            label = 'Predictive Risk Map (2014-2019 KFD Data)',
                            value = FALSE),
              checkboxInput('PredictedRiskIndia2020',
                            label = 'Unvalidated Predictive Risk Map (2014-2020 KFD Data)',
                            value = FALSE),
              htmlOutput('predicted_risk_info'),
              h4(''),
              htmlOutput('predicted_risk_info2020'),
              h4('Forest Metrics'),
              selectInput('rasterchoice', label = NULL, multiple = FALSE,
                          choices = c('<none>', c(risk_options)),
                          selected = '<none>'),
              htmlOutput('risk_info'),
              h4('Agriculture'),
              selectInput('farmingCover', label = NULL, multiple = FALSE,
                          choices = c('<none>', farming_options),
                          selected = '<none>'),
              htmlOutput('farming_risk_info'),
              h4('Elevation'),
              checkboxInput('elevation', label = 'Show Elevation Map', value = FALSE),
              htmlOutput('elevation_info')),
            bsCollapsePanel('Health, Environment and Tourism', style = 'success',
              h4('Health Data'),
              checkboxGroupInput('SocialDataHealth', label = NULL,
                                 inline = TRUE ,
                                 choices = social_choices$health,
                                 selected = NULL),
              h4('Environment Data'),
              checkboxGroupInput('SocialDataForest', label = NULL,
                                 inline = TRUE ,
                                 choices = social_choices$forest,
                                 selected = NULL),
              h4('Tourism Data'),
              checkboxGroupInput('SocialDataTourism', label = NULL,
                                 inline = TRUE ,
                                 choices = social_choices$tourism,
                                 selected = NULL)),
            bsCollapsePanel('District Selector', style = 'primary',
              h5(paste0('This menu allows you to select the KFD,',
                        ' tick and monkey data (as well as the risk layers and ',
                        'health, environment and tourism data) by district or ',
                        'taluk within your state. It produces annual summaries',
                        ' of the KFD case data within the selected region')),
              h4('State'),
              selectInput(inputId = 'StateChoice', label = NULL,
                          choices = unique(states$state),
                          selected = 'KARNATAKA', multiple = FALSE),
              h4('District'),
              selectInput(inputId = 'DistrictChoice', label = NULL,
                          choices = states$district[states$state=='KARNATAKA'],
                          selected = 'SHIMOGA', multiple = FALSE),
              h4('Taluk'),
              selectInput(inputId = 'TalukChoice', label = NULL,
                          choices = taluk_lookup$tahsil[taluk_lookup$district=='SHIMOGA'],
                          selected = 'Sagara', multiple = FALSE),
              checkboxInput('zoomto',
                            label = 'Zoom to District',
                            value = FALSE),
              checkboxInput('showtaluks',
                            label = 'Show Taluks',
                            value = FALSE),
              checkboxInput('subsetdata',
                            label = 'Subset data to district',
                            value = FALSE),
              checkboxInput('subsetdatataluk',
                            label = 'Subset data to taluk',
                            value = FALSE),
              h4(''),
              textOutput('taluk_info'),
              checkboxInput('villages',
                            label = 'Show village/town outlines',
                            value = FALSE),
              'Warning: there are a lot of village/town outlines so these can take a while to load',
              checkboxInput('popdensity',
                            label = 'Colour villages and towns by human population density from the 2011 census',
                            value = FALSE),
              h4(''),
              h5('Below is a table summary of case data over time for the region selected (all India if no region is selected'),
              htmlOutput('hSTable')
              ),
             bsCollapsePanel('District Risk Maps and Risk Factors', style = 'primary',
              'For Shivamogga district in Karnataka, Sindhudurg district in Maharashtra and Wayanad district in Kerala, some additional maps of land use types, forest loss and predicted risk of human KFD cases are available',
              h4(''),
              selectInput('forestchoice',
                          label = NULL,
                          multiple = FALSE,
                          choices = c('<none>', district_options),
                          selected = '<none>'),
              htmlOutput('district_info')))))),
    tabPanel('About KFD Explorer',
      fluidRow(
        column(width = 10,
               bsCollapse(id = 'aboutKFD', open = 'About KFD Explorer', multiple = TRUE,
                 bsCollapsePanel(
                   title = 'About KFD Explorer', style = 'success',
                         HTML('<b>Funding:</b><br>The KFDExplorer was developed through an Indo-UK project called MonkeyFeverRisk (that was supported by the Global Challenges Research Fund and funded by the Medical Research Council, Arts and Humanities Research Council, Biotechnology and Biological Sciences Research Council, the Economic and Social Research Council and the Natural Environment Research Council, United Kingdom <i>[grant number MR/P024335/1]</i>).<br><br>'),
                         img(src='fundersNERC.png', align = "left", height="100%", width="100%"),
                         HTML('<br><br>Additional support to refine and pilot the Shiny App tool and develop the linked Phone App and KOBO dashboard were provided by the UK Natural Environment Research Council under the SUNRISE project <i>[grant number NE/R000131/1]</i>.<br>'),
                         img(src='funders.png', align = "left", height="100%", width="100%"),
                         HTML('<br><br>The project partner institutes are listed above and further information on the MonkeyFeverRisk project can be found at <a href="https://www.monkeyfeverrisk.ceh.ac.uk/" rel="noreferrer">https://www.monkeyfeverrisk.ceh.ac.uk/</a>'),
                        
                         HTML('<br><br><b>How the tool was produced:</b><br>The KFDExplorer tool was co-produced with stakeholders the public health, animal health, agriculture, forestry and social welfare sectors, including state and district level disease managers and policy makers from KFD-affected areas of Karnataka, Maharashtra and Kerala. We aimed to produce decision support tools and guidance, based on improved scientific understanding of risk factors, and tailored to the needs and knowledge of key decision makers. The MonkeyFeverRisk project team are extremely grateful for the knowledge and experience contributed by these stakeholders.'),
                         HTML('<br>You can find out more about the co-production process here. <a href="https://www.monkeyfeverrisk.ceh.ac.uk/decision-support-tools-and-risk-guidance" rel="noreferrer">https://www.monkeyfeverrisk.ceh.ac.uk/decision-support-tools-and-risk-guidance</a>'),
                         HTML('<br><br><b>Acknowledgements:</b><br>We are grateful for the kind support of the Department of health and family welfare services, Government of Karnataka, including for permission to access and integrate key government datasets, and also to the Karnataka State Remote Sensing Applications Centre for facilitating access to these.'),
                         HTML('<br><br><b>Further information:</b><br>For further information or to give your feedback on the KFDExplorer tool, please contact:<br>'),
                         HTML('Dr Darshan Naryanaswamy (darshan.bio@gmail.com), Virus Diagnostic Laboratory, DHFWS, Shivamogga, who is administering the tool to users on behalf of the current Deputy Director<br>'),
                         HTML('Dr Bethan Purse (beth@ceh.ac.uk), UK Centre for Ecology and Hydrology, the Co-Principal Investigator of MonkeyFeverRisk.')
               ),
               bsCollapsePanel(
                 title = 'Additional Information regarding data sources', style = 'success',
                         HTML('<b>Risk maps and risk factors</b>'),
                         HTML('<br><br>The <b>Predictive risk maps</b> at district and India scale were developed by the MonkeyFeverRisk project team at ~2km grid square resolution. Please see Purse et al. (2020) for more details on our methods at district level.'),
                         HTML('<br><br>The <b>Forest metrics</b> at India scale were taken from the following sources:'),
                         HTML('<br><br><b>Forest loss (global)</b>: resolution of ~30m grid cells, depicts pixels in which forest was lost between 2000 and 2014 in India from Hansen et al. (2013).'),
                         HTML('<br><br><b>Deciduous Forest, Broad-leaved Deciduous Forest, Broad-leaved Evergreen Forest, Needle-leaved Evergreen Forest</b>:<br>The presence of these land use types are extracted from annual land-use land cover maps from the European Space Agency’s Climate Change Initiative (Defourny et al. 2017). The year depicted is 2015 and the resolutions is 300m grid cells.'),
                         HTML('<br><br><b>Agriculture </b>'),
                         HTML('<br><br>Cattle density (Indian Census) and Buffalo density (Indian Census) were created by rasterizing Indian Government Census data provided by the Department of Animal Husbandry, Dairying and Fisheries at village level to a 2km grid square resolution, over Maharashtra, Kerala, Karnataka and Goa states.'),
                         HTML('<br><br>Cattle density (Global) and Buffalo density (Global) are taken from Global Livestock of the World Layers from Robinson et al. (2014) due to their wider coverage across India, and have an at a resolution of 0.00833333 decimal degrees (approx 1km at the equator). It should be noted that these correlate relatively weekly (r=0.2-0.3) with Indian Government census densities at village level.'),
                         HTML('<br><br><b>Human population</b>'),
                         HTML('<br><br>Human population density is depicted as the total number of people per grid square at a resolution of 0.00833333 decimal degrees (approx 1km at the equator) and is taken from the Asia Continental Population Dataset v2 produced by WORLDPOP as described at this URL: <a href="https://www.worldpop.org.uk/methods" rel="noreferrer">http://www.worldpop.org.uk/methods</a>'),
                         HTML('<br><br><b>Climate and Topography</b>'),
                         HTML('<br><br>The <b>Elevation</b> data are in metres above sea level on a 90m grid square resolution and are taken from Jarvis at al. (2008).'),
                         HTML('<br><br><b>Climate data</b> are bio-climatic variables taken from Fick & Hijmans (2017) accessible at: <a href="https://www.worldclim.org/data/worldclim21.html" rel="noreferrer">https://www.worldclim.org/data/worldclim21.html</a>'),
                         HTML('<br><br><b>Health, Environment and Tourism</b>'),
                         HTML('<br><br>All of these datasets were kindly provided by Karnataka State Remote Sensing Applications Centre, through their K-GIS platform, with the kind permission of the Karnataka State Health and Forestry Departments.'),
                         HTML('<br><br><b>District Selector</b>'),
                         HTML('<br><br>The village-level vector polygon data were kindly provided by Karnataka State Remote Sensing Applications Centre, through their K-GIS platform, with the kind permission of the Karnataka State Health and Forestry Departments. The associated village population data are drawn from the Indian Government Census 2011.'),
                         HTML('<br><br>The taluk level vector polygon data were taken from Community Created Maps of India (http://projects.datameet.org/maps/) shared under [Creative Commons Attribution206 ShareAlike 2.5 India](http://creativecommons.org/licenses/by-sa/2.5/in/) license.'),
                         HTML('<br><br>The state and district level vector polygon data were taken from the Hindustan Times Lab dataset, available at: https://github.com/HindustanTimesLabs/shapefiles/tree/master/india'),
                         HTML('<br><br><b>District risk maps and risk factors</b>'),
                         HTML('<br><br><b>Predictive risk maps</b>'),
                         HTML('<br><br>The <b>predictive risk maps</b> for Shivamogga District were developed by the MonkeyFeverRisk project team at ~2km grid square resolution. Please see Purse et al. (2020) for more details on our methods at district level.'),
                         HTML('<br><br><b>Land Cover Land Use maps</b>'),
                         HTML('<br><br>District level Land Cover Land Use maps are available for Sindhudurg District, Maharashtra, Wayanad District, Kerala, and Shivamogga District, Karnataka at ~30m grid cell resolution from the MonkeyFeverRisk Project. These show the distribution of key forest and plantation types, agricultural and fallow areas, water-bodies and build-up areas.'),
                         HTML('<br><br><b>References</b>'),
                         HTML('<br><br>Defourny P, Boettcher M, Bontemps S, Kirches G, Lamarche C, Peters M, Santoro M, Schlerf M (2016) Land cover cci Product user guide version 2. Technical report, European Space Agency.'),
                         HTML('<br><br>Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial resolution climate surfaces for global land areas. International Journal of Climatology 37 (12): 4302-4315.'),
                         HTML('<br><br>Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. 2013. “High-Resolution Global Maps of 21st-Century Forest Cover Change.” Science 342 (15 November): 850–53. Data available on-line from: http://earthenginepartners.appspot.com/science-2013-global-forest, accessed November 2017.'),
                         HTML('<br><br>Jarvis A., H.I. Reuter, A. Nelson, E. Guevara, 2008, Hole-filled seamless SRTM data V4, International Centre for Tropical Agriculture (CIAT), available from http://srtm.csi.cgiar.org.'),
                         HTML('<br><br>Purse BV, Darshan N, Kasabi GS, Gerard F, Samrat A, George C, et al. (2020) Predicting disease risk areas through co-production of spatial models: The example of Kyasanur Forest Disease in India’s forest landscapes. PLoS Negl Trop Dis 14(4): e0008179. https://doi.org/10.1371/journal.pntd.0008179'),
                         HTML('<br><br>Robinson TP, Wint GRW, Conchedda G, Van Boeckel TP, Ercoli V, Palamara E, et al. (2014) Mapping the Global Distribution of Livestock. PLoS ONE 9(5): e96084. https://doi.org/10.1371/journal.pone.0096084')
                 )),
               verbatimTextOutput("username")))
    ),
    tabPanel('Information on KFD',
             fluidRow(
               column(width = 6,
                      bsCollapse(
                        id = 'awarenessone', open = 'Who Gets KFD?', multiple = TRUE,
                        bsCollapsePanel(
                          title = 'KFD Burden and Symptoms',
                          style = 'success',
                          HTML('Kyasanur forest disease (KFD) is a tick-borne viral haemorrhagic fever,  that causes an estimated 500 human cases each year in the Western Ghats region of Southern India with mortality rates ranging from 3–5%.  The worst affected state is Karnataka state, where KFD first emerged in the 1950s, though cases have recently spread to neighbouring states, with Goa and Maharashtra having particularly high case numbers (Fig. 1a). Within Karnataka, Shimoga district and Uttara Kannada, have been the worst affected districts in recent transmission seasons (Fig. 1b).<br>'),
                          img(src='1a.png', align = "left", height="50%", width="50%"),
                          img(src='1b.png', align = "left", height="50%", width="50%"),
                          HTML('<br>Fig.1. Number of human KFD cases (bars) and deaths (annotated numbers) during 2018-2019 (blue), 2019-2020 (orange) (a) by state and (b) by district within Karnataka State (data from the Department of Health and Family Welfare Services).<br>'),
                          HTML('<br>Symptoms of KFD appear 3–8 days after the bite of an infective tick, due to the incubation period. Typically, the disease appears with a sudden onset of fever which peaks by 3rd or 4th day. Redness of the eyes, pulsating severe head ache and myalgia are very common (Fig. 2). Malaise and anorexia with prostration (fatigue with inability to get up) are often seen in patients. Gastrointestinal symptoms like vomiting and diarrhoea may occur 3–4 days after the onset of initial symptoms and patients may have low blood pressure. KFD may occur as biphasic disease in 5–10% of patients. After 1–2 weeks of symptoms, some patients recover without complication. However, the illness is biphasic for a subset of patients who experience a second wave of symptoms at the beginning of third week. The acute phase of febrile illness lasts for about two weeks. However, the fever starts decreasing from 5th or 6th day onwards and slowly the patient comes back to normalcy between 10th and 15th day.<br>'),
                          img(src='2.png', align = "left", height="50%", width="50%"),
                          HTML('<br>Fig.2. Signs and Symptoms of Kyasanur Forest Disease<br>')
                        ),
                        bsCollapsePanel(
                          title = 'Who Gets KFD?',
                          style = 'success',
                          HTML('People most affected by Kyasanur Forest Disease live in low-income forest communities including:<br>'),
                          HTML('<ul><li>resident and migratory farmers who graze animals (largely cattle) in the forest year-round to produce manure for plantations</li>'),
                          HTML('<li>tribal forest-dwellers who harvest fuel wood and non-timber forest products, such as honey, nuts, and dry leaves</li>'),
                          HTML('<li>day labourers in plantations or for State Forest departments</li></ul>'),
                          HTML('Housewives and children living in and around forests and tourists or students using forests for recreation have also been affected in recent outbreaks.')
                        )
                      )),
               column(width = 6,
                      bsCollapse(
                        id = 'awarenesstwo', open = 'Management and surveillance for KFD', multiple = TRUE,
                        bsCollapsePanel(
                          title = 'Risk factors: risky activities, seasons and habitats',
                          style = 'success',
                          HTML('The key seasonal risk period for human exposure to KFD is between December and April each year (Fig. 1), when the numbers of infected nymphal ticks are at their highest, though sporadic cases have been detected as early as November and as late as May and June.<br>'),
                          img(src='1.png', align = "left", height="100%", width="100%"),
                          HTML('<br>Fig. 1. Monthly human cases of KFD detected in Karnataka State between the 2017/2018 and 2019/2020 transmission seasons<br>'),
                          HTML('<br>Many households in KFD-affected districts engage in agricultural and plantation-related activities for their primary livelihoods that may increase their exposure to ticks infected with KFD. These include (illustrated from left to right in Fig. 2 below), 1) collection of firewood and leaves from forested areas where ticks are at high densities 2) storage for fertiliser or animal bedding of leaves which may harbour infected ticks; 3) movement through forested areas with significant understorey, particularly areas frequently used by livestock, for livestock grazing or collection of Non-Timber Forest Products.<br>'),
                          img(src='2_2.png', align = "left", height="100%", width="100%"),
                          HTML('<br>Fig. 2. Human activities that increase the risk of exposure to KFD-infected ticks<br>'),
                          HTML('<br>Further information on ticks and how to prevent tick bites can be found in this Tick Information Card, available to download in English, Kannada and Malayalam at this link:<br>'),
                          HTML('<a href="https://www.monkeyfeverrisk.ceh.ac.uk/kfd" rel="noreferrer">https://www.monkeyfeverrisk.ceh.ac.uk/kfd</a><br><br>'),
                          img(src='3.jpg', align = "left", height="100%", width="100%")
                        ),
                        bsCollapsePanel(
                          title = 'Management and surveillance for KFD',
                          style = 'success',
                          HTML('Current interventions aiming to prevent human cases of KFD are focussed primarily on reducing human exposure through community education, tick-bite prevention (protective clothing and repellents), and vaccination, in conjunction with measures for reducing tick populations at perceived infection hotspots. Surveillance measures include passive and active surveillance of human fever cases, surveillance and autopsy of monkey deaths, and surveillance of ticks in the habitat around prior known human cases and monkey deaths in advance of the outbreak season. Further information and Standard Operating Procedures and protocols for management and surveillance can be found in the Operational Manual for Kyasanur Forest Disease (2020), produced by the Directorate of Health and Family Welfare Services, Government of Karnataka, accessible at this link:<br>'),
                          HTML('<a href="https://www.monkeyfeverrisk.ceh.ac.uk/kfd" rel="noreferrer">https://www.monkeyfeverrisk.ceh.ac.uk/kfd</a><br>'),
                          HTML('<br>Further information for communities and health workers on ticks and how to prevent tick bites can be found in this Tick Information Card, available to download in English, Kannada and Malayalam at the same link:<br>'),
                          HTML('<a href="https://www.monkeyfeverrisk.ceh.ac.uk/kfd" rel="noreferrer">https://www.monkeyfeverrisk.ceh.ac.uk/kfd</a><br><br>'),
                          img(src='3.jpg', align = "left", height="100%", width="100%")
                        )
                      ))
               )
    ),
    tabPanel('Upload Files',
             # App title ----
             titlePanel("Uploading Files"),
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 # Input: Select a file ----
                 fileInput("file1", "Choose CSV File",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 
                 radioButtons("file1Type", "Data Type",
                              choices = c("Human" = "human",
                                          "Tick and Monkey" = "tickmonkey"),
                              selected = "human"),
                 
                 textInput('uploadKey', 'Upload Key', value = "", width = '100%',
                           placeholder = 'Enter upload key'),
                 
                 textOutput('uploadReturn'),
                 
                 # add upload button
                 bsButton('uploadCSV', label = 'Upload Data', style = 'primary', size = 'small'),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Checkbox if file has header ----
                 checkboxInput("header", "Header", TRUE),
                 
                 # Input: Select separator ----
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","),
                 
                 # Input: Select quotes ----
                 radioButtons("quote", "Quote",
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = '"'),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Select number of rows to display ----
                 radioButtons("disp", "Display",
                              choices = c(Head = "head",
                                          All = "all"),
                              selected = "head")
                 
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 
                 # Output: Data file ----
                 tableOutput("contents")
                 
               )
               
             )
             
    )
))
