library(readr)
library(here)
library(dplyr)
library(rgdal)
library(rvest)
library(ggplot2)
library(ggmap)
library(plotly)
library(ggrepel)

postcode <- 'SE1 9HL'
postcode <- toupper(postcode)
postcode <- gsub(" ", "", postcode)
address_lookup <- geocode(postcode, output = "latlona", source = "google")
zoom_level = 12
plot_size =  0.5
dot_scale = 3

if (!exists('dentists_data')){
  dentists_data <- read_csv('egdpprac.csv', col_names = c('org_id','name','national_grouping','high_level_health_geo','address_line_1',
                                                          'address_line_2','address_line_3','address_line_4','address_line_5','postcode',
                                                          'open_date','close_date','status_code','sub_type_code','parent_org_id','join_parent_date',
                                                          'left_parent_date','telephone','null_1','null_2','null_3','amended_record_flag','null_4','null_5',
                                                          'null_6','null_7','null_8')) %>%
    mutate(postcode = gsub(" ", "", postcode)) %>%
    select(address_line_1, postcode, close_date) %>%
    mutate(close_date = as.character(close_date),
           close_date = as.Date(close_date, format = '%Y%m%d')) %>%
    filter(close_date >= Sys.Date() | is.na(close_date)) %>%
    select(-close_date) %>%
    mutate(flag = NA) 
  
  scotland_dentists_data <- read_csv('dentists_data_scotland.csv') %>%
    select(postcode = Postcode, address_line_1 = AddressLine1) %>%
    mutate(postcode = gsub(" ", "", postcode), 
           flag = 'Yes')
  
  dentists_data <- rbind(dentists_data %>% mutate(area = 'england_and_wales'), 
                         scotland_dentists_data %>% mutate(area = 'scotland'))
  
  postcode_directory = paste0('codepo_gb/Data/CSV/')
  colnames_postcode_data = read_csv('codepo_gb/Doc/Code-Point_Open_Column_Headers.csv')
  
  colnames_postcode_data = read_csv('codepo_gb/Doc/Code-Point_Open_Column_Headers.csv')
  file_names <- data.frame(list.files(here('codepo_gb/Data/CSV')))
  names(file_names) <- "file_name"
  file_names <- file_names %>% 
    mutate(file_name = as.character(file_name),
           file_name = substr(file_name, 1, nchar(file_name) - 4)) %>%
    arrange(file_name)
  
  postcode_data <- data.frame()
  for (i in 1:nrow(file_names)) {
    postcode_data_temp <- read_csv(paste0(postcode_directory, file_names[i,1], ".csv"), 
                                   col_names = c(unlist(colnames_postcode_data[1,])), 
                                   col_types = list(col_character(),
                                                    col_double(),
                                                    col_double(),
                                                    col_double(),
                                                    col_character(),
                                                    col_character(),
                                                    col_character(),
                                                    col_character(),
                                                    col_character(),
                                                    col_character()))
    
    postcode_data <- rbind(postcode_data, postcode_data_temp)
  }
  postcode_data <- postcode_data %>%
    mutate(Postcode = gsub(" ", "", Postcode))
  
  dentist_data <- dentists_data %>%
    left_join(postcode_data %>% select(Postcode, Eastings, Northings), by = c('postcode' = 'Postcode'))  %>%
    filter(!is.na(Eastings) & !is.na(Northings))
  
  null <- dentist_data %>%
    filter(is.na(Eastings) | is.na(Northings)) %>%
    select(postcode)
  
  # Create coordinates variable
  coords <- cbind(Easting = as.numeric(as.character(dentist_data$Eastings)),
                  Northing = as.numeric(as.character(dentist_data$Northings)))
  dentists_spatial_map <- SpatialPointsDataFrame(coords, data = data.frame(dentist_data$address_line_1), proj4string = CRS("+init=epsg:27700"))
  
  data_LL <- spTransform(dentists_spatial_map, CRS("+init=epsg:4326"))
  
  colnames(data_LL@coords)[colnames(data_LL@coords) == "Easting"] <- "Longitude"
  colnames(data_LL@coords)[colnames(data_LL@coords) == "Northing"] <- "Latitude"
  
  dentists_coords <- as.data.frame(data_LL@coords)
  dentists_data <- cbind(dentist_data, dentists_coords)
}

if (!exists('nhs_patients')) {
  total_practices = 7596
  total_pages = floor(total_practices / 100 + 1)
  
  nhs_patients <- data_frame()
  for (page in 1:total_pages){
    url <- paste0('https://www.nhs.uk/service-search/Dentists/', postcode ,'/Results/12/', address_lookup$lon, '/', address_lookup$lat, '/3/0?distance=1000&ResultsOnPageValue=100&isNational=0', 
                  '&currentPage=', page)
    webpage <- read_html(url)
    postcode_data_html <- as.data.frame(html_nodes(webpage,'img') %>% as.character())
    names(postcode_data_html) <- 'img_tag'
    accepting_nhs_patients <- postcode_data_html %>%
      mutate(img_tag = as.character(img_tag)) %>%
      filter(img_tag %in% c('<img alt="No" src="/service-search/Content/img/indicators/icon-no.png">',
                            '<img alt="Yes" src="/service-search/Content/img/indicators/icon-yes.png">',
                            '<img src="/service-search/Content/img/indicators/icon-question.png">')) %>%
      mutate(row_number = row_number(), 
             flag = ifelse(grepl('No', img_tag), 'No', 
                           ifelse(grepl('Yes', img_tag), 'Yes', 'Unknown'))) %>%
      filter((row_number %% 5) == 2)
    
    address <- as.data.frame(html_nodes(webpage, '.fcaddress') %>% as.character())
    names(address) <- 'address'
    postcodes <- address %>%
      mutate(address = as.character(address), 
             postcode = gsub('.*<br>\r\n', '', address), 
             postcode = gsub(' ', '', postcode), 
             postcode = gsub('</p>', '', postcode)) %>%
      select(postcode)
    
    nhs_patients_temp <- cbind(postcodes, accepting_nhs_patients) %>%
      select(postcode, flag)
    
    nhs_patients <- rbind(nhs_patients, nhs_patients_temp)
  }
}

dentists_accepting_nhs_patients <- dentists_data %>%
  left_join(nhs_patients %>% 
              rename(flag_nhs_ew = flag), by = 'postcode') %>%
  mutate(flag = ifelse(is.na(flag), flag_nhs_ew, flag))

# getting the map
mapgilbert <- get_map(location = c(lon = address_lookup$lon, lat =  address_lookup$lat), zoom = zoom_level,
                      source = 'google', maptype = "roadmap", scale = 1)

library('ggiraph')

# plotting the map with some points on it
plot <- ggmap(mapgilbert) +
  geom_point(data = dentists_accepting_nhs_patients %>%
               filter(flag == 'No'), aes(x = Longitude, y = Latitude), alpha = 0.8, fill = "red", size = dot_scale, shape = 21) +
  geom_point(data = dentists_accepting_nhs_patients %>%
               filter(flag == 'Unknown' | is.na(flag)), aes(x = Longitude, y = Latitude), alpha = 0.8, fill = "grey", size = dot_scale, shape = 21) +
  geom_point(data = dentists_accepting_nhs_patients %>%
               filter(flag == 'Yes'), aes(x = Longitude, y = Latitude), alpha = 0.8, fill = "green", size = dot_scale, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  geom_point(data = address_lookup, aes(x = lon, y = lat + 0.005 * plot_size), size = dot_scale, shape = 20, fill = 'black') +
  geom_line(data = rbind(address_lookup , address_lookup), aes(x = c(address_lookup$lon, address_lookup$lon), 
                                                              y = c(address_lookup$lat, address_lookup$lat + 0.005 * plot_size)), size = 1) + 
  geom_point_interactive(data = dentists_accepting_nhs_patients, aes(x = Longitude, y = Latitude, tooltip = address_line_1),
                         size=3, alpha=0.01) + # Add interactive points underneath logos  
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) #+ 
 # scale_x_continuous(limits = c(address_lookup$lon - 0.11 * plot_size, address_lookup$lon + 0.11 * plot_size)) + 
  #scale_y_continuous(limits = c(address_lookup$lat - 0.07 * plot_size, address_lookup$lat + 0.07 * plot_size)) 

ggiraph(code={print(plot)}, width=1, width_svg=5, height_svg=5) # Interactive plot

library('geosphere')
library('rlist')
get_geo_distance = function(long1, lat1, long2, lat2, units = "miles") {
  loadNamespace("geosphere")
  longlat1 = c(long1, lat1)
  longlat2 = c(long2, lat2)
  distance_m = geosphere::distHaversine(longlat1, longlat2)
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  else if (units == "miles") {
    distance = distance_m / 1609.344
  }
  else {
    distance = distance_m
    # This will return in meters as same way as distHaversine function. 
  }
  as.numeric(distance)
}

nearest_dentists <- dentists_accepting_nhs_patients %>%
  filter(flag == 'Yes') %>% 
  mutate(lon2 = address_lookup$lon, 
         lat2 = address_lookup$lat) %>%
  rowwise() %>%
  mutate(distance = get_geo_distance(Longitude, Latitude, lon2, lat2)) %>%
  arrange(distance) %>%
  top_n(-1, wt = distance)
print(paste0("Nearest dentist is ", round(nearest_dentists$distance, 2), " miles (in a straight line)"))
