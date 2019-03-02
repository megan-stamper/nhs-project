library(readr)
library(here)
library(dplyr)
library(rgdal)
library(rvest)

postcode <- 'BN11 5BH'
postcode <- gsub(" ", "", postcode)

if (!exists(dentists_data)){
  dentists_data <- read_csv('egdpprac.csv', col_names = c('org_id','name','national_grouping','high_level_health_geo','address_line_1',
                                                          'address_line_2','address_line_3','address_line_4','address_line_5','postcode',
                                                          'open_date','close_date','status_code','sub_type_code','parent_org_id','join_parent_date',
                                                          'left_parent_date','telephone','null_1','null_2','null_3','amended_record_flag','null_4','null_5',
                                                          'null_6','null_7','null_8')) %>%
    mutate(postcode = gsub(" ", "", postcode))
  
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
                                   col_names = c(unlist(colnames_postcode_data[1,])))
    
    postcode_data <- rbind(postcode_data, postcode_data_temp)
  }
  postcode_data <- postcode_data %>%
    mutate(Postcode = gsub(" ", "", Postcode))
  
  dentist_data <- dentists_data %>%
    left_join(postcode_data %>% select(Postcode, Eastings, Northings), by = c('postcode' = 'Postcode'))  %>%
    mutate(close_date = as.character(close_date),
           close_date = as.Date(close_date, format = '%Y%m%d')) %>%
    filter(close_date >= Sys.Date() | is.na(close_date), 
           !is.na(Eastings) & !is.na(Northings)) 
  
  null <- dentist_data %>%
    filter(is.na(Eastings) | is.na(Northings)) %>%
    select(postcode)
  
  # Create coordinates variable
  coords <- cbind(Easting = as.numeric(as.character(dentist_data$Eastings)),
                  Northing = as.numeric(as.character(dentist_data$Northings)))
  dentists_spatial_map <- SpatialPointsDataFrame(coords, data = data.frame(dentist_data$name), proj4string = CRS("+init=epsg:27700"))
  
  data_LL <- spTransform(dentists_spatial_map, CRS("+init=epsg:4326"))
  
  colnames(data_LL@coords)[colnames(data_LL@coords) == "Easting"] <- "Longitude"
  colnames(data_LL@coords)[colnames(data_LL@coords) == "Northing"] <- "Latitude"
  
  dentists_coords <- as.data.frame(data_LL@coords)
  dentists_data <- cbind(dentist_data, dentists_coords)
}

url <- paste0('https://www.nhs.uk/service-search/Dentists/', postcode ,'/Results/12/', address_lookup$lon, '/', address_lookup$lat, '/3/0?distance=50&ResultsOnPageValue=1000&isNational=0')
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

nhs_patients <- cbind(postcodes, accepting_nhs_patients) %>%
  select(postcode, flag)

dentists_accepting_nhs_patients <- dentists_data %>%
  left_join(nhs_patients, by = 'postcode')

library(ggplot2)
library(ggmap)
library(plotly)
library(ggrepel)

# creating a sample data.frame with your lat/lon points
lon <- c(-5, 2.5)
lat <- c(51, 56)
df <- as.data.frame(cbind(lon,lat))

address_lookup <- geocode(postcode, output = "latlona", source = "google") %>%
  mutate(text = 'you')

# getting the map
mapgilbert <- get_map(location = c(lon = address_lookup$lon, lat =  address_lookup$lat), zoom = 12,
                      source = 'google', maptype = "roadmap", scale = 1)

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = dentists_accepting_nhs_patients %>%
               filter(flag == 'Yes'), aes(x = Longitude, y = Latitude, 
                                          text = paste(name, "\n", address_line_2)), alpha = 0.8, fill = "green", size = 5, shape = 21) +
  geom_point(data = dentists_accepting_nhs_patients %>%
               filter(flag == 'No'), aes(x = Longitude, y = Latitude, 
                                         text = paste(name, "\n", address_line_2)), alpha = 0.8, fill = "red", size = 5, shape = 21) +
  geom_point(data = dentists_accepting_nhs_patients %>%
               filter(flag == 'Unknown' | is.na(flag)), aes(x = Longitude, y = Latitude, 
                                                            text = paste(name, "\n", address_line_2)), alpha = 0.8, fill = "grey", size = 5, shape = 21) +
  # geom_label_repel(data = dentists_data, aes(x = Longitude, y = Latitude, label = paste(name, "\n", address_line_2)),
  #                  family = 'Verdana',
  #                  size = 3,
  #                  box.padding = 0.2, point.padding = 0.3,
  #                  segment.color = 'grey50') +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  geom_point(data = address_lookup, aes(x = lon, y = lat + 0.005, text = text), size = 5, shape = 20, fill = 'black') +
  geom_line(data = rbind(address_lookup, address_lookup), aes(x = c(address_lookup$lon, address_lookup$lon), 
                                                              y = c(address_lookup$lat, address_lookup$lat + 0.005)), size = 1) + 
  scale_x_continuous(limits = c(address_lookup$lon - 0.11, address_lookup$lon + 0.11), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(address_lookup$lat - 0.07, address_lookup$lat + 0.07), expand = c(0, 0)) + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
  

