library(sf)
library(tidyverse)
library(httr)
library(geojsonsf)
# NPS Admin Boundaries map viewer link:
# https://nps.maps.arcgis.com/apps/mapviewer/index.html?layers=a2848257cf5541338bab9b656440bd0c

# NPS Centroids
cent <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/NPS_Land_Resources_Division_Boundary_and_Tract_Data_Service/FeatureServer/0"

# NPS Polygons
poly <-"https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/NPS_Land_Resources_Division_Boundary_and_Tract_Data_Service/FeatureServer/2"

base_url <- httr::parse_url(cent) # note that the vegetation layer is MapServer/2

base_url$path <- paste(base_url$path, "query", sep = "/")

base_url$query <- list(where = "1=1", # code in the query parameters
                       outFields = "*",
                       f = "geojson")
request <- httr::build_url(base_url) # compare this URL to the one we used in Example 1

tmp <- tempfile()
download.file(request, tmp)

# Because the data are in GeoJSON format we will use `geojsonsf::geojson_sf()` to convert the object to an `sf` object
park_cent <- geojsonsf::geojson_sf(tmp)
head(park_cent)
st_crs(park_cent) #WGS84
