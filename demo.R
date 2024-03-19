#### Setup
# Load packages
#library(blackmarbler)
library(geodata)
library(sf)
library(raster)
library(ggplot2)
library(lubridate)

#### Define NASA bearer token
bearer <- Sys.getenv("BEARER_NASA_TOKEN")

### ROI
# Define region of interest (roi). The roi must be (1) an sf polygon and (2)
# in the WGS84 (epsg:4326) coordinate reference system. Here, we use the
roi_sf <- gadm(country = "CHE", level=0, path = tempdir()) |> st_as_sf()

# BM RASTER FAILS!!
ntl_r <- bm_raster(roi_sf = roi_sf,
                   product_id = "VNP46A2",
                   date = "2023-01-01",
                   bearer = bearer,
                   variable = "Gap_Filled_DNB_BRDF-Corrected_NTL")


#### Prep data
ntl_m_r <- ntl_r |> raster::mask(roi_sf)

ntl_df <- rasterToPoints(ntl_m_r, spatial = TRUE) |> as.data.frame()
names(ntl_df) <- c("value", "x", "y")

## Distribution is skewed, so log
ntl_df$value_adj <- log(ntl_df$value+1)

##### Map
ggplot() +
  geom_raster(data = ntl_df,
              aes(x = x, y = y,
                  fill = value_adj)) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4) +
  coord_quickmap() +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

