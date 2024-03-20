#### Setup
# Load packages
#library(blackmarbler)
library(geodata)
library(sf)
library(terra)
library(ggplot2)
library(lubridate)
source('app/logic/blackmarble_utils.R')

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


# ntl is returned but ........

#### Prep data
ntl_m_r <- ntl_r |>
  mask(roi_sf)

ntl_df <- as.points(ntl_m_r) |>
  st_as_sf()

adjusted_ntl_df <- ntl_df |>
  ## Remove very low values of NTL; can be considered noise
  mutate(
    t2023_01_01 = ifelse(t2023_01_01 <= 2, 0, t2023_01_01)
  ) |>
  ## Distribution is skewed, so log
  mutate(t2023_01_01 = log(t2023_01_01 + 1))

#names(ntl_df$t2023_01_01) <- c("value", "x", "y")


##### Map
ggplot() +
  geom_sf(data = adjusted_ntl_df,
          aes(fill = t2023_01_01),
          color = NA) +
  # scale_fill_viridis_c(option = "rocket",
  #                      guide = "none",
  #                      rescaler = scales::rescale) +
  # geom_raster(data = adjusted_ntl_df,
  #             aes(x = x, y = y,
  #                 fill = t2023_01_01)) +
  # scale_fill_gradient2(low = "black",
  #                      mid = "yellow",
  #                      high = "red",
  #                      midpoint = 4) +
  theme_void()

