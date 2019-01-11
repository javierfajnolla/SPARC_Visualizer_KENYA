##################


library(tidyverse)
library(raster)
library(rgdal)

# Load data to display

# Zonation solution
# solution <- raster("D:/shiny/SPARC_Visualizer/data/solutions/AT_Birds_gf8570_BLP1_noPA.CAZ_EBLP100.rank.compressed.tif" %>%
#   aggregate(fact = 10, fun = mean)
# proj4string(solution) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# writeRaster(solution, "data/solutions/NG_Birds_CAZ_hfp_pa.CAZ_MDE.wrscr.compressed_agg10.tif")
# solution <- raster("data/solutions/NG_Birds_CAZ_hfp_pa.CAZ_MDE.wrscr.compressed_agg10.tif")

## Correct solution
# solution <- raster("data/solutions/NG_Birds_CAZ_hfp_pa.CAZ_MDE.rank.compressed.tif") %>%
#   aggregate(fact = 10, fun = mean)
# proj4string(solution) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# writeRaster(solution, "data/solutions/NG_Birds_CAZ_hfp_pa.CAZ_MDE.rank.compressed_agg10.tif")
solution <- raster("data/solutions/kenya_birds_noPA_gf8570.CAZ_E.rank.compressed.tif")
proj4string(solution) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

mask <- solution / solution

# Carbon storage
# carbon_stor <- raster("data/solutions/total_carbon.tif") %>%
#   aggregate(fact = 10, fun = sum) %>%
#   crop(mask) %>%
#   `*` (mask)   # There is data in pixels that are not in the zonation solution
# proj4string(carbon_stor) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# writeRaster(carbon_stor, "data/solutions/total_carbon_agg10.tif")
carbon_stor <- raster("data/solutions/agb_carbon.tif")

# ## Max
max_carbon <- carbon_stor %>% values %>% sum(na.rm = T)

################## Table of carbon offset for each percentage
# tbl <- tibble(perc_pixels = seq(0.01, 1, by = 0.01))
# tbl$cutoff_sol <- NA
# 
# # # Sorted priority values (to select x% with higher priority)
# sol_values_sort <- solution %>% values %>% sort %>% rev
# 
# # # Cutoff in solution to retain each percentage of the study area
# for (j in 1:nrow(tbl)){
#   tbl$cutoff_sol[j] <- sol_values_sort[(length(sol_values_sort) * tbl$perc_pixels[j]) %>% round(0)]
# }
# 
# # # Carbon storage saved by protecting each percentage of the study area
# tbl$carbon_strg <- NA
# for(i in tbl$perc_pixels){
#   sel_thr <- tbl %>%
#     filter(perc_pixels == i) %>%
#     pull(cutoff_sol)
# 
#   sol_rcl <- solution %>%
#     reclassify(tibble(from = c(0, sel_thr),
#                       to = c(sel_thr, max(values(solution), na.rm = T)),
#                       becomes = c(NA, 1)))
# 
#   tbl$carbon_strg[tbl$perc_pixels == i] <- (sol_rcl * carbon_stor) %>% values %>% sum(na.rm = T)
# }
# 
# tbl <- tbl %>%
#   mutate(prop_carbon_strg = carbon_strg / max_carbon,
#          prop_carbon_strg_rounded = round(prop_carbon_strg, 2))

### DO NOT RUN THIS PART -from here to saveRDS- , it was just testing... but it doesn't seem to solve the problems, although I might want to go back to this later.
## Fill values of the carbon storage slider that did not correspond to any particular threshold of the solution (including values between the max carbon thr and 100)
# perc_pixels_carbon <- seq(0.01, 1, by = 0.01)
# tbl_carboff <- setdiff(perc_pixels_carbon, tbl$prop_carbon_strg_rounded) %>%
#   as_tibble %>% setNames("prop_carbon_strg_rounded")
# new_tbl_carboff <- tibble()
# for (k in 1:nrow(tbl_carboff)){
#   tbl_temp <- tbl %>%
#     filter(abs(prop_carbon_strg_rounded - tbl_carboff$prop_carbon_strg_rounded[k]) == min(abs(prop_carbon_strg_rounded - tbl_carboff$prop_carbon_strg_rounded[k]))) %>%
#     mutate(prop_carbon_strg_rounded = tbl_carboff$prop_carbon_strg_rounded[k])
#   new_tbl_carboff <- new_tbl_carboff %>% bind_rows(tbl_temp)
# }
# 
# # Combine both tables
# tbl <- tbl %>% bind_rows(new_tbl_carboff)


# saveRDS(tbl, "data/tables/KENYA_tbl.rds")


### Load precalculated table to speed up
tbl <- readRDS("data/tables/KENYA_tbl.rds")
#tbl <- read_csv("data/tables/tbl.csv")


############################
## Add countries limits
# require(maps)
# require(maptools)
# require(sf)
# world <- maps::map("world", fill = TRUE, plot = FALSE)
# world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
# world_map <- SpatialPolygonsDataFrame(world_map,
#                                       data.frame(country = names(world_map), 
#                                                  stringsAsFactors = FALSE), 
#                                       FALSE)
# world_sf <- st_as_sf(world_map) %>%
#   filter(country %in% c("Kenya", "Tanzania", "Uganda", "Rwanda", "Burundi")) %>% 
#   mutate(common = "A") %>% 
#   group_by(common) %>% 
#   summarise()
# write_sf(world_sf, "data/SIG/KTURB_border.shp")
require(sf)
# country_borders <- read_sf("data/SIG/Kenya_border.shp")
# country_borders <- read_sf("data/SIG/KTU_border.shp")
country_borders <- read_sf("data/SIG/KTURB_border.shp")

##################