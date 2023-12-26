# Relevant code from Rmd -------------------------------------------------------
pacman::p_load(galah, arrow, here, tidyverse, ozmaps, sf, ggplot2, skimr, alphahull, hull2spatial, sp, purrr, dplyr)
myg_spiders <- read_parquet(here("data/galah/Mygalomorphae_withassertions_2023-09-18_ALA.parquet"))

myg_spiders_cleaned <- myg_spiders |> 
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) |>
  filter(!duplicated(decimalLatitude) & !duplicated(decimalLongitude)) 

# Filter down to species
myg_cleaned_sp <- myg_spiders_cleaned |> 
  filter(taxonRank == "species")

# Summarise number of observations by taxon
myg_cleaned_sp |> 
  group_by(scientificName) |> 
  summarise(n = n()) |> 
  arrange(-n)

# Which ones have less than or equal 3 
less_than_3_obs <- myg_cleaned_sp |> 
  group_by(scientificName) |> 
  summarise(n = n()) |> 
  filter(n <= 3) |> 
  pull(scientificName)

myg_sp_enoughobs <- myg_cleaned_sp |> 
  filter(!scientificName %in% less_than_3_obs)

# How many species
myg_sp_enoughobs |>
  dplyr::select(scientificName) |> 
  distinct() |> 
  nrow()

set.seed(987)
choosen <- myg_sp_enoughobs |> 
  slice_sample(n = 1) |> 
  pull(scientificName)

b_aurea <- myg_sp_enoughobs |> 
  filter(scientificName == choosen)

# Original alpha hull ----------------------------------------------------------
b_aurea_ahull <- b_aurea |> 
  select(decimalLongitude, decimalLatitude) |> 
  ahull(alpha = 2)
plot(b_aurea_ahull)
# Use original function
b_aurea_poly <- ahull2poly(b_aurea_ahull)
plot(b_aurea_poly, lwd = 3)


# IUCN alpha hull --------------------------------------------------------------
b_aurea_ahull_iucn <- b_aurea |> 
  select(decimalLongitude, decimalLatitude) |> 
  alphahull::ahull.IUCN(alpha = 4)
plot(b_aurea_ahull_iucn)
plot(b_aurea_ahull, add = T) # overlay original

line_1_coords <- rbind(b_aurea_ahull_iucn$x[2,],
                       b_aurea_ahull_iucn$x[13,])
line_1 <- Line(line_1_coords)

from_to <- b_aurea_ahull_iucn[["bd.ah.IUCN"]]
line_list <- list()
for (i in 1:nrow(from_to)) {
  from_i <- from_to[i, 1]
  to_i <- from_to[i, 2]
  line_i_coords <- rbind(b_aurea_ahull_iucn$x[from_i,],
                         b_aurea_ahull_iucn$x[to_i,])
  line_i <- Line(line_i_coords)
  line_list <- append(line_list, line_i)
}
lines <- Lines(line_list, ID = '1')
sp_lines <- SpatialLines(list(lines))
spatial_poly <- spLines2poly(sp_lines)
# Is this warning an issue:
# 1: In sp::Polygon(methods::slot(x, "coords")) :
# less than 4 coordinates in polygon
plot(spatial_poly)

# Function attempt -------------------------------------------------------------
ahull.IUCN2lines <- function(iucn_hull) {
  from_to <- iucn_hull[["bd.ah.IUCN"]]
  line_list <- list()
  for (i in 1:nrow(from_to)) {
    from_i <- from_to[i, 1]
    to_i <- from_to[i, 2]
    line_i_coords <- rbind(iucn_hull$x[from_i,],
                           iucn_hull$x[to_i,])
    line_i <- Line(line_i_coords)
    line_list <- append(line_list, line_i)
  }
  lines <- Lines(line_list, ID = '1')
  sp_lines <- SpatialLines(list(lines))
  st_lines <- st_as_sf(sp_lines)
  merged_lines <- st_line_merge(st_lines)
  sp_merged_lines <- as_Spatial(merged_lines)
  return(sp_merged_lines)
}

ahull.IUCN2lines <- function(iucn_hull, sp_or_sf) {
  from_to <- iucn_hull[["bd.ah.IUCN"]]
  line_list <- list()
  for (i in 1:nrow(from_to)) {
    from_i <- from_to[i, 1]
    to_i <- from_to[i, 2]
    line_i_coords <- rbind(iucn_hull$x[from_i,],
                           iucn_hull$x[to_i,])
    line_i <- Line(line_i_coords)
    line_list <- append(line_list, line_i)
  }
  lines <- Lines(line_list, ID = '1')
  sp_lines <- SpatialLines(list(lines))
  st_lines <- st_as_sf(sp_lines)
  merged_lines <- st_line_merge(st_lines)
  if(sp_or_sf == "sp") {
    sp_merged_lines <- as_Spatial(merged_lines)
    return(sp_merged_lines)
  } else if(sp_or_sf == "sf") {
    return(merged_lines)
  } else {
    stop("sp_or_sf must be either sp or sf")
  }
}

b_aurea_iucn_lines <- ahull.IUCN2lines(b_aurea_ahull_iucn, "sf")
class(b_aurea_iucn_lines)
plot(b_aurea_iucn_lines)

sf_b_aurea_iucn_lines <- ahull.IUCN2lines(b_aurea_ahull_iucn, "sf")
sp_b_aurea_iucn_lines <- ahull.IUCN2lines(b_aurea_ahull_iucn, "sp")

ahull.IUCN2poly <- function(iucn_hull, sp_or_sf) {
  hull2lines <- ahull.IUCN2lines(iucn_hull, sp_or_sf)
  if(sp_or_sf == "sp") {
    SpatialLines2SpatialPolygon <- spLines2poly(hull2lines)
    return(SpatialLines2SpatialPolygon)
  } else if(sp_or_sf == "sf") {
    sf_poly <- st_cast(hull2lines, "POLYGON")
    return(sf_poly)
  } else {
    stop("sp_or_sf must be either sp or sf")
  }
}

sf_b_aurea_iucn_poly <- ahull.IUCN2poly(b_aurea_ahull_iucn, "sf")
class(sf_b_aurea_iucn_poly)
plot(sf_b_aurea_iucn_poly)

sp_b_aurea_iucn_poly <- ahull.IUCN2poly(b_aurea_ahull_iucn, "sp")
plot(sp_b_aurea_iucn_poly)
