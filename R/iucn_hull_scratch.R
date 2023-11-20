# Original alpha hull ----------------------------------------------------------
plot(b_aurea_ahull)
# Use original function
b_aurea_poly <- ahull2poly(b_aurea_ahull)
plot(b_aurea_poly)


# IUCN alpha hull --------------------------------------------------------------
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
    line_i_coords <- rbind(b_aurea_ahull_iucn$x[from_i,],
                           b_aurea_ahull_iucn$x[to_i,])
    line_i <- Line(line_i_coords)
    line_list <- append(line_list, line_i)
  }
  lines <- Lines(line_list, ID = '1')
  sp_lines <- SpatialLines(list(lines))
  return(sp_lines)
}

b_aurea_iucn_lines <- ahull.IUCN2lines(b_aurea_ahull_iucn)
plot(b_aurea_iucn_lines)

ahull.IUCN2poly <- function(iucn_hull) {
  hull2SpatialLines <- ahull.IUCN2lines(iucn_hull)
  SpatialLines2SpatialPolygon <- spLines2poly(hull2SpatialLines)
  return(SpatialLines2SpatialPolygon)
}

b_aurea_iucn_poly <- ahull.IUCN2poly(b_aurea_ahull_iucn)
plot(b_aurea_iucn_poly)
