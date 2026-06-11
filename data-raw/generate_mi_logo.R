# =============================================================================
# Fetch the Michigan state outline for the header logo and save it locally so
# script.R can build the logo offline. The logo itself (white state + the live
# trail geometry drawn in green) is assembled in script.R from this outline plus
# the `trails` it just loaded — so the logo is a tiny copy of the actual map.
#
#   Rscript data-raw/generate_mi_logo.R
#
# Source: US Census cartographic state boundary via the `tigris` package,
# projected to Michigan Lambert (EPSG:3078), simplified, LP + UP only.
# =============================================================================
suppressPackageStartupMessages({library(sf); library(dplyr)})
options(tigris_use_cache = TRUE)

mi <- tigris::states(cb = TRUE, resolution = "500k", year = 2022, progress_bar = FALSE) |>
  filter(STUSPS == "MI") |> st_transform(3078)

# Break the multipolygon into individual polygons; keep only the big landmasses
# (Lower + Upper Peninsula), dropping small islands like Isle Royale.
polys <- st_cast(st_geometry(mi), "POLYGON") |> st_simplify(dTolerance = 1200)
areas <- as.numeric(st_area(polys))
keep  <- polys[areas > 0.05 * max(areas)]      # LP + UP
cat("kept", length(keep), "of", length(polys), "polygons\n")

out <- st_sf(geometry = st_sfc(keep, crs = 3078))
st_write(out, "data-raw/mi_outline.geojson", delete_dsn = TRUE, quiet = TRUE)
cat("wrote data-raw/mi_outline.geojson\n")
