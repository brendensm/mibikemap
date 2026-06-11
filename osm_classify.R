#!/usr/bin/env Rscript
# =============================================================================
# mibikemap — an open-source map of Michigan bike / rail trails
# -----------------------------------------------------------------------------
# Pipeline:
#   1. Pull cycling infrastructure geometry from OpenStreetMap (Overpass API)
#   2. Classify each segment: Protected path / Shared-use path / Unpaved /
#      On-road bike lane  (from OSM `highway`, `surface`, `cycleway*` tags)
#   3. Cross-check coverage against two community Google "My Maps" KMLs
#   4. Render an interactive Leaflet map
#   5. Write a single self-contained output/index.html for GitHub Pages
#
# Re-running is cheap: the raw Overpass response is cached in
# data-raw/osm_michigan.osm. Delete that file (or set REFRESH_OSM <- TRUE)
# to pull fresh data.
# =============================================================================

suppressPackageStartupMessages({
  library(osmdata)
  library(sf)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(leaflet)
  library(htmlwidgets)
  library(xml2)
})

sf::sf_use_s2(TRUE)

# ---- Config -----------------------------------------------------------------
REFRESH_OSM   <- FALSE                              # TRUE forces a fresh pull
OVERPASS_URL  <- "https://overpass-api.de/api/interpreter"
UA            <- "mibikemap/1.0 (github pages bike map; brendensmithmi@gmail.com)"
OSM_RAW       <- "data-raw/osm_michigan.osm"
OVERPASS_QL   <- "data-raw/overpass_query.txt"
KML_FILES     <- c(
  "Lansing region"     = "data-raw/community_lansing.kml",
  "Thumb / Blue Water" = "data-raw/community_thumb.kml"
)
SIMPLIFY_M    <- 6        # Douglas-Peucker tolerance (metres) for the web map
COORD_DIGITS  <- 5        # ~1 m coordinate precision in the embedded geometry
CROSSCHECK_BUFFER_M <- 40 # a community trail is "matched" within this distance
OUT_HTML      <- "output/index.html"

dir.create("data",   showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
dir.create("ref",    showWarnings = FALSE)

msg <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                 sprintf(...)))

# =============================================================================
# 1. Fetch OSM cycling infrastructure for the State of Michigan
# =============================================================================
# A single Overpass QL query (cached to OVERPASS_QL) selects, by Michigan's
# admin area:
#   * dedicated cycleways
#   * paths / footways / bridleways open to bikes
#   * roads carrying a marked cycleway lane/track
# and asks for inline geometry (`out body geom`).

build_query <- function() {
  paste0(
    "[out:xml][timeout:600];\n",
    "area[\"ISO3166-2\"=\"US-MI\"]->.mi;\n",
    "(\n",
    "  way(area.mi)[highway=cycleway];\n",
    "  way(area.mi)[highway=path][bicycle~\"yes|designated|permissive\"];\n",
    "  way(area.mi)[highway=footway][bicycle~\"yes|designated\"];\n",
    "  way(area.mi)[highway=bridleway][bicycle~\"yes|designated\"];\n",
    "  way(area.mi)[highway~\"^(primary|secondary|tertiary|residential|unclassified|trunk|primary_link|secondary_link|tertiary_link)$\"][cycleway~\"lane|track|shared_lane|buffered_lane\"];\n",
    "  way(area.mi)[highway~\"^(primary|secondary|tertiary|residential|unclassified|trunk)$\"][\"cycleway:both\"~\"lane|track\"];\n",
    "  way(area.mi)[highway~\"^(primary|secondary|tertiary|residential|unclassified|trunk)$\"][\"cycleway:left\"~\"lane|track\"];\n",
    "  way(area.mi)[highway~\"^(primary|secondary|tertiary|residential|unclassified|trunk)$\"][\"cycleway:right\"~\"lane|track\"];\n",
    ");\n",
    # Emit ways + recurse to their nodes so osmdata can assemble line geometry.
    "out body;\n",
    ">;\n",
    "out skel qt;\n"
  )
}

fetch_osm <- function() {
  q <- build_query()
  writeLines(q, OVERPASS_QL)
  if (!REFRESH_OSM && file.exists(OSM_RAW) && file.info(OSM_RAW)$size > 1e4) {
    msg("Using cached OSM extract: %s (%.1f MB)", OSM_RAW,
        file.info(OSM_RAW)$size / 1e6)
    return(invisible(q))
  }
  msg("Querying Overpass for statewide cycling infrastructure (a few minutes)...")
  resp <- httr::POST(
    OVERPASS_URL,
    body = list(data = q), encode = "form",
    httr::user_agent(UA), httr::timeout(600),
    httr::write_disk(OSM_RAW, overwrite = TRUE)
  )
  if (httr::status_code(resp) != 200)
    stop("Overpass returned HTTP ", httr::status_code(resp))
  msg("Saved %s (%.1f MB)", OSM_RAW, file.info(OSM_RAW)$size / 1e6)
  invisible(q)
}

q   <- fetch_osm()
osm <- osmdata_sf(q = q, doc = OSM_RAW, quiet = TRUE)
lines <- osm$osm_lines
if (is.null(lines) || nrow(lines) == 0) stop("No line geometry parsed from OSM.")
msg("Parsed %s OSM line features.", format(nrow(lines), big.mark = ","))

# =============================================================================
# 2. Classify each segment
# =============================================================================
# Tag columns only exist when at least one feature carries them, so pull them
# defensively.
col <- function(df, nm) if (nm %in% names(df)) df[[nm]] else NA_character_

UNPAVED <- c("unpaved", "gravel", "fine_gravel", "compacted", "ground", "dirt",
             "earth", "grass", "sand", "wood", "pebblestone", "rock",
             "woodchips", "mud", "clay", "metal", "grass_paver")
ROAD_CLASSES <- c("primary", "secondary", "tertiary", "residential",
                  "unclassified", "trunk", "primary_link", "secondary_link",
                  "tertiary_link")

lines <- lines %>%
  mutate(
    .hw   = col(., "highway"),
    .surf = tolower(col(., "surface")),
    is_onroad  = .hw %in% ROAD_CLASSES,
    is_unpaved = !is.na(.surf) & .surf %in% UNPAVED,
    category = case_when(
      is_unpaved        ~ "Unpaved trail",
      is_onroad         ~ "On-road bike lane",
      .hw == "cycleway" ~ "Protected path",
      TRUE              ~ "Shared-use path"
    ),
    nm = col(., "name")
  )

# Per-segment geodesic length in miles.
lines$length_mi <- as.numeric(sf::st_length(lines)) / 1609.344

cats <- c("Protected path", "Shared-use path", "Unpaved trail", "On-road bike lane")
lines$category <- factor(lines$category, levels = cats)

summary_tbl <- lines %>%
  st_drop_geometry() %>%
  group_by(category) %>%
  summarise(segments = n(), miles = round(sum(length_mi), 1), .groups = "drop")
msg("Classification summary:")
print(summary_tbl)

# Persist the analysed network.
keep <- c("osm_id", "nm", "category", ".hw", ".surf", "length_mi")
out_sf <- lines[, intersect(keep, names(lines))]
names(out_sf)[names(out_sf) == "nm"]    <- "name"
names(out_sf)[names(out_sf) == ".hw"]   <- "highway"
names(out_sf)[names(out_sf) == ".surf"] <- "surface"
st_write(out_sf, "data/michigan_trails.gpkg", delete_dsn = TRUE, quiet = TRUE)
msg("Wrote data/michigan_trails.gpkg")

# =============================================================================
# 3. Cross-check against the community KMLs
# =============================================================================
# For each community map, measure how much of its trail length falls within
# CROSSCHECK_BUFFER_M of an OSM trail, and flag named trails with little/no OSM
# coverage (candidates missing from / mis-tagged in OSM).

read_kml_lines <- function(path, source_name) {
  if (!file.exists(path)) return(NULL)
  layers <- tryCatch(st_layers(path)$name, error = function(e) character())
  if (!length(layers)) return(NULL)
  parts <- map(layers, function(l) {
    g <- tryCatch(suppressWarnings(st_read(path, layer = l, quiet = TRUE)),
                  error = function(e) NULL)
    if (is.null(g) || !nrow(g)) return(NULL)
    g <- st_zm(g)
    g <- g[st_geometry_type(g) %in% c("LINESTRING", "MULTILINESTRING"), ]
    if (!nrow(g)) return(NULL)
    data.frame(
      source = source_name, layer = l,
      name   = if ("Name" %in% names(g)) g$Name else NA_character_,
      stringsAsFactors = FALSE
    ) %>% st_set_geometry(st_geometry(g))
  })
  parts <- compact(parts)
  if (!length(parts)) return(NULL)
  do.call(rbind, parts) %>% st_transform(4326)
}

community <- imap(KML_FILES, read_kml_lines) %>% compact()
community_sf <- if (length(community)) do.call(rbind, community) else NULL

crosscheck <- NULL
if (!is.null(community_sf) && nrow(community_sf)) {
  # Coverage by point-sampling: walk each community trail at SAMPLE_M spacing,
  # find each sample's nearest OSM trail (STRtree-indexed) and measure that one
  # distance. % covered = share of samples within CROSSCHECK_BUFFER_M. Using
  # st_nearest_feature keeps it to one distance per point — far cheaper than a
  # global st_union or an all-candidates within-distance test over 53k lines.
  crs_m    <- 32616                        # UTM 16N — metric, covers Lower MI
  SAMPLE_M <- 30
  osm_m  <- st_geometry(st_transform(out_sf, crs_m))
  comm_m <- st_transform(community_sf, crs_m)
  comm_m$fid   <- seq_len(nrow(comm_m))
  comm_m$len_m <- as.numeric(st_length(comm_m))

  sample_pts <- function(geom, fid) {
    g <- st_sfc(geom, crs = crs_m)
    n <- max(2, ceiling(as.numeric(st_length(g)) / SAMPLE_M))
    p <- st_cast(st_line_sample(g, n = n), "POINT")
    if (!length(p)) return(NULL)
    st_sf(fid = fid, geometry = p)
  }
  pts <- do.call(rbind, Map(sample_pts, st_geometry(comm_m), comm_m$fid))
  nn  <- st_nearest_feature(pts, osm_m)
  dst <- as.numeric(st_distance(pts, osm_m[nn], by_element = TRUE))
  hit <- tapply(dst <= CROSSCHECK_BUFFER_M, pts$fid, function(x) 100 * mean(x))
  comm_m$pct_in_osm <- NA_real_
  comm_m$pct_in_osm[match(as.integer(names(hit)), comm_m$fid)] <- round(as.numeric(hit))
  community_sf$pct_in_osm <- comm_m$pct_in_osm

  crosscheck <- comm_m %>%
    st_drop_geometry() %>%
    transmute(source, layer, name,
              length_mi = round(len_m / 1609.344, 2), pct_in_osm) %>%
    arrange(pct_in_osm, desc(length_mi))
  write.csv(crosscheck, "ref/crosscheck.csv", row.names = FALSE)

  by_src <- crosscheck %>%
    group_by(source) %>%
    summarise(features = n(),
              miles = round(sum(length_mi, na.rm = TRUE), 1),
              matched_pct = round(weighted.mean(pmin(pct_in_osm, 100), length_mi,
                                                na.rm = TRUE), 0),
              .groups = "drop")
  gaps <- crosscheck %>% filter(is.na(pct_in_osm) | pct_in_osm < 25,
                                length_mi >= 0.1)
  rep <- c(
    "# Community-map cross-check", "",
    sprintf("Generated %s. A community trail counts as *covered* when it lies within %dm of an OSM trail in this map.",
            Sys.Date(), CROSSCHECK_BUFFER_M), "",
    "## Coverage by source", "",
    "| Source | Features | Miles | Length-weighted % matched in OSM |",
    "|---|---:|---:|---:|",
    by_src %>% mutate(r = sprintf("| %s | %d | %.1f | %d%% |",
                                  source, features, miles, matched_pct)) %>% pull(r),
    "",
    sprintf("## Community trails with little/no OSM match (<25%%, >= 0.1 mi) — %d features",
            nrow(gaps)), "",
    "Candidates that exist on a community map but are missing, mis-tagged, or named differently in OSM — worth a manual look or an OSM edit.", "",
    "| Source | Layer | Name | Miles | % in OSM |",
    "|---|---|---|---:|---:|",
    gaps %>% mutate(r = sprintf("| %s | %s | %s | %.2f | %s |",
                                source, layer, ifelse(is.na(name), "(unnamed)", name),
                                length_mi, ifelse(is.na(pct_in_osm), "0",
                                                  as.character(pct_in_osm)))) %>% pull(r)
  )
  writeLines(rep, "ref/crosscheck.md")
  msg("Cross-check written to ref/crosscheck.{md,csv}")
  print(by_src)
} else {
  msg("No community KML lines found; skipping cross-check.")
}

# =============================================================================
# 4. Build the Leaflet map
# =============================================================================
# Simplify + round coordinates so the self-contained HTML stays light.
web <- out_sf %>%
  st_transform(3857) %>%
  st_simplify(dTolerance = SIMPLIFY_M, preserveTopology = FALSE) %>%
  st_transform(4326)
web <- web[!st_is_empty(web), ]

pal_cols <- c(
  "Protected path"    = "#1b7837",  # dedicated, paved, separated
  "Shared-use path"   = "#2c7fb8",  # paved multi-use
  "Unpaved trail"     = "#b35806",  # gravel / dirt
  "On-road bike lane" = "#762a83"   # marked lane in the roadway
)

esc <- function(x) ifelse(is.na(x), "", htmltools::htmlEscape(x))
web$popup <- sprintf(
  "<strong>%s</strong><br/><span style='color:%s'>&#9632;</span> %s<br/>Surface: %s<br/>%.2f mi<br/><a href='https://www.openstreetmap.org/way/%s' target='_blank'>OSM way %s</a>",
  ifelse(is.na(web$name), "(unnamed trail)", esc(web$name)),
  pal_cols[as.character(web$category)], esc(web$category),
  ifelse(is.na(web$surface) | web$surface == "", "unknown", esc(web$surface)),
  web$length_mi, web$osm_id, web$osm_id
)

map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron,  group = "Light") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addProviderTiles(providers$OpenStreetMap,      group = "OSM streets")

for (cat in cats) {
  d <- web[web$category == cat, ]
  if (!nrow(d)) next
  map <- addPolylines(
    map, data = d, color = pal_cols[[cat]], weight = 3, opacity = 0.85,
    dashArray = if (cat == "On-road bike lane") "6,6" else NULL,
    group = cat, popup = ~popup,
    label = ~ifelse(is.na(name), cat, name),
    highlightOptions = highlightOptions(weight = 6, opacity = 1,
                                        bringToFront = TRUE)
  )
}

overlay_groups <- cats
if (!is.null(community_sf) && nrow(community_sf)) {
  for (src in unique(community_sf$source)) {
    g  <- sprintf("Community: %s", src)
    cd <- community_sf[community_sf$source == src, ]
    map <- addPolylines(
      map, data = cd, color = "#444444", weight = 2, opacity = 0.6,
      dashArray = "2,6", group = g,
      label = ~ifelse(is.na(name), "community trail", name)
    )
    overlay_groups <- c(overlay_groups, g)
  }
}

title_html <- sprintf(
  "<div style='background:rgba(255,255,255,.92);padding:8px 12px;border-radius:6px;font:600 15px/1.3 sans-serif;box-shadow:0 1px 4px rgba(0,0,0,.3)'>Michigan Bike &amp; Rail Trails<br/><span style='font-weight:400;font-size:12px;color:#555'>%s mi mapped &middot; OSM data %s</span></div>",
  format(round(sum(out_sf$length_mi)), big.mark = ","), Sys.Date())

map <- map %>%
  addLayersControl(
    baseGroups = c("Light", "Satellite", "OSM streets"),
    overlayGroups = overlay_groups,
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(grep("^Community", overlay_groups, value = TRUE)) %>%
  addControl(html = title_html, position = "topleft") %>%
  addLegend(position = "bottomright", colors = unname(pal_cols),
            labels = names(pal_cols), title = "Trail type", opacity = 0.9) %>%
  addControl(html = "<div style='font:11px sans-serif;color:#333;background:rgba(255,255,255,.85);padding:3px 6px;border-radius:4px'>Data &copy; OpenStreetMap contributors (ODbL) &middot; mibikemap</div>",
             position = "bottomleft") %>%
  setView(lng = -84.55, lat = 43.3, zoom = 7)

# =============================================================================
# 5. Write the self-contained HTML
# =============================================================================
saveWidget(map, file = normalizePath(OUT_HTML, mustWork = FALSE),
           selfcontained = TRUE, title = "Michigan Bike & Rail Trails")
msg("Wrote %s (%.1f MB)", OUT_HTML, file.info(OUT_HTML)$size / 1e6)
msg("Done. Open %s or host it via GitHub Pages.", OUT_HTML)
