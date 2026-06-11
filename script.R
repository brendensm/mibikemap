#!/usr/bin/env Rscript
# =============================================================================
# mibikemap — a layered map of Michigan bike / rail trails
# -----------------------------------------------------------------------------
# Stitches together existing, curated trail maps AND reconciles them into one
# shared set of trail *types*, so you can ask "show me everything off-street",
# "just the mountain-bike trails", or "just the on-road bike lanes" regardless
# of which source a segment came from.
#
# Sources (add your own in SOURCES below):
#   * MDNR Designated Biking Trails  (Michigan GIS Open Data, ArcGIS)
#   * Lansing-area community map      (Google "My Maps" KML)
#   * Bridge to Bay / Thumb community map (Google "My Maps" KML)
#
# Each source maps its native attributes onto the shared TYPES via one of three
# schemes (set per source):
#   * "dnr"       — rules over the DNR attribute fields
#   * "layer_map" — exact KML folder name -> type (give `layer_types`)
#   * "keyword"   — keyword-match the name/folder/description (default fallback)
#
# Output: one self-contained output/index.html, colored & toggleable BY TYPE.
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(leaflet)
  library(htmlwidgets)
  library(htmltools)
})

# ---- About-page / contribution settings ------------------------------------
CONTACT_EMAIL <- ""
PROJECT_REPO  <- "https://github.com/brendensm/mibikemap"

# ---- The shared trail taxonomy ---------------------------------------------
# Order = draw order (top of list drawn last / on top) and legend order.
TYPES <- c(
  "Off-street path — paved",
  "Off-street path — unpaved",
  "Mountain bike trail",
  "On-road bike lane / route",
  "Planned / under construction",
  "Hiking / foot trail",
  "Other / unclassified"
)
TYPE_COLOR <- c("#1b7837", "#b35806", "#b2182b", "#542788",
                "#737373", "#1f78b4", "#bdbdbd")
TYPE_DASH  <- c(NA, NA, NA, "6,6", "2,6", "3,5", NA)
names(TYPE_COLOR) <- names(TYPE_DASH) <- TYPES
HIDDEN_TYPES <- c("Hiking / foot trail")   # present but off by default

# Surface / road-tag vocabularies used by the DNR classifier.
PAVED_SURF   <- c("Asphalt", "Concrete", "Boardwalk", "Bridge",
                  "Separated Use Surfaces Asphalt And Grass")
UNPAVED_SURF <- c("Crushed Limestone", "Gravel", "Dirt Natural", "Grass Turf",
                  "Rock Bedrock", "Sand", "Aggregate")
ROAD_VALS    <- c("State Forest Road", "County Road", "MDOT Road",
                  "Local Road", "State Park Road", "1")
# OSM `surface` values treated as unpaved (for the "osm" scheme).
OSM_UNPAVED  <- c("unpaved", "gravel", "fine_gravel", "compacted", "ground",
                  "dirt", "earth", "grass", "sand", "cinder", "pebblestone",
                  "wood", "woodchips", "clay", "rock")

# ---- Config -----------------------------------------------------------------
REFRESH      <- FALSE
UA           <- "mibikemap/1.0 (github pages bike map; brendensmithmi@gmail.com)"
OVERPASS_URL <- "https://overpass-api.de/api/interpreter"
OUT_HTML     <- "output/index.html"
SIMPLIFY_M   <- 4

SOURCES <- list(
  list(
    id = "dnr", name = "MDNR Designated Biking Trails",
    type = "arcgis", arcgis = "4b7fc9bfa1224f1cbe753f16dc223629_3",
    file = "data-raw/dnr_trails.geojson",
    homepage = "https://gis-michigan.opendata.arcgis.com/datasets/4b7fc9bfa1224f1cbe753f16dc223629_3",
    blurb = "The official statewide layer of biking trails designated by the Michigan Department of Natural Resources — state parks, forest pathways, and rail-trails.",
    scheme = "dnr",
    name_field = c("TrailNamePrimary", "BikingName", "DNRTrail"),
    popup_fields = c("SurfaceType", "TrailUseCategory", "TrailOnRoad",
                     "RailtrailType", "TrailSiteType")
  ),
  list(
    id = "dnr_railtrails", name = "MDNR Statewide Rail-Trails",
    type = "arcgis_rest",
    rest_url = "https://utility.arcgis.com/usrsvcs/servers/860ad7121ae043418c66abb32bb01e96/rest/services/DNR/DNRTrailsOPENDATA/FeatureServer/16",
    file = "data-raw/dnr_railtrails.geojson",
    homepage = "https://gis-michigan.opendata.arcgis.com/",
    blurb = "The MDNR's statewide rail-trail inventory — every rail-trail in Michigan including county- and Friends-group-managed ones (Fred Meijer trails, Kal-Haven, Hart-Montague, Upper Peninsula grades), with surface types.",
    scheme = "dnr",   # same attribute fields as the DNR biking layer
    # TrailNamePrimary is often a unit code here; prefer the descriptive names.
    name_field = c("BikingName", "RailGradeName", "TrailNamePrimary"),
    popup_fields = c("SurfaceType", "County", "TrailOwnership")
  ),
  list(
    id = "lansing", name = "Lansing Area Trails (community)",
    type = "kml", gmap_mid = "1F_16PkqYRdjYzxvX_0o8KjRK1o3vmCw",
    file = "data-raw/community_lansing.kml",
    homepage = "https://www.google.com/maps/d/viewer?mid=1F_16PkqYRdjYzxvX_0o8KjRK1o3vmCw",
    blurb = "A community-maintained Google map of Greater Lansing's paved trails, gravel trails, and on-street bike lanes.",
    scheme = "layer_map", default_type = "Other / unclassified",
    layer_types = c(
      "Paved Trails"                         = "Off-street path — paved",
      "Gravel Trails"                        = "Off-street path — unpaved",
      "On Street Bike Lanes"                 = "On-road bike lane / route",
      "Under Construction and Funded Trails" = "Planned / under construction",
      "Hiking Trails"                        = "Hiking / foot trail"),
    name_field = "Name", popup_fields = c("layer", "Description")
  ),
  list(
    id = "thumb", name = "Bridge to Bay / Thumb (community)",
    type = "kml", gmap_mid = "1rhTFlvNHUJtNVRcf46UCbJREvA2NL5Ij",
    file = "data-raw/community_thumb.kml",
    homepage = "https://www.google.com/maps/d/viewer?mid=1rhTFlvNHUJtNVRcf46UCbJREvA2NL5Ij",
    blurb = "A community Google map of the Bridge to Bay route and bikeways across Michigan's Thumb and Blue Water region.",
    # The KML folders ("Bikeways", "Other Bike Routes") don't encode surface, but
    # each placemark's NAME does — "Sidepath" and "Off Road Trail" are off-street,
    # "Busy/Local Road Route" and "Bike Lane" are on-road. Classify by name so the
    # off-road sections of the Bridge to Bay route show as off-street paths.
    classify = function(g) {
      nm <- tolower(as.character(g$trail_name)); nm[is.na(nm)] <- ""
      dplyr::case_when(
        grepl("unimproved", nm)                                  ~ "Off-street path — unpaved",
        grepl("off.?road|sidepath|riverwalk|boardwalk", nm)      ~ "Off-street path — paved",
        grepl("bike lane|road route|busy road|local road|sidewalk|bikeway", nm) ~ "On-road bike lane / route",
        TRUE                                                     ~ "On-road bike lane / route")
    },
    name_field = "Name", popup_fields = c("layer", "Description")
  ),
  list(
    id = "traverse", name = "Traverse Area Regional Trail Network (community)",
    type = "kml", gmap_mid = "1qW01G0whDPI8FmSpTFHk7qHVHTDLOCk",
    file = "data-raw/community_traverse.kml",
    homepage = "https://www.google.com/maps/d/viewer?mid=1qW01G0whDPI8FmSpTFHk7qHVHTDLOCk",
    blurb = "A community Google map of the TART trail network around Traverse City — the TART, Leelanau, Sleeping Bear Heritage and Boardman Lake trails, plus connectors.",
    # Folders don't encode type here; classify by trail name, paved by default,
    # with a couple of known natural-surface exceptions hand-tuned.
    scheme = "keyword", default_type = "Off-street path — paved",
    overrides = c("VASA" = "Mountain bike trail"),
    name_field = "Name", popup_fields = c("layer", "Description")
  ),
  list(
    id = "midland", name = "City of Midland Trails (city GIS)",
    type = "arcgis_rest",
    rest_url = "https://arcgis1.midland-mi.org/arcgis/rest/services/Outdoors/MapServer/1",
    file = "data-raw/midland_trails.geojson",
    homepage = "https://www.cityofmidlandmi.gov/1386/Pere-Marquette-Rail-Trail",
    blurb = "The City of Midland's public trails GIS — the Midland end of the Pere Marquette Rail-Trail plus the Chippewa Trail, Grand Curve Cycle Path, and City Forest mountain-bike singletrack.",
    # Classify from the layer's own use/surface columns.
    classify = function(g) {
      sv  <- function(col) if (col %in% names(g)) as.character(g[[col]]) else rep(NA, nrow(g))
      yes <- function(col) toupper(sv(col)) %in% "YES"
      surf <- tolower(sv("SURFTYPE"))
      mtb <- yes("MTBCYCLE"); road <- yes("ROADCYCLE"); hike <- yes("HIKING")
      paved <- grepl("asphalt|concrete|paved|boardwalk", surf)
      unp   <- grepl("native|dirt|gravel|crushed|soil|grass|sand|wood|stone", surf)
      ty <- ifelse(paved, "Off-street path — paved",
            ifelse(unp & mtb, "Mountain bike trail",
            ifelse(unp, "Off-street path — unpaved", "Off-street path — paved")))
      ty[hike & !mtb & !road] <- "Hiking / foot trail"
      ty
    },
    name_field = "NAME", popup_fields = c("SURFTYPE", "CONDITION", "SKILLLEVEL")
  ),
  list(
    id = "midland_bike", name = "City of Midland Bike Routes (city GIS)",
    type = "arcgis_rest",
    rest_url = "https://arcgis1.midland-mi.org/arcgis/rest/services/Outdoors/MapServer/0",
    file = "data-raw/midland_bikeroutes.geojson",
    homepage = "https://www.cityofmidlandmi.gov/204/Non-Motorized-Transportation",
    blurb = "The City of Midland's planned non-motorized network — marked bike lanes, designated on-street bike routes, and paved-path connectors.",
    classify = function(g) {
      ty <- toupper(as.character(if ("Type" %in% names(g)) g$Type else NA))
      dplyr::case_when(
        grepl("PROPOSED", ty)                        ~ "Planned / under construction",
        grepl("LANE|ROUTE", ty)                      ~ "On-road bike lane / route",
        grepl("PAVED PATH", ty)                      ~ "Off-street path — paved",
        TRUE                                         ~ "On-road bike lane / route")
    },
    name_field = "Type", popup_fields = c("Type")
  ),
  list(
    id = "pere_marquette", name = "Pere Marquette Rail-Trail (OpenStreetMap)",
    type = "osm", osm_name = "Pere Marquette (Rail.?Trail|State Trail)",
    file = "data-raw/osm_pere_marquette.osm",
    homepage = "https://www.openstreetmap.org/search?query=Pere%20Marquette%20Rail-Trail",
    blurb = "The full Pere Marquette corridor from OpenStreetMap: the Rail-Trail (Midland–Clare) continuing as the State Trail (Clare–Baldwin).",
    scheme = "osm", name_field = "name", popup_fields = c("highway", "surface")
  ),
  list(
    id = "osm_lp_railtrails",
    name = "Lower-Peninsula rail-trails (OpenStreetMap)",
    type = "osm",
    osm_name = "Baw Beese|Genesee Valley Trail|George Atkin|Alpena-Hillman|White Lake Pathway|Linear Park Pathway|Vassar Rail|Shaver Road|Dequindre Cut|Medbery|Cass City Walking|Interurban Trail|Martin Luther King Equality",
    file = "data-raw/osm_lp_railtrails.osm",
    homepage = "https://www.openstreetmap.org/",
    blurb = "Smaller Lower-Peninsula rail-trails not in the state layers — e.g. Genesee Valley, Baw Beese, Alpena–Hillman, the Detroit Dequindre Cut — from OpenStreetMap.",
    scheme = "osm", name_field = "name", popup_fields = c("highway", "surface")
  ),
  list(
    id = "lapeer", name = "Lapeer Linear Park Pathway (Outdoor Michigan)",
    type = "kml", url = "https://outdoormichigan.org/features/getkml/14751",
    file = "data-raw/lapeer_linear_park.kml",
    homepage = "https://outdoormichigan.org/feature/14751",
    blurb = "The Lapeer Linear Park Pathway, from the Outdoor Michigan trail guide.",
    scheme = "keyword", default_type = "Off-street path — paved",
    name_const = "Lapeer Linear Park Pathway",
    name_field = "Name", popup_fields = c("Description")
  )
)

dir.create("data",   showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
msg <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                 sprintf(...)))

# ---- Download helpers -------------------------------------------------------
download_if_needed <- function(url, dest) {
  if (!REFRESH && file.exists(dest) && file.info(dest)$size > 1e3) {
    msg("cache hit: %s (%.1f MB)", dest, file.info(dest)$size / 1e6)
    return(invisible(dest))
  }
  msg("downloading %s", basename(dest))
  resp <- httr::GET(url, httr::user_agent(UA), httr::timeout(180),
                    httr::write_disk(dest, overwrite = TRUE))
  if (httr::status_code(resp) != 200)
    stop("download failed (HTTP ", httr::status_code(resp), "): ", url)
  msg("saved %s (%.1f MB)", dest, file.info(dest)$size / 1e6)
  invisible(dest)
}
arcgis_url <- function(id)
  sprintf("https://opendata.arcgis.com/api/v3/datasets/%s/downloads/data?format=geojson&spatialRefId=4326&where=1%%3D1", id)
gmap_kml_url <- function(mid)
  sprintf("https://www.google.com/maps/d/kml?mid=%s&forcekml=1", mid)

# Fetch named OSM trails within Michigan via Overpass (cached OSM XML), and
# parse to sf lines. `out body; >; out skel qt;` returns the nodes osmdata
# needs to assemble geometry.
fetch_osm_named <- function(s) {
  q <- sprintf('[out:xml][timeout:180];
area["ISO3166-2"="US-MI"]->.mi;
(way(area.mi)[highway~"cycleway|path|footway|bridleway"][name~"%s",i];);
out body;
>;
out skel qt;', s$osm_name)
  if (REFRESH || !file.exists(s$file) || file.info(s$file)$size < 1e3) {
    msg("querying Overpass for /%s/", s$osm_name)
    resp <- httr::POST(OVERPASS_URL, body = list(data = q), encode = "form",
                       httr::user_agent(UA), httr::timeout(180),
                       httr::write_disk(s$file, overwrite = TRUE))
    if (httr::status_code(resp) != 200)
      stop("Overpass HTTP ", httr::status_code(resp))
    msg("saved %s (%.1f MB)", s$file, file.info(s$file)$size / 1e6)
  } else msg("cache hit: %s", s$file)
  g <- osmdata::osmdata_sf(q = q, doc = s$file, quiet = TRUE)$osm_lines
  if (is.null(g) || !nrow(g)) return(NULL)
  g$layer <- NA_character_
  g
}

# ---- Text + geometry helpers ------------------------------------------------
clean_text <- function(x) {
  x <- as.character(x)
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("[[:space:]]+", " ", trimws(x))
  ifelse(is.na(x) | x %in% c("", "-1", "-2"), NA_character_, substr(x, 1, 300))
}
first_nonempty <- function(df, fields) {
  out <- rep(NA_character_, nrow(df))
  for (f in fields) if (f %in% names(df)) {
    v <- clean_text(df[[f]])
    v[grepl("^(LP|UP)\\s*\\d+$", v)] <- NA  # skip DNR unit codes like "LP 58"
    out[is.na(out)] <- v[is.na(out)]
  }
  out
}

# ---- Type classifiers -------------------------------------------------------
classify_dnr <- function(g) {
  surf   <- as.character(g$SurfaceType)
  onroad <- as.character(g$TrailOnRoad)
  rail   <- as.character(g$RailtrailType)
  bik    <- as.character(g$Biking)
  paved  <- surf %in% PAVED_SURF
  out <- rep("Other / unclassified", nrow(g))
  out[surf %in% UNPAVED_SURF] <- "Off-street path — unpaved"   # by surface
  out[paved]                  <- "Off-street path — paved"
  out[grepl("Mountain Biking", bik)] <- "Mountain bike trail"  # MTB over plain surface
  rt <- rail == "Railtrail"                                    # rail-trails are shared-use
  out[rt] <- ifelse(paved[rt], "Off-street path — paved", "Off-street path — unpaved")
  out[onroad %in% ROAD_VALS] <- "On-road bike lane / route"    # on-road wins
  out
}
classify_osm <- function(g) {
  gv <- function(n) if (n %in% names(g)) as.character(g[[n]]) else rep(NA_character_, nrow(g))
  surf <- tolower(gv("surface")); hw <- gv("highway"); bic <- tolower(gv("bicycle"))
  out <- rep("Off-street path — paved", nrow(g))            # default: paved path
  out[!is.na(surf) & surf %in% OSM_UNPAVED] <- "Off-street path — unpaved"
  out[hw %in% c("footway") & (is.na(bic) | bic == "no")] <- "Hiking / foot trail"
  out
}
classify_layer_map <- function(g, map_vec, default) {
  lt <- unname(map_vec[as.character(g$layer)])
  ifelse(is.na(lt), default, lt)
}
classify_keyword <- function(txt, default) {
  t <- tolower(ifelse(is.na(txt), "", txt))
  out <- rep(default, length(t))
  hit <- function(p) grepl(p, t, perl = TRUE)
  out[hit("hik|foot|pedestrian|nature trail")]                      <- "Hiking / foot trail"
  out[hit("paved|asphalt|concrete|greenway|riverwalk|boardwalk|rail.?trail")] <- "Off-street path — paved"
  out[hit("gravel|unpaved|crushed|cinder|limestone|\\bdirt\\b|natural surface")] <- "Off-street path — unpaved"
  out[hit("mountain|\\bmtb\\b|single.?track")]                      <- "Mountain bike trail"
  out[hit("bike lane|on.?street|on.?road|bikeway|sharrow|(bike|bicycle|signed)[ -]?route")] <- "On-road bike lane / route"
  out[hit("construct|funded|propos|planned|future|phase")]          <- "Planned / under construction"
  out
}
classify_source <- function(g, s) {
  ty <- if (is.function(s$classify)) s$classify(g) else switch(s$scheme %||% "keyword",
    dnr       = classify_dnr(g),
    osm       = classify_osm(g),
    layer_map = classify_layer_map(g, s$layer_types, s$default_type %||% "Other / unclassified"),
    keyword   = classify_keyword(paste(g$layer, g$trail_name,
                  if ("Description" %in% names(g)) g$Description else ""),
                  s$default_type %||% "Other / unclassified"))
  ty <- as.character(ty)
  # Optional per-source hand-tuning: exact trail name -> type.
  if (!is.null(s$overrides)) {
    ov <- s$overrides[as.character(g$trail_name)]
    ty <- ifelse(is.na(ov), ty, unname(ov))
  }
  ty[!ty %in% TYPES] <- "Other / unclassified"
  factor(ty, levels = TYPES)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Generic loader: any source -> tidy sf of typed lines -------------------
read_lines_any <- function(path) {
  layers <- tryCatch(st_layers(path)$name, error = function(e) NA_character_)
  if (length(layers) <= 1 || all(is.na(layers))) {
    g <- suppressWarnings(st_read(path, quiet = TRUE)); g$layer <- NA_character_
    out <- list(g)
  } else {
    out <- lapply(layers, function(l) {
      g <- tryCatch(suppressWarnings(st_read(path, layer = l, quiet = TRUE)),
                    error = function(e) NULL)
      if (is.null(g) || !nrow(g)) return(NULL)
      g$layer <- l; g
    })
  }
  out <- Filter(Negate(is.null), out)
  out <- lapply(out, function(g) {
    g <- st_zm(g)
    g <- g[st_geometry_type(g) %in% c("LINESTRING", "MULTILINESTRING"), ]
    if (!nrow(g)) NULL else g
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(NULL)
  allcols <- unique(unlist(lapply(out, function(g) setdiff(names(g), attr(g, "sf_column")))))
  out <- lapply(out, function(g) {
    for (c in setdiff(allcols, names(g))) g[[c]] <- NA
    g[, c(allcols, attr(g, "sf_column"))]
  })
  do.call(rbind, out) |> st_transform(4326)
}

load_source <- function(s) {
  msg("source: %s", s$name)
  if (identical(s$type, "osm")) {
    g <- fetch_osm_named(s)
  } else {
    if (identical(s$type, "arcgis")) download_if_needed(arcgis_url(s$arcgis), s$file)
    else if (identical(s$type, "arcgis_rest")) download_if_needed(
      paste0(sub("/+$", "", s$rest_url),
             "/query?where=1%3D1&outFields=*&outSR=4326&f=geojson"), s$file)
    else if (identical(s$type, "kml")) download_if_needed(
      if (!is.null(s$url)) s$url else gmap_kml_url(s$gmap_mid), s$file)
    else if (identical(s$type, "geojson") && grepl("^https?://", s$file)) {
      dest <- file.path("data-raw", paste0(s$id, ".geojson"))
      download_if_needed(s$file, dest); s$file <- dest
    }
    g <- read_lines_any(s$file)
  }
  if (is.null(g)) { warning("no line geometry in ", s$file); return(NULL) }

  g$trail_name <- first_nonempty(g, s$name_field)
  if (!is.null(s$name_const)) g$trail_name <- s$name_const  # single-trail sources
  g$type   <- classify_source(g, s)
  g$source <- s$name

  # Simplify geometry and measure length BEFORE building the popup, so each
  # segment can show its own mileage. `trail_miles` sums every segment in this
  # source that shares the same name (a named trail is usually many segments).
  geom <- g |> st_geometry() |> st_transform(3857) |>
    st_simplify(dTolerance = SIMPLIFY_M) |> st_transform(4326)
  st_geometry(g) <- geom
  g <- g[!st_is_empty(g), ]
  g$miles <- as.numeric(st_length(g)) / 1609.344
  g$trail_miles <- ave(g$miles, paste(s$id, g$trail_name), FUN = sum)

  extra <- ""
  for (f in s$popup_fields) if (f %in% names(g)) {
    v <- clean_text(g[[f]]); lab <- if (f == "layer") "Map category" else f
    extra <- paste0(extra, ifelse(is.na(v), "",
                    sprintf("<br/><span style='color:#777'>%s:</span> %s", lab, v)))
  }
  # Show the clicked segment's length, plus the full named-trail total when the
  # trail spans more than this one segment.
  miles_html <- ifelse(is.na(g$miles), "",
    ifelse(!is.na(g$trail_name) & abs(g$trail_miles - g$miles) > 0.05,
      sprintf("<br/><span style='color:#777'>Length:</span> %.1f mi <span style='color:#777'>(segment) · %.1f mi total</span>",
              g$miles, g$trail_miles),
      sprintf("<br/><span style='color:#777'>Length:</span> %.1f mi", g$miles)))
  g$popup <- sprintf(
    "<strong>%s</strong><br/><span style='color:%s'>&#9632;</span> <b>%s</b>%s<br/><span style='color:#777'>Source:</span> %s%s",
    ifelse(is.na(g$trail_name), "(unnamed)", htmltools::htmlEscape(g$trail_name)),
    TYPE_COLOR[as.character(g$type)], htmltools::htmlEscape(as.character(g$type)),
    miles_html, htmltools::htmlEscape(s$name), extra)

  g[, c("trail_name", "type", "source", "popup", "miles")]
}

loaded <- lapply(SOURCES, function(s) tryCatch(load_source(s), error = function(e) {
  warning("skipping ", s$id, ": ", conditionMessage(e)); NULL }))
loaded <- Filter(Negate(is.null), loaded)
if (!length(loaded)) stop("No sources loaded.")
trails <- do.call(rbind, loaded)
trails$type <- factor(as.character(trails$type), levels = TYPES)

# Reconciliation summary: type x source mileage.
msg("Reconciled %d segments from %d sources.", nrow(trails), length(loaded))
tab <- trails |> st_drop_geometry() |>
  group_by(type, source) |> summarise(mi = round(sum(miles), 1), .groups = "drop")
print(tidyr::pivot_wider(tab, names_from = source, values_from = mi, values_fill = 0))
type_mi <- trails |> st_drop_geometry() |> group_by(type) |>
  summarise(mi = round(sum(miles), 1), .groups = "drop")

# =============================================================================
# Build the Leaflet map — colored & toggled BY TYPE
# =============================================================================
map <- leaflet(width = "100%", height = "100%",
               options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$CartoDB.Positron,  group = "Light") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addProviderTiles(providers$OpenStreetMap,      group = "OSM streets")

present <- TYPES[TYPES %in% levels(droplevels(trails$type))]
for (ty in present) {
  d <- trails[trails$type == ty, ]
  if (!nrow(d)) next
  map <- addPolylines(
    map, data = d, color = TYPE_COLOR[[ty]], weight = 3, opacity = 0.85,
    dashArray = TYPE_DASH[[ty]], group = ty, popup = ~popup,
    label = ~ifelse(is.na(trail_name), ty, trail_name),
    highlightOptions = highlightOptions(weight = 6, opacity = 1, bringToFront = TRUE))
}

map <- map |>
  addLayersControl(
    baseGroups = c("Light", "Satellite", "OSM streets"),
    overlayGroups = present,
    options = layersControlOptions(collapsed = FALSE)) |>
  addLegend(position = "bottomright", colors = unname(TYPE_COLOR[present]),
            labels = present, title = "Trail type", opacity = 0.9) |>
  setView(lng = -84.6, lat = 44.2, zoom = 7)
hide_now <- intersect(HIDDEN_TYPES, present)
if (length(hide_now)) map <- hideGroup(map, hide_now)

# =============================================================================
# Wrap the map in a two-tab page (About + Map) and inline to one HTML file
# =============================================================================
# Note: source/type totals are deliberately NOT shown — the same trail often
# appears in more than one source, so summing them double-counts mileage. Per-
# segment lengths (shown in each segment's popup) are the reliable figures.
sources_html <- paste0("<ul class='sources'>", paste(vapply(SOURCES, function(s) {
  if (is.null(s$homepage)) return("")
  sprintf("<li><a href='%s' target='_blank' rel='noopener'>%s</a><br/>%s</li>",
          s$homepage, htmlEscape(s$name), htmlEscape(s$blurb %||% ""))
}, ""), collapse = ""), "</ul>")

contribute_html <- paste0(
  "<p>This is an open, community resource — and it's far from complete. If you know a trail that's missing, maintain a local trail map, or spot something mis-categorized, please pitch in:</p>",
  "<ul>",
  "<li>Share a map: a <b>Google My Maps</b> link, a <b>GeoJSON</b> file, or an <b>ArcGIS / open-data</b> layer can all be added as a new source.</li>",
  "<li>Flag a fix: tell us about trails that are missing, renamed, or in the wrong category.</li>",
  "</ul>",
  sprintf("<p>Open an issue on <a href='%s/issues' target='_blank' rel='noopener'>GitHub</a> — share a link to your map or describe the fix, and we'll fold it in.</p>",
          PROJECT_REPO))

about_html <- HTML(paste0(
  "<div class='about'>",
  "<h1>mibikemap</h1>",
  "<p>A single, open map of bike trails in Michigan gathered from official and community sources and sorted into one",
  " convenient map.</p>",
  "<p>Open the <b>Map</b> tab and toggle any trail type on or off using the",
  " control in the top-right corner. Click any segment to see its length and source.</p>",
  "<div class='callout'><h2 style='margin-top:0'>Help grow the map</h2>",
  contribute_html, "</div>",
  "<h2>Data sources</h2>",
  "<p>Every segment keeps a link back to its source, which you'll find in its popup:</p>",
  sources_html,
  "<p class='muted'>Background maps &copy; OpenStreetMap contributors and CARTO.</p>",
  "<p class='muted'>Claude Code was used to design this site and to scrape trail data.</p>",
  "</div>"))

# NB: `.tabbar p{display:contents}` below neutralizes a stray <p> that pandoc's
# self-contained packer wraps around the tabbar's inline children. Without it the
# logo's width:auto resolves against the full bar width and balloons. (Keep CSS
# comments OUT of the string below — pandoc mis-parses /* ... */ and drops rules.)
css <- HTML("
*{box-sizing:border-box} html,body{margin:0;height:100%}
body{font-family:system-ui,-apple-system,'Segoe UI',Roboto,sans-serif;color:#1f2937}
.tabbar{position:absolute;top:0;left:0;right:0;height:60px;display:flex;align-items:center;
  gap:4px;padding:0 16px;background:#0f5132;color:#fff;z-index:1200;box-shadow:0 1px 5px rgba(0,0,0,.3)}
.tabbar p{display:contents}
.brand{display:flex;align-items:center;height:100%;margin:0 24px 0 4px;padding-right:24px;
  border-right:1px solid rgba(255,255,255,.25)}
.brand svg{height:42px;width:auto;max-height:42px;display:block;flex:0 0 auto}
.tab-btn{appearance:none;border:0;background:transparent;color:#cfe8d8;font-size:14px;
  font-weight:600;padding:8px 16px;border-radius:7px;cursor:pointer}
.tab-btn:hover{background:rgba(255,255,255,.14);color:#fff}
.tab-btn.active{background:#fff;color:#0f5132}
.tab-panel{position:absolute;top:60px;left:0;right:0;bottom:0;visibility:hidden;overflow:auto}
.tab-panel.active{visibility:visible}
#tab-map .html-widget,#tab-map .leaflet-container{height:100%!important;width:100%!important}
.about{max-width:780px;margin:0 auto;padding:34px 22px 72px;line-height:1.62}
.about h1{font-size:27px;margin:.1em 0 .15em;color:#0f5132}
.about h2{font-size:22px;margin:1.7em 0 .45em;color:#0f5132}
.about p{margin:.55em 0} .about a{color:#0f5132;font-weight:600}
.muted{color:#6b7280;font-weight:400}
.chip{display:inline-block;width:13px;height:13px;border-radius:3px;margin-right:9px;vertical-align:middle}
.typelist{list-style:none;padding:0;margin:.4em 0}.typelist li{margin:.34em 0}
.sources{padding-left:0;list-style:none}.sources li{margin:.8em 0}
.callout{margin-top:1.6em;padding:18px 20px;background:#e7f4ec;border:1px solid #bfe0cd;border-radius:12px}
.callout ul{margin:.4em 0 .6em} .callout li{margin:.3em 0}")

js <- HTML("
function showTab(id){
  document.querySelectorAll('.tab-panel').forEach(function(p){p.classList.remove('active')});
  document.querySelectorAll('.tab-btn').forEach(function(b){b.classList.remove('active')});
  document.getElementById('tab-'+id).classList.add('active');
  document.getElementById('btn-'+id).classList.add('active');
  window.dispatchEvent(new Event('resize'));
}")

# Michigan-outline-with-bike logo for the header (real state boundary,
# Census cartographic via tigris, simplified). White state, green bike.
# ---- Header logo: a tiny copy of the map (white Michigan + green trails) ----
# Reads the saved state outline and draws the live trail geometry over it, all
# in Michigan Lambert (EPSG:3078), so the brand mark *is* the map in miniature.
# Regenerate the outline with data-raw/generate_mi_logo.R.
mi_outline <- st_transform(st_read("data-raw/mi_outline.geojson", quiet = TRUE), 3078)
.bb <- as.numeric(st_bbox(mi_outline))           # xmin, ymin, xmax, ymax
.xmin <- .bb[1]; .ymax <- .bb[4]
.sc <- 120 / (.bb[4] - .bb[2]); .W <- round((.bb[3] - .bb[1]) * .sc, 1)
xfx <- function(x) round((x - .xmin) * .sc, 1)
xfy <- function(y) round((.ymax - y) * .sc, 1)
subpaths <- function(co, grp, close) paste(vapply(split(as.data.frame(co), grp),
  function(r) paste0("M", paste0(xfx(r$X), ",", xfy(r$Y), collapse = "L"),
                     if (close) "Z" else ""), ""), collapse = "")

# White state polygons (one filled subpath per ring).
.poly <- st_coordinates(st_cast(st_geometry(mi_outline), "POLYGON"))
state_d <- subpaths(.poly, interaction(.poly[, "L1"], .poly[, "L2"], drop = TRUE), TRUE)

# Live trails -> thin green polylines, heavily simplified for a tiny mark.
.tg <- st_simplify(st_transform(st_geometry(trails), 3078), dTolerance = 650)
.tg <- suppressWarnings(st_cast(st_cast(.tg, "MULTILINESTRING"), "LINESTRING"))
.tg <- .tg[!st_is_empty(.tg)]
.tc <- st_coordinates(.tg)
trail_d <- subpaths(.tc, .tc[, "L1"], FALSE)

MI_LOGO <- HTML(sprintf(paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %s 120" width="%d" height="42"',
  ' role="img" aria-label="mibikemap" style="display:block">',
  '<path fill="#fff" d="%s"/>',
  '<path fill="none" stroke="#1b7837" stroke-width="1.4" stroke-linecap="round"',
  ' stroke-linejoin="round" d="%s"/></svg>'),
  .W, round(.W / 120 * 42), state_d, trail_d))

# Same mark as a browser-tab favicon: the logo on the header-green tile (so the
# white state reads on any tab colour), inlined as an SVG data URI. Single quotes
# + minimal %-encoding keep it valid inside the href without base64 bloat.
favicon_svg <- sprintf(paste0(
  "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 120 120'>",
  "<rect width='120' height='120' rx='22' fill='#0f5132'/>",
  "<g transform='translate(%s 12) scale(0.8)'>",
  "<path fill='#fff' d='%s'/>",
  "<path fill='none' stroke='#1b7837' stroke-width='2.1' stroke-linecap='round'",
  " stroke-linejoin='round' d='%s'/></g></svg>"),
  round((120 - .W * 0.8) / 2, 1), state_d, trail_d)
enc <- favicon_svg
enc <- gsub("%", "%25", enc, fixed = TRUE); enc <- gsub("#", "%23", enc, fixed = TRUE)
enc <- gsub("<", "%3C", enc, fixed = TRUE);  enc <- gsub(">", "%3E", enc, fixed = TRUE)
enc <- gsub(" ", "%20", enc, fixed = TRUE)
FAVICON <- paste0("data:image/svg+xml,", enc)

page <- tagList(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("mibikemap"),
    tags$link(rel = "icon", type = "image/svg+xml", href = FAVICON),
    tags$style(css)),
  tags$div(class = "tabbar",
    tags$span(class = "brand", MI_LOGO),
    tags$button(id = "btn-about", class = "tab-btn active",
                onclick = "showTab('about')", "About"),
    tags$button(id = "btn-map", class = "tab-btn",
                onclick = "showTab('map')", "Map")),
  tags$div(id = "tab-about", class = "tab-panel active", about_html),
  tags$div(id = "tab-map", class = "tab-panel", map),
  tags$script(js))

# Render with external dependencies, then inline them all into one file using
# htmlwidgets' own self-contained packer (the same one saveWidget uses).
out_abs <- file.path(getwd(), OUT_HTML)
tmp <- tempfile(fileext = ".html")
save_html(page, tmp, libdir = "lib")
htmlwidgets:::pandoc_self_contained_html(tmp, out_abs)
unlink(c(tmp, file.path(dirname(tmp), "lib")), recursive = TRUE)
if (!file.exists(out_abs)) stop("self-contained packing failed")
# save_html() emits the document's real <title> empty (the one in our tags$head
# lands in the body); set the browser-tab title here so it actually shows.
html <- readLines(out_abs, warn = FALSE)
html <- sub("<title></title>", "<title>mibikemap</title>", html, fixed = TRUE)
writeLines(html, out_abs)
msg("Wrote %s (%.1f MB)", OUT_HTML, file.info(out_abs)$size / 1e6)
msg("Done. Open %s or host it via GitHub Pages.", OUT_HTML)
