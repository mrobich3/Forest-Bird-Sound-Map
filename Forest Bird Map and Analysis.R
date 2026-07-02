#WHERE THE FOREST SINGS
#Poland Municipal Forest bird transect, 2026-06-19 6/17/26 - 6/23/26
#15 stops (06:01–08:13), Merlin Sound ID, 4-min listens


library(sf); library(osmdata); library(leaflet)
library(dplyr); library(tidyr); library(readr); library(MASS); library(stringr)

PROJ <- "C:/Users/mrobi/OneDrive/Documents/Information Design/Final Project/"

pins    <- read_csv(paste0(PROJ, "pins.csv"))
spp_raw <- read_csv(paste0(PROJ, "species_long.csv"))


#Poland sits in the Black-capped x Carolina Chickadee contact zone, the two hybridize and learn each other's songs, and Merlin flips between them.

spp <- spp_raw %>%
  mutate(species = ifelse(species %in% c("Black-capped Chickadee","Carolina Chickadee"),
                          "Chickadee Species.", species))

#2. OUTCOMES + TIME COVARIATE + LINKED BIRD LISTS
rich <- spp %>% distinct(session_id, species) %>% count(session_id, name = "richness")
card <- spp %>% group_by(session_id) %>%
  summarise(cardinal = as.integer(any(species == "Northern Cardinal")), .groups = "drop")

pins <- pins %>%
  left_join(rich, by = "session_id") %>%
  left_join(card, by = "session_id") %>%
  mutate(richness = replace_na(richness, 0L),
         cardinal = replace_na(cardinal, 0L))

#bird-name -> All About Birds link (photo + sound)
aab_url <- function(sp) {
  fix <- c("Chickadee Species." = "Black-capped_Chickadee",
           "Northern House Wren" = "House_Wren")
  key <- ifelse(sp %in% names(fix), fix[sp], gsub(" ", "_", sp))
  paste0("https://www.allaboutbirds.org/guide/", key)
}
birdlink <- function(sp) paste0("<a href='", aab_url(sp), "' target='_blank'>", sp, "</a>")

birdlist <- spp %>% group_by(session_id) %>%
  summarise(birds = paste(birdlink(sort(unique(species))), collapse = "<br>"),
            .groups = "drop")
pins <- left_join(pins, birdlist, by = "session_id")

# 3. DISTANCES FROM OPENSTREETMAP (meters)  -- writes pins_with_distances.csv
pins_sf <- st_as_sf(pins, coords = c("lon","lat"), crs = 4326, remove = FALSE)
prj <- 32617  # UTM 17N -> meters
bb  <- st_bbox(st_transform(st_buffer(st_transform(pins_sf, prj), 4000), 4326))

water_lines <- opq(bb, timeout=60) %>% add_osm_feature("waterway", c("stream","river","ditch")) %>%
  osmdata_sf() %>% .$osm_lines
water_poly  <- opq(bb, timeout=60) %>% add_osm_feature("natural", "water") %>%
  osmdata_sf() %>% .$osm_polygons
free_lines  <- opq(bb, timeout=60) %>% add_osm_feature(key = "highway", value = "motorway") %>%
  osmdata_sf() %>% .$osm_lines

P  <- st_transform(pins_sf, prj)
W  <- st_union(st_transform(c(st_geometry(water_lines), st_geometry(water_poly)), prj))
Fr <- st_union(st_transform(st_geometry(free_lines), prj))
W_draw <- st_transform(st_geometry(water_lines), 4326)   # lines only, for the map

pins$dist_water_m   <- as.numeric(st_distance(P, W))
pins$dist_freeway_m <- as.numeric(st_distance(P, Fr))

# remake pins_sf so it carries the distance columns, then write the file
pins_sf <- st_as_sf(pins, coords = c("lon","lat"), crs = 4326, remove = FALSE)
write_csv(st_drop_geometry(pins_sf),
          paste0(PROJ, "pins_with_distances.csv"))

# MASTER DATASET = your field data + coordinates/distances
dat <- read_csv(paste0(PROJ, "forest_master_dataset.csv")) %>%
  rename(richness = Count, zone = Location, db = dB,
         animals = Animals, house = `House in Sight`,
         field_notes = Notes, temp = Temp)

dist <- read_csv(paste0(PROJ, "pins_with_distances.csv")) %>%
  dplyr::select(session_id, dist_water_m, dist_freeway_m)
dat <- left_join(dat, dist, by = "session_id")

#derive squirrel count + animal-present flag from the free-text Animals column
dat <- dat %>% mutate(
  squirrels = as.integer(str_match(animals, "(\\d+)\\s*Squirrel")[,2]) %>% replace_na(0L),
  animal_present = as.integer(animals != "None"))

#closest trail name + type per stop
dat <- dat %>% mutate(trail = dplyr::case_when(
  session_id == "S01" ~ "Parkway Drive Entrance",
  session_id == "S02" ~ "Bluebell Trail Bridge (Main)",
  session_id == "S03" ~ "Log Cabin Trail (Side)",
  session_id == "S04" ~ "Butler Trail (Main)",
  session_id == "S05" ~ "South Pine Trail (Side)",
  session_id == "S06" ~ "Beaver Dam Trail East (Side)",
  session_id == "S07" ~ "Beaver Dam Trail West (Side)",
  session_id == "S08" ~ "Lower McKinley Trail West (Main)",
  session_id == "S09" ~ "Lower McKinley Trail East (Main)",
  session_id == "S10" ~ "Gutknecht Trail (Side)",
  session_id == "S11" ~ "Upper McKinley Trail (Main)",
  session_id == "S12" ~ "College Lane Entrance",
  session_id == "S13" ~ "Manor Trail (Side)",
  session_id == "S14" ~ "Big Willow Trail (Main)",
  session_id == "S15" ~ "Thatcher Trail (Side)",
  TRUE ~ zone))

#build raw GitHub image URLs: session S07
dat <- dat %>% mutate(
  photo_num = as.integer(sub("S", "", session_id)),
  photo_url = paste0(
    "https://raw.githubusercontent.com/mrobich3/Forest-Bird-Sound-Map/main/Photo%20",
    photo_num, ".jpeg"))

#carry trail + dB onto pins_sf for the popups
pins_sf <- pins_sf %>% left_join(dplyr::select(dat, session_id, trail, db, photo_url), by = "session_id")

# 4. INTERACTIVE MAP (OpenStreetMap base)
library(htmltools); library(htmlwidgets)

pal <- colorNumeric("YlOrRd", domain = pins_sf$richness)
pins_sf$photo_html <- ifelse(is.na(pins_sf$photo_url), "",
                             paste0("<br><img src='", pins_sf$photo_url, "' width='200'>"))

pins_sf$popup <- sprintf(
  "<b>%s</b><br>Species heard: %d<br>
   Water: %dm &nbsp;|&nbsp; Freeway: %dm &nbsp;|&nbsp; %d dB%s<hr>%s",
  pins_sf$trail, pins_sf$richness,
  round(pins_sf$dist_water_m), round(pins_sf$dist_freeway_m), pins_sf$db,
  pins_sf$photo_html, pins_sf$birds)

#species points for per-species layers
spp_sf <- spp %>% left_join(pins, by = "session_id") %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

#linked toggle labels: "Wood Thrush (info)" where (info) is the link
species_list   <- sort(unique(spp$species))
species_labels <- paste0(species_list,
  " <a href='", aab_url(species_list), "' target='_blank'>(info)</a>")
names(species_labels) <- species_list

#build the map
m <- leaflet(pins_sf) %>%
  addTiles() %>%
  addPolylines(data = W_draw, color = "#33aabb", weight = 2, group = "Water") %>%
  addCircleMarkers(radius = ~4 + richness, color = ~pal(richness),
                   weight = 1, fillOpacity = .85,
                   label = ~lapply(trail, htmltools::HTML),
                   labelOptions = labelOptions(textsize = "15px", direction = "top",
                                               style = list("font-weight" = "bold")),
                   popup = ~popup, group = "All stops (richness)") %>%
  addLegend(pal = pal, values = ~richness, title = "Species<br>richness")

for (s in species_list) {
  pts <- dplyr::filter(spp_sf, species == s)
  m <- addCircleMarkers(m, data = pts, radius = 6, color = "#cc00cc",
                        stroke = FALSE, fillOpacity = .9,
                        label = s, group = species_labels[[s]])
  if (nrow(pts) >= 3) {
    hull <- st_convex_hull(st_union(pts))
    m <- addPolygons(m, data = hull, color = "#cc00cc",
                     weight = 1, fillOpacity = .12, group = species_labels[[s]])
  }
}

m <- m %>% addLayersControl(
  overlayGroups = c("Water", "All stops (richness)", unname(species_labels)),
  options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(unname(species_labels)) %>%
  setView(lng = mean(pins$lon), lat = mean(pins$lat), zoom = 16)

title_html <- tags$div(
  HTML("<b>Where the Forest Sings</b><br>
        <span style='font-size:13px;font-weight:normal'>
        Bird species heard at 15 listening stops across Poland Municipal Forest,<br>
        recorded between 6 AM and 8 AM in June<br>
        Click a stop to see its birds; toggle a species to map where it was found.</span>"),
  style = "position: relative; background: rgba(255,255,255,0.88);
           padding: 8px 14px; border-radius: 6px; font-size: 18px;
           font-family: sans-serif; box-shadow: 0 1px 4px rgba(0,0,0,0.3);")
m <- m %>% addControl(title_html, position = "topleft")
m

# htmlwidgets::saveWidget(m, paste0(PROJ, "forest_sound_map.html"), selfcontained = TRUE)

#5. MODELS  (distances scaled to 100 m -> coefficients read "per 100 m")
pins$water100 <- pins$dist_water_m   / 100
pins$free100  <- pins$dist_freeway_m / 100

cat("\n--- predictor collinearity ---\n")
cat("cor(dist_water, dist_freeway) =",
    round(cor(pins$water100, pins$free100), 2), "\n")



#sensitivity check: drop the freeway-edge stop (S08) to test acoustic masking
clean_data <- pins %>% dplyr::filter(!session_id %in% c("S08"))

cat("\n=== Poisson (all 15 stops): richness ~ water + freeway ===\n")
m_pois <- glm(richness ~ water100 + free100, family = poisson, data = pins)
print(summary(m_pois))
disp <- sum(residuals(m_pois, "pearson")^2) / df.residual(m_pois)
cat("dispersion =", round(disp, 2), " (>~1.5 -> refit quasipoisson / glm.nb)\n")
cat("\nIncidence rate ratios (per 100 m) + 95% CI:\n")
print(round(exp(cbind(IRR = coef(m_pois), confint(m_pois))), 3))

cat("\n=== Poisson: richness ~ distance-to-water + distance-to-freeway ===\n")
m_pois <- glm(richness ~ water100 + free100, family = poisson, data = clean_data)
print(summary(m_pois))
disp <- sum(residuals(m_pois, "pearson")^2) / df.residual(m_pois)
cat("dispersion =", round(disp, 2), " (>~1.5 -> refit quasipoisson / glm.nb)\n")
cat("\nIncidence rate ratios (per 100 m) + 95% CI:\n")
print(round(exp(cbind(IRR = coef(m_pois), confint(m_pois))), 3))

# secondary: cardinal presence (EXPLORATORY -- 11/15 present, unstable; read direction not p)
cat("\n=== Logistic: cardinal ~ water + freeway ===\n")
m_card <- glm(cardinal ~ water100 + free100, family = binomial, data = pins)
print(summary(m_card))
cat("\nOdds ratios (per 100 m) + 95% CI:\n")
print(round(exp(cbind(OR = coef(m_card), confint(m_card))), 3))


cat("\n=== Logistic Sensitivity: cardinal ~ water + freeway ===\n")
m_card <- glm(cardinal ~ water100 + free100, family = binomial, data = clean_data)
print(summary(m_card))
cat("\nOdds ratios (per 100 m) + 95% CI:\n")
print(round(exp(cbind(OR = coef(m_card), confint(m_card))), 3))

#richness vs. distance to I-680 figure
library(ggplot2)

pins$lab <- dplyr::case_when(
  pins$richness == max(pins$richness) ~ paste0(pins$session_id, ": ", pins$richness, " species (interior)"),
  pins$richness == min(pins$richness) ~ paste0(pins$session_id, ": ", pins$richness, " species (near highway)"),
  TRUE ~ NA_character_)

p <- ggplot(pins, aes(dist_freeway_m, richness)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              formula = y ~ x, color = "#1a7a3a", fill = "#1a7a3a", alpha = .12, linewidth = 1) +
  geom_point(aes(size = richness), color = "#b03030", alpha = .85, show.legend = FALSE) +
  ggrepel::geom_text_repel(aes(label = lab), size = 3.2, color = "grey25",
                           na.rm = TRUE, box.padding = .6, min.segment.length = 0) +
  scale_size(range = c(2, 7)) +
  labs(title = "Birdsong diversity by distance from the freeway",
       subtitle = "Poland Municipal Forest · 15 stops",
       x = "Distance to I-680 (m)", y = "Species richness (Merlin)",
       caption = "Line: Poisson fit ± 95% CI. Each point is one stop.") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
p
ggsave(paste0(PROJ, "richness_vs_freeway.png"), p, width = 8, height = 5.5, dpi = 300)

#7. DESCRIPTIVES
#richness by zone
dat %>% group_by(zone) %>%
  summarise(n = n(), mean_rich = round(mean(richness),1),
            min = min(richness), max = max(richness), .groups="drop") %>% print()

#dB by zone
dat %>% group_by(zone) %>%
  summarise(mean_dB = round(mean(db),1), max_dB = max(db), .groups="drop") %>% print()

#richness by "house in sight"
dat %>% group_by(house) %>%
  summarise(n = n(), mean_rich = round(mean(richness),1), .groups="drop") %>% print()

#descriptive correlations
cat("\nrichness vs freeway dist:", round(cor(dat$richness, dat$dist_freeway_m, use="complete"),2),
    "\nrichness vs dB         :", round(cor(dat$richness, dat$db),2),
    "\nrichness vs temp       :", round(cor(dat$richness, dat$temp),2),
    "\nrichness vs squirrels  :", round(cor(dat$richness, dat$squirrels),2),
    "\ndB vs freeway dist     :", round(cor(dat$db, dat$dist_freeway_m, use="complete"),2), "\n")

#per-stop table
dat %>% dplyr::arrange(session_id) %>%
  dplyr::select(session_id, Time, zone, trail, richness, db, temp, squirrels, house,
                dist_freeway_m, dist_water_m, field_notes) %>% print(n = 15)

#species frequency: stops each bird was detected at
bird_freq <- spp %>% distinct(session_id, species) %>%
  count(species, name = "n_stops") %>%
  mutate(pct = round(100*n_stops/15)) %>% dplyr::arrange(desc(n_stops))
print(bird_freq, n = 100)

#species x location matrix (1/0) with row totals
mat <- spp %>% distinct(session_id, species) %>% mutate(p = 1) %>%
  pivot_wider(names_from = session_id, values_from = p, values_fill = 0)
mat$n_stops <- rowSums(mat[,-1])
print(dplyr::arrange(mat, desc(n_stops)), n = 100)
