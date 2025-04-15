##################################################################
# SPECIES DISTRIBUTION MODELLING HONOURS ASSIGNMENT
##################################################################

# load the relevant libraries and ggplot tweaks
source("setup.R")

if (!dir.exists("figs")) dir.create("figs")

# I've already downloaded the GPS data for the Argentine ant for you from GBIF
# It is split up into six different geographic areas to make it easier to subset

pres.data.argentine.ant = readRDS("data/linepithema_humile_distributions.rds")
names(pres.data.argentine.ant)

##################################################################
# PLOT THE GLOBAL DISTRIBUTION MAP
##################################################################

color_mapping = c(
  "USA" = "darkblue",
  "EUR" = "purple",
  "AFR" = "darkred",
  "AU" = "lightblue3",
  "NZ" = "darkorange",
  "NativeRange" = "darkgreen"
)

world_map = rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Create the base plot with the world map
global_distr = ggplot() +
  geom_sf(data = world_map, alpha = 0.5)

# Add GPS points for the ant
global_distr = global_distr +
  purrr::imap(pres.data.argentine.ant, ~ 
                geom_point(data = .x, size = 1, colour = color_mapping[.y], 
                           aes(x = lon, y = lat, alpha = 0.5))
  ) +
  coord_sf(crs = 4326, expand = FALSE) +
  xlab("Longitude") +
  ylab("Latitude") 

global_distr

##################################################################
# GET BACKGROUND POINTS
##################################################################

# These have already been generated for you, using the Koppen-Geiger zone
# approach presented during the course
# read in background points

full.data.argentine.ant = readRDS("data/full.linepithema.humile.rds")

# We now have both presence and background points available

str(full.data.argentine.ant)

##################################################################
# PLOT FOR A PARTICULAR RANGE
##################################################################

world_map = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# select target region -> AFRICA
target_map = world_map[world_map$continent == "Africa", ]

# read in desired species KG zone and GPS points for presences and absences
KG.zone = sf::read_sf("data/AFR_KGZONE.shp")

GPS.points.afr = full.data.argentine.ant$AFR

distr.afr = ggplot() +
  # Add raster layer of world map
  geom_sf(data = target_map) +
  geom_sf(data = KG.zone, fill = "forestgreen", alpha = 0.3) +
  # Add GPS points
  geom_point(
    data = GPS.points.afr,
    size = 1, colour = "black", shape = 21,
    aes(
      x = lon,
      y = lat,
      fill = type
    )
  )  +
  # Set world map CRS
  coord_sf(
    xlim = c(10, 40), ylim = c(-36, -15), # Africa
    #xlim = c(-170, -50), ylim = c(5, 85),  # USA
    #xlim = c(105, 160), ylim = c(-45, -7), #AUS
    #xlim = c(-15, 90), ylim = c(25, 80), #EUR
    #xlim = c(165,180), ylim = c(-48,-34), #NZ
    crs = 4326,
    expand = FALSE
  ) +
  labs(title = "", x = "Longitude", y = "Latitude") +
  scale_fill_manual(values = c("Presence" = "red", "Background" = "grey30"), name = "GPS points") +
  theme(legend.position = "right") +
  # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "br",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.15
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = unit(0.03, "in"),
    pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_fancy_orienteering
  ) 

distr.afr

##################################################################
# SELECT VARIABLES: USING THE GLOBAL DISTRIBUTION
##################################################################

# Load the WORLDCLIM rasters layers we already have downloaded 
pred.clim = terra::rast( list.files(
  here::here("current_climate_layers/wc2.1_2.5m/") ,
  full.names = TRUE,
  pattern = '.tif'
))  

str(full.data.argentine.ant)
combined_data = dplyr::bind_rows(full.data.argentine.ant, .id = "region")
str(combined_data)

# get presence points across all ranges
full.pres = combined_data %>%
  dplyr::filter(type == "Presence") %>%
  dplyr::select(lon, lat)

# get background points across all ranges
full.background = combined_data %>%
  dplyr::filter(type == "Background") %>%
  dplyr::select(lon, lat)

# These predictors have been selected after running multicollinearity and VIF tests
# read them in:
reduced_preds_all19r2_VIF = terra::rast("data/reduced_preds_all19r2_VIF.tif")
reduced_preds_all19r2_VIF

#########################################################
# FIT THE MAXENT MODEL
##########################################################

# Model tuning has already been run for you, and showed that the best Feature Class (FC) was "Hinge"
# and that the best Regularisation Multiplier (RM) was 3

if (!dir.exists("model")) dir.create("model")

# read in pres and back points 
pres.pointsWD = readRDS("data/occlist.rds") %>%
  dplyr::select(-species)
head(pres.pointsWD)

back.pointsWD = readRDS("data/bglist.rds") %>%
  dplyr::select(-species)
head(back.pointsWD)

# Combine the climate data for the focal species and background points 
data = dplyr::bind_rows(
  pres.pointsWD,
  back.pointsWD
) %>% dplyr::select(-lat, -lon)

head(data)

# Provide a vector containing 0 (indicating background points) and 1 (indicating 
# presence points)
p_vector = c(
  replicate(nrow(pres.pointsWD), "1"),
  replicate(nrow(back.pointsWD), "0")
) 
length(p_vector)
nrow(data)

# Fit MaxEnt model, using the tuned parameters:
# 'betamultiplier=3.0'
# 'hinge=true'

climvar.mod = dismo::maxent(
  x = data,
  p = p_vector,
  path = here::here("model"),
  replicates = 20,
  args = c(
    # Insert the optimal RM value here
    'betamultiplier=3.0',
    # Turn these on/off to change FC combinations 
    # - To only use quadratic features, turn all to false except quadratic
    'linear=false',
    'quadratic=false',
    'product=false',
    'threshold=false',
    'hinge=true',
    # Don't change anything from here down 
    'threads=2',
    #'doclamp=true',
    'fadebyclamping=true',
    'responsecurves=true',
    'jackknife=true',
    'askoverwrite=false',
    'responsecurves=true',
    'writemess=true',
    'writeplotdata=true',
    'writebackgroundpredictions=true'
  )
)

response_data_climvars = dismo::response(climvar.mod, expand = TRUE)

variable_importance.climvars = climvar.mod@results
percent_contributions.climvars = variable_importance.climvars[grep("contribution", rownames(variable_importance.climvars)), , drop = FALSE]
percent_contributions.climvars

# get the response data for each variable
bio1.response.climvars = as.data.frame(dismo::response(climvar.mod, var = "wc2.1_2.5m_bio_1"))  %>%
  dplyr::mutate(name = "bio1") %>% dplyr::rename(value = V1)
bio2.response.climvars = as.data.frame(dismo::response(climvar.mod, var = "wc2.1_2.5m_bio_2"))  %>%
  dplyr::mutate(name = "bio2") %>% dplyr::rename(value = V1)
bio3.response.climvars = as.data.frame(dismo::response(climvar.mod, var = "wc2.1_2.5m_bio_3"))  %>%
  dplyr::mutate(name = "bio3") %>% dplyr::rename(value = V1)
bio12.response.climvars = as.data.frame(dismo::response(climvar.mod, var = "wc2.1_2.5m_bio_12"))  %>%
  dplyr::mutate(name = "bio12") %>% dplyr::rename(value = V1)
bio19.response.climvars = as.data.frame(dismo::response(climvar.mod, var = "wc2.1_2.5m_bio_19"))  %>%
  dplyr::mutate(name = "bio19") %>% dplyr::rename(value = V1)

# join everything together
bioresponses.climvars = rbind(bio1.response.climvars, bio2.response.climvars, 
                              bio3.response.climvars, bio12.response.climvars, 
                              bio19.response.climvars) %>%
  dplyr::mutate(name = factor(name, levels = c("bio1", "bio2", "bio3", "bio9", "bio12", "bio19")))

# make a custom ggplot
response.plots.climvars = ggplot(bioresponses.climvars, aes(x = value, y = p)) +
  geom_line(linewidth = 1, colour = "darkgrey") +
  facet_wrap(~name, scales = "free") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "Predictor Value", y = "Predicted Response", 
       title = "Maxent Response Curves: Linepithema humile")

response.plots.climvars

# Since bio2 contributed only 1.45% to the model's performance, we can re-run the model without this variable:

climvar.mod = dismo::maxent(
  x = dplyr::select(data, -wc2.1_2.5m_bio_2),
  p = p_vector,
  path = here::here("model"),
  replicates = 20,
  args = c(
    # Insert the optimal RM value here
    'betamultiplier=3.0',
    # Turn these on/off to change FC combinations 
    # - To only use quadratic features, turn all to false except quadratic
    'linear=false',
    'quadratic=false',
    'product=false',
    'threshold=false',
    'hinge=true',
    # Don't change anything from here down 
    'threads=2',
    #'doclamp=true',
    'fadebyclamping=true',
    'responsecurves=true',
    'jackknife=true',
    'askoverwrite=false',
    'responsecurves=true',
    'writemess=true',
    'writeplotdata=true',
    'writebackgroundpredictions=true'
  )
)

bioresponses.climvars = rbind(bio1.response.climvars,  
                              bio3.response.climvars, bio12.response.climvars, 
                              bio19.response.climvars) %>%
  dplyr::mutate(name = factor(name, levels = c("bio1", "bio3", "bio9", "bio12", "bio19")))

# make a custom ggplot
response.plots.climvars = ggplot(bioresponses.climvars, aes(x = value, y = p)) +
  geom_line(linewidth = 1, colour = "darkgrey") +
  facet_wrap(~name, scales = "free") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "Predictor Value", y = "Predicted Response", 
       title = "Maxent Response Curves: Linepithema humile (full range)")

response.plots.climvars

#####################################################################################
# PROJECT MODEL
#####################################################################################

source("fit_maxent.R")

reduced_preds_all19r2_VIF

# but remember, we removed bio2 earlier, so let's remove it from our predictor variables here too:
reduced_preds_all19r2_VIF_red = subset(reduced_preds_all19r2_VIF, 
                                        c("wc2.1_2.5m_bio_1", "wc2.1_2.5m_bio_3", 
                                          "wc2.1_2.5m_bio_12", "wc2.1_2.5m_bio_19"))

# we want to project onto these regions:

# Define the bounding boxes for each location
bbox.AU = sf::st_bbox(
  c(xmin = 112,  xmax = 155,  ymax = -10,  ymin = -44), crs = sf::st_crs(4326))

bbox.SA = sf::st_bbox(
  c(xmin = 15,xmax = 34,ymax = -36, ymin = -21), crs = sf::st_crs(4326))

bbox.NZ = sf::st_bbox(
  c(xmin = 165, xmax = 180, ymax = -34, ymin = -48), crs = sf::st_crs(4326))

# get all the EXTs
au_ext = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(name == "Australia") %>%
  sf::st_crop(., bbox.AU)

sa_ext = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(name %in% c("South Africa", "Lesotho", "eSwatini")) %>%
  sf::st_crop(., bbox.SA)

nz_ext = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(name == "New Zealand") %>%
  sf::st_crop(., bbox.NZ)

####################################################################################
# PROJECT ONTO SOUTH AFRICA
####################################################################################

SA.climvars.map = fit_maxent_climvars(WORLDCLIM.RASTERS = reduced_preds_all19r2_VIF_red,
                                      MAXENTMODEL = climvar.mod,  WRITEMAP = FALSE, 
                                      LEGEND = "right", TITLE = "Trained on full range",
                                      PRESENCE.GPS = pres.data.argentine.ant$AFR,
                                      EXT = sa_ext, SAVENAME = "SA_worldclim"); SA.climvars.map

ggsave(filename = "figs/SA_fullrange_predictions.png", 
       plot = SA.climvars.map$plot, 
       width = 6,  # Set the width in inches
       height = 6,  # Set the height in inches
       dpi = 450)  # Set the resolution

####################################################################################
# PROJECT ONTO AUSTRALIA
####################################################################################

AU.climvars.map = fit_maxent_climvars(WORLDCLIM.RASTERS = reduced_preds_all19r2_VIF_red,
                                      MAXENTMODEL = climvar.mod,  WRITEMAP = FALSE, 
                                      LEGEND = "right", TITLE = "Trained on full range",
                                      PRESENCE.GPS = pres.data.argentine.ant$AU,
                                      EXT = au_ext, SAVENAME = "AU_worldclim",
                                      scale.pos = "bl", compass.pos = "bl"); AU.climvars.map

ggsave(filename = "figs/AU_fullrange_predictions.png", 
       plot = AU.climvars.map$plot, 
       width = 6,  # Set the width in inches
       height = 6,  # Set the height in inches
       dpi = 450)  # Set the resolution

####################################################################################
# PROJECT ONTO NEW ZEALAND
####################################################################################

NZ.climvars.map = fit_maxent_climvars(WORLDCLIM.RASTERS = reduced_preds_all19r2_VIF_red,
                                      MAXENTMODEL = climvar.mod,  WRITEMAP = FALSE, 
                                      LEGEND = "right", TITLE = "Trained on full range",
                                      PRESENCE.GPS = pres.data.argentine.ant$NZ,
                                      EXT = nz_ext, SAVENAME = "NZ_worldclim",
                                      scale.pos = "br", compass.pos = "br"); NZ.climvars.map

ggsave(filename = "figs/NZ_fullrange_predictions.png", 
       plot = NZ.climvars.map$plot, 
       width = 6,  # Set the width in inches
       height = 6,  # Set the height in inches
       dpi = 450)  # Set the resolution


##################################################################
# REPEAT THE PROCESS, BUT TRAIN THE MODEL ON NATIVE RANGE RECORDS ONLY
# USE THE SAME TUNING PARAMS, H3, AND THE SAME PREDICTOR VARIABLES
##################################################################

#########################################################
# FIT THE MAXENT MODEL
##########################################################

if (!dir.exists("model_native")) dir.create("model_native")

# the presence and background points have already been generated for you

# read in pres and back points from the NATIVE RANGE
pres.points.native.WD = readRDS("data/occlist_native.rds") %>%
  dplyr::select(-species)
head(pres.points.native.WD)

back.points.native.WD = readRDS("data/bglist_native.rds") %>%
  dplyr::select(-species)
head(back.points.native.WD)

# Combine the climate data for the focal species and background points 
data.native = dplyr::bind_rows(
  pres.points.native.WD,
  back.points.native.WD
) %>% dplyr::select(-lat, -lon)

head(data.native)

# Provide a vector containing 0 (indicating background points) and 1 (indicating 
# presence points)
p_vector_native = c(
  replicate(nrow(pres.points.native.WD), "1"),
  replicate(nrow(back.points.native.WD), "0")
) 
length(p_vector_native)
nrow(data.native)

################################################################################
################################################################################
# ASSIGNMENT STARTS HERE
################################################################################
################################################################################

##############################################
# PART 1: FIT THE MAXENT MODEL
##############################################

# Fit MaxEnt model, using dismo::maxent() with the same tuned parameters:
# 'betamultiplier=3.0'
# 'hinge=true'
# Save the model in this directory:
# path = here::here("model_native"),

##############################################
# PART 2: GET THE RESPONSE CURVES AND THE
# PERCENTAGE CONTRIBUTIONS OF EACH VARIABLE
##############################################

# check which variables did not contribute towards the model's performance

##############################################
# PART 3: RE-RUN THE MODEL WITHOUT THE 
# VARIABLES THAT DID NOT CONTRIBUTE
##############################################

##############################################
# PART 4: PROJECT THE MODEL ONTO:
# SOUTH AFRICA
# AUSTRALIA
# NEW ZEALAND
##############################################

# save the maps

################################################
# PART 5: PLOT THE MAPS SIDE-BY-SIDE
################################################

# Plot the maps side by side comparing the full-range vs native range-trained models for:
# South Africa, Australia, and New Zealand

# Use the gridExtra::grid.arrange() function
