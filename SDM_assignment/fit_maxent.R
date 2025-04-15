fit_maxent_climvars = function(WORLDCLIM.RASTERS, MAXENTMODEL, PRESENCE.GPS=NULL, EXT,
                               SAVENAME, scale.pos = "br", compass.pos = "br",
                               LEGEND = "right", WRITEMAP = TRUE, TITLE = "", PTSIZE = 1){
  
  envlayers = raster::crop(WORLDCLIM.RASTERS, EXT) %>%
    raster::mask(., EXT)
  
  if (!dir.exists("data")) dir.create("data")
  # Extract MaxEnt predictions for South Africa
  predict_maxent_climvars <- terra::predict(MAXENTMODEL, envlayers)
  
  predict_maxent_climvars.df = as.data.frame(predict_maxent_climvars, xy = TRUE, na.rm = TRUE)
  
  if(WRITEMAP == TRUE){
    terra::writeRaster(predict_maxent_climvars, paste0("data/", SAVENAME, ".tif"),
                       overwrite = TRUE)
    message(paste0("Raster written to data/", SAVENAME, ".tif"))
  }
  
  
  #terra::plot(predict_maxent_allvars)
  #class(predict_maxent_allvars)
  
  # Plot publication-quality figure   
  climvars.plot = ggplot() +
    # Plot MaxEnt prediction raster
    tidyterra::geom_spatraster(
      data = predict_maxent_climvars,
      maxcell = 5e+7         # maxcell = Inf
    ) +
    # Control raster colour and legend
    tidyterra::scale_fill_whitebox_c(
      palette = "muted",
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1)
    ) +
    # Plot S Africa boundary
    geom_sf(data = EXT, fill = NA, color = "black", size = 0.1) +
    
    # Control axis and legend labels 
    labs(
      x = "Longitude",
      y = "Latitude",
      fill = "P(suitability)"
    ) +
    # Create title for the legend
    theme(legend.position = LEGEND) +
    # Add scale bar to bottom-right of map
    ggspatial::annotation_scale(
      location = scale.pos,          # 'bl' = bottom left
      style = "ticks",
      width_hint = 0.2
    ) +
    # Add north arrow
    ggspatial::annotation_north_arrow(
      location = compass.pos,
      which_north = "true",
      pad_x = unit(0.175, "in"),
      pad_y = unit(0.3, "in"),
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    # Change appearance of the legend
    guides(
      fill = guide_colorbar(ticks = FALSE)
    ) +
    ggtitle(TITLE)
  
  # Conditionally add the GPS points layer if PLOT.GPS == TRUE
  if (!is.null(PRESENCE.GPS)) {
    climvars.plot <- climvars.plot +
      geom_point(
        data = PRESENCE.GPS,
        aes(x = lon, y = lat),
        size = PTSIZE,
        colour = "black", alpha = 0.6
      )
  }
  
  # Return both objects as a list
  return(list(plot = climvars.plot, raster = predict_maxent_climvars,
              raster.df = predict_maxent_climvars.df))
  
}