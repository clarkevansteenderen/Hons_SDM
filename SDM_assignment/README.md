# Assignment: Argentine Ant MaxEnt Modeling

Download the contents of this **SDM_assignment** folder, and access the associated data in this [Google Drive folder](https://drive.google.com/drive/folders/1JXmjOVD9QxfNadFVna6n6dLdg5I8rvG7?usp=drive_link)

## Overview
The script `argentine_ant_exercise.R` implements a MaxEnt SDM for the full distribution of the Argentine ant, covering its native South American range and invaded regions (Africa, Europe, Australia, South Africa, New Zealand). The goal is to reproduce the full-distribution model, generate prediction maps, and complete additional tasks to explore native-range modeling and model comparisons.

## Assignment Tasks
### Fit a Native-Range MaxEnt Model
Train a MaxEnt model using only the native South American range data, with the same parameters as the full-distribution model: betamultiplier=3.0 and hinge=true.
[10 marks]

### Analyze Response Curves and Variable Contributions
Generate response curves for the native-range model and report each variable's percentage contribution to model performance.
[10 marks]

### Refine the Model
Re-run the native-range model, excluding variables with less than 2% contribution.
[10 marks]

### Project to All Ranges
Project the refined native-range model onto the six ranges: South America, Africa, Europe, Australia, South Africa, and New Zealand.
[15 marks]

### Compare Full-Range vs. Native-Range Models
Create "difference" maps for each region to compare the full-range and native-range models. Threshold maps at 0.7 to retain suitable areas, then subtract the full-range map from the native-range map. Example for South America:

```R
SA.native.thresh0.7 = SA.climvars.map.native$raster > 0.7
SA.full.thresh0.7 = SA.climvars.map$raster > 0.7
SA.diff = SA.native.thresh0.7 - SA.full.thresh0.7
```

Convert the difference raster to a data frame and plot with ggplot, using colors: -1 (darkred, native underestimates), 0 (grey, models agree), 1 (darkgreen, native overestimates):

```
scale_fill_gradientn(colors = c("darkred", "grey85", "darkgreen"), na.value = "transparent")
```
[15 marks]

## Write a Summary Report
Produce a report with all prediction maps (full-range and native-range models across all regions). Address:
Differences between full-range and native-range models.
Factors contributing to these differences.
Implications for conservation planning and invasive species management.
Most climatically suitable areas for the Argentine ant based on full-range models.
[40 marks]

**Total: 100 marks**
