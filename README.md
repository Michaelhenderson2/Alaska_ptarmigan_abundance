# Alaska_ptarmigan_abundance
Repository for completing ptarmigan analysis using distance sampling.

First, data was cleaned using ptarmigancleaning2026.R script. This script combines the historical observations from 2016-2025 with the new raw 2026 data that comes out of Survey123. Everything is cleaned and checked before merging. 

The raw data CSVs that get read into this script are as follows:
-2025_08-26_all_detection_data.csv (observation level)
-2025-08-26_survey_level_data.csv (survey level data that provides transect, date, round, road, etc...)
-2026_prey_det_level.csv (the raw Survey123 observation level file)
-2026_prey_parent_level.csv (the raw Survey123 survey level file)

The clean updated files that are created and saved from this script are:
-2026_07_21_survey_level_data.csv (survey level, good if you need dates, weather, etc..)
-2026_07-23_all_prey_data.csv (the master observation level file for all years that will be used for analysis)

Next, ptarm_locations.R script uses the updated master observation file to add the latitude and longitude to each waypoint. 
- this script uses csvs of gps data generated from basecamp for all years individually. let kara know if you need them.
- generates clean_prey_obs_withlocations.csv master csv (only difference from above is three new columns for lat/long/elevation
