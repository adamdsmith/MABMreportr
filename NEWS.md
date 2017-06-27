# MABMreportr 0.3.1

* If interactive HTML maps are requested, and PDFtk is available, the HTML maps are attached to the final report rather than maintained as separate files

# MABMreportr 0.3

* Optionally creates an interactive HTML (leaflet) map of detections for each route along with the final report

# MABMreportr 0.2.1

* Canonical shapefiles now loaded as `sf`; updated package dependencies as necessary

# MABMreportr 0.2

* Several substantial functionality improvements, including:
    - Generates route map with (georeferenced) bat detections (requires 'canonical' survey route shapefile and specific file hierarchy; see details in `?MABM_report`) (#1)
        - Improved documentation regarding the required file hierarchy
    - With proper file hierarchy (see above), `MABM_report` attempts to output annual reports to a dedicated 'Annual Report' directory beneath each station directory (#2)
    - User can now specify the export location of files from the MABM database (#3)
        - recommended to base of MABM file hierarchy
        - occurs from within call to `MABM_report` rather than separate function with optional argument to update after updating MABM database (e.g., after new field season)
* General improvements to handle the variability in station data (e.g., new stations vs. established stations, missing georeferences, unexpected file hierarchy)

# MABMreportr 0.1

* Basic functionality for annual reports:
    - Base narrative customized for each year/station
    - Tables by route for:
        - survey route summary (dates, # detections, route notes)
        - summary of species detection by survey date
        - summary of species detections over all years of study
    - Figure by route for:
        - summary of species detections over all years of study (crude)
* Several limitations:
    - No user control over exports from MABM database for reports
    - Crude (i.e., manual only) placement of annual report output
    - No route map with (georeferenced) bat detection overlay
    - No customization of base narrative beyond year/station
    
        
