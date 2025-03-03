# newplot_shiny
A newplot-like R Shiny app for use with new EDM excavation data collection application


### End of day workflow
1) Copy site folder to local computer with newplot_shiny
2) Run newplot_shiny and open site JSON file 
3) After field data has loaded in DATA TRANSFER tab, click "Transfer data"
4) Check data in Plots tab and Database table tab, make any necessary edits
5) Close newplot_shiny and transfer entire site folder back to data collection tablet


### Current functions:
- Read points table from JSON file
- Transfer field points to a site database that consists of CSV files: Context, XYZ, Units, Poles, Datums
- Display data tables and plan-, side-, and front-view plots from CSV database
- Display multi-point data, display on 2-shot data
- Click on plots to display data associated with a point
- Zoom on plots by selecting an area and double clicking
- Find and highlight records in plots using a UNIT-ID search function
- Plot specific codes, units, levels (currently can only make selections from one of these at a time)
- Color points by code, unit, level
- Plot units
- Plot datums
- Download CSV of point data from those displayed on a plot from plot tabs
- Download PNG of plot images
- Edit a selected record from plot via a dialog box
- Save all edits to database CSV files
- Write daily edit log to track changes made to database files

### Upcoming functions:
- Plot refits
- Edit units, datums, and prism data in the field JSON file and the database CSV files 


### Things to test
- can R shiny app be run from tablet?



