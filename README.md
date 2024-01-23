# newplot_shiny
A newplot-like R Shiny app for use with new EDM excavation data collection application



### Current functions:
- Read database from JSON file

- Display data tables and plan-, side-, and front-view plots from JSON data

- Display multi-point data
- Click on plots to display data associated with a point
- Zoom on plots by selecting an area and double clicking
- Find and highlight records in plots using a UNIT-ID search function
- Plot specific codes, units, levels (currently can only make selections from one of these at a time)
- Color points by code, unit, level
- Plot units
- Plot datums
- Download CSV of point data from those displayed on a plot from plot tabs
- Download PNG of plot images

- Edit units, datums, and prism tables 
- Edit a selected record from plot via a dialog box
- Save all edits to database JSON file

- Download entire database from Data table tab


### Upcoming functions:
- Plot intersection of specific codes, units, levels (e.g. code lithics from unit X)
- Plot refits (how does this work in original newplot?)


### Things to test
- need to test with moving between two different machines (one collector and one analyzer)
  - can R shiny app be run from tablet?
- testing Android tablet with total station



