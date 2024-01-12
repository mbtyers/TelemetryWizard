# Magical Telemetry Wizard!
An interactive Shiny app to read receiver data, select locations, and export to .csv

## Purpose

The purpose of this app is to streamline the process of converting raw location data from a telemetry receiver (likely formatted as a .txt file with a lengthy preamble at the head and end-of-data report at the tail) to a tidy data spreadsheet.  The app also allows interactive selection of the single "best" observation per instrumented individual for a given flight.

## Using the wizard...

Likely steps are given below for data import, point selection, and download.  Data input is intended to be format-agnostic, and table/column selection is left to the user.

#### Data import

* Choose a data file with "Browse...".  *Note: an example dataset may be loaded by pasting the provided link into the file selection window.*
* Change data delimitation if needed.  A .txt file will very likely be whitespace delimited.
* View the "Input as raw text" printed, and select the first and last line numbers of the data table.  Do not include the header row.
* Supply the line number of the header row, if desired (and if a header row is present.)
  - If this step displays an error, it is likely that there is whitespace in the column names from the header row (e.g. "Tag ID").  Edit the header row as needed in the next space for input.
* Select column *numbers* for Latitude and Longitude
* Optionally, select a column *number* for signal strength
* Optionally, select column *numbers* that give unique fish ID (e.g. frequency and code), separated by commas

#### Point selection

* In the "Point Selection" tab, a map and data table will be displayed for each unique fish.  Navigate between fish as needed with the "<= Prev fish" and "Next fish =>" buttons.
* Optionally, a checkbox is provided to note (for yourself) if a given fish has been reviewed.  This does not affect the downloadable data.
* The map allows interactive panning and zooming much like Google maps.  However, the "Zoom level" buttons allow the user to pre-set the zoom level that the map reverts to after every action.
* For each fish, select the single best entry using the buttons provided.  Note that the selection appears in the data table.
* Optionally, you can insert a comment for a given row.  The interface isn't great, but it works.

#### Download

* Once the individual fish have been reviewed as desired, the resulting data can be downloaded as a .csv file.  You can either download "All records" (which does include a column to denote which observations were selected), or "Selected records" (just the selected subset).
* If running the app locally, the filename and location can be edited; if running the app on the server, it is automatically generated and will likely go to Downloads.

## Repo structure

### /R

Contains the source code in a single .R script

### /ExampleData

Contains example data formatted a few different ways for convenient error checking.

Note: currently the "Lake trout survey ..." files are not read correctly!