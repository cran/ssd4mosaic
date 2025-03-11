# ssd4mosaic 1.0.2

## Bug fixes

* Fix Upload file button in the shiny application.

## New features

* Add the option to compute Hazardous Concentrations (HC) for any x% value.

# ssd4mosaic 1.0.1

## Enhancements

* Changes to comply to CRAN guidelines
    - Clarification of the authorship and ownership of the package
    - Completion of the functions documentation
    - Removed calls to `print()` in bootstrap function
    - Various syntax changes
* Update repository url
## Bug fixes

* Fix the bug of names being displayed in user input order on non-censored data plots, instead of being associated to the correct data point.

* For HCx values, replace medians of the bootstrap samples by quantiles calculated from the fitted distribution in order to match the behaviour of the previous MOSAIC ssd.

## New features

* Added a rate unit (g/ha) in unit selection list

# ssd4mosaic 1.0.0

## Enhancements

* The way to precise the role of each column of the input data column has be reviewed to improve readability.

* More information texts have been added to the website to help users in their choices.

* Group plot readability has been improved by moving the legend to the bottom of the plot instead of the side.

* The colors used in the plot have been modified to improve accessibility for color blindness.

## Bug fixes

* Reports downloaded in markdown format now include required image files.

## New features

* 90% confidence intervals can now be displayed on the plot and computed for the parameter values and HCx values.

* A link toward a Frequently Asked Questions page was added.

* Two vignettes were added to the package: "Getting started" and "Using ssd4mosaic functions in R".

# ssd4mosaic 0.1.0

New features

* Created the package
* Added a `NEWS.md` file to track changes to the package.
