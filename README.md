# Code snippets for easy re-use.

The goal of this repository is to store and share snippets of code that members of the 
FRAM team find useful. The following is an overview of the current code, which we hope
will streamline finding useful code. This will be updated periodically, and the repository may contain
snippets that are not documented here.

At present, all scripts are in `R/`

## Visualization tools

### Making plots

- `correlation_visualization.R` has code to make a heatmap plot of correlations between
variables of a dataframe, in a ggplot framework.
- `correlation-paired-plot.R` is a way to compare correlations between variables of a dataframe using base R.
- `geom_pie2.R` is a modified version of geom_pie from the `scatterpie` package. The scatterpie package
can be used to add piechart layers onto maps, making it useful for some very specific types of plots. As of 2018 when Collin Edwards was using the package regularly, however, geom_pie only worked if the variables of the data frame used had specific names. `geom_pie2()` fixes this problem, making `geom_pie` behave consistently to other ggplot type functions.

### Support
- `color-blind-palette.R` contains a simple vector of colorblind friendly colors
that may be more visually appealing than the discrete values provided in the `viridis` package. (The `viridis` package has hugely helpful tools for making figures color-blind friendly, and seems particularly well suited for continuous color gradients). Colorblind friendliness of ggplot objects can easily be tested with `colorBlindness::cvdPlot()`. The `colorBlindness` package also has many good color palettes for discrete
colors. See https://cran.r-project.org/web/packages/colorBlindness/vignettes/colorBlindness.html.
- `doy_2md.R` has a few functions to simplify translating day of year into a month/year. While this could
be useful in data manipulation, there are likely better tools for this, depending on the task (check out the `lubridate` package!). However, `doy_2md()` can be helpful when creating custom date axes. 
- `ggplot-axis-relabeling.R` has a series of functions to streamline making custom axis labels
based on transformations of the data. The main use case we see is plotting proportion data
with appropriate percent axis ticks, such as when working with exploitation rate.
- `ggtheme-larger.R` and `ggtheme-fram.R` define ggplot themes that serve as good baselines when making figures for general consumption. `ggtheme-larger.R` is deprecated, and we are only leaving as a reference point for now. We recommend `ggtheme-fram.R` as a starting point.


## Data manipulation

- `data-frame-comparison.R` has code to compare two data frames to look for mismatches. 
This can be very helpful for QA/QC (e.g., comparing the output of a new method to the output of
an old method, when the results should be the same).
- `data-frame-duplicates.R` has code to identify duplicate rows within a dataframe, with a lot of flexibility
of which columns to include when looking for duplicates.
- `fishery-renamer.R` has a function that translates the fishery names used in FRAM
and TAMM into consistent, easily readable strings. With optional argument `sep = TRUE`, it instead
returns a data frame, with separate columns for area name and fishery type (e.g. "Troll", "Sport", "Net"). 

## File manipulation

- `db-pull-example.R` has example code to pull information from a local database. When working with FRAM files, note that the FRAMrsquared package (https://github.com/FRAMverse/framrsquared) has a lot of functions that massively streamline working with the FRAM databases. When appropriate, we strongly recommend the
FRAMrsquared functions instead. 
- 



## Misc

- `example-ode-solver.R` has example code for numerically solving and plotting a system of ordinary differential equations (a form of continuous time dynamic models). Many theoretical ecology models are constructed of ordinary differential equations.
- `multi-list-dopar.R` has an example of parallelization with multiple objects output per loop. 

## Workflow
- `readme-maker.txt` has directions for using windows commands to create a text file of the directory tree of the current directory. We have found this helpful when documenting complex projects for sharing/publication (for example: see "README.txt" of the data and code repository here: https://figshare.com/articles/dataset/Data_and_analysis_for_Plant_defense_synergies_and_antagonisms_affect_performance_of_specialist_herbivores_of_common_milkweed_/20421633)
- `search-for-file-name.txt` has an example for using windows command-line to carry out searching for specific filename using wildcards.
- `search-sh.txt` has an example for using windows command-line to carry out a seach of the CONTENTS of files. This can be particularly useful when trying to find the script that generated a file (presuming the file name wasn't programmatically generated, the script in question should contain the file name in it).
