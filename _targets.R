## i ran the code below in the console before this file was generated
## ---------------------------------------------------------------------------

# ## install CRAN version of package
# install.packages("targets")
# targets::use_targets()

## this creates the _targets.R file (this one) and other items in the project
## ---------------------------------------------------------------------------


# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tidyverse)
# library(here)

# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tidyverse", "here", "lubridate") # packages that your targets need to run
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #   controller = crew::crew_controller_local(workers = 2)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     workers = 50,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.0".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multicore")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  
  ## create file name
  tar_target(name = file, command = file.path(here::here("data/traceybit-lastfm.csv")), format = "file"),
  
  ## read in and clean the data 
  tar_target(name = data, command = clean_data(file)),
  
  ## create table of play counts
  tar_target(name = plays_artists_df, command = find_plays(data)),

  ## plot heatmap
  tar_target(name = listening_heatmap, command = plot_heatmap(plays_artists_df)),

  ## create top artist table
  tar_target(name = top_artist_df, command = find_top_artists(data)),

  ## plot top artists
  tar_target(name = artist_fig, command = make_artist_fig(top_artist_df))
  
  )
  
