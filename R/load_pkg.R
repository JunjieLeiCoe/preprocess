##################################################################
##                      Creator : Junjie L                      ##
##                  Creation Date : 03/21/2024                  ##
##               Title : R pkg data preprocessing               ##
##################################################################


## Showing current repository
fs_df <- function(){
  getwd()
  cat('Current working dir -->', getwd(), '\n')
  library(fs)
  fs::dir_tree()
}

# Package Loading Sequences;
## ---------------------------------------------------------------
load_pkg <- function(){
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(tidyr)
}


