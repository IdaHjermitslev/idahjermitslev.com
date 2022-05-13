library(drake)
library(tidyverse)

source("CleanDK2018.R")

plan <- drake_plan(
  raw_data = read_csv(file_in("Coalition Heuristics Denmark 2018 Final.csv")),
  clean_data = cleanDK2018(raw_data),
  save_clean_data = save(clean_data, file=file_out("Coalition Heuristics Denmark Clean.RData")),
  report = rmarkdown::render(
    knitr_in("ResultsDK2018.Rmd"),
    output_file = file_out("ResultsDK2018.html"),
    quiet = TRUE)
)

make(plan)


