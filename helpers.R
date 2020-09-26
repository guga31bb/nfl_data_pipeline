library(magrittr)
library(tibble)

# laptop
if (grepl("Documents",getwd())){
  data_path <- "../"
} else { ### server
  data_path <- "/home/ben/data"
}
# message(glue::glue('Things will be saved in {data_path}'))
