library(arrow)
library(tidyverse)

dir.create("./data", showWarnings = FALSE)

seattle_arrow <- open_dataset(
  "data/seattle-library-checkouts.csv",
  col_types = schema(ISBN = string()),
  format = "csv"
)
seattle_arrow |> glimpse()
seattle_arrow |> summary()
