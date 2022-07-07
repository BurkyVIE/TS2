TS2_make <- function (what, much = 1, data = TS2_reso) {

  # LIBRARIES ----
  library(tidyverse)
  
  # FUNCTIONS ----  
  '%notin%' <- Negate('%in%')
  
  # BODY ----
  
  ## initialise ----
  result <- NULL
  root <- filter(data, is.na(Component)) |> pull(Good)
  
  work <- filter(data, Good == what)
  need <- head(work, 1) |> select(Good:Patch)
  
  
  ## loop ----
  while(dim(work)[1] > 0) {
    result <- bind_rows(result,
                        filter(work, Component %in% root) |> select(Component:Quantity))
    work <- filter(work, Component %notin% root)
    work_new <- filter(data, Good %in% work$Component) 
    work <- work_new
  }
  
  result <- result |> group_by(Component) |> 
    summarise(Quantity = sum(Quantity), .groups = "drop") |> 
    mutate(Quantity = Quantity * ceiling(much / pull(need, Patch)))
    
  ## return ----
  cat("for", much, need$Good, "get:\n\n")
  return(as.data.frame(result))
}
