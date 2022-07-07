TS2_make <- function (what, units = 1, data = TS2_reso) {
  
  # LIBRARIES ----
  library(tidyverse)
  
  # BODY ----
  
  ## initialise ----
  source("TS2_reso.r")
  
  work <- filter(data, Good == what)
  need <- head(work, 1) |> select(Good:Patch)
  
  ## root selected ----
  if(is.na(need$Patch)) {
    need$Patch <- units
    cat("for", units, need$Good, "get:\n\n")
    return(as.data.frame(need))
    
    break()
  }
  
  ## loop ----
  while(!all(work$Basic1)) {
    remainder <- filter(work, !Basic1) |> pull(Component)
    work <- bind_rows(
      filter(work, Basic1),
      filter(data, Good %in% remainder)
    )
    }
  
  work <- work |> group_by(Component) |> 
    summarise(Quantity = sum(Quantity), .groups = "drop") |> 
    mutate(Quantity = Quantity * ceiling(units / pull(need, Patch)))
  
  ## return ----
  cat("for", units, need$Good, "get:\n\n")
  return(as.data.frame(work))
}
