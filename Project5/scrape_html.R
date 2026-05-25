scrape_search_results <- function(page_path) {
  # Read HTML
  html_doc <- read_html(page_path)
  
  # Extract the isolated search result nodes (the individual cards)
  # This will depend on how you've acquired the results
  # Look for .search-result or .teaser (though there may be others)
  nodes <- html_doc %>% html_elements(".search-result")
  
  # Map over each individual search result
  map_df(nodes, function(node) {
    # Title
    title_element <- node %>% html_element(".views-field-title")
    title <- title_element %>% html_text2()
    
    # Date and time
    date_element <- node %>% html_element(".views-field-field-issue-date, time")
    datetime  <- date_element %>% html_attr("datetime") # Raw timestamp
    date_text <- date_element %>% html_text2()          # Human-readable text
    
    # Summary
    summary <- node %>% html_element(".views-field views-field-search-api-excerpt") %>% html_text2()
    
    # Ministers
    minister <- node %>%
      html_elements(".views-field-field-ministers .minister__title") %>%
      html_text2() %>%
      # trim out excess white space noise, and collapse them using a semicolon
      str_trim() %>%
      paste(collapse = ";")
    
    # Portfolios
    portfolio <- node %>%
      html_elements(".views-field-field-portfolios a") %>%
      html_text2() %>%
      # trim out excess white space noise, and collapse them using a semicolon
      str_trim() %>%
      paste(collapse = ";")
    
    # Combine individual results into a tidy tibble
    tibble(
      datetime   = datetime,
      date_text  = date_text,
      title      = title,
      ministers  = minister,
      portfolios = portfolio,
      summary    = summary
    )
  })
}
