## Collects the articles listed in `altmetrics` for a given publication
##   and organizes them into a useful dataframe.
##
## Seems to miss a small number of articles (finds 504 out of 526 for Edwards 2025 Science
##  paper, for eaxmple).
##
## Altmeter stores article urls as links to a news aggregator which as a redirect.
## By default, scrape_paper_news_coverage() tracks down the actual link. But this
## takes time -- you can make the function run faster by adding optional argument
## `trace_link = FALSE`

library(tidyverse)
library(rvest) #for primary scraping
library(httr) #for tracking down final url
library(cli) #for loading information

# Function to get the final destination URL, because altmeter links
#  point to a news aggregator site.
get_final_url <- function(url) {
  tryCatch({
    response <- GET(url, config = config(followlocation = FALSE))
    if (status_code(response) %in% c(301, 302, 303, 307, 308)) {
      return(headers(response)$location)
    } else {
      return(url)  # No redirect
    }
  }, error = function(e) {
    return(NA)
  })
}

## arguments

scrape_paper_news_coverage = function(base_html, ## The almetric site for a paper,
                                      ## not including sub-address stuff like /news.
                                      ## For example, Edwards 2025 Science paper is
                                      ## https://science.altmetric.com/details/174951305
                                      j.safety = 20, #safety valve for the while loop. 
                                      ## Maximum number of pages to check. Altemeter puts 100 articles per page
                                      trace_link = TRUE, #If false, just stores the links to 
                                      ## news aggregator ct.moreover.com redirects rather than directly to article. Faster this way.
                                      verbose = TRUE # print to console the page number being read?
){
  
  ## setup
  j = 1
  do.next = TRUE
  res.ls = list()
  counter = 1
  
  while(do.next & j < j.safety){
    cli::cli_alert_info("Starting page {j}...")
    html = read_html(paste0(base_html,"/news/page:", j))
    block = html %>% 
      html_elements(".block_link")
    
    if(length(block) == 0){
      do.next = FALSE
    } else {
      if(verbose){
        cli::cli_progress_bar("Parsing articles", total = length(block))
      }
      
      for (i in 1:length(block)){
        link = block[[i]] %>% 
          html_attr("href") 
        if(trace_link){
          link = link |> 
            get_final_url()
        }
        sub = block[[i]] %>% 
          html_elements(".with_image")
        outlet = sub %>% 
          html_elements("h4") %>% 
          html_text2()
        outlet.name = outlet %>% 
          str_extract("^.*,") |> 
          gsub(",$", "", x = _)
        outlet.date = outlet |> 
          gsub(".*, ", "", x = _)
        title = sub %>%
          html_elements("h3") %>% 
          html_text2()
        
        res.ls[[counter]]  = data.frame(outlet = outlet.name, date = outlet.date, title = title, link = link)
        
        counter = counter + 1
        if(verbose){cli::cli_progress_update()}
      }
    }
    j = j+1
  }
  res = do.call (rbind, res.ls)
  return(res)
}

## Example usage, using the Edwards et al. 2025 science paper:

news_coverage = find_news_coverage(base_html = "https://science.altmetric.com/details/174951305"
)
## Note: some of the titles are not csv-friendly. Saving to excel file is helpful
library(writexl)
write_xlsx(news_coverage, path = "butterfly science paper news coverage.xlsx")
