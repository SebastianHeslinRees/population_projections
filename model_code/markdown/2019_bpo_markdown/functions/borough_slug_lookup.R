borough_slug_lookup <- function(bpo_name){
  
  borough_name_lookup <- data.frame(full = c("City of London","Barking and Dagenham","Barnet","Bexley","Brent","Bromley",
                                             "Camden","Croydon","Ealing",
                                             "Enfield","Greenwich","Hackney","Hammersmith and Fulham","Haringey","Harrow",
                                             "Havering","Hillingdon","Hounslow","Islington","Kensington and Chelsea", "Kingston",
                                             "Lambeth","Lewisham","Merton","Newham","Redbridge", "Richmond",
                                             "Southwark","Sutton","Tower Hamlets",
                                             "Waltham Forest","Wandsworth","Westminster")) %>%
    mutate(full = tolower(gsub(" ","-",full))) %>%
    mutate(short = substr(full,1,4))
  
  bpo_name <- tolower(bpo_name)
  
  borough_df <- data.frame(short = substr(bpo_name,1,4), stringsAsFactors = FALSE) %>%
    left_join(borough_name_lookup, by="short") 
  
  #validate
  if(nrow(borough_df)==0){stop("problem identifying borough for ", full_name)}
  if(nrow(borough_df)>1){stop("more than 1 borough identified for ", full_name)}
  
  borough <- as.character(borough_df$full)
  
  return(borough)
}