library(mailR)
library(dplyr)
library(data.table)


email_list <- fread("Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/email_list.csv") %>% data.frame()
attachments_dir <- "Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/"
guidance <- paste0(attachments_dir, "2020-based BPO guidance.pdf")

#-------------------------------------------------------------------------------

email_text <- paste("Dear colleagues,",
                    "We are beginning preparations for the 2020-based round of Borough Preferred Option (BPO) projections which will be run in the Spring. I\'m writing now to provide development trajectory templates for you to complete.",
                    "We are in the process of developing a model which will allow us to run projections on 2022 ward boundaries for all London Boroughs. To that end I am attaching a development trajectory template for 2022 wards in your borough. However, I understand that in some authorities providing data on the 2022 geography may be difficult and so I am also providing the template for 2013 wards as well. Please only complete one template.",
                    "Attached is a guidance document to assist in the completion of the development trajectory template.",
                    "The first BPO projections are scheduled to be run in early March. Please return a completed template to me by the end of February to ensure your projections are part of the initial run.",
                    "If you feel that this email has been sent to you in error please let me know and I'll remove you from my contacts list.",
                    "Regards,",
                    "Wil",
                    sep = "\n\n")

signature <- paste("Wil Tonkiss",
                   "Senior Data Scientist", "City Intelligence Unit",
                   "Greater London Authority",
                   "",
                   sep = "\n")

body <- paste(email_text, signature, sep = "\n\n")

#-------------------------------------------------------------------------------
boroughs <- unique(email_list$Organisation)

boroughs <- c("Richmond upon Thames", "Wandsworth")

for(b in 1:2){
  
  i <- boroughs[b]
  
  if(i == "City of London"){next()}
  print(i)
  
  dev_temp_2013 <- paste0(attachments_dir, "blank_templates/2013_wards/", i, " (2013 wards).csv")
  dev_temp_2022 <- paste0(attachments_dir, "blank_templates/2022_wards/", i, " (2022 wards).csv")
  
  email_to <- filter(email_list, Organisation == i)$Email
  if(length(email_to)==0){stop()}
  
  smtp <- list(host.name = "proxy.london.gov.uk",
               user.name = "william.tonkiss@london.gov.uk",
               passwd = Sys.getenv("email_password"))
  
  send.mail(from = "william.tonkiss@london.gov.uk",
            to = c(email_to, "demography@london.gov.uk"),
            subject = paste(i, "2020-based BPO development data"),
            body = body,
            attach.files = c(dev_temp_2013, dev_temp_2022, guidance),
            smtp = smtp)
  
}
