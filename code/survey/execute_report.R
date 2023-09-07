#OUTPUT_FORMAT
#change output format to either be a word docx or html

#OUTPUT_FILE
#change second argument of paste0 string to label version of output

#PARAMS
#set_title - title of report
#yearofsurvey - this is the latest data year collected from the MCLC survey (numeric)

rmarkdown::render('05_multiple_imputation.Rmd',
                  #output_format = Gmisc::docx_document(),
                  output_format  = "html_document",
                  output_file = paste0(
                    'MCLCestimates_v',
                    "1",
                    "_",
                    Sys.Date(), 
                    '.html'
                  ),
                  params = list(
                    set_title    = "2022 MCLC Survey Results",
                    yearofsurvey = 2021
                  )
)
