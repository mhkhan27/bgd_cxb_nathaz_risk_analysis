rmarkdown::render("scripts/nathaz_shelter_exposure.Rmd",
                  params = list(pdf_mode = FALSE),
                  output_file = "report.html",)
# pagedown::chrome_print(input ="file:///C:/01_REACH_BGD/02_GIS_DataUnit/02_Themes/07_Natural_Hazards/bgd_cxb_nathaz_risk_analysis/scripts/report.html",output = "asdfa.html" )


# browseURL(url = "nathaz_shelter_exposure.html",encodeIfNeeded = TRUE)
