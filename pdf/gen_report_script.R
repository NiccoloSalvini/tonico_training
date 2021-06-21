
library(pdfreport)

rmd.path <- system.file("example/template_example.Rmd", package = "pdfreport")

author <- "TT"
title <- "general report"
description <- "a sport nutrition template"
company <- "Tonico Traning"
company_url <- "https://rtask.thinkr.fr"
slogan <- "Nice & Solid"
created_on <- "Created on"
email <- "sebastien@thinkr.fr"
link.col <- "#be0302"
section.color <- "#0099ff"
main.col <- "#be0302"
lang <- "en" # For latex text formatting specificities, date format, ...
bg = here::here("assets", "Background_deepred_topdown.png")
bg.title  = here::here("assets", "Background_Title_deepred.png")


prepare_for_knit(
  rmd.path, 
  fig_caption = TRUE, 
  keep_tex = TRUE,
  number_sections = TRUE, 
  toc = TRUE,
  lang = lang,  
  out_format = c("pdf_document2", "pdf_book")[1],
  author = author, 
  title = title, description = description,
  email = email,
  slogan = slogan, created_on = created_on,
  bg = bg, bg.title = bg.title,
  link.col = link.col, section.color = section.color, main.col = main.col,
  company = company, company_url = company_url,
  knit = TRUE, output_dir = tempdir())
