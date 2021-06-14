## alt theme
theme_TT = function (base_size = 11, base_family = "") {
  theme_bw() %+replace%
    theme(
      text = element_text(family = "sans", size = 12),
      plot.title = ggplot2::element_text(size = 18, colour = "#454545",hjust = 0.5, margin = ggplot2::margin(b = 10)),
      panel.background  = element_blank(),
      panel.grid.major = ggplot2::element_line(linetype = "dotted",colour = "#454545",size = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = element_line(colour = "grey70", size = 0.2),
      axis.line = ggplot2::element_line(color = "#454545", size = 0.3),
      plot.background = element_rect(fill="white", colour=NA),
      panel.border = element_rect(linetype = "blank", fill = NA),
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA),
      strip.background = ggplot2::element_rect(fill = "transparent",colour = NA),
      axis.title = ggplot2::element_text(size = 13, colour = "#454545",hjust = 0.95),
      axis.text = ggplot2::element_text(size = 10, colour = "#212121"),
      strip.text = ggplot2::element_text(size = 12, colour = "#454545",
                                         margin = ggplot2::margin(10, 10,
                                                                  10, 10,
                                                                 "pt"))
    )
}


# 
# # TEST ----
# mtcars %>%
#   ggplot(aes(x = mpg, y = wt)) +
#   geom_point() +
#   labs(
#     title = "mpg VS wt"
#   ) +
#   bbc_style()
  # theme_TT()

## BBC theme (stolen)
## 
## 
##  TODO: IMPORT FONT 
# 
# bbc_style <- function() {\
#   font <- "Helvetica"
#   
#   ggplot2::theme(
#     
#     #Text format:
#     #This sets the font, size, type and colour of text for the chart's title
#     plot.title = ggplot2::element_text(family=font,
#                                        size=28,
#                                        face="bold",
#                                        color="#222222"),
#     #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
#     plot.subtitle = ggplot2::element_text(family=font,
#                                           size=22,
#                                           margin=ggplot2::margin(9,0,9,0)),
#     plot.caption = ggplot2::element_blank(),
#     #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
#     
#     #Legend format
#     #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
#     legend.position = "top",
#     legend.text.align = 0,
#     legend.background = ggplot2::element_blank(),
#     legend.title = ggplot2::element_blank(),
#     legend.key = ggplot2::element_blank(),
#     legend.text = ggplot2::element_text(family=font,
#                                         size=18,
#                                         color="#222222"),
#     
#     #Axis format
#     #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
#     axis.title = ggplot2::element_blank(),
#     axis.text = ggplot2::element_text(family=font,
#                                       size=18,
#                                       color="#222222"),
#     axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
#     axis.ticks = ggplot2::element_blank(),
#     axis.line = ggplot2::element_blank(),
#     
#     #Grid lines
#     #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
#     panel.grid.minor = ggplot2::element_blank(),
#     panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
#     panel.grid.major.x = ggplot2::element_blank(),
#     
#     #Blank background
#     #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
#     panel.background = ggplot2::element_blank(),
#     
#     #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
#     strip.background = ggplot2::element_rect(fill="white"),
#     strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
#   )
# }
# 
# 
# 
# create_footer <- function (source_name, logo_image_path) {
#   #Make the footer
#   footer <- grid::grobTree(grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.1, "npc")),
#                            grid::textGrob(source_name,
#                                           x = 0.004, hjust = 0, gp = grid::gpar(fontsize=16)),
#                            grid::rasterGrob(png::readPNG(logo_image_path), x = 0.944))
#   return(footer)
#   
# 
# }
# 
# save_plot <- function (plot_grid, width, height, save_filepath) {
#   grid::grid.draw(plot_grid)
#   #save it
#   ggplot2::ggsave(filename = save_filepath,
#                   plot=plot_grid, width=(width/72), height=(height/72),  bg="white")
# }
# 
# #Left align text
# left_align <- function(plot_name, pieces){
#   grob <- ggplot2::ggplotGrob(plot_name)
#   n <- length(pieces)
#   grob$layout$l[grob$layout$name %in% pieces] <- 2
#   return(grob)
# }
# 
# 
# finalise_plot <- function(plot_name,
#                           source_name,
#                           save_filepath=file.path(Sys.getenv("TMPDIR"), "tmp-nc.png"),
#                           width_pixels=640,
#                           height_pixels=450,
#                           logo_image_path = file.path(system.file("data", package = 'bbplot'),"placeholder.png")) {
#   
#   footer <- create_footer(source_name, logo_image_path)
#   
#   #Draw your left-aligned grid
#   plot_left_aligned <- left_align(plot_name, c("subtitle", "title", "caption"))
#   plot_grid <- ggpubr::ggarrange(plot_left_aligned, footer,
#                                  ncol = 1, nrow = 2,
#                                  heights = c(1, 0.045/(height_pixels/450)))
#   ## print(paste("Saving to", save_filepath))
#   save_plot(plot_grid, width_pixels, height_pixels, save_filepath)
#   ## Return (invisibly) a copy of the graph. Can be assigned to a
#   ## variable or silently ignored.
#   invisible(plot_grid)
# }
# 
# 
# 
