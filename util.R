save_plot <- function(p_plot, name_file){
  ggsave(
    filename = paste("./image/",name_file, ".jpg", sep=""),
    device = "jpg",
    plot = p_plot,
    width = 1920,
    height = 1080,
    units = "px"
  )
}