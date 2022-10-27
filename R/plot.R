harmony_plot <- function(x,columns,title=NULL, x_expansion_mult=0.2,
                         max_overlaps = Inf) {

  colour_factor = colour_factor_homey(x,columns[1])
  color_values = color_values_homey()

  ggplot2::ggplot(x,ggplot2::aes_string(x = columns[1], y = columns[2], colour=colour_factor)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = color_values, guide="none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = x_expansion_mult),
                                limits=c((0-max(abs(x[columns[1]]))),(0+max(abs(x[columns[1]]))))) +
    ggplot2::ggtitle(title) +
    theme_homey() +
    ggrepel::geom_text_repel(ggplot2::aes(label=.data$label), segment.color = colors_homey()$subtle_foreground,  max.overlaps = max_overlaps)
}

save_harmony_plots <- function(x,filetypes=c('svg','png','pdf')) {
  filetypes %>% purrr::map(~save_harmony_plot(x,.x))
}

save_harmony_plot <- function(x,filetype='svg') {
  filename=paste0(substr(paste0(
    getwd(),'/_plots/_',filetype,'/',fs::path_sanitize(gsub(' ', '', x$labels$title))),1,128),
    '.',filetype)
  if (filetype=='pdf') {
    suppressMessages(ggplot2::ggsave(filename,plot=x,device=cairo_pdf))
  } else {
    suppressMessages(ggplot2::ggsave(filename,plot=x))
  }
}

colors_homey <- function() {
  list(
    'background'        = '#664433',
    'foreground'        = '#F3DDAB',
    'subtle_foreground' = '#7F745A',
    'minor'             = '#ABDAF3',
    'neutral'           = '#FF5500',
    'major'             = '#F3A904'
  )
}
colour_factor_homey <- function(x,column_name) {
  checkmate::qassert(column_name,"S1")
  cut(x[[column_name]],c(-Inf,-1e-6,1e-6,Inf),labels=c("minor","neutral","major"))
}
color_values_homey <- function() {
  c("minor"=colors_homey()$minor,"neutral"=colors_homey()$neutral,"major"=colors_homey()$major)
}
path_homey <- function() {
  ggplot2::geom_path(ggplot2::aes(group=1),
                     arrow = grid::arrow(length = grid::unit(0.1, "inches"),
                                         ends = "last", type = "closed"))
}
theme_homey <- function(){
  font <- "Helvetica"   #assign font family up front

  ggplot2::theme_minimal()

  ggplot2::`%+replace%`  #replace elements we want to change

  ggplot2::theme(
    plot.title = ggplot2::element_text(color=colors_homey()$foreground),
    axis.title = ggplot2::element_text(color=colors_homey()$foreground),
    axis.text = ggplot2::element_text(color=colors_homey()$foreground),
    axis.ticks = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = colors_homey()$background),
    panel.background = ggplot2::element_rect(fill = colors_homey()$background),
    panel.grid.major = ggplot2::element_line(color = colors_homey()$foreground, size=0.2),
    panel.grid.minor = ggplot2::element_line(color = colors_homey()$foreground, size=0.05, linetype ="dashed")
  )
}
