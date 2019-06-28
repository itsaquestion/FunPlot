#' theme_textbook
#'
#' 教科书ggplot主题（无grid大白板）
#'
#' @return ggplot主题
#' @export
#'
#' @examples
theme_textbook = function() {
  theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(plot.margin = unit(c(1, 0.5, 0, 0), "cm")) + theme(legend.position = "none") 

}
