#' Combine multiple browser images
#'
#' @param ggplots list: List of ggplots.
#' @param strip logical
#' @param ... additional arguments passed to wrap_plots.
#'
#' @return Vertically aligned combined plot.
#' @export
#'
#' @examples
#' #TBA
browseStack <- function(ggplots, strip=TRUE, ...){
    stopifnot(is.list(ggplots),
              #all(vapply(ggplots, is.ggplot, FUN.VALUE = logical(1))),
              is.logical(strip),
              length(strip) == 1L)

    # Remove x axis
    if(isTRUE(strip)){
        l <- length(ggplots)
        ggplots <- c(lapply(ggplots[-l], strip_x_axis), ggplots[l])
    }

    # Plot
    # ggpubr::ggarrange(plotlist = ggplots,
    #                   ncol = 1,
    #                   nrow = l,
    #                   align = "v",
    #                   #common.legend = sharedLegend,
    #                   legend = "right",
    #                   labels = names(ggplots), ...)
    patchwork::wrap_plots(ggplots, ncol=1, nrow=l, ...)
}
