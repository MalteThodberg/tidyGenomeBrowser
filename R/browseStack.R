#' Combine multiple browser images
#'
#' @param ggplots list: List of ggplots (with identical x-axis).
#' @param strip logical: Whether to strip x-axis of all but the bottom track.
#' @param squeeze logical: Whether to squeeze plots together by removing vertical margins.
#' @param ... additional arguments passed to wrap_plots.
#'
#' @return Vertically aligned combined tracks.
#' @export
#'
#' @examples
#' #TBA
browseStack <- function(ggplots, strip=TRUE, squeeze=FALSE, ...){
    stopifnot(is.list(ggplots),
              #all(vapply(ggplots, is.ggplot, FUN.VALUE = logical(1))),
              is.logical(strip),
              length(strip) == 1L,
              is.logical(squeeze),
              length(squeeze) == 1L)

    # Remove x axis
    if(isTRUE(strip)){
        l <- length(ggplots)
        ggplots <- c(lapply(ggplots[-l], strip_x_axis), ggplots[l])
    }

    # Squeeze plots
    if(isTRUE(squeeze)){
        ggplots <- lapply(ggplots, function(x) x + theme(plot.margin = margin(0, 0, 0, 0, "cm")))
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
