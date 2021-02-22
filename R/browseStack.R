#' Combine multiple browser images
#'
#' @param ggplots list: List of ggplots (with identical x-axis).
#' @param strip logical: Whether to strip x-axis of all but the bottom track.
#' @param squeeze character: Whether to squeeze plots together by removing
#'   margins: "none" is no squeezing, "all" shrinks all margins and "internal"
#'   only shrink margins between plots.
#' @param ... additional arguments passed to wrap_plots.
#'
#' @return Vertically aligned combined tracks.
#' @export
#'
#' @examples
#' #TBA
browseStack <- function(ggplots, strip=TRUE, squeeze="none", ...){
    stopifnot(is.list(ggplots),
              #all(vapply(ggplots, is.ggplot, FUN.VALUE = logical(1))),
              is.logical(strip),
              length(strip) == 1L)

    squeeze <- match.arg(squeeze, choices=c("none", "internal", "all"))

    l <- length(ggplots)

    # Remove x axis
    if(isTRUE(strip)){
        ggplots <- c(lapply(ggplots[-l], strip_x_axis), ggplots[l])
    }

    # Squeeze plots
    if(squeeze == "all"){
        ggplots <- lapply(ggplots, function(x) x + theme(plot.margin = margin(0, 0, 0, 0, "cm")))
    }else if(squeeze == "internal"){
        ggplots <- strip_margins(ggplots)
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
