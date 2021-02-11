# Strip X-axis from a single plot
strip_x_axis <- function(x){
    x + theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())
}

# Strip internal margins
strip_margins <- function(xx){
    stopifnot(is.list(xx))

    # Length
    l <- length(xx)

    if(l >= 2){
        # Get default marings
        def_margins <- theme_get()$plot.margin
        zeropoint <- unit(0, units="points")

        # New margins
        top_margins <- bottom_margins <- def_margins
        top_margins[3] <- zeropoint
        bottom_margins[1] <- zeropoint

        # Edges
        xx[[1]] <- xx[[1]] + theme(plot.margin = top_margins)
        xx[[l]] <- xx[[l]] + theme(plot.margin = bottom_margins)

        # Replace middle
        if(l >= 3){
            # New margins
            middle_margins <- bottom_margins
            middle_margins[3] <- zeropoint

            # Plot to be replaces
            middle_tracks <- seq(from=2, to=l-1, by=1)

            # Replace
            xx[middle_tracks] <- lapply(xx[middle_tracks], function(i) i + theme(plot.margin = middle_margins))
        }
    }

    # Return
    xx
}

#
#
#
# # Function for checking dataframe
# validatePlotData <- function(x){
#     stopifnot(is.data.frame(x),
#               is.factor(x$strand),
#               all(levels(x$strand) == c("+", "-", "*")))
#
#     # Check bin
#     if(!is.null(x$bin)){
#         stopifnot(is.integer(x$bin))
#         bin_flag <- TRUE
#     }else{
#         bin_flag <- FALSE
#     }
#
#     # Check name
#     if(!is.null(x$name)){
#         stopifnot(is.character(x$name) | is.factor(x$name))
#         name_flag <- TRUE
#     }else{
#         name_flag <- FALSE
#     }
#
#     # Check name
#     if(!is.null(x$name)){
#         stopifnot(is.character(x$name) | is.factor(x$name))
#         name_flag <- TRUE
#     }else{
#         name_flag <- FALSE
#     }
#
#
#     col_types <- vapply(x, class, character(1))
#
#
# }
#
# react <- function(x){
#     stopifnot(is.data.frame(x))
# }