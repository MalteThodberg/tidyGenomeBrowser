strip_x_axis <- function(x){
    x + theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())
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