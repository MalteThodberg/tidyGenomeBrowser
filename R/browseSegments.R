#' @export
#' @rdname browseSegments
setMethod("browseSegments", signature(object = "ANY"),
          function(object, region=NULL){
              stop("object must be a GenomicRangesList, GenomicRanges or data.frame!")
          })

#' @export
#' @rdname browseSegments
setMethod("browseSegments", signature(object = "GenomicRangesList"),
          function(object, region = NULL, plot = TRUE){
              # Coerce to GenomicRanges
              o <- unlist(object, use.names=FALSE)

              # Add group
              if(is.null(names(object))){
                  o$facet <- rep(x = seq_along(object),
                                 times = elementNROWS(object))
              }else{
                  o$facet <- rep(x = names(object),
                                 times = elementNROWS(object))
              }

              # Next method
              browseSegments(object = o, region = region, plot = plot)
          })

#' @export
#' @rdname browseSegments
setMethod("browseSegments", signature(object = "GenomicRanges"),
          function(object, region = NULL, plot = TRUE){
              stopifnot(is.logical(plot),
                        !is.null(object$color))

              # Get plotting regions
              region <- flattenRegion(region = region, object = object)

              # Subset down
              o <- subsetByOverlaps(object, region, ignore.strand = TRUE)
              message("Features within region: ", length(o))

              # Ensure name exists and is a factor
              if(is.null(o$name)){
                  o$name <- factor("State")
              }

              # if(!is.factor(o$name)){
              #     o$name <- factor(o$name)
              # }

              # Would be good to check if the input is disjoint here!

              # Format as GPos, but save widths
              o$size <- width(o)
              o <- resize(o, fix="center", width=1)
              o <- methods::as(o, "GPos")

              # Coerce
              o <- as.data.frame(o)

              # Plot of necessary
              if(isTRUE(plot)){
                  o <- browseSegments(object = o, region = region)
              }

              # Return
              o
          })


#' @export
#' @rdname browseSegments
setMethod("browseSegments", signature(object = "data.frame"),
          function(object, region){
              # Setup plot
              o <- ggplot(object)

              # Add ranges
              o <- o + geom_tile(aes(x = .data$pos,
                                  y = .data$name,
                                  width = .data$size,
                                  height = 1,
                                  fill = .data$color))

              # Add facetting
              if(!is.null(object$facet) & nrow(object) != 0L){
                  message("Found facets...")
                  o <- o + facet_grid(facet~., scales="free_y", space="free_y")
              }

              # Remove y-axis
              o <- o +
                  scale_y_discrete(expand = expansion(add = getOption("tidyGenomeBrowser.expansion"))) +
                  ylab("") +
                  theme(panel.grid.minor.y = element_blank(),
                        panel.grid.major.y = element_blank())

              # Add layout
              o <- o +
                  scale_x_continuous(breaks = scales::pretty_breaks(n = getOption("tidyGenomeBrowser.breaks")),
                                     labels = scales::unit_format(unit = "MB",
                                                                  scale = 1e-6,
                                                                  accuracy=getOption("tidyGenomeBrowser.decimals")),
                                     expand = expansion(add = c(0, 0))) +
                  coord_cartesian(xlim = c(start(region),
                                           end(region))) +
                  xlab(paste0(getOption("tidyGenomeBrowser.prefix"),
                              as.character(seqnames(region))))

              # Return
              o
          })
