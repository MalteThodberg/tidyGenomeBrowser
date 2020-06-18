#' @export
#' @rdname flattenRegion
setMethod("flattenRegion", signature(region="NULL", object="GenomicRanges"),
          function(region, object){
  # Simply reduce all ranges
  o <- reduce(methods::as(object, "GRanges"),
              min.gapwidth = 1e9L,
              ignore.strand = TRUE)

  # Check
  if(length(o) != 1){
    stop("Could not collect all ranges/signal into a single region!")
  }

  # Helpful message
  message("Plotting region size: ", width(o))

  # Return
  o
})

#' @export
#' @rdname flattenRegion
setMethod("flattenRegion", signature(region="NULL", object="GenomicRangesList"),
          function(region, object){
            flattenRegion(region=NULL,
                          object=unlist(object))
          })

#' @export
#' @rdname flattenRegion
setMethod("flattenRegion", signature(region="NULL", object="GInteractions"),
          function(region, object){
            flattenRegion(region=NULL,
                          object=GRangesList(anchors(object)))
          })

#' @export
#' @rdname flattenRegion
setMethod("flattenRegion", signature(region="GRanges", object="ANY"),
          function(region, object){
            # Check if genome compatible
            tmp <- GenomeInfoDb::checkCompatibleSeqinfo(region, object)

            # Next
            flattenRegion(region=NULL, object=region)
          })

#' @export
#' @rdname flattenRegion
setMethod("flattenRegion", signature(region="GInteractions", object="ANY"),
          function(region, object){
            flattenRegion(region=GRangesList(anchors(region)),
                          object=object)
          })