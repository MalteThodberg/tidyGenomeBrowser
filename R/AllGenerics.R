#' Browse genomic signal
#'
#' Genome browser-type plot of genoomic signal, e.g. BigWig/bedGraph-like data.
#'
#' @param object GenomicRangesList, GenomicRanges or data.frame: Genomic signal.
#' @param region GRanges or NULL: Plotting window.
#' @param plot logical: Whether to produce a plot or return the plot data.
#' @param ... not currently used.
#'
#' @details browseSignal needs the following columns: `score` and can react to: `pos`, `color`, `facet`.
#'
#' @return ggplot or data.frame.
#' @export
#'
#' @examples
#' browseSignal(ATAC_signal)
#' browseSignal(CAGE_CTSSs)
#' # See readme for more examples!
setGeneric("browseSignal", function(object, ...) {
    standardGeneric("browseSignal")
})

#' Browse genomic interactions
#'
#' Genome browser-type plot of genomic interactions, e.g. BEDPE-like data.
#'
#' @param object GInteractions: Genomic interactions.
#' @param region GRanges or NULL: Plotting window.
#' @param plot logical: Whether to produce a plot or return the plot data.
#' @param ... not currently used.
#'
#' @return ggplot or data.frame.
#' @export
#'
#' @examples
#' # TBA
setGeneric("browseInteractions", function(object, ...) {
    standardGeneric("browseInteractions")
})

#' Browse transcript structures
#'
#' Genome browser-type plot of transcript-structures (intron/exon/CDS), e.g. GTF-like data.
#'
#' @param object GRangesList: Transcripts-per-gene.
#' @param region GRanges or NULL: Plotting window.
#' @param CDS GRangesList: CDS-per-gene.
#' @param plot logical: Whether to produce a plot or return the plot data.
#' @param ... not currently used.
#'
#' @return ggplot or data.frame.
#' @export
#'
#' @examples
#' # TBA
setGeneric("browseTranscripts", function(object, ...) {
    standardGeneric("browseTranscripts")
})

#' Browse genomic intervals
#'
#' Genome browser-type plot of intervals, e.g. BED-like data.
#'
#' @param object GRangesList, GRanges or data.frame: Genomic intervals.
#' @param region GRanges or NULL: Plotting window.
#' @param plot logical: Whether to produce a plot or return the plot data.
#' @param ... not currently used.
#'
#' @details browseIntervals can react to metadata columns: `color`, `facet`, `thick`, `thickStart`, `thickEnd`
#'
#' @return ggplot or data.frame.
#' @export
#'
#' @examples
#' # TBA
setGeneric("browseIntervals", function(object, ...) {
    standardGeneric("browseIntervals")
})

#' Browse genomic positions
#'
#' Genome browser-type plot of single-bp positions, e.g. VCF-like data.
#'
#' @param object GPos, GenomicRangesList or data.frame: Genomic positions
#' @param region GRanges or NULL: Plotting window.
#' @param plot logical: Whether to produce a plot or return the plot data.
#' @param ... not currently used.
#'
#' @details browseFeatures can react to metadata columns: `color`, `facet`, `score`, `shape`.
#'
#' @return ggplot or data.frame.
#' @export
#'
#' @examples
#' # TBA
setGeneric("browsePositions", function(object, ...) {
    standardGeneric("browsePositions")
})

#' Easy plotting region / window extraction.
#'
#' Attempts to a single or multiple genomic ranges into a single plotting window.
#'
#' @param region GRanges: region(s) to be plotted.
#' @param object GenomicRanges: data to be plotted.
#'
#' @return GRanges of length 1.
#' @export
#'
#' @examples
#' # TBA
setGeneric("flattenRegion", function(region, object) {
    standardGeneric("flattenRegion")
})

