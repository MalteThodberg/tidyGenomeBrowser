#' tidyGenomeBrowser package-wide settings.
#'
#' To avoid too many arguments to plotting functions while still retaining flexibility, some plotting parameters can be changed using package-wide settings.
#'
#' @section Package-wide settings:
#' * tidyGenomeBrowser.prefix: Prefix to seqlevel on X-axis.
#' * tidyGenomeBrowser.strand: Strand colors for plus, minus and unstranded.
#' * tidyGenomeBrowser.interactions: Colors or interactions arches inside/outside of plotting window in browseInteractions.
#' * tidyGenomeBrowser.expansion: How much white space to add arund track (e.g. for placing names) for browseIntervals, browseTranscripts and browsePositions.
#' * tidyGenomeBrowser.alpha: Transparency for browseSignal and browsePositions.
#' * tidyGenomeBrowser.nudge: How much to nudge names upwards relative to the feature start in browseIntervals and browseTranscripts.
#' * tidyGenomeBrowser.wiggle: Extra distance around each feature when binning features along the Y-axis in browseIntervals, browsePosition and browseTranscripts.
#' * tidyGenomeBrowser.height: Height of features in browseIntervals.
#' * tidyGenomeBrowser.thick: Height of thick features in browseIntervals.
#' * tidyGenomeBrowser.flip: Whether to flip signal on the minus strand to negative values in browseSignal.
#' * tidyGenomeBrowser.name: Whether to add names to features in browseIntervals and browseTranscripts.
#' * tidyGenomeBrowser.size: Size of text label in browseIntervals, browseTranscripts and browsePositions.
#' * tidyGenomeBrowser.tx: Line-thickness of Intron, Exon and CDS regions in browseTranscripts.
#' @md
#' @docType package
#' @name tidyGenomeBrowser
#' @import S4Vectors IRanges GenomicRanges InteractionSet ggplot2 ggrepel patchwork
#' @importFrom rlang .data
NULL