#' tidyGenomeBrowser package-wide settings.
#'
#' To avoid too many arguments to plotting functions while still retaining flexibility, some plotting parameters can be changed using package-wide settings.
#'
#' @section Package-wide settings:
#' Control axes:
#' * tidyGenomeBrowser.breaks: Number of x-axis breaks.
#' * tidyGenomeBrowser.decimals: Number of decimals on X-axis.
#' * tidyGenomeBrowser.prefix: Prefix to seqlevel on X-axis.
#' * tidyGenomeBrowser.expansion: How much white space to add arund track (e.g. for placing names) for browseIntervals, browseTranscripts and browsePositions.
#'
#' Control color and transparency:
#' * tidyGenomeBrowser.strand: Strand colors for plus, minus and unstranded.
#' * tidyGenomeBrowser.interactions: Colors or interactions arches inside/outside of plotting window in browseInteractions.
#' * tidyGenomeBrowser.alpha: Transparency for browseSignal and browsePositions.
#'
#' Control appearance:
#' * tidyGenomeBrowser.size: Size of points in browsePositions.
#' * tidyGenomeBrowser.height: Height of features in browseIntervals.
#' * tidyGenomeBrowser.thick: Height of thick features in browseIntervals.
#' * tidyGenomeBrowser.flip: Whether to flip signal on the minus strand to negative values in browseSignal.
#' * tidyGenomeBrowser.tx: Line-thickness of Intron, Exon and CDS regions in browseTranscripts.
#' * tidyGenomeBrowser.wiggle: Extra distance around each feature when binning features along the Y-axis in browseIntervals, browsePosition and browseTranscripts.
#' * tidyGenomeBrowser.name: Whether to add names to features in browseIntervals and browseTranscripts.
#'
#' Control labels:
#' * tidyGenomeBrowser.fontnudge: How much to nudge names upwards relative to the feature start.
#' * tidyGenomeBrowser.fontsize: Size of text label.
#' * tidyGenomeBrowser.fontforce: Force of repulsion between labels.
#' * tidyGenomeBrowser.fontpull: Force of attraction between points and labels.
#' * tidyGenomeBrowser.fontface: Font of text label, e.g. "italic".
#' * tidyGenomeBrowser.fontpad: Padding around label.
#' * tidyGenomeBrowser.fontsegment: Minimum length of segment between points and labels.
#' @md
#' @docType package
#' @name tidyGenomeBrowser
#' @import S4Vectors IRanges GenomicRanges InteractionSet ggplot2 ggrepel patchwork
#' @importFrom rlang .data
NULL