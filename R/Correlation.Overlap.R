#' @title CorrelationOverlap
#' @description This function computes the correlation between Nodes and the Overlapping Nodes of interest.
#' @param Data data.frame containing the expression data. Nodes on the Rows, Individuals on the Columns.
#'  Don't forget to give the names to the Nodes and to the Individuals.
#'  Nodes must have the row.names() with the Node Name.
#' @param Overlap  A vector containg the names of the Nodes of interest.
#' @param method  A vector containg the names of the Nodes a character string 
#'  indicating which correlation coefficient (or covariance) is to be computed. 
#'  One of "pearson" (default), "kendall", or "spearman": can be abbreviated.Spearman,
#'  Kendall, or Pearson correlation
#' @param use  an optional character string giving a method for computing covariances
#'  in the presence of missing values. This must be (an abbreviation of) one of the strings
#'  "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs".
#' @rdname CorrelationOverlap
#' @author Deisy Morselli Gysi <deisy at bioinf.uni-leipzig.de>

#' @export
#' @importFrom stats cor


CorrelationOverlap = function(Data, Overlap, method, use = "everything" ){
    COR <- suppressMessages(suppressWarnings(stats::cor(t(Data),
                                                       method = method,
                                                       use = use)))
  
  diag(COR) <- 0
  COR[is.na(COR)] = 0
  # Final_Correlation = subset(COR, row.names(COR) %in% Overlap)
  return(COR)
}
