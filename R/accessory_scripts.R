utils::globalVariables(names = c('lambda', 'chr', 'value', 'rt', '.'))

#' Converts chromatograms to long format
#' @name convert_chrom
#' @importFrom tidyr pivot_longer
#' @param x A chromatographic matrix in wide format.
#' @return A chromatographic matrix in long format.
#' @author Ethan Bass

convert_chrom <- function(x){
  as.data.frame(x)
  data <- pivot_longer(data.frame(rt=rownames(x), x), cols=-rt, names_to="lambda")
  data$lambda <- gsub("X", "", data$lambda)
  data
}

#' Converts list of chromatograms to long format
#' @name tidy_chrom_converter
#' @param x A list of chromatographic matrices in wide format.
#' @return A list of chromatographic matrices in long format.
#' @author Ethan Bass

tidy_chrom_converter <- function(x){
  dat<-lapply(seq_along(x), function(i){
    xx<-convert_chrom(x[[i]])
    xx$chr <- names(x)[[i]]
    xx
  })
  do.call(rbind,dat)

}

#' Summarizes peak information
#' @name summarize_peak_info
#' @param peak_table A \code{peak_table} object.
#' @importFrom stats median sd
#' @return A \code{peak_table} object.
#' @author Ethan Bass

summarize_peak_info <- function(peak_table){
  mean_area <- sapply(peak_table$tab, mean)
  median_area <- sapply(peak_table$tab, median)
  sd_area <- sapply(peak_table$tab, sd)
  peak_table$pk_meta <- rbind(peak_table$pk_meta, rbind(mean_area, median_area, sd_area))
  peak_table
}
