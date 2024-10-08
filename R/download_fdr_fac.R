# Either export from nhdplusTools or leave this
check7z = function (){
  tryCatch({
    system("7z", intern = TRUE)
  }, error = function(e) {
    stop(simpleError("Please Install 7zip (Windows) or p7zip (MacOS/Unix). Choose accordingly:\n        Windows: https://www.7-zip.org/download.html\n        Mac: 'brew install p7zip' or 'sudo port install p7zip'\n        Linux: https://sourceforge.net/projects/p7zip/"))
  })
}

#' Download Elevation and Derivatives
#' @param product character DEM, hydroDEM, or FDRFAC.  
#' @param out_dir path to directory to store output.
#' @param regions character vector of two digit hydrologic 
#' @export
download_elev <- function(product, out_dir, regions = NULL) {
  
  dev_null <- check7z()
  
  allowable <- c("DEM" = ".*NEDSnapshot.*", "hydroDEM" = ".*HydroDem.*", "FDRFAC" = ".*FdrFac.*")
  
  if(!all(product %in% names(allowable))) stop(paste("product must be one of", allowable))
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  root <- "http://www.horizon-systems.com"
  base <- "/NHDPlusData/NHDPlusV21/Data/"
  
  check <- paste0(root, base)
  found <- c()
  i <- 1
  
  while(i <= length(check)) {
    url <- check[i]
    
    pg <- rvest::read_html(url)
    
    pg_links <- paste0(url, html_attr(html_nodes(pg, "a"), "href"))
    
    pg_links <- pg_links[!pg_links %in% check]
    
    check <- c(check, pg_links[!grepl("[.][.]/", pg_links) & grepl(".*/$", pg_links)])
    
    found <- c(found, pg_links[grepl(paste(allowable[product], collapse = "|"), pg_links, ignore.case = TRUE)])
    
    i <- i + 1
    
    if(i > 10000) stop("something is wrong with the URL finder loop.")
  }
  
  if(!is.null(regions)) {
    found <- found[grepl(paste(paste0(".*_", regions, "?[a-z]_.*"), 
                               collapse = "|"), found, ignore.case = TRUE)]
  }
  
  for(f in found) {
    fi <- basename(f)
    url <- f
    out_fi <- file.path(out_dir, fi)
    
    message(out_fi)
    
    if(!file.exists(out_fi)) {
      RETRY("GET", url, write_disk(out_fi), progress())
      
      system(paste0("7z -y -o", path.expand(out_dir), " x ", 
                    path.expand(out_fi)), intern = TRUE)
    }
  }
}

#' Download FDR FAC
#' @inheritParams download_elev
#' @export
download_fdr_fac <- function(out_dir, regions = NULL) {
  
  download_elev(product = c("FDRFAC"), out_dir, regions)
  
}
