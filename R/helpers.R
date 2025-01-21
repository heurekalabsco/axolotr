#' Read and encode PDF file
#'
#' @param path Path to the PDF file or URL
#' @return Base64 encoded string of the PDF content
#' @importFrom httr GET content
#' @importFrom base64enc base64encode
#' 
#' @export
encode_pdf <- function(path) {
  tryCatch({
    # Check if the path is a URL
    is_url <- grepl("^https?://", path)
    
    if (is_url) {
      # Download from URL
      response <- httr::GET(path)
      content <- httr::content(response, "raw")
    } else {
      # Read local file
      content <- readBin(path, "raw", file.info(path)$size)
    }
    
    # Encode to base64
    base64enc::base64encode(content)
  }, error = function(e) {
    stop(paste("Error encoding PDF:", e$message))
  })
}

#' Create PDF content block for API request
#'
#' @param pdf_data Base64 encoded PDF data
#' @param cache Logical indicating whether to cache the PDF. Default is FALSE
#' @return List containing the PDF content block
#' 
#' @export
create_pdf_block <- function(pdf_data, cache = FALSE) {
  block <- list(
    type = "document",
    source = list(
      type = "base64",
      media_type = "application/pdf",
      data = pdf_data
    )
  )
  
  if (cache) {
    block$cache_control <- list(type = "ephemeral")
  }
  
  return(block)
}

