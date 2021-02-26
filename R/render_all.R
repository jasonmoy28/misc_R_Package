#' RMarkdown Render
#'
#' Center all columns with respect to the grand mean
#'
#' @param dir directory path
#' @param render_exist render existed pdf files or not
#'
#' @return
#'
#' @export
#'
#' @examples
#'
render_all = function(dir,render_exist = T) {
  files = list.files(dir)
  if (!render_exist) {
    pdf_files = files[stringr::str_detect(pattern = '.pdf',string = files)]
    pdf_cleaned = stringr::str_replace(pdf_files,'.pdf','')
    files = files[!stringr::str_detect(pattern = pdf_cleaned,string = files)]
  }
  files = files[stringr::str_detect(pattern = '.Rmd',string = files)]
  print(paste('List of rendering files:', paste(files,collapse = '; ')))
  for (file in files) {
    file_path = paste(dir,'/',file,sep = '')
    rmarkdown::render(file_path)
  }
}
