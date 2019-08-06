#' Get operating system architecture
#' 
#' This package returns the type of OS you use.
#' 
#' This function is stolen from somewhere on the web. I will look it up later :). 
#' 
#' @export
getOS <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

#' Sources all files from the designated path
#' 
#' Refresh functions
#' 
#' @param path Character value. A path to a folder. 
#' @export
funky<- function(path="scripts/methods/"){
  all<-list.files(path)
  # get the R files
  allList <- strsplit(all, "\\.")
  logR<- unlist(lapply(allList, function(x){
    x[2]=="R"
  }))

  all<-all[logR]
  for(i in 1:length(all)){
     source(file.path(path,all[i]))
  }
 
}