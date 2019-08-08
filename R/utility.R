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
#' @rdname refresh
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


#' @rdname refresh
#' @export
plotz<- function(path="scripts/plots/"){
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


#' Result storage
#' 
#' Funtions to save results (R-binary objects) in a version-specific folder and retrieve them all.
#' 
#' The purpose of this code is to separate meaningful R objects and merge huge workspaces without making a mess. Merging workspaces is necessary if the combined statistical analysis/plotting/reporting is required.
#' 
#' @param x R object to be saved as a result. 
#' @param v Character. The version of the results.
#' @param within Character. Path leading to the folder for the version. 
#' @param folder 
#' @rdname results
#' @export
stores <- function(x, v, within="export/", folder="res/"){

  path<- paste(within, v,"/",folder, sep="")

  theName <- match.call(stores)
  theNameList <- as.list(theName)
  theNameChar <- as.character(theNameList$x)

  assign(theNameChar, x)

  save(list=theNameChar, file=paste(path, theNameChar, ".RData", sep=""))

}

#' @rdname results
#' @export
results<- function(v, within="export/", folder="res/"){
  path<- paste(within, v,"/",folder, sep="")
 all<-list.files(path)
  # get the R files

  allList <- strsplit(all, "\\.")

  logR<- unlist(lapply(allList, function(x){
    x[2]=="RData"
  }))

  all<-all[logR]

  objname <- unlist(lapply(allList[logR], function(x) x[1]))

  for(i in 1:length(all)){
     load(file.path(path,all[i]), envir=globalenv())

  }
}
