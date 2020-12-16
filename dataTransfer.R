# Script for transferring data to a new space
#  @source: source of all the data files
#  @destination: where you're going to put them all
#  @target = level of tiles to copy from - the deeper, the slower
library(dplyr)
library(pbapply)
transferData <- function(source = '.', destination, target = 1){
  now <- Sys.time()
  folders <- list(c(),c('www','data'))
  names(folders) <- c('levels','notLevels')
  lapply(folders[[2]], FUN = function(i) dir.create(file.path(destination, i)))
  cat('Copied top level folders\n\n')
  getNext <- function(level, target, depth, destination){
    # Find files
    d <- list.files(level$notLevels, full.names = TRUE)
    # Find which of these are directories
    dirs <- list.dirs(level$notLevels, recursive = FALSE)
    # Find which of these are levels
    levels <- d[grepl('^[0-9]+$', basename(d))]
    # Create all non-level directories
    cat('Creating non-level directories')
    pblapply(dirs[!(dirs %in% levels)], FUN = function(i) dir.create(file.path(destination,i)))
    # Copy all non-directory files
    files <- d[(!(d %in% levels))&(!(d %in% dirs))]
    cat('Copying non-level files\n')
    pblapply(files, FUN = function(file){
      file.copy(file, file.path(destination, file))
      cat(file)
    })
    # Create all level directories
    levelsToCopy <- levels[(basename(levels) %>% as.numeric())<=target]
    cat('Creating level folders\n')
    pblapply(levelsToCopy, FUN = function(i) dir.create(file.path(destination,i)))
    # Copy recursively contents of these levels
    cat('Copying level contents\n')
    pblapply(levelsToCopy, FUN = function(i){
      lapply(file.path(destination,list.dirs(i,recursive = FALSE,full.names = TRUE)),
             FUN = function(i) dir.create(i))
      toCopy <- list.files(i, recursive = TRUE, full.names = TRUE)
      file.copy(toCopy, file.path(destination, toCopy))
      cat(toCopy[1])
    })
    # Output results for next level
    d <- d[!(d %in% levels)]
    results <- list(levels, d)
    names(results) <- c('levels','notLevels')
    cat('Copied all files from',depth,'deep\n\n')
    results
  }
  depth = 2
  while(length(folders$notLevels)>0){
    folders <- getNext(folders, target, depth, destination)
    depth <- depth+1
  }
  Sys.time() - now
  cat('All done\n')
}
transferData(source = '.',destination = 'W:/PYWELL_SHARED/Pywell Projects/BRC/Mark Logie/tmp_MFR',target = 5)
