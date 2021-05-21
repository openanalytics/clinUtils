#' Compare reference with new figures in png format with similar names.
#' 
#' The figures are mapped based on filename and only the 
#' figures present both in \code{pathFigRef} and \code{pathFigNew}
#' are considered.
#' 
#' If \href{https://imagemagick.org/}{ImageMagick} is available, in case the figures differ, the figures with
#' the difference is saved in the \code{outputDir} folder, under the name '[filename]-diff.png'
#' @param pathFigRef String with path of directory where the reference figures have been stored,
#' or directly the paths of the reference files.
#' @param pathFigNew String with path of directory where the new figures have been stored,
#' or directly the paths of the new files.
#' @param ... Additional parameters for \code{\link{list.files}} function,
#'  e.g. pattern of files to consider
#' @inheritParams compareFigOne
#' @inherit compareFigOne return
#' @importFrom utils file_test
#' @example inst/examples/compareFigs-example.R
#' @author Laure Cougnaud
#' @export
compareFigs <- function(
	pathFigRef, pathFigNew, 
	outputDir = "./compareFigs",
	...){

	# list figures if not directory
	extractPath <- function(path){
		# check if file(s) exist
		idxFileNotExist <- which(!file.exists(path))
		if(length(idxFileNotExist)){
			warning(path[idxFileNotExist], "don't exist, so are not considered.")
			path <- path[-idxFileNotExist]
		}
		# check if file(s) is(are) directory(ies)
		idxIsDir <- which(file_test(op = "-d", x = path))
		if(length(idxIsDir)){
			pathFromDir <- list.files(path = path[idxIsDir], full.names = TRUE, ...)
			path <- c(path[-idxIsDir], pathFromDir)
		}
		return(path)
	}
	
	figuresNew <- extractPath(path = pathFigNew)
	figuresOld <- extractPath(path = pathFigRef)
	
	figuresCommon <- intersect(basename(figuresNew), basename(figuresOld))
	
	for(figure in figuresCommon){
		
		figNewPath <- figuresNew[basename(figuresNew) == figure]
		figOldPath <- figuresOld[basename(figuresOld) == figure]
		compareFigOne(pathFigRef = figOldPath, pathFigNew = figNewPath, outputDir = outputDir)
		
	}
	
}

#' Compare two figures to each other
#' 
#' If the figures are in PNG format, raster image are checked if they are identifical.
#' If ImageMagick is available (and only in case the figures differ for PNG), the figure with
#' the difference is saved in the \code{outputDir} folder, under the name '[filename]-diff.png'
#' @param pathFigRef String with path to the reference figure (only one)
#' @param pathFigNew String with path to the new figure (only one)
#' @param outputDir String with path to the folder where figure with difference are saved (if any)
#' as produced with 'ImageMagick compare' command.
#' @return No returned value, message is printed in the current
#' console with 'ok!' if no difference, otherwise: 'DIFFERENT!'
#' @author Laure Cougnaud
#' @importFrom png readPNG
#' @importFrom grDevices as.raster
#' @importFrom tools file_path_sans_ext
#' @export
compareFigOne <- function(pathFigRef, pathFigNew, outputDir = "./compareFigs"){
	
	figure <- paste(unique(c(basename(pathFigRef), basename(pathFigNew))), collapse = " versus ")
	
	isPNG <- all(c(file_ext(pathFigRef), file_ext(pathFigNew)) == "png")
	
	# check if figures are identical (only for PNG format)
	compareFig <- if(isPNG){
		picNew <- as.raster(readPNG(pathFigNew))
		picOld <- as.raster(readPNG(pathFigRef))
		!identical(picNew, picOld)
	}else TRUE
	
	if(compareFig){
		
		imError <- try(system("magick --help", ignore.stdout = TRUE, ignore.stderr = TRUE, intern = TRUE), silent = TRUE)
		imageMagickAvailable <- !inherits(imError, "try-error")
		
		if(imageMagickAvailable){
			
			if(!dir.exists(outputDir))	dir.create(outputDir, recursive = TRUE)
			figDiffPath <- paste(unique(file_path_sans_ext(basename(c(pathFigRef, pathFigNew)))), collapse = "_")
			figDiffPath <- file.path(outputDir, paste0(figDiffPath, "-diff.png"))
#				figNewPathForIM <- file.path("new.png");file.copy(from = figNewPath, to = figNewPathForIM)
#				pathFigRefForIM <- "old.png";file.copy(from = pathFigRef, to = pathFigRefForIM)
			system(paste("magick compare", pathFigNew, pathFigRef, figDiffPath))
			msgExtra <- paste("\n  Check difference in:", figDiffPath)
			
		}else	msgExtra <- ""
		
		if(isPNG){
			message("Comparing: ", figure, ": DIFFERENT!", msgExtra)
		}else	message("Comparing: ", figure, msgExtra)
		
	}else	message("Comparing: ", figure, ": ok!")
	
}