###################### RUN to install packages from local repo ######################
# install and load required packages
options(stringsAsFactors = FALSE)
requiredPackages <- c("RCurl", "getopt")
if(!requireNamespace(requiredPackages, quietly = TRUE))
    install.packages(requiredPackages, repos="https://ftp.fau.de/cran/")
x <- lapply(requiredPackages, require, character.only=TRUE)

# parameter specification
spec = matrix(c(
  'repoDir', 'r', 1, 'character',
  'threads', 't', 2, 'integer',
  'installDir'   , 'i', 2, 'character'
), byrow=TRUE, ncol=4)
opt = getopt(spec)

if(is.null(opt$repoDir)) {
	cat(getopt(spec, usage=TRUE))
	q(status=1)
}
# ensure that dir does exist
if(!dir.exists(opt$repoDir)) {
	print(paste("ERROR: dir '", opt$repoDir,"' does not exist!", sep=""))
	q(status=1)
}
THREADS <- 4
LIB <- NULL
if(!is.null(opt$threads)) { THREADS <- opt$threads }
if(!is.null(opt$installDir)) { 
	LIB <- opt$installDir 
	if(dir.exists(LIB)) {
		print(paste("ERROR: install dir '", opt$downloadDir,"' does already exist!", sep=""))
		q(status=1)
	} else {
		dir.create(LIB, recursive = TRUE)
	}

}

# define some paths
URL_SAVE_PATH <- paste(opt$repoDir, "R_packinfo", sep="/")
URL_LOCAL_REPO <- paste(opt$repoDir, "R_repo/", sep="/")

# load the packages to install and install them!
load(URL_SAVE_PATH)
install.packages(pkgs = urls$name, lib = LIB, repos = paste("file:///", URL_LOCAL_REPO, sep=""), type="source", Ncpus = THREADS)
