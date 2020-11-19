###################### RUN to create a local repo from installed packages ######################
# install and load required packages
options(stringsAsFactors = FALSE)
requiredPackages <- c("RCurl", "getopt")
if(!requireNamespace(requiredPackages, quietly = TRUE))
    install.packages(requiredPackages, repos="https://ftp.fau.de/cran/")
x <- lapply(requiredPackages, require, character.only=TRUE)

# wait x seconds to inform the user on whats going on
SLEEP_TIME <- 15

# parameter specification
spec = matrix(c(
  'repoDir', 'r', 1, 'character',
  'sleepTime', 's', 2, 'integer',
  'verbose'   , 'v', 0, 'logical'
), byrow=TRUE, ncol=4)
opt = getopt(spec)

if(is.null(opt$repoDir)) {
	cat(getopt(spec, usage=TRUE))
	q(status=1)
}
# ensure that dir does not exist
if(dir.exists(opt$repoDir)) {
	print(paste("ERROR: dir '", opt$repoDir,"' does already exist!"), sep="")
	q(status=1)
} else {
	dir.create(opt$repoDir, recursive = TRUE)
}
if(!is.null(opt$sleepTime)) { SLEEP_TIME <- opt$sleepTime }
VERBOSE <- !is.null(opt$verbose)

# get bioconductor version if installed
getBioconductorVersion <- function() {
	if(length(which(as.data.frame(installed.packages()) == 'BiocManager')) == 1) {
		return(as.character(BiocManager::version()))
	}
	return(NA)
}

# get versions of R and bioconductor
rVersion <- paste(R.version$major, ".", R.version$minor, sep="")
bioconducterVersion <- getBioconductorVersion()

# define some paths
URL_SAVE_PATH <- paste(opt$repoDir, "R_packinfo", sep="/")
URL_LOCAL_REPO <- paste(opt$repoDir, "R_repo/", sep="/")
URL_PACKAGES_INFO <- paste(opt$repoDir, "R_craninfo.rds", sep="/")
URL_TMP <- paste(opt$repoDir, "R_random_tmp_file.rds", sep="/")

# set package URLs (currently bioconductor + cran)
baseCRAN <- "https://cran.r-project.org/src/contrib/"
cransList <- list(cran=baseCRAN)
if(!is.na(bioconducterVersion)) {
	baseBioconductor <- paste("https://bioconductor.org/packages/", bioconducterVersion, "/bioc/src/contrib/", sep="")
	baseBioconductorAnnotation <- paste("https://bioconductor.org/packages/", bioconducterVersion, "/data/annotation/src/contrib/", sep="")
	cransList <- append(cransList, list(bioc=baseBioconductor, biocAnno=baseBioconductorAnnotation), after = 0)
}

# set URLs to archives without meta-data (PACKAGES.rds files)
cRANArchive <- "https://cran.r-project.org/src/contrib/Archive/"
archiveList <- list(cran=cRANArchive)
if(!is.na(bioconducterVersion)) {
	bioconductorArchive <- paste("https://bioconductor.org/packages/", bioconducterVersion, "/bioc/src/contrib/Archive/", sep="")
	bioconductorAnnotationArchive <- paste("https://bioconductor.org/packages/", bioconducterVersion, "/data/annotation/src/contrib/Archive/", sep="")
	archiveList <- append(archiveList, list(bioc=bioconductorArchive, biocAnno=bioconductorAnnotationArchive), after = 0)
}


################################### FUNCTIONS ###################################
###################################		###################################
#################################################################################

# get all installed packages
getInstalledPackages <- function() {
	info <- installed.packages()
	info <- as.data.frame(info)
	return(info)
}

# get dependencies of all installed packages
getRequiredDependencies <- function(pksInfo) {
	dependencies <- paste(c(pksInfo$Depends, pksInfo$Imports), collapse=",")

	# remove junk from strings	
	dependencies <- gsub("\n", "", dependencies)
	dependencies <- gsub(", ", ",", dependencies)

	# split at ',' and remove NA and R-version dependencies
	dependencies <- unlist(strsplit(dependencies, ","))
	dependencies <- dependencies[dependencies != "NA"]
	dependencies <- dependencies[grep("^R \\(", dependencies, invert=T)]
	dependencies <- unique(dependencies)

	# get version restrictions 
	depVersions <- sapply(dependencies, getVersion, simplify=TRUE)
	depVersions <- data.frame(matrix(unlist(depVersions), nrow=ncol(depVersions), byrow=T))
	colnames(depVersions) <- c("Package", "minV", "maxV")

	# aggregate over all packages by package name
	minV <- aggregate(depVersions$minV, by=list(depVersions$Package), FUN=max)
	maxV <- aggregate(depVersions$maxV, by=list(depVersions$Package), FUN=min)
	colnames(minV) <- colnames(depVersions)[c(1,2)]
	colnames(maxV) <- colnames(depVersions)[c(1,3)]
	depVersions <- merge(minV, maxV, by="Package")

	return(depVersions)
}

# increment a version number
incrementVersion <- function(ver) {
	v <- unlist(strsplit(ver, "[.-]")) # split at . and -
	# increase last part
  	n <- length(v)
	old <- as.integer(v[n])
	new <- old+1
	# replace last part
	v <- gsub(paste(old, "$", sep=""), new, ver)
	return(v)	
}

# decrement a version number
decrementVersion <- function(ver) {
	# split at . and -
	containsDash <- grep("-", ver) > 0
	v <- unlist(strsplit(ver, "[.-]")) 
  	n <- length(v)
	ok <- 0
	# set last parts on maximal value while part == 0
	while(ok == 0 && n > 0) {
		old <- as.integer(v[n])
		if(old > 0) {
			print(old)
			new <- old-1
			v[n] <- new
			ok <- 1
		} else {
			v[n] <- 999999999
			n <- n-1
		}
	}
	# rebuild version number
	vv <- paste(v, collapse=".")
	if(containsDash) { vv <- gsub("\\.[0-9]+$", paste("-", v[length(v)], sep=""), vv)}
	return(vv)	
}

# gets min and max version based on a string (e.g. 'packageName (>=0.2.4)')
getVersion <- function(nameAndCond) {
	# set default min and max versions
	minVersion <- 0
	maxVersion <- 999999999

	# split at '('
	x <- unlist(strsplit(nameAndCond, " *\\("))
	name <- x[1] # name of the package
	if(length(x) == 2) { # some additional info is given
		x[2] <- gsub(")", "", x[2])
		y <- unlist(strsplit(x[2], "[0-9]+"))
		operator <- gsub(" ", "", y[1]) # get the operator
		vers <- sub(operator, "", x[2]) # get the version
		
		# apply the operator
		switch(operator, ">="={minVersion=vers}, ">"={minVersion=incrementVersion(vers)}, "<="={maxVersion=vers}, "<"={maxVersion=decrementVersion(vers)}, "="={minVersion=vers; maxVersion=vers}, {print(paste("unsupported operator: ", nameAndCond, sep=""))})
	} else {
		if(length(x) > 2) print(paste("unexpected format: ", nameAndCond, sep=""))
	}
	return(c(name, minVersion, maxVersion))
}

# remove dependencies that are already fullfilled
removeFulfilledDependencies <- function(pksInfo, depVersions) {
	for(i in 1:nrow(pksInfo)) {
		# package that will be installed later on
		rr <- pksInfo[i, ]

		# test if depdency is already fulfilled
		if(rr$Package %in% depVersions$Package) {
			rid <- which(depVersions$Package == rr$Package)
			dv <- depVersions[rid, ]
			# check the versions and remove dependency
			v <- rr$Version
			if(compareVersion(dv$minV, v) <=0 && compareVersion(dv$maxV, v) >= 0) {
				depVersions <- depVersions[-rid, ]
			}
		}
	}
	return(depVersions)
}

# downloads a file or exists if download fails
download <- function(u, dest) {
	ret <- download.file(u, destfile=dest, quiet = TRUE)
	if(ret == 0)  {
		if(VERBOSE) { print(paste("downloaded: '", u, "'", sep="")) }
	} else {
		print(paste("ERROR: failed to download: '", u, "'", sep=""))
		quit(status=1)
	}	
}

# check if there are PACKAGES.rds files and which packages can be installed from there
getPackageInfoFromRepo <- function(cransList) {
	packageCRANInfo <- NULL
	for(i in 1:length(cransList)) {
		x <- cransList[[i]]
		u <- paste(x, "PACKAGES.rds", sep="/")
		if(!url.exists(u)) {
			print(paste("missing 'PACKAGES.rds': ", u))
		} else {
			# download and read the file
			download(u, URL_PACKAGES_INFO)	
			d <- readRDS(URL_PACKAGES_INFO)
			d <- as.data.frame(d)
			d$source <- names(cransList)[i]
			if(is.null(d$Path)) { d$Path <- NA }

			# append it to result
			if(is.null(packageCRANInfo)) {
				packageCRANInfo <- d
			} else {
				packageCRANInfo <- rbind(packageCRANInfo, d)
			}
		}
	}
	return(packageCRANInfo)
}

# parse all versions of a package from all archive pages (index html page...)
getPackageVersionsFromArchive <- function(archiveList, name, sourceName) {
	versions <- NULL
	# version name pattern
	pattern <- paste("(", name, "_[0-9]+(.*[0-9]+)?\\.tar\\.gz)", sep="")
	patternReplace <- paste(".*", pattern, ".*", sep="")

	# iterate over all archives
	for(a in archiveList) {
		u <- paste(a, name, sep="/")
		# download the index if it exists
		if(url.exists(u)) {
			download(u, URL_TMP)
			# iterate over the file and find lines with matching patterns
			con <- file(URL_TMP, "r")
			while(TRUE) {
				line <- readLines(con, n = 1)
				if(length(line) == 0) {
					break
				}
				# test if pattern matches
				ret <- grep(pattern, line) == 1
				if(length(ret) == 1 && ret > 0) {
					# extract and save version
					match <- gsub(patternReplace, "\\1", line)
					v <- unlist(strsplit(match, "_"))
					v <- sub(".tar.gz", "", v[length(v)], fixed=TRUE)
					versions <- rbind(versions, c(v, a, sourceName))
				}
			}
			close(con)
		}
	}
	return(versions)
}

# updates the package index
writePackageIndex <- function(sourcePath, rVersion) {
	if(compareVersion(rVersion, "3.5.0") <=0) {
		tools::write_PACKAGES(dir=sourcePath, type="source", rds_compress=2) 
	} else {
		tools::write_PACKAGES(dir=sourcePath, type="source") 
	}
}

# download the packages to install
downloadAndIndex <- function(urls, rVersion) {
	# create process bar 
	pb <- txtProgressBar(min = 0, max = nrow(urls), style = 3)

	sourcePath <- paste(URL_LOCAL_REPO, "/src/contrib/", sep="")
	dir.create(sourcePath, recursive = TRUE)
	x <- sapply(1:nrow(urls), FUN = function(i) { x <- setTxtProgressBar(pb, i); download(urls[i, "url"], paste(URL_LOCAL_REPO, "/src/contrib/", basename(urls[i, "url"]), sep=""))}, simplify=TRUE)

	# test if all downloads were ok
	close(pb)
	print(paste("download of ", nrow(urls), " packages ok!", sep=""))
	print("building index...")
	n <- writePackageIndex(sourcePath, rVersion)
	if(n == nrow(urls)) {
		print(paste("index for ", nrow(urls), " packages ok!", sep=""))
	} else {
		print("Failed to index some packages!")
	}
}

# test which URL exists in the order as given in baseList (data must contain 'Package' and 'Version' or ('minV' and 'maxV')
getURL <- function(data, cranInfoMeta, cransList, archiveList) {
	package <- data$Package
	if(!is.null(data$Version)) {
		data$minV <- data$Version
		data$maxV <- data$Version
	}

	# test if the version can be found in the meta-data
	gotPackage <- cranInfoMeta[which(cranInfoMeta$Package == package), ]
	for(i in 1:nrow(gotPackage)) {
		d <- gotPackage[i, ]
		if(compareVersion(data$minV, d$Version) <=0 && compareVersion(data$maxV, d$Version) >= 0) {
			base <- cransList[[which(names(cransList) == d$source)]]
			name <- paste(package, "_", d$Version, ".tar.gz", sep="")
			path <- ""
			if(!is.na(d$Path)) { path = d$Path }
			url <- paste(base, "/", path, name, sep="")
			if(url.exists(url)) {
				return(c(package, url, d$source))
			}
		}
	}

	# if not hit so far - get all versions from the archives
	archiveVersions <- sapply(1:length(archiveList), FUN = function(i) { getPackageVersionsFromArchive(archiveList[[i]], package, names(archiveList)[i]) }, simplify=TRUE)
	archiveVersions <- data.frame(matrix(unlist(archiveVersions), ncol=3, byrow=F))
	colnames(archiveVersions) <- c("version", "base", "source")

	# check if any of these is good in reverse order
	for(ii in nrow(archiveVersions):1) {
		version <- archiveVersions[ii, "version"]
		if(compareVersion(data$minV, version) <=0 && compareVersion(data$maxV, version) >= 0) {
			base <- archiveVersions[ii, "base"]
			name <- paste(package, "_", version, ".tar.gz", sep="")
			url <- paste(base, "/", package, "/", name, sep="")
			if(url.exists(url)) {
				return(c(package, url, paste(archiveVersions[ii, "source"], "Archive", sep="")))
			}
		}
	}
	# no hit was found...
	print(paste("NOT FOUND DEPENDENCY:", package))
	return(c(package, "-1", ""))
}

# get the urls of all packages that should be donwloaded
getDownloadURLs <- function(info, packageCRANInfo, cransList, archiveList) {
	if(is.null(info) || nrow(info) == 0) { return(NULL) }
	pb <- txtProgressBar(min = 0, max = nrow(info), style = 3)
	urls <- sapply(1:nrow(info), FUN = function(i) { x <- setTxtProgressBar(pb, i); getURL(info[i, ], packageCRANInfo, cransList, archiveList)}, simplify=TRUE)
	urls <- data.frame(matrix(unlist(urls), nrow=ncol(urls), byrow=T))
	colnames(urls) <- c("name", "url", "source")
	close(pb)
	return(urls)
}
#################################################################################
#################################################################################
# get package info from these repos
print("1) getting packages available from these repositories:")
x <- lapply(1:length(cransList), FUN = function(i) {print(paste(names(cransList[i]), ": ", cransList[[i]], sep=""))})
Sys.sleep(SLEEP_TIME)
packageCRANInfo <- getPackageInfoFromRepo(cransList)
print(paste(nrow(packageCRANInfo), " packages are available!", sep=""))

# get list of installed packages
print("2) getting installed packages from:")
print(.libPaths())
Sys.sleep(SLEEP_TIME)
pksInfo <- getInstalledPackages()
libFolders <- unique(pksInfo$LibPath)
print(paste(nrow(pksInfo), " packages are installed!", sep=""))

# parse required dependencies
depVersions <- getRequiredDependencies(pksInfo)
depVersions <- removeFulfilledDependencies(pksInfo, depVersions)
# remove the base packages and R dependency
before <- nrow(pksInfo)
depVersions <- depVersions[!depVersions$Package == "R", ]
pksInfo <- pksInfo[is.na(pksInfo$Priority) | pksInfo$Priority != "base", ]
pksInfo <- pksInfo[, c("Package", "Version")]
pksInfo <- unique(pksInfo)
print(paste("removed ", before-nrow(pksInfo), " base packages", sep=""))
if(nrow(depVersions) > 0) {
	print(paste("found ", nrow(depVersions), " dependencies that are not satisfied with the installed packages:", sep=""))
	print(depVersions) 
}

print("3) getting URLs of source packages... 1/2")
Sys.sleep(SLEEP_TIME)
# get the urls to the source packages that must be installed
urls <- getDownloadURLs(pksInfo, packageCRANInfo, cransList, archiveList)
print("3) getting URLs of source packages... 2/2")
urlsDep <- getDownloadURLs(depVersions, packageCRANInfo, cransList, archiveList)
# remove not found onces (might cause problems during install!)
urls <- rbind(urls, urlsDep)
urls <- urls[urls$url != "-1", ]
urls <- unique(urls)

# print stats
print(paste("4) downloading ", nrow(urls), " packages from these sources:", sep=""))
print(table(urls$source))
Sys.sleep(SLEEP_TIME)

# download and build local index
downloadAndIndex(urls, rVersion)

# save list of packages to install
save(file=URL_SAVE_PATH, urls)
print("5) FINISHED!")
###################### END ######################

