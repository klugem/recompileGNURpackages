GNU R scripts to create a local source repository for all installed packages.
Please note that the versions of the packages are preserved! If you don't care about your installed versions, just run 

```
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager") 
pkgs <- rownames(installed.packages()) 
BiocManager::install(pkgs, type = "source", checkBuilt = TRUE)
```

#### 1) get scripts
```
SCR_PATH="/tmp/recompileGNURpackages/"
git clone https://github.com/klugem/recompileGNURpackages.git "$SCR_PATH"
SCR_PATH="$SCR_PATH/src/"
```

#### 1) create local source repository
```
PACKAGE_DIR="/tmp/packagesR/"
Rscript "$SCR_PATH/buildLocalRepo.R" -r "$PACKAGE_DIR"`
```
packages are obtained by default from
- https://cran.r-project.org/src/contrib/
- https://bioconductor.org/packages/$bioconducterVersion/bioc/src/contrib/
- https://bioconductor.org/packages/$bioconducterVersion/data/annotation/src/contrib/
- and archives of these repositories

#### 2) install packages from local source repository
```
Rscript "$SCR_PATH/installLocalRepo.R" -r "$PACKAGE_DIR" -t threads -i /path/to/new/library_dir
```

#### 3) optional: replace libraries in another dir - use with care!
```
NEW_DIR="/path/to/new/library_dir/"
OLD_DIR="/path/to/old/library_dir/"
ls "$NEW_DIR" | grep -vP "^\\." | xargs -i rm -rf "${OLD_DIR}/{}"
ls "$NEW_DIR" | grep -vP "^\\." | xargs -i mv "$NEW_DIR/{}" "${OLD_DIR}/."
```
