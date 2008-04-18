if (!is.R()) {
	## install required package RSQLite (and DBI) from CSAN if necessary
	
	# create a temporary installation directory for install.packages() calls.
	# setting destdir explicitly in install.packages() avoids a message printed
	# to the command line defining where the temporary installation took place.
	tmpdir <- file.path(tempfile("dir"), "downloaded_packages")
	if(!file.exists(tmpdir) && !dir.create(tmpdir, recursive=T))
	  stop(sprintf("Unable to create temporary directory '%s'", tmpdir))
	
	"quietRequire" <- function(package)
	{
	  if (!is.character(package) || length(package) > 1)
	    stop("package must be a scalar character string")
	  package <- as.character(substitute(package))
	  val <- try(library(package, character.only=T))
	  return(!inherits(val, "Error"))
	}
	
	# pkgutils
	pkgutils.pos <- attached.where("pkgutils", nomatch=0)
	if (!pkgutils.pos) {
	  dscfile <- system.file(package="pkgutils", "DESCRIPTION")
	  if (!file.exists(dscfile)) {
	    cat("Updating required library: pkgutils\n")
	    install.pkgutils(update=T)
	  }
	
	  cat("Loading required library: pkgutils.\n")
	  library("pkgutils")
	}
	
	# DBI
	DBI.pos <- attached.where("DBI", nomatch=0)
	if (!DBI.pos) {
	  if (!quietRequire("DBI")) {
	    cat("Installing required library: DBI.\n")
	    install.packages("DBI", lib=.libPaths()[1], destdir=tmpdir)
	  }
	
	  cat("Loading required library: DBI.\n")
	  library("DBI")
	}
	
	# RSQLite
	RSQLite.pos <- attached.where("RSQLite", nomatch=0)
	if (!RSQLite.pos) {
	  if (!quietRequire("RSQLite")) {
	    cat("Installing required library: RSQLite.\n")
	    install.packages("RSQLite", lib=.libPaths()[1], destdir=tmpdir)
	  }
	
	  cat("Loading required library: RSQLite.\n")
	  library("RSQLite")
	}
}

if(!require(RSQLite)) stop("RSQLite is required but not available")
dbname <- ifelse(is.R(), paste(system.file("data", package="msDilution"), "Dilution2005Raw.db", sep="/"), "Dilution2005Raw.db")	
con <- dbConnect(drv = dbDriver("SQLite"), dbname = dbname)
mz <- dbReadTable(con, "mz", row.names="row_names", check.names=FALSE)
intensity <- dbReadTable(con, "intensity", row.names="row_names", check.names=FALSE)
type <- dbReadTable(con, "type", row.names="row_names", check.names=FALSE)

# restore NA's in dir.org
dir.org <- dbReadTable(con, "dir_org_na", row.names="row_names", check.names=FALSE)
dir.org[280, c("Conc", "Replicate", "Manual")] <- NA
colnames(dir.org)[1] <- "Index" # was changed to "Index_1"  when written to db?

cyt.ind <- dbReadTable(con, "cyt_ind", row.names="row_names", check.names=FALSE)
pep.ind <- dbReadTable(con, "pep_ind", row.names="row_names", check.names=FALSE)
ser.ind <- dbReadTable(con, "ser_ind", row.names="row_names", check.names=FALSE)
mix.ind <- dbReadTable(con, "mix_ind", row.names="row_names", check.names=FALSE)
err.ind <- dbReadTable(con, "err_ind", row.names="row_names", check.names=FALSE)

coding <- list(dir.org=dir.org, cyt.ind=cyt.ind[,1], pep.ind=pep.ind[,1],
	ser.ind=ser.ind[,1], mix.ind=mix.ind[,1], err.ind=err.ind[,1])

Dilution2005Raw <- list(mz=mz[,1], intensity=as.matrix(intensity), 
	type=as.factor(type[,1]), coding=coding)
attr(Dilution2005Raw, "class") <- "msSet"
attr(Dilution2005Raw, "data.name") <- "Dilution 2005 Raw"
dbDisconnect(con)
rm(dbname, con, mz, intensity, type, dir.org,
	cyt.ind, pep.ind, ser.ind, mix.ind, err.ind, coding)