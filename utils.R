verify_fits_file <- function(fits) {
  stopifnot(!is.null(fits))
  
  # List
  stopifnot(is.list(fits))
  
  # At least one:
  stopifnot(length(fits) >= 1L)
  
  # All disclapmixfit object
  stopifnot(isTRUE(all(sapply(fits, inherits, "disclapmixfit"))))
  
  # Same loci in all fits:
  loci <- colnames(fits[[1L]]$y)
  stopifnot(isTRUE(all(sapply(fits, function(x) isTRUE(all.equal(colnames(x$y), loci))))))
  
  # On same number of observations
  nobs <- fits[[1L]]$model_observations
  stopifnot(isTRUE(all(sapply(fits, function(x) isTRUE(all.equal(nobs, x$model_observations))))))
  
  # Different number of clusters
  clus <- sapply(fits, function(x) nrow(x$y))
  stopifnot(length(clus) == length(unique(clus)))
}

# Assumes fits passed verify_fits_file
verify_process_profiles_file <- function(fits, profiles) {
  
  stopifnot(!is.null(profiles))
  stopifnot(inherits(profiles, "data.frame"))

  # OK from verify_fits_file()
  fits_loci <- colnames(fits[[1L]]$y)
  
  profiles_columns <- colnames(profiles)
  profile_names <- setdiff(profiles_columns, fits_loci)
  if (length(profile_names) == 0L) {
    profile_names <- NULL
  }
  if (length(profile_names) >= 2L) {
    stop("Too many columns besides loci known from fits object (too many potential sample names)")
  }
  
  missing_columns <- setdiff(fits_loci, profiles_columns)
  if (length(missing_columns) > 0L) {
    stop("Loci missing: ", paste0(missing_columns, collapse = ", "))
  }
  
  # Reorder data
  new_profiles <- as.matrix(profiles[, fits_loci, drop = FALSE])
  
  new_profile_names <- NULL
  if (!is.null(profile_names)) {
    new_profile_names <- profiles[, profile_names]
  }
  
  res <- list(
    profiles = new_profiles,
    profile_names = new_profile_names
  )
  
  return(res)
}


