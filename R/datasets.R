#' @keywords internal
.ls_datasets_catalog_path <- function() {
  .ls_path("datasets", "catalog.json")
}

#' @keywords internal
.ls_datasets_files_dir <- function() {
  .ls_path("datasets", "files")
}

#' @keywords internal
.ls_datasets_load <- function() {
  .ls_read_json(.ls_datasets_catalog_path(), list())
}

#' @keywords internal
.ls_datasets_save <- function(catalog) {
  .ls_write_json(catalog, .ls_datasets_catalog_path())
}

#' @keywords internal
.ls_file_md5 <- function(path) {
  as.character(tools::md5sum(path))
}

#' Can `username` access this catalog entry?
#'
#' Access rule (multi-tenant isolation): an entry is accessible to a user when
#' it is public, has no owner/grants (legacy shared reference data), is owned by
#' the user, or explicitly grants the user. `username = NULL` (admin) sees all.
#' @keywords internal
.ls_dataset_can_access <- function(entry, username) {
  if (is.null(username)) {
    return(TRUE)
  }
  if (isTRUE(entry$public)) {
    return(TRUE)
  }
  owner <- as.character(entry$owner %||% "")
  allowed <- as.character(entry$allowed_users %||% character(0))
  if (!nzchar(owner) && length(allowed) == 0L) {
    # Legacy datasets without ownership metadata are admin-only (not global).
    return(FALSE)
  }
  identical(owner, username) || username %in% allowed
}

#' Register a dataset on the cluster (admin)
#'
#' @param dataset_id Short identifier.
#' @param file_path Path to an RDS data file.
#' @param label Optional description.
#' @param owner Optional owning username (restricts access to owner + grants).
#' @param allowed_users Optional character vector of additional usernames.
#' @param public If TRUE, the dataset is visible to all users.
#' @keywords internal
.ls_dataset_register <- function(dataset_id, file_path, label = NULL,
                                 owner = NULL, allowed_users = NULL,
                                 public = FALSE) {
  .ls_init_storage()
  dataset_id <- gsub("[^a-zA-Z0-9._-]", "", dataset_id)
  if (!nzchar(dataset_id)) {
    stop("Invalid dataset_id.", call. = FALSE)
  }
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path, call. = FALSE)
  }
  dest <- file.path(.ls_datasets_files_dir(), paste0(dataset_id, ".rds"))
  file.copy(file_path, dest, overwrite = TRUE)
  md5 <- .ls_file_md5(dest)
  catalog <- .ls_datasets_load()
  catalog[[dataset_id]] <- list(
    id = dataset_id,
    label = label %||% dataset_id,
    file = basename(dest),
    md5 = md5,
    size_bytes = file.info(dest)$size,
    owner = as.character(owner %||% ""),
    allowed_users = as.character(allowed_users %||% character(0)),
    public = isTRUE(public),
    updated = .ls_now()
  )
  .ls_datasets_save(catalog)
  catalog[[dataset_id]]
}

#' List registered datasets visible to a user (NULL = admin, sees all)
#' @keywords internal
.ls_dataset_list <- function(username = NULL) {
  catalog <- .ls_datasets_load()
  if (length(catalog) > 0L && !is.null(username)) {
    keep <- vapply(catalog, function(x) .ls_dataset_can_access(x, username), logical(1L))
    catalog <- catalog[keep]
  }
  if (length(catalog) == 0L) {
    return(data.frame(
      id = character(),
      label = character(),
      md5 = character(),
      size_bytes = numeric(),
      owner = character(),
      updated = character(),
      stringsAsFactors = FALSE
    ))
  }
  ids <- names(catalog)
  data.frame(
    id = ids,
    label = vapply(catalog, function(x) as.character(x$label %||% x$id), character(1L)),
    md5 = vapply(catalog, function(x) as.character(x$md5 %||% ""), character(1L)),
    size_bytes = vapply(catalog, function(x) as.numeric(x$size_bytes %||% 0), numeric(1L)),
    owner = vapply(catalog, function(x) as.character(x$owner %||% ""), character(1L)),
    updated = vapply(catalog, function(x) as.character(x$updated %||% ""), character(1L)),
    stringsAsFactors = FALSE
  )
}

#' Resolve dataset path and verify MD5
#'
#' @param dataset_id Dataset id.
#' @param expected_md5 Client-supplied MD5; job fails on mismatch.
#' @param username Requesting user; enforces access scoping (NULL = no check).
#' @return Path to dataset file.
#' @keywords internal
.ls_dataset_resolve <- function(dataset_id, expected_md5 = NULL, username = NULL) {
  catalog <- .ls_datasets_load()
  entry <- catalog[[dataset_id]]
  if (is.null(entry)) {
    stop("Unknown dataset: ", dataset_id, call. = FALSE)
  }
  if (!.ls_dataset_can_access(entry, username)) {
    # Do not reveal existence of datasets the caller may not access.
    stop("Unknown dataset: ", dataset_id, call. = FALSE)
  }
  path <- file.path(.ls_datasets_files_dir(), entry$file)
  if (!file.exists(path)) {
    stop("Dataset file missing on server: ", dataset_id, call. = FALSE)
  }
  actual <- .ls_file_md5(path)
  if (!is.null(expected_md5) && nzchar(expected_md5) &&
      !identical(tolower(actual), tolower(as.character(expected_md5)))) {
    stop(
      "Dataset MD5 mismatch for ", dataset_id,
      " (expected ", expected_md5, ", got ", actual, ").",
      call. = FALSE
    )
  }
  if (!identical(actual, entry$md5)) {
    stop(
      "Dataset catalog MD5 stale for ", dataset_id,
      " — re-register the dataset.",
      call. = FALSE
    )
  }
  path
}
