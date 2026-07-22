.ls_audit_path <- function(root) {
  file.path(.ls_ensure_dir(file.path(root, "server")), "audit.rds")
}

.ls_audit_hash <- function(value) {
  unname(paste0(openssl::sha256(serialize(value, NULL, version = 3L))))
}

.ls_audit_append <- function(root, action, username = "", details = list()) {
  path <- .ls_audit_path(root)
  lock <- paste0(path, ".lock")
  started <- proc.time()[["elapsed"]]
  repeat {
    if (dir.create(lock, showWarnings = FALSE)) break
    if (proc.time()[["elapsed"]] - started > 5) .ls_stop("Timed out acquiring audit lock.")
    Sys.sleep(0.01)
  }
  on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)
  audit <- if (file.exists(path)) .ls_read_rds(path) else list(
    schema = "liberties.audit", version = 1L, events = list()
  )
  previous <- if (length(audit$events)) audit$events[[length(audit$events)]]$hash else "GENESIS"
  event <- list(
    id = .ls_new_id(), at = .ls_now(), action = as.character(action),
    username = as.character(username %||% ""), details = details,
    previous = previous
  )
  event$hash <- .ls_audit_hash(event)
  audit$events[[length(audit$events) + 1L]] <- event
  .ls_atomic_save_rds(audit, path)
  invisible(event)
}

#' Read and verify the LibeRties administration audit chain
#'
#' @param root LibeRties server root.
#' @return A data frame with a logical `valid` attribute.
#' @export
ls_audit_read <- function(root = .ls_default_root()) {
  path <- .ls_audit_path(root)
  if (!file.exists(path)) return(structure(data.frame(), valid = TRUE))
  audit <- .ls_read_rds(path)
  valid <- is.list(audit) && identical(audit$schema %||% "", "liberties.audit")
  previous <- "GENESIS"
  if (valid) {
    for (stored in audit$events) {
      supplied <- stored$hash
      event <- stored
      event$hash <- NULL
      valid <- valid && identical(event$previous, previous) &&
        identical(supplied, .ls_audit_hash(event))
      previous <- supplied
    }
  }
  rows <- if (!length(audit$events %||% list())) data.frame() else do.call(rbind, lapply(
    audit$events, function(event) data.frame(
      id = event$id, at = event$at, action = event$action,
      username = event$username, hash = event$hash, stringsAsFactors = FALSE
    )
  ))
  structure(rows, valid = valid)
}
