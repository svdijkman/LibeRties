#' Shared admin Shiny job push hub (one background worker per admin app process).
#' @keywords internal
.ls_admin_job_hub <- new.env(parent = emptyenv())

#' @keywords internal
.ls_admin_job_hub_init <- function() {
  hub <- .ls_admin_job_hub
  if (isTRUE(hub$initialized)) {
    return(invisible(hub))
  }
  hub$initialized <- TRUE
  hub$sessions <- list()
  hub$rev <- 0L
  hub$event_offset <- 0L
  hub$dir <- file.path(
    tempdir(),
    "LibeRties_admin_job_hub",
    paste0("pid", Sys.getpid())
  )
  dir.create(hub$dir, recursive = TRUE, showWarnings = FALSE)
  hub$subs_file <- file.path(hub$dir, "subscriptions.json")
  hub$events_file <- file.path(hub$dir, "events.jsonl")
  writeLines("{}", hub$subs_file)
  writeLines("", hub$events_file)
  invisible(hub)
}

#' @keywords internal
.ls_admin_jobs_watch_signature <- function() {
  root <- file.path(ls_sandbox_root(), "sandboxes")
  if (!dir.exists(root)) {
    return("0")
  }
  users <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  users <- users[nzchar(users)]
  if (length(users) == 0L) {
    return("0")
  }
  parts <- character()
  for (u in users) {
    uroot <- file.path(root, u, "jobs")
    if (!dir.exists(uroot)) {
      next
    }
    ids <- list.dirs(uroot, full.names = FALSE, recursive = FALSE)
    ids <- ids[nzchar(ids)]
    for (id in ids) {
      jp <- file.path(uroot, id)
      for (nm in c("meta.rds", "worker.log", "worker.progress", "worker.heartbeat",
                   "result.rds", "error.txt")) {
        fp <- file.path(jp, nm)
        if (file.exists(fp)) {
          info <- file.info(fp)
          parts <- c(parts, paste(u, id, nm, info$mtime, info$size, sep = ":"))
        }
      }
    }
  }
  if (length(parts) == 0L) {
    return("0")
  }
  paste(sort(parts), collapse = "|")
}

#' @keywords internal
.ls_admin_job_hub_poll_sec <- function() {
  as.numeric(getOption("LibeRties.admin_job_poll_sec", 2))[1L]
}

#' @keywords internal
.ls_admin_job_hub_write_subscriptions <- function() {
  hub <- .ls_admin_job_hub
  if (length(hub$sessions) == 0L) {
    writeLines("{}", hub$subs_file)
    return(invisible(0L))
  }
  subs <- lapply(names(hub$sessions), function(token) {
    list(token = token)
  })
  jsonlite::write_json(subs, hub$subs_file, auto_unbox = TRUE, pretty = FALSE)
  invisible(length(subs))
}

#' @keywords internal
.ls_admin_job_hub_subscribe <- function(session) {
  .ls_admin_job_hub_init()
  hub <- .ls_admin_job_hub
  token <- session$token
  hub$sessions[[token]] <- list(session = session)
  .ls_admin_job_hub_write_subscriptions()
  .ls_admin_job_hub_start_worker()
  .ls_admin_job_hub_start_tick()
  invisible(token)
}

#' Register an admin Shiny session with the shared job push hub.
#' @param session Shiny session.
#' @export
ls_admin_job_hub_register <- function(session) {
  .ls_admin_job_hub_subscribe(session)
  session$onSessionEnded(function() {
    .ls_admin_job_hub_unsubscribe(session$token)
  })
  invisible(session$token)
}

#' @keywords internal
.ls_admin_job_hub_unsubscribe <- function(token) {
  hub <- .ls_admin_job_hub
  hub$sessions[[token]] <- NULL
  .ls_admin_job_hub_write_subscriptions()
  if (length(hub$sessions) == 0L) {
    .ls_admin_job_hub_stop_worker()
  }
  invisible(TRUE)
}

#' @keywords internal
.ls_admin_job_hub_start_worker <- function() {
  hub <- .ls_admin_job_hub
  if (!is.null(hub$worker) && isTRUE(hub$worker$is_alive())) {
    return(invisible(hub$worker))
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    return(invisible(NULL))
  }
  hub$worker <- callr::r_bg(
    func = function(subs_file, events_file, poll_sec) {
      if (!requireNamespace("LibeRties", quietly = TRUE)) {
        stop("LibeRties required for admin job hub worker.", call. = FALSE)
      }
      suppressPackageStartupMessages(library(LibeRties))
      LibeRties:::.ls_admin_job_hub_worker_loop(subs_file, events_file, poll_sec)
    },
    args = list(
      subs_file = hub$subs_file,
      events_file = hub$events_file,
      poll_sec = .ls_admin_job_hub_poll_sec()
    ),
    libpath = .libPaths(),
    repos = getOption("repos"),
    stdout = if (.Platform$OS.type == "windows") "NUL" else "/dev/null",
    stderr = if (.Platform$OS.type == "windows") "NUL" else "/dev/null",
    supervise = TRUE
  )
  invisible(hub$worker)
}

#' @keywords internal
.ls_admin_job_hub_stop_worker <- function() {
  hub <- .ls_admin_job_hub
  if (!is.null(hub$worker)) {
    tryCatch(hub$worker$kill(), error = function(e) NULL)
    hub$worker <- NULL
  }
  hub$tick_active <- FALSE
  invisible(TRUE)
}

#' @keywords internal
.ls_admin_job_hub_worker_loop <- function(subs_file, events_file, poll_sec) {
  last_sig <- NULL
  repeat {
    if (!file.exists(subs_file)) {
      Sys.sleep(poll_sec)
      next
    }
    subs <- tryCatch(
      jsonlite::read_json(subs_file, simplifyVector = FALSE),
      error = function(e) list()
    )
    if (length(subs) == 0L) {
      Sys.sleep(poll_sec)
      next
    }
    sig <- .ls_admin_jobs_watch_signature()
    if (identical(last_sig, sig)) {
      Sys.sleep(poll_sec)
      next
    }
    last_sig <- sig
    hub_rev <- as.integer(Sys.time())
    for (sub in subs) {
      token <- sub$token %||% ""
      if (!nzchar(token)) {
        next
      }
      evt <- list(
        token = token,
        rev = hub_rev,
        sig = sig,
        time = as.character(Sys.time())
      )
      cat(jsonlite::toJSON(evt, auto_unbox = TRUE), "\n",
          file = events_file, append = TRUE)
    }
    Sys.sleep(poll_sec)
  }
}

#' @keywords internal
.ls_admin_job_hub_read_events <- function() {
  hub <- .ls_admin_job_hub
  if (!file.exists(hub$events_file)) {
    return(list())
  }
  lines <- readLines(hub$events_file, warn = FALSE)
  if (length(lines) == 0L) {
    return(list())
  }
  offset <- hub$event_offset %||% 0L
  if (offset >= length(lines)) {
    return(list())
  }
  new_lines <- lines[(offset + 1L):length(lines)]
  hub$event_offset <- length(lines)
  lapply(new_lines, function(ln) {
    if (!nzchar(trimws(ln))) {
      return(NULL)
    }
    tryCatch(jsonlite::fromJSON(ln, simplifyVector = TRUE), error = function(e) NULL)
  })
}

#' @keywords internal
.ls_admin_job_hub_flush <- function() {
  hub <- .ls_admin_job_hub
  events <- .ls_admin_job_hub_read_events()
  events <- events[!vapply(events, is.null, logical(1L))]
  if (length(events) == 0L) {
    return(invisible(0L))
  }
  n <- 0L
  for (evt in events) {
    token <- evt$token
    entry <- hub$sessions[[token]]
    if (is.null(entry) || is.null(entry$session)) {
      next
    }
    hub$rev <- hub$rev + 1L
    n <- n + 1L
    tryCatch(
      entry$session$sendCustomMessage(
        "liberties_job_push",
        list(
          rev = hub$rev,
          sig = evt$sig %||% "",
          time = evt$time %||% ""
        )
      ),
      error = function(e) NULL
    )
  }
  invisible(n)
}

#' @keywords internal
.ls_admin_job_hub_start_tick <- function() {
  hub <- .ls_admin_job_hub
  if (isTRUE(hub$tick_active)) {
    return(invisible(FALSE))
  }
  hub$tick_active <- TRUE
  tick <- function() {
    if (length(hub$sessions) == 0L) {
      hub$tick_active <- FALSE
      .ls_admin_job_hub_stop_worker()
      return(invisible(NULL))
    }
    if (!is.null(hub$worker) && !isTRUE(hub$worker$is_alive())) {
      hub$worker <- NULL
      .ls_admin_job_hub_start_worker()
    }
    .ls_admin_job_hub_flush()
    if (requireNamespace("later", quietly = TRUE)) {
      later::later(tick, delay = 0.4)
    }
    invisible(NULL)
  }
  if (requireNamespace("later", quietly = TRUE)) {
    later::later(tick, delay = 0.4)
  } else {
    hub$tick_active <- FALSE
  }
  invisible(TRUE)
}
