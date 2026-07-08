#' @keywords internal
.ls_admin_job_key <- function(user, id) {
  paste0(as.character(user), "::", as.character(id))
}

#' @keywords internal
.ls_admin_parse_job_key <- function(key) {
  parts <- strsplit(as.character(key), "::", fixed = TRUE)[[1L]]
  if (length(parts) < 2L) {
    return(NULL)
  }
  list(user = parts[[1L]], id = paste(parts[-1L], collapse = "::"))
}

#' @keywords internal
.ls_admin_format_job_time <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return("")
  }
  txt <- as.character(x)[1L]
  if (is.na(txt) || !nzchar(txt)) {
    return("")
  }
  pt <- suppressWarnings(as.POSIXct(txt))
  if (is.na(pt)) {
    return(txt)
  }
  format(pt, "%Y-%m-%d %H:%M:%S")
}

#' @keywords internal
.ls_admin_fmt_duration <- function(secs) {
  secs <- as.numeric(secs)
  if (!is.finite(secs) || secs < 0) {
    return("")
  }
  if (secs < 60) {
    return(paste0(round(secs, 1), " s"))
  }
  if (secs < 3600) {
    return(paste0(round(secs / 60, 1), " min"))
  }
  paste0(round(secs / 3600, 2), " h")
}

#' @keywords internal
.ls_admin_job_status_label <- function(status) {
  .ls_job_status_label(status)
}

#' @keywords internal
.ls_admin_job_duration_label <- function(started, finished, status = NULL) {
  if (is.null(started) || !nzchar(as.character(started)[1L])) {
    return("")
  }
  t0 <- suppressWarnings(as.POSIXct(started))
  if (is.na(t0)) {
    return("")
  }
  running <- identical(status, "running") || identical(status, "queued")
  t1 <- if (!is.null(finished) && nzchar(as.character(finished)[1L])) {
    suppressWarnings(as.POSIXct(finished))
  } else if (running) {
    Sys.time()
  } else {
    NA
  }
  if (is.na(t1)) {
    return("")
  }
  .ls_admin_fmt_duration(as.numeric(difftime(t1, t0, units = "secs")))
}

#' @keywords internal
.ls_admin_job_error_text <- function(user, id, err = "") {
  err <- as.character(err %||% "")
  if (nzchar(trimws(err))) {
    return(err)
  }
  job_path <- .ls_job_path(user, id)
  err_path <- file.path(job_path, "error.txt")
  if (file.exists(err_path)) {
    err <- paste(readLines(err_path, warn = FALSE), collapse = "\n")
    if (nzchar(trimws(err))) {
      return(err)
    }
  }
  log_path <- file.path(job_path, "worker.log")
  snippet <- .ls_worker_log_snippet(log_path)
  if (nzchar(snippet)) {
    return(snippet)
  }
  ""
}

#' Admin job tree list (matches LibeRation client Jobs tab style)
#' @keywords internal
.ls_admin_jobs_tree_ui <- function(df, selected_key, expanded_keys) {
  if (nrow(df) == 0L) {
    return(tags$p(
      class = "text-muted",
      style = "font-size: 11px; padding: 6px;",
      "No jobs yet."
    ))
  }
  if (!"error" %in% names(df)) {
    df$error <- ""
  }
  rows <- lapply(seq_len(nrow(df)), function(i) {
    jid <- df$id[[i]]
    user <- df$user[[i]]
    key <- .ls_admin_job_key(user, jid)
    status <- df$status[[i]]
    can_expand <- status %in% c("success", "error")
    expanded <- key %in% expanded_keys
    arrow <- if (can_expand) {
      if (expanded) "\u25BC" else "\u25B6"
    } else {
      ""
    }
    sel <- identical(key, selected_key)
    label <- as.character(df$label[[i]] %||% jid)
    if (is.na(label) || !nzchar(label)) {
      label <- jid
    }
    type_lbl <- if (identical(df$job_type[[i]], "sim")) "simulation" else "estimation"
    meta <- paste0("user: ", user, " | ", as.character(df$method[[i]] %||% ""))
    started <- .ls_admin_format_job_time(df$started[[i]])
    finished <- .ls_admin_format_job_time(df$finished[[i]])
    duration_lbl <- .ls_admin_job_duration_label(df$started[[i]], df$finished[[i]], status)
    detail <- if (can_expand && expanded) {
      if (identical(status, "error")) {
        err_txt <- .ls_admin_job_error_text(user, jid, df$error[[i]])
        if (!nzchar(trimws(err_txt))) {
          err_txt <- "Job failed (see worker log)."
        }
        tags$div(
          class = "job-detail-panel job-detail-error",
          tags$p(
            style = "font-size: 11px; margin: 4px 0 6px; color: #c0392b;",
            tags$strong("Error:"),
            htmltools::htmlEscape(err_txt)
          )
        )
      } else {
        obj <- df$objective[[i]]
        tags$div(
          class = "job-detail-panel",
          tags$p(
            style = "font-size: 11px; margin: 4px 0 6px;",
            tags$strong("Objective:"),
            if (!is.null(obj) && is.finite(obj)) round(obj, 4) else "—"
          )
        )
      }
    } else {
      NULL
    }
    tagList(
      tags$div(
        class = paste(
          "job-row", paste0("job-row-", status),
          if (sel) "selected" else NULL
        ),
        `data-job` = key,
        if (can_expand) {
          tags$span(class = "job-toggle", `data-job` = key, arrow)
        } else {
          tags$span(class = "job-toggle-spacer")
        },
        tags$span(class = "job-id", jid),
        tags$span(class = "job-label", label),
        tags$span(
          class = paste("job-status", paste0("job-status-", status)),
          .ls_admin_job_status_label(status)
        ),
        tags$span(class = "job-type", type_lbl),
        tags$span(class = "job-meta", meta),
        tags$span(
          class = "job-time",
          paste0(
            if (nzchar(started)) paste0("started: ", started) else "",
            if (nzchar(duration_lbl)) {
              paste0(if (nzchar(started)) " \u00b7 " else "", duration_lbl)
            } else if (nzchar(finished)) {
              paste0(if (nzchar(started)) " \u00b7 finished: " else "finished: ", finished)
            } else {
              ""
            }
          )
        )
      ),
      if (!is.null(detail)) detail
    )
  })
  tags$div(class = "version-tree job-tree", rows)
}

#' CSS for admin job tree (shared look with LibeRation client)
#' @keywords internal
.ls_admin_jobs_tree_css <- function() {
  tags$style(HTML("
    .job-tree { max-height: 420px; overflow: auto; margin-top: 6px; }
    .job-row { display: flex; align-items: center; gap: 8px; padding: 4px 6px; border-radius: 3px; cursor: pointer; font-size: 11px; }
    .job-row:hover { background: #f5f8fb; }
    .job-row.selected { background: #e8f0fe; }
    .job-toggle, .job-toggle-spacer { width: 14px; flex: 0 0 14px; color: #666; user-select: none; }
    .job-toggle { cursor: pointer; }
    .job-id { font-weight: 600; color: #333; min-width: 120px; }
    .job-label { color: #444; flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
    .job-row-error { background-color: #fdecea !important; }
    .job-status { font-size: 10px; text-transform: uppercase; padding: 1px 5px; border-radius: 3px; background: #eee; color: #555; }
    .job-status-success { background: #d4edda; color: #155724; }
    .job-status-error { background: #f8d7da; color: #721c24; }
    .job-status-running { background: #cce5ff; color: #004085; }
    .job-status-queued { background: #e2e3e5; color: #383d41; }
    .job-status-cancelled { background: #fff3cd; color: #856404; }
    .job-type, .job-meta { color: #666; font-size: 10px; white-space: nowrap; }
    .job-meta { max-width: 220px; overflow: hidden; text-overflow: ellipsis; }
    .job-time { color: #999; font-size: 10px; white-space: nowrap; margin-left: auto; }
    .job-detail-panel { margin: 0 0 6px 20px; padding: 6px 8px; border-left: 2px solid #dce6f2; background: #fafcff; }
    .job-detail-error { border-left-color: #f5c6cb; background: #fdf2f2; }
  "))
}

#' JavaScript handlers for admin job tree clicks
#' @keywords internal
.ls_admin_jobs_tree_js <- function() {
  tags$script(HTML("
    $(document).on('click', '.job-toggle', function(e) {
      e.stopPropagation();
      Shiny.setInputValue('admin_job_tree_event', {
        action: 'toggle',
        job: $(this).data('job'),
        ts: new Date().getTime()
      }, {priority: 'event'});
    });
    $(document).on('click', '.job-row', function(e) {
      if ($(e.target).hasClass('job-toggle')) { return; }
      Shiny.setInputValue('admin_job_tree_event', {
        action: 'select',
        job: $(this).data('job'),
        ts: new Date().getTime()
      }, {priority: 'event'});
    });
  "))
}
