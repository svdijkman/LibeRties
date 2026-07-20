.ls_admin_asset_data <- function(path) {
  file <- system.file("admin-assets", path, package = "LibeRties")
  if (!nzchar(file) || !file.exists(file)) return("")
  paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

.ls_admin_jobs <- function(root, users = ls_user_list(root)) {
  if (!nrow(users)) return(.ls_empty_jobs())
  records <- lapply(seq_len(nrow(users)), function(index) {
    limits <- as.list(users[index, c(
      "max_concurrent_jobs", "max_queued_jobs", "max_payload_mb", "max_result_mb",
      "max_storage_mb", "max_runtime_seconds", "max_cpu_seconds", "max_memory_mb"
    ), drop = FALSE])
    LibeRQueue$new(
      root, users$username[[index]], max_workers = 1L, limits = limits
    )$list()
  })
  records <- Filter(nrow, records)
  if (!length(records)) return(.ls_empty_jobs())
  do.call(rbind, records)
}

.ls_admin_limit_names <- c(
  "max_concurrent_jobs", "max_queued_jobs", "max_payload_mb", "max_result_mb",
  "max_storage_mb", "max_runtime_seconds", "max_cpu_seconds", "max_memory_mb"
)

.ls_admin_user_limits <- function(row) {
  as.list(row[1L, .ls_admin_limit_names, drop = FALSE])
}

.ls_admin_pick_js <- function(input_id, value) {
  encoded <- jsonlite::toJSON(as.character(value), auto_unbox = TRUE)
  sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'});", input_id, encoded)
}

.ls_admin_limits_from_input <- function(input, prefix = "new_") {
  list(
    max_concurrent_jobs = as.integer(input[[paste0(prefix, "workers")]]),
    max_queued_jobs = as.integer(input[[paste0(prefix, "queued")]]),
    max_payload_mb = as.numeric(input[[paste0(prefix, "payload")]]),
    max_result_mb = as.numeric(input[[paste0(prefix, "result")]]),
    max_storage_mb = as.numeric(input[[paste0(prefix, "storage")]]),
    max_runtime_seconds = as.numeric(input[[paste0(prefix, "runtime")]]),
    max_cpu_seconds = as.numeric(input[[paste0(prefix, "cpu")]]),
    max_memory_mb = as.numeric(input[[paste0(prefix, "memory")]])
  )
}

.ls_admin_limit_fields <- function(prefix = "new_", limits = .ls_default_limits()) {
  list(
    shiny::numericInput(paste0(prefix, "workers"), "Concurrent workers", limits$max_concurrent_jobs, min = 1),
    shiny::numericInput(paste0(prefix, "queued"), "Queued jobs", limits$max_queued_jobs, min = 1),
    shiny::numericInput(paste0(prefix, "payload"), "Payload limit (MB)", limits$max_payload_mb, min = 1),
    shiny::numericInput(paste0(prefix, "result"), "Result limit (MB)", limits$max_result_mb, min = 1),
    shiny::numericInput(paste0(prefix, "storage"), "Storage quota (MB)", limits$max_storage_mb, min = 1),
    shiny::numericInput(paste0(prefix, "runtime"), "Runtime limit (seconds)", limits$max_runtime_seconds, min = 1),
    shiny::numericInput(paste0(prefix, "cpu"), "CPU limit (seconds)", limits$max_cpu_seconds, min = 1),
    shiny::numericInput(paste0(prefix, "memory"), "Memory limit (MB)", limits$max_memory_mb, min = 1)
  )
}

#' Create the LibeRties server-administration application
#'
#' The browser session is protected by an administrator token whose digest is
#' held only in the app process. It is deliberately separate from tenant API
#' tokens. Use a long random value supplied through `LIBERTIES_ADMIN_TOKEN`.
#'
#' @param root LibeRties server storage root.
#' @param admin_token Administrator login token. Defaults to the
#'   `LIBERTIES_ADMIN_TOKEN` environment variable.
#' @return A Shiny application object.
#' @export
ls_admin_gui <- function(root = .ls_default_root(),
                         admin_token = Sys.getenv("LIBERTIES_ADMIN_TOKEN")) {
  root <- .ls_ensure_dir(root)
  admin_token <- as.character(admin_token)
  if (length(admin_token) != 1L || is.na(admin_token) || nchar(admin_token) < 16L) {
    .ls_stop("`admin_token` must contain at least 16 characters.")
  }
  admin_hash <- .ls_token_hash(admin_token)
  rm(admin_token)
  favicon <- .ls_admin_asset_data("favicon.svg")
  favicon_href <- if (nzchar(favicon)) {
    paste0("data:image/svg+xml,", utils::URLencode(favicon, reserved = TRUE))
  } else ""
  css <- .ls_admin_asset_data("admin.css")

  login_ui <- shiny::div(
    class = "la-login-shell",
    shiny::div(
      class = "la-login-card",
      shiny::div(class = "la-mark", shiny::HTML(favicon)),
      shiny::h1("LibeRties administration"),
      shiny::p("Manage isolated users, resource limits and execution jobs."),
      shiny::div(class = "la-notice-host", shiny::uiOutput("notice")),
      shiny::passwordInput("admin_token", "Administrator token"),
      shiny::actionButton("login", "Sign in", class = "btn la-primary")
    )
  )

  admin_ui <- shiny::div(
    class = "la-app",
    shiny::tags$header(
      class = "la-header",
      shiny::div(class = "la-brand", shiny::HTML(favicon),
                 shiny::div(shiny::strong("LibeRties"), shiny::span("Server administration"))),
      shiny::div(class = "la-header-meta", shiny::span(normalizePath(root, winslash = "/")),
                 shiny::tags$label(
                   class = "la-theme-toggle", title = "Toggle light and dark theme",
                   shiny::span(class = "la-theme-label", "Light"),
                   shiny::tags$input(
                     type = "checkbox", class = "la-theme-checkbox",
                     onchange = "window.liberAdminSetTheme(this.checked);"
                   ),
                   shiny::tags$i()
                 ),
                 shiny::actionButton("logout", "Sign out", class = "btn la-quiet"))
    ),
    shiny::div(class = "la-message-host", shiny::uiOutput("notice")),
    shiny::tabsetPanel(
      id = "admin_section", type = "tabs",
      shiny::tabPanel(
        "Users",
        shiny::div(
          class = "la-grid la-grid-users",
          shiny::tags$section(
            class = "la-panel",
            shiny::div(class = "la-panel-title", shiny::h2("Server users"),
                       shiny::actionButton("refresh_users", "Refresh", class = "btn la-quiet")),
            shiny::textInput(
              "user_search", "Search users", placeholder = "Username, first name or last name"
            ),
            shiny::uiOutput("users_list")
          ),
          shiny::tags$section(
            class = "la-panel",
            shiny::h2("Create user"),
            shiny::textInput("new_username", "Username"),
            shiny::div(
              class = "la-name-grid",
              shiny::textInput("new_first_name", "First name"),
              shiny::textInput("new_last_name", "Last name")
            ),
            shiny::checkboxInput("new_enabled", "Account enabled", TRUE),
            shiny::div(class = "la-limit-grid", .ls_admin_limit_fields("new_")),
            shiny::actionButton("create_user", "Create user", class = "btn la-primary")
          ),
          shiny::tags$section(
            class = "la-panel",
            shiny::div(class = "la-panel-title", shiny::h2("Selected-user settings"),
                       shiny::uiOutput("selected_user_label")),
            shiny::div(
              class = "la-name-grid",
              shiny::textInput("edit_first_name", "First name"),
              shiny::textInput("edit_last_name", "Last name")
            ),
            shiny::checkboxInput("edit_enabled", "Account enabled", TRUE),
            shiny::div(class = "la-limit-grid", .ls_admin_limit_fields("edit_")),
            shiny::div(
              class = "la-actions",
              shiny::actionButton("update_user", "Save settings", class = "btn la-primary"),
              shiny::actionButton("rotate_token", "Rotate token", class = "btn la-secondary")
            ),
            shiny::checkboxInput("confirm_delete_user", "I understand that user storage will be deleted", FALSE),
            shiny::actionButton("delete_user", "Delete user", class = "btn la-danger")
          )
        )
      ),
      shiny::tabPanel(
        "Jobs",
        shiny::div(
          class = "la-grid la-grid-jobs",
          shiny::tags$section(
            class = "la-panel",
            shiny::div(class = "la-panel-title", shiny::h2("Execution jobs"),
                       shiny::actionButton("refresh_jobs", "Refresh", class = "btn la-quiet")),
            shiny::uiOutput("jobs_list")
          ),
          shiny::tags$section(
            class = "la-panel",
            shiny::div(class = "la-panel-title", shiny::h2("Job details"),
                       shiny::uiOutput("selected_job_label")),
            shiny::div(class = "la-actions",
                       shiny::actionButton("cancel_job", "Cancel job", class = "btn la-danger")),
            shiny::h3("Worker output"),
            shiny::verbatimTextOutput("job_logs", placeholder = TRUE)
          )
        )
      ),
      shiny::tabPanel(
        "Server",
        shiny::div(class = "la-grid la-grid-server",
                   shiny::tags$section(class = "la-panel", shiny::h2("Runtime"), shiny::tableOutput("runtime")),
                   shiny::tags$section(class = "la-panel", shiny::h2("Security boundary"),
                                  shiny::p("Each tenant has a separate filesystem namespace and bearer token. Worker processes run with restricted inherited environment variables and enforced resource limits."),
                                  shiny::p("The administrator credential is kept as a one-way digest in this application process and is not written to the server registry.")))
      )
    )
  )

  ui <- shiny::fluidPage(
    class = "la-page",
    shiny::tags$head(
      shiny::tags$title("LibeRties"),
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      if (nzchar(favicon_href)) shiny::tags$link(rel = "icon", type = "image/svg+xml", href = favicon_href),
      shiny::tags$script(shiny::HTML(
        "(function(){\n  var dark=false;\n  function apply(){\n    document.body.classList.toggle('la-theme-dark',dark);\n    document.querySelectorAll('.la-theme-checkbox').forEach(function(node){node.checked=dark;});\n    var label=dark?'Dark':'Light';\n    document.querySelectorAll('.la-theme-label').forEach(function(node){if(node.textContent!==label)node.textContent=label;});\n  }\n  window.liberAdminSetTheme=function(value){dark=!!value;try{localStorage.setItem('libertiesDarkTheme',dark?'1':'0');}catch(e){}apply();};\n  function restore(){try{dark=localStorage.getItem('libertiesDarkTheme')==='1';}catch(e){dark=false;}apply();}\n  document.addEventListener('DOMContentLoaded',restore);\n  new MutationObserver(apply).observe(document.documentElement,{childList:true,subtree:true});\n})();"
      )),
      shiny::tags$style(shiny::HTML(css))
    ),
    shiny::uiOutput("gate")
  )

  server <- function(input, output, session) {
    initial_users <- ls_user_list(root)
    authenticated <- shiny::reactiveVal(FALSE)
    notice <- shiny::reactiveVal(NULL)
    users <- shiny::reactiveVal(initial_users)
    jobs <- shiny::reactiveVal(.ls_empty_jobs())
    selected_user <- shiny::reactiveVal("")
    selected_job <- shiny::reactiveVal("")
    logs <- shiny::reactiveVal("Select a job to view its worker log.")

    set_notice <- function(message, level = "info") {
      notice(list(message = as.character(message), level = level))
    }
    normalize_user_selection <- function(preferred = NULL) {
      if (is.null(preferred)) preferred <- shiny::isolate(selected_user())
      frame <- users()
      if (!preferred %in% frame$username) {
        preferred <- if (nrow(frame)) frame$username[[1L]] else ""
      }
      selected_user(preferred)
      invisible(preferred)
    }
    normalize_job_selection <- function(preferred = NULL) {
      if (is.null(preferred)) preferred <- shiny::isolate(selected_job())
      frame <- jobs()
      keys <- if (nrow(frame)) paste(frame$user, frame$id, sep = "::") else character()
      if (!preferred %in% keys) preferred <- if (length(keys)) keys[[1L]] else ""
      selected_job(preferred)
      invisible(preferred)
    }
    refresh_user_data <- function(preferred = NULL) {
      if (is.null(preferred)) preferred <- shiny::isolate(selected_user())
      users(ls_user_list(root))
      normalize_user_selection(preferred)
    }
    refresh_job_data <- function(preferred = NULL) {
      if (is.null(preferred)) preferred <- shiny::isolate(selected_job())
      jobs(.ls_admin_jobs(root, users()))
      normalize_job_selection(preferred)
    }
    selected_job_parts <- function() {
      key <- strsplit(selected_job() %||% "", "::", fixed = TRUE)[[1L]]
      if (length(key) != 2L) character() else key
    }
    load_selected_logs <- function() {
      key <- selected_job_parts()
      if (length(key) != 2L) {
        logs("Select a job to view its worker log.")
        return(invisible(NULL))
      }
      row <- users()[users()$username == key[[1L]], , drop = FALSE]
      if (!nrow(row)) return(invisible(NULL))
      queue <- LibeRQueue$new(
        root, key[[1L]], 1L, limits = .ls_admin_user_limits(row)
      )
      value <- tryCatch(
        c("--- stdout ---", queue$logs(key[[2L]], stream = "stdout"),
          "--- stderr ---", queue$logs(key[[2L]], stream = "stderr")),
        error = identity
      )
      if (inherits(value, "error")) {
        set_notice(conditionMessage(value), "error")
      } else {
        logs(paste(value, collapse = "\n"))
      }
      invisible(NULL)
    }
    perform <- function(operation, success, refresh = FALSE) {
      result <- tryCatch(force(operation), error = identity)
      if (inherits(result, "error")) {
        set_notice(conditionMessage(result), "error")
        return(invisible(NULL))
      }
      if (isTRUE(refresh)) {
        refresh_user_data()
        if (identical(input$admin_section, "Jobs") ||
            identical(input$admin_section, "Server")) refresh_job_data()
      }
      set_notice(success, "success")
      invisible(result)
    }
    output$gate <- shiny::renderUI(if (authenticated()) admin_ui else login_ui)
    output$notice <- shiny::renderUI({
      item <- notice()
      if (is.null(item)) {
        item <- list(
          message = if (authenticated()) "Server administration ready." else
            "Administrator authentication required.",
          level = "info"
        )
      }
      level <- as.character(item$level %||% "info")
      if (!level %in% c("info", "success", "error", "token")) level <- "info"
      shiny::div(
        class = paste("la-notice la-message-bar", paste0("la-", level)),
        shiny::span(class = paste("la-message-dot", paste0("la-dot-", level))),
        shiny::span(class = "la-message-text", item$message)
      )
    })
    output$users_list <- shiny::renderUI({
      frame <- users()
      query <- tolower(trimws(as.character(input$user_search %||% "")))
      if (nzchar(query) && nrow(frame)) {
        haystack <- tolower(paste(frame$username, frame$first_name, frame$last_name))
        frame <- frame[grepl(query, haystack, fixed = TRUE), , drop = FALSE]
      }
      if (!nrow(frame)) {
        return(shiny::div(class = "la-empty", "No users match the search."))
      }
      shiny::div(
        class = "la-selection-list la-user-list",
        lapply(seq_len(nrow(frame)), function(index) {
          row <- frame[index, , drop = FALSE]
          username <- row$username[[1L]]
          full_name <- trimws(paste(row$first_name[[1L]], row$last_name[[1L]]))
          shiny::tags$button(
            type = "button",
            class = paste("la-select-row la-user-row",
                          if (identical(selected_user(), username)) "selected" else ""),
            onclick = .ls_admin_pick_js("admin_user_pick", username),
            shiny::span(class = "la-row-primary", username),
            shiny::span(class = "la-row-secondary", if (nzchar(full_name)) full_name else "Name not set"),
            shiny::span(
              class = paste("la-state-pill", if (isTRUE(row$enabled[[1L]])) "enabled" else "disabled"),
              if (isTRUE(row$enabled[[1L]])) "Enabled" else "Disabled"
            )
          )
        })
      )
    })
    output$selected_user_label <- shiny::renderUI({
      if (!nzchar(selected_user())) return(shiny::span(class = "la-selection-label", "None"))
      shiny::span(class = "la-selection-label", selected_user())
    })
    output$jobs_list <- shiny::renderUI({
      frame <- jobs()
      if (!nrow(frame)) return(shiny::div(class = "la-empty", "No execution jobs."))
      shiny::div(
        class = "la-selection-list la-job-list",
        lapply(seq_len(nrow(frame)), function(index) {
          row <- frame[index, , drop = FALSE]
          key <- paste(row$user[[1L]], row$id[[1L]], sep = "::")
          shiny::tags$button(
            type = "button",
            class = paste("la-select-row la-job-row",
                          if (identical(selected_job(), key)) "selected" else ""),
            onclick = .ls_admin_pick_js("admin_job_pick", key),
            shiny::span(class = "la-row-primary", row$label[[1L]] %||% row$id[[1L]]),
            shiny::span(class = "la-row-secondary", paste(row$user[[1L]], row$type[[1L]], sep = " \u00b7 ")),
            shiny::span(class = paste("la-state-pill", row$status[[1L]]), row$status[[1L]]),
            shiny::span(class = "la-row-id", row$id[[1L]])
          )
        })
      )
    })
    output$selected_job_label <- shiny::renderUI({
      key <- selected_job_parts()
      shiny::span(class = "la-selection-label", if (length(key)) key[[2L]] else "None")
    })
    output$job_logs <- shiny::renderText(logs())
    output$runtime <- shiny::renderTable({
      frame <- users()
      queue <- jobs()
      count <- function(status) sum(queue$status == status)
      data.frame(
        Setting = c(
          "Storage root", "Platform", "R version", "Users", "Running jobs",
          "Failed jobs", "Cancelled jobs", "Completed jobs", "Queued jobs",
          "Storage used (MB)"
        ),
        Value = c(
          normalizePath(root, winslash = "/"), R.version$platform, R.version.string,
          nrow(frame), count("running"), count("failed"), count("cancelled"),
          count("completed"), count("queued"),
          sprintf("%.2f", .ls_storage_bytes(root, "") / 1024^2)
        ), stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = FALSE)

    shiny::observeEvent(input$login, {
      supplied <- .ls_token_hash(input$admin_token %||% "")
      if (.ls_constant_time_equal(supplied, admin_hash)) {
        authenticated(TRUE)
        shiny::updateTextInput(session, "admin_token", value = "")
        set_notice("Administrator session authenticated.", "success")
        session$onFlushed(
          function() shiny::isolate(normalize_user_selection()),
          once = TRUE
        )
      } else {
        set_notice("Invalid administrator token.", "error")
      }
    })
    shiny::observeEvent(input$logout, {
      authenticated(FALSE)
      notice(NULL)
      logs("Select a job to view its worker log.")
    })
    shiny::observeEvent(input$refresh_users, {
      refresh_user_data()
      set_notice("User registry refreshed.", "success")
    })
    shiny::observeEvent(input$refresh_jobs, {
      refresh_job_data(); load_selected_logs()
      set_notice("Job list refreshed.", "success")
    })
    shiny::observeEvent(input$admin_user_pick, {
      selected_user(as.character(input$admin_user_pick %||% ""))
    })
    shiny::observeEvent(input$admin_job_pick, {
      selected_job(as.character(input$admin_job_pick %||% ""))
      load_selected_logs()
    })
    shiny::observeEvent(selected_user(), {
      row <- users()[users()$username == selected_user(), , drop = FALSE]
      if (!nrow(row)) return()
      shiny::updateTextInput(session, "edit_first_name", value = row$first_name[[1L]])
      shiny::updateTextInput(session, "edit_last_name", value = row$last_name[[1L]])
      shiny::updateCheckboxInput(session, "edit_enabled", value = isTRUE(row$enabled[[1L]]))
      mapping <- c(workers = "max_concurrent_jobs", queued = "max_queued_jobs",
                   payload = "max_payload_mb", result = "max_result_mb",
                   storage = "max_storage_mb", runtime = "max_runtime_seconds",
                   cpu = "max_cpu_seconds", memory = "max_memory_mb")
      for (key in names(mapping)) {
        shiny::updateNumericInput(session, paste0("edit_", key), value = row[[mapping[[key]]]][[1L]])
      }
    })
    shiny::observeEvent(input$create_user, {
      created <- perform(
        ls_user_create(
          root, input$new_username, .ls_admin_limits_from_input(input, "new_"),
          input$new_enabled, input$new_first_name, input$new_last_name
        ),
        "User created. Copy the one-time token now.", refresh = TRUE
      )
      if (!is.null(created)) {
        selected_user(created$username)
        set_notice(paste("One-time token for", created$username, ":", created$token), "token")
      }
    })
    shiny::observeEvent(input$update_user, {
      perform(ls_user_update(
        root, selected_user(), input$edit_enabled,
        .ls_admin_limits_from_input(input, "edit_"),
        input$edit_first_name, input$edit_last_name
      ),
              "User settings saved.", refresh = TRUE)
    })
    shiny::observeEvent(input$rotate_token, {
      token <- perform(ls_user_rotate_token(root, selected_user()),
                       "Token rotated.", refresh = FALSE)
      if (!is.null(token)) set_notice(paste("New one-time token for", selected_user(), ":", token), "token")
    })
    shiny::observeEvent(input$delete_user, {
      if (!isTRUE(input$confirm_delete_user)) {
        set_notice("Confirm storage deletion before deleting the user.", "error")
        return()
      }
      perform(ls_user_delete(root, selected_user(), remove_jobs = TRUE),
              "User and retained storage deleted.", refresh = TRUE)
      shiny::updateCheckboxInput(session, "confirm_delete_user", value = FALSE)
    })
    shiny::observeEvent(input$cancel_job, {
      key <- selected_job_parts()
      if (length(key) != 2L) return()
      row <- users()[users()$username == key[[1L]], , drop = FALSE]
      if (!nrow(row)) return()
      queue <- LibeRQueue$new(
        root, key[[1L]], 1L, limits = .ls_admin_user_limits(row)
      )
      perform(queue$cancel(key[[2L]]), "Job cancellation requested.", refresh = TRUE)
      refresh_job_data(); load_selected_logs()
    })
    shiny::observe({
      shiny::invalidateLater(2000, session)
      if (!isTRUE(authenticated())) return()
      section <- input$admin_section %||% "Users"
      if (!section %in% c("Jobs", "Server")) return()
      tryCatch({
        refresh_job_data()
        if (identical(section, "Jobs")) load_selected_logs()
      }, error = function(error) set_notice(conditionMessage(error), "error"))
    })
  }
  shiny::shinyApp(ui, server)
}

#' Run the LibeRties server-administration application
#'
#' @param root LibeRties server storage root.
#' @param admin_token Administrator token.
#' @param host Listening host. Loopback is the secure default.
#' @param port Listening port; `NULL` selects a free port.
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @export
ls_run_admin <- function(root = .ls_default_root(),
                         admin_token = Sys.getenv("LIBERTIES_ADMIN_TOKEN"),
                         host = "127.0.0.1", port = NULL,
                         launch.browser = getOption("shiny.launch.browser", interactive()), ...) {
  if (!host %in% c("127.0.0.1", "localhost", "::1")) {
    .ls_stop("The admin GUI only binds to a loopback host; use an authenticated reverse proxy for remote access.")
  }
  shiny::runApp(
    ls_admin_gui(root, admin_token), host = host, port = port,
    launch.browser = launch.browser, ...
  )
}
