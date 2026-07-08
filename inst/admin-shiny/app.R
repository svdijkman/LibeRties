library(shiny)
library(DT)

.ls_admin_theme_head <- function() {
  tagList(
    tags$script(HTML("
      (function() {
        function boot() {
          try {
            if (localStorage.getItem('libertiesDarkTheme') !== '0') {
              document.body.classList.add('theme-dark');
            }
          } catch (e) {
            document.body.classList.add('theme-dark');
          }
        }
        if (document.body) boot();
        else document.addEventListener('DOMContentLoaded', boot);
      })();
    ")),
    tags$style(HTML("
      body {
        transition: background-color 0.2s, color 0.2s;
      }
      .libe-admin-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin: 0 0 12px 0;
        padding-bottom: 8px;
        border-bottom: 1px solid #dee2e6;
      }
      .libe-admin-header h2,
      .libe-admin-header h3 {
        margin: 0;
        font-size: 24px;
        font-weight: 600;
      }
      .theme-toggle-wrap {
        display: flex;
        align-items: center;
        gap: 8px;
        flex-shrink: 0;
      }
      .theme-toggle-label {
        font-size: 12px;
        color: #6c757d;
        min-width: 32px;
        text-align: right;
      }
      .theme-switch {
        position: relative;
        display: inline-block;
        width: 40px;
        height: 22px;
        margin: 0;
      }
      .theme-switch input {
        opacity: 0;
        width: 0;
        height: 0;
      }
      .theme-slider {
        position: absolute;
        cursor: pointer;
        inset: 0;
        background-color: #b0b8c4;
        border-radius: 22px;
        transition: background-color 0.25s;
      }
      .theme-slider:before {
        position: absolute;
        content: '';
        height: 16px;
        width: 16px;
        left: 3px;
        bottom: 3px;
        background-color: #fff;
        border-radius: 50%;
        transition: transform 0.25s;
        box-shadow: 0 1px 2px rgba(0,0,0,0.25);
      }
      .theme-switch input:checked + .theme-slider {
        background-color: #3d6fa8;
      }
      .theme-switch input:checked + .theme-slider:before {
        transform: translateX(18px);
      }
      body.theme-dark {
        background: #121212;
        color: #e8e8e8;
      }
      body.theme-dark .libe-admin-header {
        border-bottom-color: #404040;
      }
      body.theme-dark .theme-toggle-label,
      body.theme-dark .help-block,
      body.theme-dark .text-muted {
        color: #adb5bd !important;
      }
      body.theme-dark .well {
        background: #1e1e1e;
        border-color: #404040;
        color: #e8e8e8;
      }
      body.theme-dark .form-control,
      body.theme-dark textarea.form-control {
        background: #2b2b2b;
        color: #e8e8e8;
        border-color: #505050;
      }
      body.theme-dark .nav-tabs > li > a {
        color: #adb5bd;
      }
      body.theme-dark .nav-tabs > li.active > a,
      body.theme-dark .nav-tabs > li.active > a:hover,
      body.theme-dark .nav-tabs > li.active > a:focus {
        background: #2b2b2b;
        color: #e8e8e8;
        border-color: #404040 #404040 transparent;
      }
      body.theme-dark .tab-content {
        color: #e8e8e8;
      }
      body.theme-dark table.dataTable thead th {
        background: #2b2b2b;
        color: #e8e8e8;
        border-color: #404040;
      }
      body.theme-dark table.dataTable tbody tr {
        background: #1e1e1e;
        color: #e8e8e8;
      }
      body.theme-dark table.dataTable tbody tr:nth-child(even) {
        background: #252525;
      }
      body.theme-dark .dataTables_wrapper,
      body.theme-dark .dataTables_info,
      body.theme-dark .dataTables_length,
      body.theme-dark .dataTables_filter,
      body.theme-dark .dataTables_paginate {
        color: #adb5bd !important;
      }
      body.theme-dark .job-row:hover {
        background: #2a2a2a;
      }
      body.theme-dark .job-row.selected {
        background: #2d3a4f;
      }
      body.theme-dark .job-id,
      body.theme-dark .job-label {
        color: #e8e8e8;
      }
      body.theme-dark .job-type,
      body.theme-dark .job-meta,
      body.theme-dark .job-time {
        color: #9aa0a6;
      }
      body.theme-dark .job-detail-panel {
        background: #252525;
        border-left-color: #404040;
      }
      body.theme-dark .job-log-scroll {
        background: #1a1a1a !important;
        border-color: #404040 !important;
        color: #d4d4d4;
      }
      body.theme-dark pre {
        background: #1a1a1a;
        color: #d4d4d4;
        border-color: #404040;
      }
      body.theme-dark hr {
        border-color: #404040;
      }
    ")),
    tags$script(src = "liberties-theme.js")
  )
}

.ls_admin_theme_toggle <- function() {
  tags$div(
    class = "theme-toggle-wrap",
    tags$span(class = "theme-toggle-label", id = "theme_label", "Dark"),
    tags$label(
      class = "theme-switch",
      `aria-label` = "Toggle dark theme",
      tags$input(type = "checkbox", id = "theme_toggle", checked = NA),
      tags$span(class = "theme-slider")
    )
  )
}

ui <- fluidPage(
  .ls_admin_theme_head(),
  tags$div(
    class = "libe-admin-header",
    tags$h2("LibeRties"),
    .ls_admin_theme_toggle()
  ),
  conditionalPanel(
    condition = "output.admin_authed != true",
    titlePanel("Admin Login"),
    tags$p("Enter the admin API token to manage users, jobs, and datasets."),
    passwordInput("login_token", "Admin token"),
    actionButton("login_submit", "Sign in", class = "btn-primary"),
    verbatimTextOutput("login_msg")
  ),
  conditionalPanel(
    condition = "output.admin_authed == true",
    LibeRties:::.ls_admin_jobs_tree_css(),
    LibeRties:::.ls_admin_jobs_tree_js(),
    tags$head(tags$script(src = "job-push.js")),
    titlePanel("Admin"),
    uiOutput("sandbox_banner"),
  tabsetPanel(
    id = "admin_tabs",
    tabPanel(
      "Users",
      br(),
      fluidRow(
        column(
          4L,
          h4("Create user"),
          textInput("new_username", "Username"),
          numericInput("lim_jobs", "Max concurrent jobs", 1L, min = 1L),
          numericInput("lim_disk", "Max disk (MB)", 5120L, min = 100L),
          numericInput("lim_cpu", "Max CPU cores", 4L, min = 1L),
          numericInput("lim_mem", "Max memory (MB)", 8192L, min = 512L),
          actionButton("create_user", "Create user", class = "btn-primary")
        ),
        column(
          8L,
          h4("Users"),
          DTOutput("users_table"),
          br(),
          h4("Edit limits"),
          uiOutput("edit_user_ui"),
          actionButton("save_limits", "Save limits", class = "btn-primary"),
          actionButton("issue_token", "Issue new token", class = "btn-warning"),
          actionButton("remove_user", "Remove user", class = "btn-danger")
        )
      ),
      verbatimTextOutput("user_action_msg")
    ),
    tabPanel(
      "Jobs",
      br(),
      fluidRow(
        column(
          3L,
          numericInput(
            "jobs_refresh_sec",
            "Fallback refresh (seconds)",
            value = 30L,
            min = 5L,
            max = 120L,
            step = 1L,
            width = "100%"
          ),
          tags$p(
            class = "text-muted",
            style = "font-size: 11px;",
            "Jobs update via push hub (~2s). Fallback poll when hub is idle."
          )
        ),
        column(
          9L,
          actionButton("refresh_jobs", "Refresh", class = "btn-sm"),
          actionButton("cancel_job", "Cancel selected", class = "btn-warning btn-sm"),
          actionButton("cleanup_jobs", "Clear finished", class = "btn-sm")
        )
      ),
      textOutput("jobs_refresh_clock", inline = TRUE),
      br(),
      uiOutput("job_status_banner"),
      br(),
      uiOutput("jobs_tree"),
      tags$hr(),
      h4("Worker log"),
      div(
        class = "job-log-scroll",
        style = "max-height: 360px; overflow-y: auto; border: 1px solid #ddd;",
        verbatimTextOutput("job_log")
      )
    ),
    tabPanel(
      "Datasets",
      br(),
      fluidRow(
        column(
          4L,
          textInput("ds_id", "Dataset ID"),
          textInput("ds_label", "Label"),
          textInput("ds_file", "File path (.rds)"),
          actionButton("register_ds", "Register dataset", class = "btn-primary")
        ),
        column(8L, DTOutput("datasets_table"))
      ),
      verbatimTextOutput("ds_msg")
    ),
    tabPanel(
      "Settings",
      br(),
      textInput("admin_token", "Set admin API token", value = ""),
      actionButton("save_admin_token", "Save admin token", class = "btn-primary"),
      tags$hr(),
      verbatimTextOutput("config_display")
    )
  )
  )
)

server <- function(input, output, session) {
  admin_authed <- reactiveVal(FALSE)
  output$admin_authed <- reactive({ admin_authed() })
  outputOptions(output, "admin_authed", suspendWhenHidden = FALSE)

  output$login_msg <- renderText("")
  observeEvent(input$login_submit, {
    tok <- trimws(as.character(input$login_token %||% ""))
    if (LibeRties:::.ls_admin_token_verify(tok)) {
      admin_authed(TRUE)
      output$login_msg <- renderText("")
    } else {
      output$login_msg <- renderText("Invalid admin token.")
    }
  }, ignoreInit = TRUE)

  LibeRties::ls_admin_job_hub_register(session)
  users_rev <- reactiveVal(0L)
  jobs_rev <- reactiveVal(0L)
  ds_rev <- reactiveVal(0L)
  selected_job_key <- reactiveVal("")
  expanded_jobs <- reactiveVal(character())

  .admin_refresh_config <- function() {
    LibeRties::ls_config_reload()
  }

  output$sandbox_banner <- renderUI({
    users_rev()
    .admin_refresh_config()
    info <- LibeRties:::.ls_startup_info()
    worker_env <- LibeRties:::.ls_job_worker_env()
    worker_lbl <- if (identical(worker_env$mode, "dev")) {
      paste0("dev: ", worker_env$nm_root)
    } else if (requireNamespace("LibeRation", quietly = TRUE)) {
      "installed LibeRation"
    } else {
      "LibeRation not found (set LIBERATION_ROOT)"
    }
    tags$div(
      class = "well well-sm",
      style = "font-size: 12px; margin-bottom: 10px;",
      tags$p(
        style = "margin: 0 0 4px;",
        tags$strong("Sandbox:"), info$sandbox
      ),
      tags$p(
        style = "margin: 0 0 4px;",
        tags$strong("Users file:"), info$users_file,
        " (", info$n_users, " user(s))"
      ),
      tags$p(
        style = "margin: 0;",
        tags$strong("Worker packages:"), worker_lbl
      )
    )
  })

  users_df <- reactive({
    users_rev()
    .admin_refresh_config()
    LibeRties::ls_user_list()
  })

  output$users_table <- renderDT({
    df <- users_df()
    if (nrow(df) == 0L) {
      return(datatable(data.frame(message = "No users yet."), options = list(dom = "t")))
    }
    datatable(df, selection = "single", options = list(pageLength = 10L))
  })

  selected_user <- reactive({
    sel <- input$users_table_rows_selected
    df <- users_df()
    if (length(sel) != 1L || nrow(df) == 0L) {
      return(NULL)
    }
    df$username[[sel]]
  })

  output$edit_user_ui <- renderUI({
    u <- selected_user()
    req(u)
    df <- users_df()
    row <- df[df$username == u, , drop = FALSE]
    tagList(
      tags$p(strong("Selected:"), u),
      numericInput("edit_jobs", "Max concurrent jobs", row$max_concurrent_jobs, min = 1L),
      numericInput("edit_disk", "Max disk (MB)", row$max_disk_mb, min = 100L),
      numericInput("edit_cpu", "Max CPU cores", row$max_cpu, min = 1L),
      numericInput("edit_mem", "Max memory (MB)", row$max_memory_mb, min = 512L),
      checkboxInput("edit_enabled", "Account enabled", value = row$enabled)
    )
  })

  observeEvent(input$create_user, {
    req(nzchar(input$new_username))
    out <- tryCatch(
      LibeRties::ls_user_create(
        input$new_username,
        limits = list(
          max_concurrent_jobs = as.integer(input$lim_jobs),
          max_disk_mb = as.integer(input$lim_disk),
          max_cpu = as.integer(input$lim_cpu),
          max_memory_mb = as.integer(input$lim_mem)
        )
      ),
      error = function(e) e
    )
    if (inherits(out, "error")) {
      output$user_action_msg <- renderText(conditionMessage(out))
      return()
    }
    output$user_action_msg <- renderText(paste0(
      "Created user ", out$username, "\nAPI token (save now):\n", out$token
    ))
    users_rev(users_rev() + 1L)
  })

  observeEvent(input$save_limits, {
    u <- selected_user()
    req(u)
    tryCatch(
      LibeRties::ls_user_set_limits(
        u,
        max_concurrent_jobs = as.integer(input$edit_jobs),
        max_disk_mb = as.integer(input$edit_disk),
        max_cpu = as.integer(input$edit_cpu),
        max_memory_mb = as.integer(input$edit_mem),
        enabled = isTRUE(input$edit_enabled)
      ),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
    users_rev(users_rev() + 1L)
    showNotification("Limits updated.", type = "message")
  })

  observeEvent(input$issue_token, {
    u <- selected_user()
    req(u)
    token <- tryCatch(
      LibeRties::ls_user_issue_token(u),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
    if (!is.null(token)) {
      output$user_action_msg <- renderText(paste0(
        "New token for ", u, " (save now):\n", token
      ))
    }
  })

  observeEvent(input$remove_user, {
    u <- selected_user()
    req(u)
    tryCatch(
      LibeRties::ls_user_remove(u, remove_sandbox = FALSE),
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
    users_rev(users_rev() + 1L)
    showNotification(paste("Removed user:", u), type = "warning")
  })

  jobs_df <- reactive({
    jobs_rev()
    input$job_push_rev
    input$refresh_jobs
    LibeRties:::.ls_job_list_all(reconcile = "active", dispatch = TRUE)
  })

  observe({
    input$job_push_rev
    if (identical(input$admin_tabs, "Jobs")) {
      isolate(jobs_rev(jobs_rev() + 1L))
    }
  })

  observe({
    input$refresh_jobs
    input$jobs_refresh_sec
    refresh_sec <- as.integer(input$jobs_refresh_sec %||% 30L)
    if (!is.finite(refresh_sec) || refresh_sec < 5L) {
      refresh_sec <- 30L
    }
    if (!identical(input$admin_tabs, "Jobs")) {
      invalidateLater(refresh_sec * 1000L, session)
      return()
    }
    invalidateLater(refresh_sec * 1000L, session)
    isolate(jobs_rev(jobs_rev() + 1L))
  })

  observeEvent(input$cancel_job, {
    key <- selected_job_key()
    if (!nzchar(key)) {
      showNotification("Select a job to cancel.", type = "warning")
      return()
    }
    parsed <- LibeRties:::.ls_admin_parse_job_key(key)
    if (is.null(parsed)) {
      showNotification("Could not parse selected job.", type = "error")
      return()
    }
    out <- tryCatch(
      LibeRties:::.ls_job_cancel(parsed$user, parsed$id),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
    if (!is.null(out)) {
      jobs_rev(jobs_rev() + 1L)
      showNotification(paste("Job cancelled:", parsed$id), type = "message")
    }
  })

  observeEvent(input$cleanup_jobs, {
    n <- LibeRties:::.ls_job_cleanup_all()
    jobs_rev(jobs_rev() + 1L)
    if (n > 0L) {
      showNotification(paste("Removed", n, "finished job(s)."), type = "message")
    } else {
      showNotification("No finished jobs to remove.", type = "message")
    }
  })

  observeEvent(input$admin_job_tree_event, {
    ev <- input$admin_job_tree_event
    if (is.null(ev) || is.null(ev$action) || is.null(ev$job)) {
      return()
    }
    key <- as.character(ev$job)
    if (!nzchar(key)) {
      return()
    }
    if (identical(ev$action, "toggle")) {
      expanded <- expanded_jobs()
      if (key %in% expanded) {
        expanded_jobs(setdiff(expanded, key))
      } else {
        expanded_jobs(c(expanded, key))
      }
      return()
    }
    if (identical(ev$action, "select")) {
      selected_job_key(key)
    }
  }, ignoreInit = TRUE)

  selected_job <- reactive({
    key <- selected_job_key()
    if (!nzchar(key)) {
      return(NULL)
    }
    parsed <- LibeRties:::.ls_admin_parse_job_key(key)
    if (is.null(parsed)) {
      return(NULL)
    }
    df <- jobs_df()
    if (nrow(df) > 0L) {
      idx <- which(df$user == parsed$user & df$id == parsed$id)
      if (length(idx) == 1L) {
        row <- df[idx, , drop = FALSE]
        return(list(
          user = parsed$user,
          id = parsed$id,
          status = row$status[[1L]],
          method = row$method[[1L]],
          started = row$started[[1L]],
          finished = row$finished[[1L]],
          objective = row$objective[[1L]],
          error = row$error[[1L]]
        ))
      }
    }
    st <- LibeRties:::.ls_job_read_meta(parsed$user, parsed$id)
    if (is.null(st)) {
      return(NULL)
    }
    st$user <- parsed$user
    st
  })

  output$jobs_tree <- renderUI({
    jobs_df()
    selected_job_key()
    expanded_jobs()
    LibeRties:::.ls_admin_jobs_tree_ui(
      jobs_df(),
      selected_job_key(),
      expanded_jobs()
    )
  })

  output$jobs_refresh_clock <- renderText({
    jobs_df()
    df <- jobs_df()
    n_active <- if (nrow(df) > 0L) sum(df$status %in% c("queued", "running")) else 0L
    paste(
      "Updated:", format(Sys.time(), "%H:%M:%S"),
      "|", nrow(df), "job(s)",
      if (n_active > 0L) paste0("(", n_active, " active)") else ""
    )
  })

  output$job_status_banner <- renderUI({
    jobs_df()
    st <- selected_job()
    if (is.null(st)) {
      return(NULL)
    }
    cls <- switch(
      st$status,
      success = "alert-success",
      error = "alert-danger",
      cancelled = "alert-warning",
      running = "alert-info",
      queued = "alert-secondary",
      "alert-light"
    )
    err <- LibeRties:::.ls_admin_job_error_text(st$user, st$id, st$error)
    duration_lbl <- LibeRties:::.ls_admin_job_duration_label(
      st$started, st$finished, st$status
    )
    tags$div(
      class = paste("alert", cls),
      role = "alert",
      tags$p(
        tags$strong("Job:"), st$id,
        " | ", tags$strong("User:"), st$user,
        " | ", tags$strong("Status:"), LibeRties:::.ls_job_status_label(st$status),
        if (nzchar(duration_lbl)) {
          tags$span(" | ", tags$strong("Duration:"), duration_lbl)
        },
        if (!is.null(st$objective) && is.finite(st$objective)) {
          tags$span(" | ", tags$strong("Objective:"), round(st$objective, 4))
        }
      ),
      if (identical(st$status, "error") && nzchar(err)) {
        tags$pre(style = "white-space: pre-wrap; margin: 0;", err)
      }
    )
  })

  output$job_log <- renderText({
    jobs_df()
    st <- selected_job()
    if (is.null(st)) {
      return("Select a job to view its worker log.")
    }
    log_txt <- LibeRties:::.ls_job_log(st$user, st$id, tail = 80L)
    if (nzchar(log_txt)) {
      log_txt
    } else {
      "(No worker log yet.)"
    }
  })

  output$datasets_table <- renderDT({
    ds_rev()
    df <- LibeRties:::.ls_dataset_list()
    if (nrow(df) == 0L) {
      return(datatable(data.frame(message = "No datasets."), options = list(dom = "t")))
    }
    datatable(df, options = list(pageLength = 10L))
  })

  observeEvent(input$register_ds, {
    req(nzchar(input$ds_id), nzchar(input$ds_file))
    out <- tryCatch(
      LibeRties:::.ls_dataset_register(
        input$ds_id,
        input$ds_file,
        label = input$ds_label
      ),
      error = function(e) e
    )
    if (inherits(out, "error")) {
      output$ds_msg <- renderText(conditionMessage(out))
    } else {
      output$ds_msg <- renderText(paste("Registered:", out$id, "MD5:", out$md5))
      ds_rev(ds_rev() + 1L)
    }
  })

  observeEvent(input$save_admin_token, {
    req(nzchar(input$admin_token))
    LibeRties::ls_admin_token_set(input$admin_token)
    showNotification("Admin token saved.", type = "message")
  })

  output$config_display <- renderPrint({
    info <- LibeRties:::.ls_startup_info()
    cat(
      "Sandbox root:\n ", info$sandbox, "\n\n",
      "Users file:\n ", info$users_file, "\n",
      "Registered users: ", info$n_users, "\n\n",
      "Full config:\n",
      sep = ""
    )
    print(LibeRties::ls_config())
    cat("\nWorker env:\n")
    print(LibeRties:::.ls_job_worker_env())
  })
}

shinyApp(ui, server)
