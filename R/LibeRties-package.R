#' LibeRties: Task Infrastructure and Execution Service
#'
#' LibeRties schedules **LibeRation** estimation jobs in isolated sandboxes with
#' API token authentication, per-user resource limits, dataset MD5 verification,
#' and an admin Shiny application.
#'
#' @section Quick start:
#' * Configure: [ls_config_set()], [ls_sandbox_root()]
#' * Admin token: [ls_admin_token_set()]
#' * Users: [ls_user_create()], [ls_user_list()], [ls_user_issue_token()]
#' * Run services: [ls_run_api()], [ls_run_admin()]
#'
#' @section LibeRation client:
#' Register a remote server in **LibeRation** with `nm_remote_server_add()` and
#' submit jobs via `nm_job_submit(..., server = )`. The LibeRation Shiny GUI
#' exposes the same workflow on the Jobs tab.
#'
#' @section Security:
#' User API tokens are hashed at rest. Optional per-user encryption wraps job
#' keys; use [ls_user_issue_token()] with `current_token` when rotating tokens.
#' Bind the API to localhost or terminate TLS at a reverse proxy (see
#' `inst/deploy/`).
#'
#' @section Environment variables:
#' * `LIBERTIES_SANDBOX_ROOT` — sandbox directory
#' * `LIBERTIES_API_HOST`, `LIBERTIES_API_PORT` — API bind address
#' * `LIBERATION_ROOT` — dev path to LibeRation for the worker subprocess
#'
#' @seealso [ls_config()] for all configuration keys.
"_PACKAGE"
