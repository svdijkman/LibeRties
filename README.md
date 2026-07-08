# LibeRties — Task Infrastructure and Execution Service

Remote job scheduling for LibeRation: container sandboxes, API tokens, per-user limits, and an admin Shiny app.

## Quick start

```r
install.packages(c("plumber", "jsonlite", "digest", "callr"))
R CMD INSTALL LibeRties

library(LibeRties)

# Configure sandbox (default: %LOCALAPPDATA%/LibeRties on Windows)
ls_config_set(sandbox_root = "C:/liberties/sandbox", launcher = "local")
# Important: restart ls_run_api() and ls_run_admin() after changing sandbox_root.

# Create admin token and a user with limits
ls_admin_token_set("change-me-admin")
u <- ls_user_create("alice", limits = list(
  max_concurrent_jobs = 2,
  max_disk_mb = 5120,
  max_cpu = 4,
  max_memory_mb = 8192
))
u$token  # save this — shown once

# Start API + admin UI
ls_run_api()    # http://0.0.0.0:8080
ls_run_admin()  # http://127.0.0.1:8081
```

## Client (LibeRation)

```r
nm_remote_server_add("HPC", "http://cluster:8080", "alice", token = u$token)
job <- nm_job_submit(model, data, method = "FO", server = "srv_...")
nm_job_status(job$id)
fit <- nm_job_result(job$id)
```

The LibeRation Shiny app adds **Add remote server** on the Jobs tab and a **Run on** cluster selector in the estimation dialog.

## Admin Shiny

Manage users, per-user limits (concurrent jobs, disk, CPU, memory), API tokens, datasets (with MD5), and view all jobs.

## Docker worker

Build from repo root:

```bash
docker build -f LibeRties/inst/docker/Dockerfile -t liberties-worker:latest .
```

Set `launcher = "docker"` in config.

Environment variables use the `LIBERTIES_*` prefix (legacy `LIBERATION_*` names still work).
