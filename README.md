# LibeRties

LibeRties provides durable local and remote job execution for the LibeR
population PK/PD modelling system. It uses a versioned typed-JSON contract,
background R workers, per-tenant namespaces, authenticated HTTP access,
restart recovery, quotas, resource limits, integrity checks, cancellation,
logs, and result provenance.

## Local queue

```r
library(LibeRties)

queue <- ls_local_queue("~/LibeR/workspace/.jobs")
queue$poll(start = TRUE)
```

LibeRation creates and restores this persistent queue automatically when
`liber_gui()` is launched with LibeRties installed.

## Remote service

```r
library(LibeRties)

root <- "D:/liberties-data"
user <- ls_user_create(
  root, "alice", first_name = "Alice",
  scopes = c("jobs:read", "jobs:write"),
  expires = Sys.time() + 90 * 24 * 3600
)
# Store user$token securely; it is returned only when created or rotated.
Sys.setenv(LIBERTIES_STORAGE_KEY = ls_generate_storage_key())
ls_server_preflight(root, "127.0.0.1")
ls_run_api(root, host = "127.0.0.1", port = 8000L, production = FALSE)
```

Bind the R service to a private or loopback interface and terminate TLS at a
maintained reverse proxy for remote deployment. Production hosting should add
OS-account or container isolation around the restricted worker processes.
The subprocess is not a hostile-code sandbox. For non-loopback deployment,
`ls_run_api(..., production = TRUE)` performs a fail-closed preflight for the
declared TLS and storage-encryption boundary and requires verifiable
OS-isolation evidence. A `LIBERTIES_OS_ISOLATION` label alone is not proof:
connect `isolation_probe` to the actual service manager/container boundary (or
use the built-in Linux container/cgroup detection). Configure `trusted_proxies`
explicitly before forwarded client addresses are accepted. Rate-limit state is
memory bounded, remote logs are size-limited and secret-redacted, and terminal
logs are authenticated-encrypted when a storage key is configured.

The administration interface is launched with `ls_run_admin()`. Persistent
users and job history are read from `LIBERTIES_ROOT` or
`options(LibeRties.root = ...)`, not from the installed package directory.

## AI-assisted development

GPT-5.6 was used as an AI engineering collaborator to help implement and review
the typed job contracts, queue and server infrastructure, security controls, administration GUI, tests, and documentation.
Architecture, threat-model decisions, validation criteria, and release approval remain the responsibility of the project owner.

LibeRties requires R 4.1 or newer and is MIT licensed.
