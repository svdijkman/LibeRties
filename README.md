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
user <- ls_user_create(root, "alice", first_name = "Alice")
# Store user$token securely; it is returned only when created or rotated.
ls_run_api(root, host = "127.0.0.1", port = 8000L)
```

Bind the R service to a private or loopback interface and terminate TLS at a
maintained reverse proxy for remote deployment. Production hosting should add
OS-account or container isolation around the restricted worker processes.

The administration interface is launched with `ls_run_admin()`. Persistent
users and job history are read from `LIBERTIES_ROOT` or
`options(LibeRties.root = ...)`, not from the installed package directory.

## AI-assisted development

GPT-5.6 was used as an AI engineering collaborator to help implement and review
the typed job contracts, queue and server infrastructure, security controls, administration GUI, tests, and documentation.
Architecture, threat-model decisions, validation criteria, and release approval remain the responsibility of the project owner.

LibeRties requires R 4.1 or newer and is MIT licensed.
