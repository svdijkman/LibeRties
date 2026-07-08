# LibeRties Security Model

LibeRties schedules and runs population PK/PD estimation jobs that may contain
**GDPR-sensitive, pharma-owned patient data**. This document describes the
security architecture, the threat model it achieves, its one explicit
limitation, and how to operate it safely.

## Overview

| Concern | Control |
|---|---|
| Data in transit | TLS via a reverse proxy (Caddy/nginx); the API binds to `127.0.0.1` |
| Data at rest | Application-level, per-tenant envelope encryption (libsodium) |
| Cross-tenant access via the API | Every job/dataset route is scoped to the authenticated user |
| Co-tenant reading the sandbox on disk | Ciphertext-only at rest + owner-only filesystem ACLs |
| Concurrency abuse / resource exhaustion | Per-user `max_concurrent_jobs`, `max_cpu`, `max_memory_mb`; server-wide `max_global_running` |

## Threat model

**Protected against:**

- A network attacker between client and server (TLS).
- An attacker with a copy of the on-disk sandbox: job payloads (`args.enc`),
  results (`result.enc`), and wrapped keys (`key.enc`) are ciphertext only.
- One tenant trying to read another tenant's jobs, results, logs, or datasets
  through the API (owner-scoped routes; access checks on datasets).
- A **non-privileged co-tenant OS user** on the same host: at-rest data is
  encrypted, and the sandbox tree is ACL-restricted to the service account.

**NOT fully protected against (documented limitation):**

- A **root / administrative OS user** who can read a *live worker process's
  memory* while a job runs. Estimation inherently needs the plaintext model and
  data in memory during computation, and the per-job key is passed to the worker
  in memory. There is no way to compute on data without it being in RAM at some
  point. Mitigate operationally: restrict who has root/admin on the host, use
  full-disk encryption, and prefer short-lived hosts.

## Key management (token-derived, nothing recoverable server-side)

- Each user has a random 256-bit API token. Only its SHA-256 hash is stored.
- Each user has a non-secret random **salt** (`enc_salt` in `users.json`).
- The **user key (UK)** is derived on demand from `scrypt(token, salt)`. It is
  **never written to disk**; it is cached only in the API process memory, and
  only while the owner is actively authenticating.
- Each job gets a random **data-encryption key (DEK)**. The DEK encrypts the job
  payload and result (libsodium `secretbox`, XSalsa20-Poly1305 AEAD). The DEK is
  **wrapped** (encrypted) with the UK and stored as `key.enc`.

Consequence, **by design**: if a user loses their token, their at-rest data is
**permanently unrecoverable** — the server holds no recovery key. Plan token
handling and backups accordingly.

### Queued-job constraint

Because a job's DEK can only be unwrapped with the owner's UK, an encrypted
queued job can only be **launched while its owner is authenticated** (e.g. their
client is polling). Background/admin dispatch cannot start an encrypted job
without the owner online. In practice the client polls continuously while a user
works, so the queue drains normally.

## On-disk layout of an encrypted job

```
sandboxes/<user>/jobs/<job_id>/
  meta.rds       # operational metadata (status/times/pid/method/limits, label)
  env.rds        # worker package env + serialization flag (no secrets)
  args.enc       # AEAD(model + data, DEK)     <- ciphertext
  key.enc        # AEAD(DEK, UK)               <- wrapped key
  result.enc     # AEAD(fit/result, DEK)       <- ciphertext
  worker.log     # progress log (ACL-restricted; may echo parameter values)
```

`args.rds` / `result.rds` (plaintext) only appear for legacy jobs created before
encryption was enabled, or when encryption is explicitly disabled.

**Note on metadata:** `meta.rds` (including the job **label**) is kept in the
clear for operational listing and admin visibility. Do not put sensitive
identifiers in job labels. Worker logs are ACL-restricted and owner-scoped over
the API but are not encrypted; avoid logging raw patient data.

## Configuration reference

Set via `LibeRties::ls_config_set(...)` (persisted to `config.json`):

| Key | Default | Purpose |
|---|---|---|
| `api_host` | `"127.0.0.1"` | Bind address. Keep localhost; expose via the proxy. |
| `encrypt_at_rest` | `TRUE` | Envelope-encrypt payloads/results. |
| `proxy_shared_secret` | `""` | If set, API rejects requests without matching `X-Proxy-Secret`. |
| `api_cors_origin` | `""` | Optional single allowed browser origin (no wildcard). |
| `worker_serial_native` | `FALSE` | If TRUE, force native workers to run one-at-a-time. |
| `max_global_running` | `0` | Server-wide cap on concurrent jobs (0 = unlimited). |

Per-user: `max_concurrent_jobs`, `max_cpu`, `max_memory_mb`, `max_disk_mb`.

See `inst/deploy/` for `Caddyfile` / `nginx.conf` and `RUNBOOK.md`.
