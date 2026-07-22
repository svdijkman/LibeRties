# LibeRties 0.7.0

- Upgrades typed jobs and results to wire contract v2, while retaining read
  compatibility with v1. The contract now preserves all current LibeRation
  model semantics and typed LibeRality result classes.
- Adds scoped and expiring tokens, per-token/IP request throttling, a
  hash-chained administrative audit trail, production preflight checks, and
  security response headers.
- Adds optional authenticated at-rest encryption for queue RDS metadata,
  payloads, and results using a server-owned key; checksums continue to verify
  the encrypted artefacts.
- Monitors and terminates complete worker process trees. Documentation and
  status metadata now call the built-in boundary a restricted subprocess, not
  an operating-system sandbox; production mode requires TLS termination and a
  separately configured OS/container isolation layer.

# LibeRties 0.6.1

- Added typed local and remote queue execution for ordered LibeRation
  estimation sequences, preserving stage configuration and model output
  selections across the wire contract.

# LibeRties 0.6.0

- Extended the typed literature contract from indexing/assessment to the full
  LibeRary pipeline: triage, Docling parsing, independent dual extraction, and
  third-model adjudication.

- Rebuilt local and remote execution around a versioned typed-JSON job and
  result contract.
- Added persistent cross-platform local queues with background workers,
  cancellation, logs, restart recovery, and result provenance.
- Added authenticated multi-tenant HTTP execution with token digests,
  tenant-derived namespaces, checksums, quotas, resource limits, and locked
  state transitions.
- Added durable users, first/last-name administration, searchable/selectable
  admin tables, job-state dashboards, worker logs, and matching light/dark
  branding.
- Added secure client/server settings persistence across package upgrades.

This release is an architectural and API break from the 0.4.x series.
