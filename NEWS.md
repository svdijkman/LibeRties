# LibeRties 0.6.0

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
