# LibeRties Operations Runbook

Practical steps to deploy and operate LibeRties securely for GDPR-sensitive
data. Read alongside `../../SECURITY.md`.

## 1. First-time secure setup

1. **Install** LibeRties and its dependency `sodium` (pulled in automatically),
   plus LibeRation/LibeRtAD for the workers.
2. **Bind the API to localhost** (default). Confirm:
   ```r
   LibeRties::ls_config()$api_host   # should be "127.0.0.1"
   ```
   If a prior install left `"0.0.0.0"`, fix it once the proxy is ready:
   ```r
   LibeRties::ls_config_set(api_host = "127.0.0.1")
   ```
3. **Enable at-rest encryption** (default TRUE). Confirm:
   ```r
   LibeRties::ls_config()$encrypt_at_rest   # TRUE
   ```
4. **Set a proxy shared secret** so only the reverse proxy can reach the API:
   ```r
   LibeRties::ls_config_set(proxy_shared_secret = "<long-random-string>")
   ```
   Put the same value in the proxy config (`X-Proxy-Secret`).
5. **Set the admin token**:
   ```r
   LibeRties::ls_admin_token_set("<admin-secret>")
   ```
6. **Start the API**:
   ```r
   LibeRties::ls_run_api()
   ```
7. **Start the TLS reverse proxy** using `Caddyfile` or `nginx.conf` in this
   folder (edit the domain, cert paths, and `X-Proxy-Secret` first).

## 2. TLS / certificates

- **Caddy** (recommended): automatic issuance and renewal from Let's Encrypt for
  a public DNS name; use `tls internal` for internal-only names (clients must
  trust the Caddy root, or set `insecure = TRUE` on the client for dev only).
- **nginx**: obtain a cert (certbot or an internal CA) and set up auto-renewal
  (`certbot renew` cron/systemd timer). Reload nginx after renewal.
- Verify the client connects over `https://` and that verification is ON (the
  client keeps TLS verification enabled unless a server entry sets
  `insecure = TRUE`, which is dev-only and warns loudly).

## 3. User & token handling

- Create users (admin): via the admin GUI or `ls_user_create()`. Each gets a
  256-bit token shown **once** — transmit it to the user over a secure channel.
- **Rotating a token** for a user who has encrypted jobs — supply the current
  token so keys are migrated:
  ```r
  LibeRties::ls_user_issue_token("alice", current_token = "<old token>")
  ```
  Or via the API: `POST /v1/admin/users/alice/token` with body
  `{"current_token":"<old>"}`.
  Without the current token, rotation refuses (to avoid silently orphaning data).
  To rotate anyway and **discard** the user's encrypted data, pass `force=TRUE`.
- **Lost token = unrecoverable data.** There is no server-side recovery key.

## 4. Concurrency tuning

- Per user: `ls_user_set_limits("alice", max_concurrent_jobs = 4, max_cpu = 1,
  max_memory_mb = 8192)`. `max_cpu`/`max_memory_mb` are the primary throttle.
- Server-wide guard against oversubscription:
  ```r
  LibeRties::ls_config_set(max_global_running = <N>)   # 0 = unlimited
  ```
  A good starting point for `max_global_running` is the number of physical cores
  available to workers (each native worker uses one core).
- Only set `worker_serial_native = TRUE` if the estimation engine must never run
  concurrently on the host.

## 5. Migrating existing plaintext jobs

If jobs were created before encryption was enabled, either:

- **Clean start** (simplest during testing): remove old job directories, or
- **Encrypt in place** per user (needs their current token):
  ```r
  LibeRties::ls_user_encrypt_existing("alice", token = "<alice's token>")
  ```
  This converts `args.rds`/`result.rds` to `args.enc`/`result.enc` and removes
  the plaintext.

## 6. Datasets (multi-tenant)

- Datasets are admin-registered. By default a dataset with no owner is treated as
  shared reference data visible to all users.
- Scope a dataset to a tenant on registration:
  `POST /v1/admin/datasets` with `{"dataset_id":..., "file_path":...,
  "owner":"alice"}` (optionally `"allowed_users":[...]` or `"public":true`).
- Listing and resolution are access-checked; a user cannot see or use another
  tenant's private dataset.

## 7. Filesystem & backups

- The sandbox tree is created with owner-only ACLs (Windows `icacls`,
  Unix `chmod 700`). Keep the service account distinct from ordinary users.
  Disable with `options(LibeRties.harden_acls = FALSE)` only if it conflicts
  with your environment.
- **Backups**: back up the whole sandbox (ciphertext is safe to store), but
  remember that restoring data is only useful if the corresponding user tokens
  still exist. Back up `users.json` (contains salts + token hashes, no tokens).
  Never back up plaintext tokens.
- Consider full-disk encryption on the host as defense-in-depth for the
  live-memory limitation described in `SECURITY.md`.

## 8. Local production rehearsal (single machine)

To validate the full production transport path — TLS, certificates, the proxy
trust filter, and the client warnings — without a second machine, use
`Caddyfile.local` in this folder. It terminates HTTPS on `:8443` with Caddy's
internal CA and forwards to the API on `127.0.0.1:8080`.

1. Server side (same machine):
   ```r
   LibeRties::ls_config_set(api_host = "127.0.0.1", encrypt_at_rest = TRUE,
                            proxy_shared_secret = "local-test-secret")
   LibeRties::ls_admin_token_set("admin-secret")
   LibeRties::ls_run_api()
   ```
2. Start the proxy: `caddy run --config ./Caddyfile.local`
3. Client side, choose one:
   - **Quick (skip cert trust):** register with `insecure = TRUE` — this is
     dev-only and deliberately triggers the "TLS verification DISABLED" warning:
     ```r
     nm_remote_server_add("Local TLS", "https://127.0.0.1:8443", "alice",
                          token = "lr_...", insecure = TRUE)
     ```
   - **Full verification (like production):** trust Caddy's local root, then
     register without `insecure`:
     ```r
     # shell:  caddy trust
     nm_remote_server_add("Local TLS", "https://localhost:8443", "alice",
                          token = "lr_...")
     ```

Things to verify during the rehearsal:
- Connecting the client to `http://127.0.0.1:8080` directly still works, but
  through the proxy on `:8443` you get end-to-end TLS.
- Hitting the API directly with the wrong/without `X-Proxy-Secret` returns 403
  (proxy-trust filter). The proxy injects the correct header.
- To also see the plain-`http` warning locally (suppressed by default on
  loopback), set `options(LibeRation.warn_plain_http_local = TRUE)` before using
  an `http://` server entry.
- Pointing the client at `:8080` with `https://` (no TLS there) fails with an
  SSL handshake error — expected; `https://` must target the proxy port `:8443`.

## 9. Health & verification checklist

- [ ] `ls_config()$api_host == "127.0.0.1"`
- [ ] `ls_config()$encrypt_at_rest == TRUE`
- [ ] `proxy_shared_secret` set on both API and proxy
- [ ] Proxy terminates TLS; direct `http://host:8080` is not reachable off-box
- [ ] A submitted job shows `args.enc`/`key.enc`/`result.enc` (no `*.rds` payload)
- [ ] Client connects via `https://` with verification enabled
