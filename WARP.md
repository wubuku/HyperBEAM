# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## What this repo is
HyperBEAM is an Erlang/OTP (rebar3) implementation of the AO-Core protocol. The runtime is an HTTP server (Cowboy) that accepts “AO-Core messages” over HTTP semantics, routes them to AO-Core “devices”, and returns a message response.

## Common commands

### Build
```bash
# Compile Erlang + native components
rebar3 compile

# Clean build artifacts
rebar3 clean
```

Notes:
- `rebar3 compile` runs important hooks from `rebar.config`, including:
  - generating `_build/hb_buildinfo.hrl` from `git rev-parse` + build timestamp
  - building WAMR into `_build/wamr` via `make wamr`

### Run a local node (dev)
```bash
# Starts an Erlang shell running the hb application
rebar3 shell

# Verify the node is responding (default config uses config.flat; repo’s config.flat sets port 10000)
curl http://localhost:10000/~meta@1.0/info
```

Configuration is normally loaded from `config.flat` (see below).

### Optional build profiles
Profiles are defined in `rebar.config`:
```bash
# Enable RocksDB store backend (adds deps + compile-time define)
rebar3 as rocksdb compile
rebar3 as rocksdb shell

# Enable HTTP/3 support (adds quicer)
rebar3 as http3 shell

# Enable genesis-wasm support (runs a pre-hook that clones + npm installs a server)
rebar3 as genesis_wasm shell

# Combine profiles
rebar3 as rocksdb,http3 shell
```

### Run tests
The repo primarily uses EUnit.

```bash
# Run all EUnit tests
rebar3 eunit

# Run a single module’s EUnit tests
rebar3 eunit -m hb_http

# Run specific test function(s)
# Format: Module:Func1+Func2
rebar3 eunit -t hb_http:simple_ao_resolve_unsigned_test
```

There is also a configured alias:
```bash
# Runs EUnit for dev_lua_test
rebar3 lua-test
```

### Static analysis
```bash
rebar3 dialyzer
```

### Build a production release
```bash
rebar3 release

# Release output (default profile)
ls _build/default/rel/hb
```

### Documentation site
Docs are built with MkDocs + Material, driven by `./docs/build-all.sh`.

```bash
# Python deps (one-time / per-venv)
python3 -m venv venv
source venv/bin/activate
pip3 install mkdocs mkdocs-material mkdocs-git-revision-date-localized-plugin

# Build docs (also compiles + runs rebar3 edoc)
./docs/build-all.sh

# View the static site
python3 -m http.server --directory mkdocs-site 8000
# open http://127.0.0.1:8000/
```

The script also generates LLM-oriented context files in `docs/llms.txt` and `docs/llms-full.txt`.

### Metrics (Prometheus + Grafana)
```bash
docker compose -f metrics/docker-compose.yml up
# Prometheus: http://localhost:9090
# Grafana:    http://localhost:3000
```

### Git hooks
This repo includes a Conventional Commits `commit-msg` hook.

```bash
# One-time setup to point git at .githooks/
./.githooks/_/install.sh
```

## High-level architecture (big picture)

### OTP application lifecycle
- `src/hb.app.src` defines the OTP application (`hb`).
- `src/hb_app.erl` is the application callback; it:
  - calls `hb:init/0`
  - starts `hb_sup`
  - starts `dev_scheduler_registry` and `ar_timestamp`
  - starts the HTTP server via `hb_http_server:start/0`

### HTTP ingress → AO-Core execution
- `src/hb_http_server.erl` is the Cowboy handler and is the main runtime entrypoint.
  - It loads node configuration (by default from `config.flat`) and starts Cowboy.
  - Each inbound request is converted into the internal message format (TABM-like map) via `hb_http:req_to_tabm_singleton/3`.
  - The request is then executed by calling `dev_meta:handle/2`.

### The meta device as the “request pipeline”
- `src/dev_meta.erl` (`~meta@1.0`) is the default entrypoint device.
  - Applies the node’s `request` hook (pre-processor), then calls the AO-Core resolver (`hb_ao:resolve_many/2`), then applies the `response` hook (post-processor).
  - Exposes the node message/config at `/~meta@1.0/info` (GET) and supports updates via POST when authorized.

### Messages, codecs, and commitments
- `src/hb_message.erl` is the adapter between different wire/storage formats.
  - Normalizes data into “TABM” (deep maps where keys map to nested TABMs or binaries).
  - Converts between codecs implemented as devices/modules under `src/dev_codec_*.erl` (e.g. `httpsig@1.0`, `ans104@1.0`, `structured@1.0`, `flat@1.0`, `json@1.0`).
  - Handles signing/verification and “commitments” (`hb_message:commit/2`, `hb_message:verify/1`).

### Devices
- AO-Core “devices” are predominantly implemented as `src/dev_*.erl` modules.
- The default set of preloaded devices and many runtime defaults live in `hb_opts:default_message/0`.
- When tracking behavior across devices, start from the HTTP request → `dev_meta` → `hb_ao` resolution chain, then identify the device module responsible for the relevant `device@version`.

### Storage and caching
- `src/hb_store.erl` defines a store abstraction: it composes one or more store backends and tries them in order.
- Key backends:
  - `src/hb_store_fs.erl`: local filesystem store (`cache-*` directories).
  - `src/hb_store_gateway.erl`: remote Arweave gateway-backed reads (scope `remote`), with optional local caching.
  - `src/hb_store_rocksdb.erl`: only when built with the `rocksdb` profile.

### Outbound HTTP + relaying
- `src/hb_http.erl` implements outbound HTTP request/reply conversion for messages.
- `src/dev_relay.erl` (`~relay@1.0`) relays messages to other HTTP endpoints (sync `call` or async `cast`), often used as a request hook with routing.

### Native components (WASM runtime, crypto, TEEs)
This repo includes native code built as part of `rebar3 compile`:
- WAMR (wasm-micro-runtime) is cloned/built into `_build/wamr` by `make wamr` (hooked from `rebar.config`).
- Port/NIF shared libs are produced into `priv/` (see `rebar.config` `port_specs`), built from C sources under `native/`:
  - `native/hb_beamr/` (WASM/runtime integration)
  - `native/hb_keccak/` (keccak)
- Rust crates (via `rebar3_rustler` / cargo hooks) live under `native/` (e.g. `native/dev_snp_nif/`) and back the `~snp@1.0` functionality.

## Configuration quick reference
- Default config file: `config.flat` (repo root). Example in this repo sets `port: 10000`.
- Common environment variables (see `hb_opts`):
  - `HB_CONFIG` (path to config file)
  - `HB_KEY` (wallet keyfile path; default `hyperbeam-key.json`)
  - `HB_PORT`, `HB_MODE`, `HB_PRINT`

Runtime config can be inspected at:
- `GET /~meta@1.0/info`

(Updates are performed via `POST /~meta@1.0/info` when authorized.)
