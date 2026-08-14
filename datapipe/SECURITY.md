# Security and offline operation

This application is designed to run entirely on your own machine. It makes no
network connections of its own — not for assets, not for updates, not for
telemetry. This document says exactly what it does and does not do, and gives
you a way to verify it rather than take it on trust.

---

## What it does not do

- **No outbound connections.** Nothing in the application contacts a remote
  host. There is no update check, no analytics, no error reporting, no licence
  check, no "phone home" of any kind.
- **No external web assets.** Every stylesheet, script and font used by the
  interface is served from the R packages installed on your machine. There are
  no CDN links, and the served HTML contains no absolute URLs at all.
- **No cloud storage or remote databases.** The only files touched are the ones
  you point at.
- **Your data never leaves the machine.** It is read from your folders,
  processed in memory (or in DuckDB's temporary directory), and written to the
  export path you choose.

## What it does do

- **Listens on `127.0.0.1` only.** The web interface is bound to the loopback
  address, so it is reachable only from the same machine. It is not visible on
  your network.
- **Reads and writes the paths you select.** Input files, the export path, and
  the `pipelines/` folder for saved pipelines. Uploaded files are copied into
  `uploads/` inside the project folder so that a saved pipeline can find them
  again on a later run.
- **Uses a temporary directory.** DuckDB spills large intermediate tables to
  the system temp directory, which is cleared when the session ends.

## DuckDB is explicitly locked down

DuckDB ships able to fetch extensions on demand from `extensions.duckdb.org`.
This application needs none of them, so every connection it opens disables that
capability before doing any work (`R/engine_duckdb.R`):

```sql
SET autoinstall_known_extensions = false;
SET autoload_known_extensions   = false;
SET allow_community_extensions  = false;
```

The driver is also opened with `shared_home = FALSE`, so nothing is written to
your home directory and nothing outlives the run.

With those set, an attempt to reach a URL fails cleanly rather than silently
downloading anything:

```
SELECT * FROM read_csv('https://example.com/a.csv');
-- Missing Extension Error: File ... requires the extension httpfs to be loaded
```

`enable_external_access` is deliberately **not** set, because it would also
block reading your own CSV files — which is the entire job.

---

## Verifying it yourself

Run the built-in check on your own machine:

```bash
Rscript verify.R
```

It reports your R and package versions, confirms the DuckDB lockdown is in
effect on a connection opened exactly the way the application opens one,
scans the shipped source for anything network-capable, and runs both test
suites. It exits non-zero if anything is wrong.

### Proving it the hard way

The strongest check is to run it with no network at all. On Linux:

```bash
# a namespace with no network interfaces except loopback
unshare -rn --map-root-user bash -c '
  python3 -c "
import socket,struct,fcntl
s=socket.socket(socket.AF_INET,socket.SOCK_DGRAM)
f=struct.unpack(\"16sh\",fcntl.ioctl(s,0x8913,struct.pack(\"16sh\",b\"lo\",0)))[1]
fcntl.ioctl(s,0x8914,struct.pack(\"16sh\",b\"lo\",f|1))"
  Rscript tests/test_engine.R | tail -2
  Rscript tests/test_duckdb.R | tail -2
  Rscript run_pipeline.R example_monthly_sales
'
```

Everything passes and the pipeline runs. This is how the release was checked:
with no route out, all 157 engine checks and 132 differential checks pass, the
command-line runner produces its output, the web interface serves, and a
browser driven through the whole application made 33 requests — all of them to
`127.0.0.1`, none anywhere else.

You can also watch it from the outside while it runs normally:

```bash
# on Linux; should show no outbound connections from the R process
ss -tanp 2>/dev/null | grep -i rsession
lsof -i -a -p "$(pgrep -f 'Rscript app.R' | head -1)" 2>/dev/null
```

---

## The one thing that needs the internet

**Installing the R packages**, once, on a machine that has never had them.
That is `install.packages()` talking to CRAN, not this application. After that
the application never needs a connection again.

To install on a machine with no internet at all, see
[`setup/OFFLINE-INSTALL.md`](setup/OFFLINE-INSTALL.md): download the package
files on a connected machine, copy them across, and install from the local
files.

---

## If you change the defaults

- **`DATAPIPE_HOST`** — setting this to anything other than `127.0.0.1` (for
  example `0.0.0.0`) exposes the interface to your network. **There is no
  authentication**, so anyone who can reach that port can read any file the
  application can read and write files where it can write. Only do this on a
  trusted, isolated network, and prefer an SSH tunnel instead:

  ```bash
  ssh -L 8080:127.0.0.1:8080 user@host   # then browse to 127.0.0.1:8080 locally
  ```

- **Pipeline files are configuration, not data.** A saved pipeline is plain
  JSON containing file paths, field names and transformation settings. It does
  not contain your data, but the paths and column names may themselves be
  sensitive — treat a pipeline file the way you would treat a report template.

- **Pipeline files are executed as configuration, not as code.** There is no
  `eval` anywhere in the application: a pipeline can only name operations from
  the built-in library, with parameters. Opening a pipeline file from an
  untrusted source cannot run arbitrary code, though it could of course point
  at paths you did not intend, so read one before running it.

---

## Reporting

If you find something that contradicts any of the above, that is a bug worth
raising — the claims here are meant to be checkable, and `verify.R` exists so
that they can be re-checked on every machine you deploy to.
