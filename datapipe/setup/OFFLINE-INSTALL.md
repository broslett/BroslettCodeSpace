# Installing on a machine with no internet

The application never needs a network connection. Its R packages do — once, to
get onto the machine. This is how to do that when the target machine has no
connection at all.

You need two things on the target machine before you start: **R 4.1 or newer**,
and a C++ toolchain *only if* you want the DuckDB engine and no binary build is
available for your platform (Windows and macOS binaries are, so this normally
only applies to Linux).

---

## The short version

On a machine that **does** have internet, running the same OS and R version:

```r
dir.create("datapipe-pkgs")
download.packages(
  c("shiny", "bslib", "DT", "shinyjs", "jsonlite",
    "data.table", "readxl", "writexl", "DBI", "duckdb"),
  destdir = "datapipe-pkgs",
  type = .Platform$pkgType,     # binary on Windows/macOS, source on Linux
  dependencies = TRUE
)
```

Copy `datapipe-pkgs/` to the target machine alongside the unzipped
application, then:

```bash
Rscript setup/install_packages.R /path/to/datapipe-pkgs
Rscript verify.R
```

`install_packages.R` with a folder argument uses `repos = NULL`, which installs
straight from the files and never opens a connection.

---

## Getting the dependencies too

`download.packages()` above does not pull in *dependencies of* dependencies
unless you ask. The reliable way is to resolve the full set first:

```r
pkgs <- c("shiny", "bslib", "DT", "shinyjs", "jsonlite",
          "data.table", "readxl", "writexl", "DBI", "duckdb")

deps <- unique(unlist(tools::package_dependencies(
  pkgs, recursive = TRUE, which = c("Depends", "Imports", "LinkingTo"))))

# skip whatever ships with R itself
base <- rownames(installed.packages(priority = "base"))
all_pkgs <- setdiff(unique(c(pkgs, deps)), base)

dir.create("datapipe-pkgs", showWarnings = FALSE)
download.packages(all_pkgs, destdir = "datapipe-pkgs", type = .Platform$pkgType)
```

That is typically around 30 packages. Copy the whole folder across.

Install order does not matter: `install.packages()` with a vector of local
files works out the order itself.

---

## Platform notes

**The files must match the target machine.** A Windows `.zip` binary will not
install on Linux, and a macOS `.tgz` will not install on Windows. Download on a
machine with the same operating system, and ideally the same R minor version
(4.3.x binaries are not guaranteed to load under 4.4.x).

If you cannot match the platform, download **source** packages instead
(`type = "source"`, which gives `.tar.gz`) and build them on the target. That
needs a toolchain: Rtools on Windows, Xcode command line tools on macOS,
`r-base-dev` plus `g++` on Debian/Ubuntu.

**Linux distribution packages are usually easier.** Most of what is needed is
already packaged, which avoids compiling anything:

```bash
sudo apt-get install -y r-base-core r-cran-shiny r-cran-bslib r-cran-dt \
  r-cran-shinyjs r-cran-jsonlite r-cran-data.table r-cran-readxl \
  r-cran-writexl r-cran-dbi
```

DuckDB is not in the Debian or Ubuntu archive, so it has to come from CRAN or
be built from source. Building it takes a while — it is a large C++ project.

---

## If you skip DuckDB

You do not have to install it. Without it the application runs on its R engine,
which produces identical output and is simply slower on large files. Everything
in the interface works the same; the engine indicator on the last screen just
says it is running in R.

This is a perfectly reasonable choice if compiling DuckDB on an air-gapped
machine is more trouble than the speed is worth. You can always add it later —
nothing about a saved pipeline changes.

---

## Checking the result

```bash
Rscript verify.R
```

This confirms the packages are present, that the DuckDB lockdown is in effect,
that no shipped source can reach the network, and that both test suites pass.
It exits non-zero if anything is wrong, so it works as a deployment gate.

See [`../SECURITY.md`](../SECURITY.md) for how to prove the running application
makes no connections, using a network namespace with no interfaces.
