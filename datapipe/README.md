# Data Pipeline Builder

A data transformation application in R. Import your files and mapping
documents, repair the fields you need to match on, link everything into one
table, shape the output, export it — then save the whole thing as a pipeline
you can re-run next month with one click or one command.

Built with Shiny for the interface and DuckDB for the execution. Every piece
of configuration lives in an ordinary JSON file next to your data, so
pipelines are easy to find, read, diff, copy between machines and check into
version control.

---

## Quick start

```bash
cd datapipe

# generate the example data (first time only)
Rscript examples/make_examples.R

# launch the app, then open the address it prints
Rscript app.R
```

Press **Load** on the first screen to open the worked example, then walk
through the six steps to see how it is put together.

To run a saved pipeline without the interface:

```bash
Rscript run_pipeline.R pipelines/example_monthly_sales.json
```

### Requirements

R 4.1 or newer and these packages:

```r
install.packages(c("shiny", "bslib", "DT", "shinyjs",
                   "jsonlite", "data.table", "readxl", "writexl",
                   "duckdb", "DBI"))
```

`duckdb` is optional but recommended -- see [Engines](#engines) below. Without
it everything still works, just in R.

On Debian or Ubuntu the packaged builds are quicker:

```bash
sudo apt-get install -y r-base-core r-cran-shiny r-cran-bslib r-cran-dt \
  r-cran-shinyjs r-cran-jsonlite r-cran-data.table r-cran-readxl \
  r-cran-writexl r-cran-dbi
```

DuckDB is not in the Ubuntu archive; install it from R with
`install.packages("duckdb")`.

---

## The six steps

**1. Start** — run a pipeline you saved earlier, or begin a new one. The
project folder set here is what relative paths are stored against, so moving
or copying the whole folder keeps everything working.

**2. Import files** — add each data file and each mapping document. CSV, TSV,
Excel (`.xlsx`/`.xls`/`.xlsm`, any worksheet), any custom delimiter, and
fixed-width. Point at an exact file, or at a **folder plus a filename
pattern** such as `sales_*.csv` — then choose the newest match, the first by
name, or stack every match into one table. The pattern form is what lets a
saved pipeline pick up next month's file without being edited.

Everything is read as text. That is deliberate: it is the only way to keep
leading zeros, long reference numbers and mixed-format codes intact. Type
conversion happens later, where you ask for it.

**3. Linking fields** — clean up the fields you will match on, with a
before/after preview on your real data. Steps are chained in order, so the
usual repair is `trim → uppercase → numbers only → left pad`, which turns
`AC-000123 `, `ac/456` and ` 789` into `00000123`, `00000456`, `00000789`.

| Group | Operations |
|---|---|
| Clean | trim (either or both ends), collapse inner spaces, upper/lower/title/sentence case, remove accents |
| Filter | numbers only, letters only, letters and numbers only, remove specific characters, keep only these characters |
| Compose | add prefix, add suffix, remove prefix, remove suffix, find and replace (literal or regex), set to fixed value |
| Length | left pad, right pad, remove leading zeros, left N, right N, substring, truncate |
| Extract | extract by pattern (with capture groups), split and take part |
| Type | format as number, round, parse and format date |
| Blanks | blank to empty, fill empty values |

Dates parse from sixteen common layouts plus Excel serial numbers, and are
written back in whatever format you choose.

**4. Combine tables** — pick the table that drives the output, then link the
others onto it one at a time. Mapping tables are joined exactly like data
tables. Each link offers:

- **Type** — keep all base rows (left), only matching rows (inner), everything
  from both (full), all rows of the linked table (right), base rows that have
  a match (semi), or base rows with *no* match (anti, for finding the gaps).
- **Match on** — one or several field pairs, optionally ignoring case.
- **Fields to bring across**, with a prefix or suffix on the incoming names.
- **If a name already exists** — keep both, overwrite, fill only where blank,
  or drop the incoming column.
- **If the linked table has duplicate keys** — keep every match, keep the
  first, or stop and tell you.

**Check the match** reports how many rows found a partner before you commit to
anything.

**5. Final table** — choose which columns to keep, rename them, reorder them,
and apply any last formatting with the same operations as step 3. Then
optionally filter rows, remove duplicates and sort.

**6. Export & save** — write CSV, TSV, Excel or a custom delimiter, with
control over the header row, how blanks are written, the delimiter and line
endings. Then save the pipeline so it can be run again.

---

## Engines

The pipeline can be executed two ways, and they are required to agree.

**DuckDB** (default when installed) turns the whole pipeline into SQL: the
CSVs are read by DuckDB's scanner, and the matching, filtering, sorting and
de-duplicating happen inside its query engine.

On the four-table example pipeline scaled up — transactions joined to
customers and two mapping tables, with key repair, a filter, number and date
formatting and a sort:

| Input rows | R | DuckDB |
|---:|---:|---:|
| 500,000 | 34.3s | 9.0s |
| 2,000,000 | 131.3s | 33.8s |

Same output, byte for byte, in both cases.

**R** is the reference implementation. It is what `tests/test_engine.R`
pins down, and it is what runs when DuckDB is unavailable.

The engine is chosen per pipeline on the last screen, or with `--engine`:

```bash
Rscript run_pipeline.R pipelines/monthly.json --engine duckdb
Rscript run_pipeline.R pipelines/monthly.json --engine r
Rscript run_pipeline.R pipelines/monthly.json               # auto
```

On `auto` — the default — DuckDB is used whenever it can reproduce the
pipeline exactly. Three transformations have no faithful SQL translation:

| Operation | Why |
|---|---|
| Change case → title | R's rule leaves letters after an apostrophe alone; expressing that needs case conversion inside a regex replacement, which RE2 has no way to do |
| Round number | R rounds half-to-even and then formats, dropping trailing zeros; SQL rounds half-away-from-zero and keeps them |
| Remove accents | R transliterates (`ß` becomes `ss`, non-Latin becomes `?`); DuckDB's `strip_accents` only removes diacritics. Different operations, not two spellings of one |

A pipeline using one of these runs in R instead, and the run log says so.
Nothing is silently approximated: `--engine duckdb` on such a pipeline is an
error rather than a near-enough answer.

Otherwise both engines produce byte-identical exports. That is not an
aspiration but a test: `tests/test_duckdb.R` runs every operation and the
whole example pipeline through both and compares the results, down to the
bytes of the exported file.

**One documented exception.** Upper- and lower-casing go through glibc in R
and through ICU in DuckDB, and the two disagree on the handful of characters
Unicode gives a special casing rule. In practice that means the German sharp
s: `straße` uppercases to `STRAßE` in R and `STRAẞE` in DuckDB. Each engine is
self-consistent, so joins and comparisons still behave, but the two are not
byte-identical on such text. The test pins this exact case so the exception
cannot quietly grow.

---

## Re-running out of the same folders

This is the point of saving a pipeline. Once saved, the same configuration can
be re-run in three ways:

```bash
# 1. the same folders as before
Rscript run_pipeline.R pipelines/monthly.json

# 2. a different month's folders, with no edit to the pipeline
Rscript run_pipeline.R pipelines/monthly.json --root /data/2026-09

# 3. from the app -- 'Run now' on the first screen
```

Paths are stored relative to the project folder whenever the files live under
it, so the folder can be moved, copied to a colleague's machine or checked out
somewhere else and still work. `--root` re-points every relative path at once,
which is how one pipeline serves every reporting period.

Other options:

```
--out <file>      override the export path
--format <fmt>    override the export format
--describe        print the pipeline outline and exit
--validate        check the pipeline and exit
--list            list saved pipelines
--quiet           only print errors
```

The runner exits `0` on success, `1` on a failed or invalid run and `2` on a
bad invocation, so it drops straight into cron, Task Scheduler or a CI job.

---

## Layout

```
datapipe/
  app.R                     the Shiny interface
  run_pipeline.R            command-line runner
  R/
    utils.R                 helpers, path handling, locale
    i18n.R                  string lookup
    ops.R                   the transformation library (the specification)
    io.R                    readers and writers
    pipeline.R              spec, validation, and the R execution engine
    sql.R                   the same transformations, translated to SQL
    engine_duckdb.R         the DuckDB execution engine and engine choice
  locale/en.json            every visible string
  pipelines/                saved pipelines (JSON)
  examples/                 example data and its generator
  tests/test_engine.R       157 engine checks
  output/                   exports land here by default
```

The interface never transforms data itself — previews and runs both go through
the same `dp_execute()` the command-line runner uses. What you see while
building is what a scheduled re-run produces.

---

## Translating and rewording

Every visible string is in `locale/en.json`. Copy it to `locale/fr.json`,
translate the values, and start the app with:

```bash
DATAPIPE_LANG=fr Rscript app.R
```

The `_language_name` key is what appears in the language list. Missing keys
fall back to English, so a partial translation is safe to ship. The same file
is the place to reword labels for in-house vocabulary without touching R code.

---

## Testing

```bash
Rscript tests/test_engine.R     # the engine's behaviour
Rscript tests/test_duckdb.R     # DuckDB agrees with R, operation by operation
```

`test_engine.R` has 157 checks covering every transformation operation, key building, all six join
types, duplicate and blank-key handling, name-conflict resolution, filtering,
sorting, de-duplication, reader/writer round-trips, UTF-8 integrity, pipeline
save/load/migration, and an end-to-end run of the example.

`test_duckdb.R` has 132 and is a differential test rather than a second set
of expectations: for every transformation, every join type, every filter
condition and the example pipeline as a whole, it runs both engines over the
same input and asserts the results are identical -- down to the bytes of the
exported file. If a SQL translation ever drifts from the R behaviour, that
test fails rather than the user finding out from a wrong report.

The example data is deliberately awkward — account numbers with three
different punctuation styles, an Excel export that dropped its leading zeros,
a mapping table with a duplicate key, an unmatched account, an accented name
and a row to be filtered out — so a passing run means the parts that usually
break are working.

---

## Notes on behaviour

Things worth knowing, because they are decisions rather than accidents:

- **Everything is read as text.** Leading zeros and long numbers survive.
- **Blank keys never match each other.** Two rows with an empty account number
  are not the same account, so they are not joined.
- **Duplicate keys in a lookup are surfaced, not hidden.** They either
  multiply rows (with a warning saying so), keep the first match, or stop the
  run — your choice, made explicitly.
- **The row filter runs on the combined table**, before columns are dropped,
  so you can filter on a status flag you do not export. Sorting and
  de-duplication run afterwards, on the exported column names.
- **Every run is logged** with row and column counts at each stage, and how
  many rows matched at each join. Unmatched rows are a warning, never silent.
- **A field named in a pipeline that is missing from the data is an error**,
  not a silently empty column.
- **The two engines must agree.** Where SQL cannot reproduce R exactly, the
  run falls back to R rather than returning something close.
