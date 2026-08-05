# Reopen a persisted BioNeMo job

Each operation dispatched through the job runner creates a run directory
under `<workspace>/.bionemor/runs/<name>`. The directory records the
request, command plan, state, logs, outputs, and provenance needed to
inspect the run after the R session that started it has ended. A
`BioNeMoJob` is a handle to those persisted files and the local or Slurm
execution backend.

## Usage

``` r
bionemo_job(path)
```

## Arguments

- path:

  Path to a run directory created by bionemor. It must contain the
  persisted request, command plan, and state files.

## Value

A `BioNeMoJob` for the persisted run.

## Details

Operations that support `async` return their typed result directly when
`async = FALSE`. With `async = TRUE`, they return a `BioNeMoJob`
immediately. Pass that job to
[`job_status()`](https://t-kalinowski.github.io/bionemor/reference/job_status.md),
[`job_logs()`](https://t-kalinowski.github.io/bionemor/reference/job_logs.md),
[`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md),
[`job_cancel()`](https://t-kalinowski.github.io/bionemor/reference/job_cancel.md),
or
[`job_result()`](https://t-kalinowski.github.io/bionemor/reference/job_result.md).
`bionemo_job()` reconstructs the same handle from its run directory,
including for a run that is still active or already complete. It does
not resubmit or restart the operation. The recorded operation and result
format must be supported by the installed package.

## See also

Other BioNeMo job lifecycle:
[`job_cancel()`](https://t-kalinowski.github.io/bionemor/reference/job_cancel.md),
[`job_logs()`](https://t-kalinowski.github.io/bionemor/reference/job_logs.md),
[`job_path()`](https://t-kalinowski.github.io/bionemor/reference/job_path.md),
[`job_result()`](https://t-kalinowski.github.io/bionemor/reference/job_result.md),
[`job_status()`](https://t-kalinowski.github.io/bionemor/reference/job_status.md),
[`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Save job_path(job), then reopen the run in this or a later R session.
job <- bionemo_job(
  "/shared/workspace/.bionemor/runs/my-generation"
)
job_status(job)
result <- job_wait(job)
} # }
```
