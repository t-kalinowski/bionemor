# Return the run directory for a BioNeMo job

The run directory identifies a job and stores the files needed to
inspect or reopen it. Save this path to reopen the job with
[`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md)
in another R session. Keep the directory intact: status updates, logs,
result materialization, and provenance all use files stored below it.

## Usage

``` r
job_path(x)
```

## Arguments

- x:

  A `BioNeMoJob` returned by an asynchronous operation or
  [`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md).

## Value

A length-one character vector containing the normalized absolute run
path.

## See also

Other BioNeMo job lifecycle:
[`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md),
[`job_cancel()`](https://t-kalinowski.github.io/bionemor/reference/job_cancel.md),
[`job_logs()`](https://t-kalinowski.github.io/bionemor/reference/job_logs.md),
[`job_result()`](https://t-kalinowski.github.io/bionemor/reference/job_result.md),
[`job_status()`](https://t-kalinowski.github.io/bionemor/reference/job_status.md),
[`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md)

## Examples

``` r
if (FALSE) { # \dontrun{
job <- bionemo_job(
  "/shared/workspace/.bionemor/runs/my-generation"
)
path <- job_path(job)

# Reopen the same run later.
same_job <- bionemo_job(path)
} # }
```
