# Read BioNeMo job logs

`job_logs()` reads a snapshot of the persisted log files, so it can be
called while a job is running and again after completion. With
`stream = "both"`, stdout is returned first and stderr second; the two
files are not merged in chronological order. `tail` is applied after the
selected streams are combined. Credential-like values are redacted
before lines are returned.

## Usage

``` r
job_logs(x, tail = NULL, stream = c("both", "stdout", "stderr"))
```

## Arguments

- x:

  A `BioNeMoJob` returned by an asynchronous operation or
  [`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md).

- tail:

  `NULL` to return every available line, or a positive integer giving
  the number of final lines to return.

- stream:

  Which persisted log stream to read: `"stdout"`, `"stderr"`, or
  `"both"`.

## Value

A character vector with one log line per element. The result is empty
when no selected log file has content.

## See also

Other BioNeMo job lifecycle:
[`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md),
[`job_cancel()`](https://t-kalinowski.github.io/bionemor/reference/job_cancel.md),
[`job_path()`](https://t-kalinowski.github.io/bionemor/reference/job_path.md),
[`job_result()`](https://t-kalinowski.github.io/bionemor/reference/job_result.md),
[`job_status()`](https://t-kalinowski.github.io/bionemor/reference/job_status.md),
[`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md)

## Examples

``` r
if (FALSE) { # \dontrun{
job <- bionemo_job(
  "/shared/workspace/.bionemor/runs/my-generation"
)
job_logs(job, tail = 20L)
job_logs(job, tail = 20L, stream = "stderr")
} # }
```
