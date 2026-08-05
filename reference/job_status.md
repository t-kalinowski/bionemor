# Return a BioNeMo job state

With `refresh = TRUE`, `job_status()` reconciles the persisted state
with the execution backend. Local jobs are checked against their runner
process, and Slurm jobs are checked with scheduler accounting. Terminal
states are persisted in the run directory. With `refresh = FALSE`, the
function returns the state held by the job handle without a routine
backend query; that value may be stale. Reopening a job with
[`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md)
loads its persisted state.

## Usage

``` r
job_status(x, refresh = TRUE)
```

## Arguments

- x:

  A `BioNeMoJob` returned by an asynchronous operation or
  [`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md).

- refresh:

  Whether to query the execution backend for current state. Set this to
  `FALSE` to read the state held by the job handle without a routine
  backend query.

## Value

A length-one character vector containing the job state.

## States

A job has one of these state strings:

- `"created"`: the run directory has been initialized.

- `"submitted"`: the backend accepted the job and it is waiting to
  start.

- `"starting"`: the local runner is starting the operation.

- `"running"`: the operation is running or finalizing its outputs.

- `"succeeded"`: the operation completed and its result can be read.

- `"failed"`: the operation ended with an error.

- `"cancelled"`: cancellation was confirmed.

- `"unknown"`: the backend state could not be mapped to a supported
  state.

The terminal states are `"succeeded"`, `"failed"`, and `"cancelled"`.
[`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md)
and
[`job_result()`](https://t-kalinowski.github.io/bionemor/reference/job_result.md)
report non-success states as typed bionemor errors.

## See also

Other BioNeMo job lifecycle:
[`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md),
[`job_cancel()`](https://t-kalinowski.github.io/bionemor/reference/job_cancel.md),
[`job_logs()`](https://t-kalinowski.github.io/bionemor/reference/job_logs.md),
[`job_path()`](https://t-kalinowski.github.io/bionemor/reference/job_path.md),
[`job_result()`](https://t-kalinowski.github.io/bionemor/reference/job_result.md),
[`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md)

## Examples

``` r
if (FALSE) { # \dontrun{
job <- bionemo_job(
  "/shared/workspace/.bionemor/runs/my-generation"
)
job_status(job)
job_status(job, refresh = FALSE)
} # }
```
