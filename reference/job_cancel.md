# Cancel a BioNeMo job

`job_cancel()` asks the local or Slurm backend to stop an active job and
waits for the backend to report a terminal state. The default requests
an orderly termination; local execution escalates if the process does
not stop. `force = TRUE` requests immediate termination instead. A job
that is already terminal is returned unchanged.

## Usage

``` r
job_cancel(x, force = FALSE)
```

## Arguments

- x:

  A `BioNeMoJob` returned by an asynchronous operation or
  [`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md).

- force:

  Whether to request immediate termination. The default first requests
  an orderly stop.

## Value

`x`, updated to the backend's terminal state, invisibly.

## Details

Cancellation races with normal completion. The final state may therefore
be `"succeeded"` or `"failed"` if the operation finishes before
cancellation is confirmed. Cancelling does not delete the run directory,
logs, or outputs.

## See also

Other BioNeMo job lifecycle:
[`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md),
[`job_logs()`](https://t-kalinowski.github.io/bionemor/reference/job_logs.md),
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
job_cancel(job)
job_status(job)

# To skip orderly termination, use this instead on an active job:
# job_cancel(job, force = TRUE)
} # }
```
