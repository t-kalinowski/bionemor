# Return a completed BioNeMo job result

`job_result()` refreshes job state and materializes the portable outputs
of a successful run as the same typed R object returned by the
corresponding synchronous operation. It does not wait for an active job;
use
[`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md)
when the operation may still be running.

## Usage

``` r
job_result(x)
```

## Arguments

- x:

  A `BioNeMoJob` returned by an asynchronous operation or
  [`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md).

## Value

The operation's typed result, such as an `evo2_generation` or
`evo2_scores` data frame, an embedding matrix, an `Evo2Model`, an
`Evo2Dataset`, a `BioNeMoCheckpoint`, or a `BioNeMoArtifact`.

## Details

A failed, cancelled, active, or unknown job produces a typed bionemor
error. The condition includes the run path and available log context so
the saved run can be inspected after the error is caught.

## See also

Other BioNeMo job lifecycle:
[`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md),
[`job_cancel()`](https://t-kalinowski.github.io/bionemor/reference/job_cancel.md),
[`job_logs()`](https://t-kalinowski.github.io/bionemor/reference/job_logs.md),
[`job_path()`](https://t-kalinowski.github.io/bionemor/reference/job_path.md),
[`job_status()`](https://t-kalinowski.github.io/bionemor/reference/job_status.md),
[`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md)

## Examples

``` r
if (FALSE) { # \dontrun{
job <- bionemo_job(
  "/shared/workspace/.bionemor/runs/my-generation"
)
if (job_status(job) == "succeeded") {
  generated <- job_result(job)
}
} # }
```
