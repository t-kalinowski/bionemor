# Wait for a BioNeMo job and return its result

`job_wait()` refreshes job state every `poll` seconds until the run
succeeds, reaches a non-success state, or the wait reaches `timeout`. A
successful run is materialized as the same typed R object returned by
the corresponding operation with `async = FALSE`. Failed, cancelled, and
unknown states produce typed bionemor errors with the run path and
available log context.

## Usage

``` r
job_wait(x, poll = 2, timeout = Inf)
```

## Arguments

- x:

  A `BioNeMoJob` returned by an asynchronous operation or
  [`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md).

- poll:

  Positive polling interval in seconds.

- timeout:

  Positive maximum number of seconds to spend waiting, or `Inf` to wait
  without a limit. A wait timeout does not cancel the job.

## Value

The operation's typed result.

## Details

`timeout` limits only this call to `job_wait()`. It is measured from the
time the call starts, does not change the operation's own execution
timeout, and does not cancel the job. After a wait timeout, use
[`job_status()`](https://t-kalinowski.github.io/bionemor/reference/job_status.md)
or
[`job_logs()`](https://t-kalinowski.github.io/bionemor/reference/job_logs.md)
to inspect the still-running job, call `job_wait()` again, or cancel it
explicitly with
[`job_cancel()`](https://t-kalinowski.github.io/bionemor/reference/job_cancel.md).

## See also

Other BioNeMo job lifecycle:
[`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md),
[`job_cancel()`](https://t-kalinowski.github.io/bionemor/reference/job_cancel.md),
[`job_logs()`](https://t-kalinowski.github.io/bionemor/reference/job_logs.md),
[`job_path()`](https://t-kalinowski.github.io/bionemor/reference/job_path.md),
[`job_result()`](https://t-kalinowski.github.io/bionemor/reference/job_result.md),
[`job_status()`](https://t-kalinowski.github.io/bionemor/reference/job_status.md)

## Examples

``` r
if (FALSE) { # \dontrun{
compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "/shared/workspace")
model <- evo2(
  "7b",
  checkpoint = "/shared/workspace/checkpoints/evo2-7b"
)
job <- evo2_generate(
  model,
  c(example = "ACGT"),
  compute,
  num_tokens = 32L,
  async = TRUE
)

generated <- job_wait(job, poll = 2, timeout = 600)
} # }
```
