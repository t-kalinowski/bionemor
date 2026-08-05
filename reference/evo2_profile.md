# Write positional Evo 2 log-probability profiles

Profiles retain one log probability per scored nucleotide. Positions are
one-based coordinates in the supplied sequence. Reverse-strand positions
are mapped back to those coordinates; `strand = "both"` averages forward
and reverse values at each aligned position. Context parallelism must be
one.

## Usage

``` r
evo2_profile(
  object,
  newdata,
  compute = NULL,
  strand = c("forward", "reverse", "both"),
  batch_size = 1L,
  normalize = c("dna", "none"),
  control = evo2_inference_control(),
  output = NULL,
  name = NULL,
  async = FALSE
)
```

## Arguments

- object:

  An Evo 2 model with an explicit checkpoint.

- newdata:

  Sequences or a FASTA path.

- compute:

  A BioNeMo compute descriptor. `NULL` uses the descriptor attached by
  [`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md)
  or a previous fine-tuning run.

- strand:

  Score the supplied sequence, its reverse complement, or both.

- batch_size:

  Prediction micro-batch size.

- normalize:

  Sequence normalization mode. With `normalize = "none"`, reverse-strand
  operations require uppercase IUPAC DNA.

- control:

  Controls from
  [`evo2_inference_control()`](https://t-kalinowski.github.io/bionemor/reference/evo2_inference_control.md).

- output:

  Required Parquet output path inside the compute workspace for
  container execution.

- name:

  Optional run name.

- async:

  Whether to return before completion.

## Value

With `async = FALSE`, a `BioNeMoArtifact` for a Parquet file with
columns `id` (string), `position` (int64), `base` (string),
`log_probability` (double), and `strand` (string). With `async = TRUE`,
a `BioNeMoJob`.

## Examples

``` r
if (FALSE) { # \dontrun{
compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
compute <- bionemo_install(compute)
model <- evo2_model("7b", compute)

profile <- evo2_profile(
  model,
  c(reference = "ACGTACGT"),
  strand = "both",
  output = "outputs/reference-profile.parquet"
)
profile@path
} # }
```
