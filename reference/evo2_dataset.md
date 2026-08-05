# Describe an Evo 2 dataset

`evo2_dataset()` records sequence partitions before the recipe converts
them to indexed training data. Each input may be a named character
vector, a data frame, an `XStringSet` such as `DNAStringSet`, or an
existing FASTA or gzip-compressed FASTA path.

## Usage

``` r
evo2_dataset(
  train,
  validation = NULL,
  test = NULL,
  split = c(train = 0.8, validation = 0.1, test = 0.1),
  seed = 1L,
  id_col = "id",
  sequence_col = "sequence"
)
```

## Arguments

- train, validation, test:

  Sequence inputs. `train` is required. `validation` and `test` may be
  `NULL`.

- split:

  Named train, validation, and test proportions used when both explicit
  validation and test inputs are absent. Assignment uses a stable hash
  of `seed` and sequence ID, so it is unchanged by input order. The
  proportions are probabilities and need not produce exact row counts.

- seed:

  Non-negative seed used for stable hash partitioning.

- id_col, sequence_col:

  Data-frame column names containing sequence IDs and sequence text.

## Value

An S7 `Evo2Dataset`.

## Details

Character-vector names become sequence IDs; unnamed vectors receive
`seq_1`, `seq_2`, and so on. Data frames use `id_col` and
`sequence_col`. IDs must be non-empty and unique, and sequences must be
non-empty.

## Examples

``` r
sequences <- c(
  first = "ACGTACGT",
  second = "TGCATGCA",
  third = "GATTACA"
)
data <- evo2_dataset(
  sequences,
  split = c(train = 0.7, validation = 0.2, test = 0.1),
  seed = 17L
)
data@provenance$partition_method
#> [1] "stable-hash"
```
