# Inspect checkpoint metadata

`checkpoint_path()` returns the normalized path to checkpoint weights.
`checkpoint_manifest()` reads the adjacent bionemor manifest, including
the checkpoint identity, format, source, recipe revision, and
provenance.

## Usage

``` r
checkpoint_path(x)

checkpoint_manifest(x)
```

## Arguments

- x:

  A checkpoint, model, or one checkpoint path.

## Value

`checkpoint_path()` returns one normalized path. `checkpoint_manifest()`
returns a named list containing checkpoint identity, provenance, format,
recipe revision, source revision, and inspection metadata.

## See also

[`evo2_checkpoint()`](https://t-kalinowski.github.io/bionemor/reference/evo2_checkpoint.md)

## Examples

``` r
checkpoint_path("checkpoints/evo2-7b")
#> [1] "/home/runner/work/bionemor/bionemor/docs/reference/checkpoints/evo2-7b"
```
