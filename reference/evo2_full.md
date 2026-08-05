# Describe full Evo 2 fine-tuning

Full fine-tuning updates the supported parameters in the base checkpoint
instead of adding LoRA adapters. It requires more accelerator memory
than
[`evo2_lora()`](https://t-kalinowski.github.io/bionemor/reference/evo2_lora.md).

## Usage

``` r
evo2_full()
```

## Value

An S7 `Evo2FullFineTune`.

## Examples

``` r
evo2_full()
#> <bionemor::Evo2FullFineTune>
#>  @ kind: chr "full"
```
