# List the pinned ESM-2 models

`esm2_models()` describes the NVIDIA ESM-2 checkpoints available to the
package. It is offline and does not download weights.

## Usage

``` r
esm2_models()
```

## Value

A data frame with model names, sizes, embedding dimensions, and
immutable Hugging Face source identifiers.

## Examples

``` r
esm2_models()
#>   name          model_size parameters context_length embedding_size
#> 1   8m    esm2_t6_8M_UR50D    8.0e+06           1024            320
#> 2  35m  esm2_t12_35M_UR50D    3.5e+07           1024            480
#> 3 150m esm2_t30_150M_UR50D    1.5e+08           1024            640
#> 4 650m esm2_t33_650M_UR50D    6.5e+08           1024           1280
#> 5   3b   esm2_t36_3B_UR50D    3.0e+09           1024           2560
#> 6  15b  esm2_t48_15B_UR50D    1.5e+10           1024           5120
#>                       source                          source_revision
#> 1    nvidia/esm2_t6_8M_UR50D 3674a6acb6c217bbeff709d182a11b196125dfc3
#> 2  nvidia/esm2_t12_35M_UR50D ca13c2d411d8aad9ea8dfa1f24a80a36a8946b5e
#> 3 nvidia/esm2_t30_150M_UR50D a2e82bef92128da1852464b0864651b1a11337d0
#> 4 nvidia/esm2_t33_650M_UR50D 118f470e7e96ba8741227ab898e54758850e9563
#> 5   nvidia/esm2_t36_3B_UR50D 0850cbdf0494675c6db7a5d0d7390ceed3b3504c
#> 6  nvidia/esm2_t48_15B_UR50D 5c74319d3e978737870f326e464f142eb2f7c666
#>   source_format
#> 1   huggingface
#> 2   huggingface
#> 3   huggingface
#> 4   huggingface
#> 5   huggingface
#> 6   huggingface
```
