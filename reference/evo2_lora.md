# Describe Evo 2 LoRA fine-tuning

LoRA freezes the dense base model and attaches trainable low-rank
matrices to selected linear modules. `targets` names R-level groups
rather than prediction outcomes or individual layers:

## Usage

``` r
evo2_lora(
  rank = 16L,
  alpha = 32,
  dropout = 0.1,
  targets = c("hyena", "attention", "mlp"),
  fully_trainable = character()
)
```

## Arguments

- rank:

  LoRA rank `r`, which controls the adapter bottleneck dimension.

- alpha:

  LoRA scaling numerator. The effective scale is `alpha / rank`.

- dropout:

  Dropout probability applied on the LoRA path.

- targets:

  One or more of `"hyena"`, `"attention"`, and `"mlp"`.

- fully_trainable:

  Plain upstream module names to train without adapters. A name cannot
  also be selected through `targets`. Evo 2 ties `word_embeddings` and
  `output_layer`, so those two names must be supplied together.

## Value

An S7 `Evo2LoRA`.

## Details

- `"hyena"` expands to `dense_projection` and `dense` in Hyena mixers.

- `"attention"` expands to `linear_qkv` and `linear_proj`.

- `"mlp"` expands to `linear_fc1` and `linear_fc2`.

The default adapts all three groups. A module named in `fully_trainable`
remains unfrozen and receives no adapter. Raw upstream wildcard target
patterns are intentionally not part of this interface.

## References

[Pinned BioNeMo Recipes Evo 2 LoRA
fine-tuning](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#lora-fine-tuning)

## Examples

``` r
# Adapt every supported inner projection (the default).
evo2_lora(rank = 16L, alpha = 32)
#> <bionemor::Evo2LoRA>
#>  @ kind           : chr "lora"
#>  @ rank           : int 16
#>  @ alpha          : num 32
#>  @ dropout        : num 0.1
#>  @ targets        : chr [1:3] "hyena" "attention" "mlp"
#>  @ fully_trainable: chr(0) 

# Adapt only the attention and MLP projections.
evo2_lora(
  rank = 8L,
  alpha = 16,
  dropout = 0,
  targets = c("attention", "mlp")
)
#> <bionemor::Evo2LoRA>
#>  @ kind           : chr "lora"
#>  @ rank           : int 8
#>  @ alpha          : num 16
#>  @ dropout        : num 0
#>  @ targets        : chr [1:2] "attention" "mlp"
#>  @ fully_trainable: chr(0) 

# Train the tied vocabulary weights directly alongside Hyena adapters.
evo2_lora(
  targets = "hyena",
  fully_trainable = c("word_embeddings", "output_layer")
)
#> <bionemor::Evo2LoRA>
#>  @ kind           : chr "lora"
#>  @ rank           : int 16
#>  @ alpha          : num 32
#>  @ dropout        : num 0.1
#>  @ targets        : chr "hyena"
#>  @ fully_trainable: chr [1:2] "word_embeddings" "output_layer"
```
