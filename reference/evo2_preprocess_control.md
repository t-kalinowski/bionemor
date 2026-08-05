# Construct typed Evo 2 preprocessing controls

These controls are written to the pinned `preprocess_evo2` configuration
by
[`evo2_preprocess()`](https://t-kalinowski.github.io/bionemor/reference/evo2_preprocess.md).
The defaults retain input case, append the tokenizer's end-of-document
token, and keep each input sequence once.

## Usage

``` r
evo2_preprocess_control(
  uppercase = FALSE,
  embed_reverse_complement = FALSE,
  random_reverse_complement = 0,
  random_lineage_dropout = 0,
  transcribe = c("none", "transcribe", "back_transcribe"),
  append_eod = TRUE,
  sample_length = NULL,
  drop_empty_sequences = TRUE,
  filter_nnn = FALSE,
  taxonomy = NULL,
  prompt_spacer_length = 131072L,
  workers = 1L,
  concurrency = 100000L,
  chunk_size = 1L,
  seed = 1L
)
```

## Arguments

- uppercase:

  Whether to convert sequences to uppercase during preprocessing.

- embed_reverse_complement:

  Whether to include each sequence's reverse complement in the indexed
  data.

- random_reverse_complement:

  Probability of applying a random reverse complement to a training
  record.

- random_lineage_dropout:

  Probability of dropping taxonomy lineage fields when taxonomy prompts
  are used.

- transcribe:

  Sequence conversion: `"none"`, DNA to RNA with `"transcribe"`, or RNA
  to DNA with `"back_transcribe"`.

- append_eod:

  Whether to append an end-of-document token.

- sample_length:

  Optional fixed tokenized sample length. Use this to match
  `sequence_length` in
  [`evo2_fit_control()`](https://t-kalinowski.github.io/bionemor/reference/evo2_fit_control.md)
  when fixed records are required.

- drop_empty_sequences:

  Whether to discard empty sequences.

- filter_nnn:

  Whether to discard sequences containing `NNN`.

- taxonomy:

  Optional JSON or YAML path, data frame, or named list. Data frames
  require an `id` column. Each key is matched as a substring of the
  sequence ID, and the first matching key supplies its lineage.
  Supported fields are `domain`, `phylum`, `class`, `order`, `family`,
  `genus`, and `species`; `class` is written upstream as `clazz`.

- prompt_spacer_length:

  Character interval between the starts of repeated taxonomy prompts. It
  counts each tag plus the intervening sequence bases before
  tokenization.

- workers:

  Preprocessing worker processes.

- concurrency:

  Maximum number of preprocessing tasks in flight.

- chunk_size:

  Preprocessing tasks batched per multiprocessing worker dispatch.

- seed:

  Non-negative preprocessing seed.

## Value

An S7 `Evo2PreprocessControl`.

## References

[Pinned BioNeMo Recipes Evo 2
preprocessing](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#data-preprocessing-preprocess_evo2)

## Examples

``` r
control <- evo2_preprocess_control(
  uppercase = TRUE,
  sample_length = 1024L,
  workers = 4L,
  seed = 17L
)
control@sample_length
#> [1] 1024
```
