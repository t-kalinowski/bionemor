# Construct an Evo 2 phylogenetic prompt tag

Evo 2 represents taxonomy context as a pipe-enclosed,
semicolon-delimited tag with the short rank keys `d`, `p`, `c`, `o`,
`f`, `g`, and `s`. Missing ranks are serialized as `None`. With
`uppercase = TRUE`, the complete serialized tag, including missing-rank
markers, is converted to uppercase.

## Usage

``` r
evo2_phylo_tag(
  domain = NULL,
  phylum = NULL,
  class = NULL,
  order = NULL,
  family = NULL,
  genus = NULL,
  species = NULL,
  uppercase = TRUE
)
```

## Arguments

- domain, phylum, class, order, family, genus, species:

  Optional taxonomy ranks. Values cannot contain `;`, `|`, or line
  breaks.

- uppercase:

  Whether to uppercase the serialized tag.

## Value

One Evo 2 phylogenetic prompt tag.

## Examples

``` r
evo2_phylo_tag(
  domain = "Bacteria",
  phylum = "Proteobacteria",
  genus = "Escherichia"
)
#> [1] "|D__BACTERIA;P__PROTEOBACTERIA;C__NONE;O__NONE;F__NONE;G__ESCHERICHIA;S__NONE|"
```
