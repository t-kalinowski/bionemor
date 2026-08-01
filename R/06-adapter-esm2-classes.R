Esm2Model <- new_class(
  "Esm2Model",
  package = "bionemor",
  parent = BioNeMoModel,
  properties = list(
    size = prop_string(),
    model_size = prop_string(),
    context_length = prop_integer(min = 1L),
    embedding_size = prop_integer(min = 1L),
    revision = prop_string()
  )
)
