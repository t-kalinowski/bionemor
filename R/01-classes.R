BioNeMoRecipe <- new_class(
  "BioNeMoRecipe",
  package = "bionemor",
  properties = list(
    adapter = prop_string(),
    repository = prop_string(),
    revision = prop_string(),
    recipe_version = prop_string(),
    subdirectory = prop_string(),
    base_image = prop_string(),
    base_image_digest = prop_string(allow_null = TRUE),
    bridge_protocol = prop_integer(min = 1L),
    verified = prop_bool(FALSE)
  )
)

BioNeMoModel <- new_class(
  "BioNeMoModel",
  package = "bionemor",
  abstract = TRUE,
  properties = list(
    family = prop_string(),
    checkpoint = new_property(class = class_any, default = NULL),
    compute = new_property(class = class_any, default = NULL),
    task = prop_string(),
    config = prop_list(),
    provenance = prop_list()
  )
)

BioNeMoCheckpoint <- new_class(
  "BioNeMoCheckpoint",
  package = "bionemor",
  properties = list(
    path = prop_string(),
    format = prop_string(),
    kind = prop_string(),
    family = prop_string(),
    variant = prop_string(),
    source = prop_string(allow_null = TRUE),
    source_format = prop_string(allow_null = TRUE),
    source_revision = prop_string(allow_null = TRUE),
    recipe_revision = prop_string(),
    base_checkpoint = new_property(class = class_any, default = NULL),
    manifest = prop_string(allow_null = TRUE),
    provenance = prop_list()
  )
)

BioNeMoCompute <- new_class(
  "BioNeMoCompute",
  package = "bionemor",
  properties = list(
    backend = prop_string(),
    engine = prop_string(),
    workspace = prop_string(),
    recipe = BioNeMoRecipe,
    image = prop_string(allow_null = TRUE),
    image_digest = prop_string(allow_null = TRUE),
    gpus = prop_integer(1L, min = 1L),
    nodes = prop_integer(1L, min = 1L),
    queue = prop_string(allow_null = TRUE),
    account = prop_string(allow_null = TRUE),
    walltime = prop_string(allow_null = TRUE),
    config = prop_list()
  )
)

BioNeMoJob <- new_class(
  "BioNeMoJob",
  package = "bionemor",
  properties = list(
    path = prop_string(),
    id = prop_string(),
    kind = prop_string(),
    state = prop_string(),
    compute = BioNeMoCompute,
    command_plan = new_property(class = class_any),
    log = prop_string(allow_null = TRUE),
    expected_result = new_property(class = class_any, default = NULL),
    timeout = class_double,
    process = new_property(class = class_any, default = NULL),
    metadata = prop_list()
  )
)

BioNeMoArtifact <- new_class(
  "BioNeMoArtifact",
  package = "bionemor",
  properties = list(
    path = prop_string(),
    format = prop_string(),
    kind = prop_string(),
    shape = new_property(class = class_any, default = NULL),
    schema = new_property(class = class_any, default = NULL),
    metadata = prop_list(),
    provenance = prop_list()
  )
)

BioNeMoWorkflow <- new_class(
  "BioNeMoWorkflow",
  package = "bionemor",
  properties = list(
    id = prop_string(),
    adapter = prop_string(),
    adapter_version = prop_integer(min = 1L),
    family = prop_string(),
    task = prop_string(),
    protocol_version = prop_integer(min = 1L),
    input_schema = prop_string(),
    result_schema = prop_string()
  )
)

BioNeMoDoctor <- new_class(
  "BioNeMoDoctor",
  package = "bionemor",
  properties = list(
    target = prop_string(),
    ok = prop_bool(FALSE),
    checks = class_data.frame,
    verbose = prop_bool(TRUE)
  )
)
