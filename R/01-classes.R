BioNeMoModel <- new_class(
  "BioNeMoModel",
  package = "bionemor",
  abstract = TRUE,
  properties = list(
    family = prop_string(),
    checkpoint = new_property(class = class_any, default = NULL),
    pretrained = prop_bool(TRUE),
    task = prop_string(allow_null = TRUE),
    config = prop_list(),
    provenance = prop_list()
  )
)

Evo2Model <- new_class(
  "Evo2Model",
  package = "bionemor",
  parent = BioNeMoModel,
  properties = list(
    size = prop_string()
  )
)

BioNeMoCheckpoint <- new_class(
  "BioNeMoCheckpoint",
  package = "bionemor",
  properties = list(
    path = prop_string(),
    format = prop_string(),
    family = prop_string(),
    variant = prop_string(),
    source = prop_string(),
    profile = prop_string(),
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
    image = prop_string(allow_null = TRUE),
    gpus = prop_integer(1L, min = 1L),
    nodes = prop_integer(1L, min = 1L),
    queue = prop_string(allow_null = TRUE),
    account = prop_string(allow_null = TRUE),
    walltime = prop_string(allow_null = TRUE),
    profile = prop_string(),
    config = prop_list()
  )
)

BioNeMoJob <- new_class(
  "BioNeMoJob",
  package = "bionemor",
  properties = list(
    id = prop_string(),
    kind = prop_string(),
    state = prop_string(),
    compute = BioNeMoCompute,
    command = prop_string(),
    log = prop_string(allow_null = TRUE),
    expected_result = new_property(class = class_any, default = NULL),
    timeout = class_double,
    process = new_property(class = class_any, default = NULL),
    metadata = prop_list()
  )
)

BioNeMoPrediction <- new_class(
  "BioNeMoPrediction",
  package = "bionemor",
  properties = list(
    type = prop_string(),
    data = new_property(class = class_any, default = NULL),
    provenance = prop_list(),
    metadata = prop_list()
  )
)

BioNeMoArtifact <- new_class(
  "BioNeMoArtifact",
  package = "bionemor",
  properties = list(
    path = prop_string(),
    format = prop_string(),
    metadata = prop_list()
  )
)

BioNeMoSetupPlan <- new_class(
  "BioNeMoSetupPlan",
  package = "bionemor",
  properties = list(
    target = prop_string(),
    compute = BioNeMoCompute,
    model = new_property(class = class_any, default = NULL),
    path = prop_string(),
    files = class_character,
    commands = class_character,
    executed = prop_bool(FALSE)
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

Evo2FitControl <- new_class(
  "Evo2FitControl",
  package = "bionemor",
  properties = list(
    sequence_length = prop_integer(min = 1L),
    learning_rate = class_double,
    minimum_learning_rate = prop_double(allow_null = TRUE),
    warmup_steps = prop_integer(allow_null = TRUE, min = 0L),
    micro_batch_size = prop_integer(min = 1L),
    gradient_accumulation = prop_integer(min = 1L),
    precision = prop_string(),
    clip_gradient = prop_double(allow_null = TRUE),
    weight_decay = prop_double(allow_null = TRUE),
    attention_dropout = prop_double(allow_null = TRUE),
    hidden_dropout = prop_double(allow_null = TRUE),
    validation_interval = prop_integer(allow_null = TRUE, min = 1L),
    validation_batches = prop_integer(allow_null = TRUE, min = 1L),
    activation_checkpoint_layers = prop_integer(allow_null = TRUE, min = 1L),
    workers = prop_integer(min = 1L),
    seed = prop_integer(min = 0L),
    split = class_double,
    asynchronous_checkpoint = prop_bool(FALSE),
    extra_args = class_character
  )
)
