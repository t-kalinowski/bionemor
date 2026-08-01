Evo2Model <- new_class(
  "Evo2Model",
  package = "bionemor",
  parent = BioNeMoModel,
  properties = list(
    size = prop_string(),
    model_size = prop_string(),
    context_length = prop_integer(min = 1L),
    revision = prop_string()
  )
)

Evo2Dataset <- new_class(
  "Evo2Dataset",
  package = "bionemor",
  properties = list(
    train = new_property(class = class_any),
    validation = new_property(class = class_any, default = NULL),
    test = new_property(class = class_any, default = NULL),
    split = class_double,
    seed = prop_integer(min = 0L),
    id_col = prop_string(),
    sequence_col = prop_string(),
    prepared = prop_bool(FALSE),
    path = prop_string(allow_null = TRUE),
    manifest = prop_list(),
    provenance = prop_list()
  )
)

Evo2FineTuneMethod <- new_class(
  "Evo2FineTuneMethod",
  package = "bionemor",
  abstract = TRUE,
  properties = list(
    kind = prop_string()
  )
)

Evo2LoRA <- new_class(
  "Evo2LoRA",
  package = "bionemor",
  parent = Evo2FineTuneMethod,
  properties = list(
    rank = prop_integer(min = 1L),
    alpha = class_double,
    dropout = class_double,
    targets = class_character,
    fully_trainable = class_character
  )
)

Evo2FullFineTune <- new_class(
  "Evo2FullFineTune",
  package = "bionemor",
  parent = Evo2FineTuneMethod
)

Evo2InferenceControl <- new_class(
  "Evo2InferenceControl",
  package = "bionemor",
  properties = list(
    tensor_parallel_size = prop_integer(min = 1L),
    context_parallel_size = prop_integer(min = 1L),
    max_sequence_length = prop_integer(allow_null = TRUE, min = 1L),
    max_batch_size = prop_integer(min = 1L),
    precision = prop_string(),
    mixed_precision_recipe = prop_string(allow_null = TRUE),
    vortex_style_fp8 = prop_string(),
    cuda_graphs = prop_string(),
    subquadratic_ops = prop_bool(FALSE),
    chunked_prefill = prop_bool(FALSE),
    dynamic_max_tokens = prop_integer(allow_null = TRUE, min = 1L),
    dynamic_block_size = prop_integer(min = 1L),
    extra = prop_list()
  )
)

Evo2PreprocessControl <- new_class(
  "Evo2PreprocessControl",
  package = "bionemor",
  properties = list(
    uppercase = prop_bool(FALSE),
    embed_reverse_complement = prop_bool(FALSE),
    random_reverse_complement = class_double,
    random_lineage_dropout = class_double,
    transcribe = prop_string(),
    append_eod = prop_bool(TRUE),
    sample_length = prop_integer(allow_null = TRUE, min = 1L),
    drop_empty_sequences = prop_bool(TRUE),
    filter_nnn = prop_bool(FALSE),
    taxonomy = new_property(class = class_any, default = NULL),
    prompt_spacer_length = prop_integer(min = 0L),
    workers = prop_integer(min = 1L),
    concurrency = prop_integer(min = 1L),
    chunk_size = prop_integer(min = 1L),
    seed = prop_integer(min = 0L)
  )
)

Evo2FitControl <- new_class(
  "Evo2FitControl",
  package = "bionemor",
  properties = list(
    sequence_length = prop_integer(min = 1L),
    global_batch_size = prop_integer(min = 1L),
    micro_batch_size = prop_integer(min = 1L),
    learning_rate = class_double,
    minimum_learning_rate = class_double,
    warmup_steps = prop_integer(min = 0L),
    decay_steps = prop_integer(allow_null = TRUE, min = 1L),
    constant_steps = prop_integer(min = 0L),
    weight_decay = class_double,
    eval_interval = prop_integer(min = 1L),
    eval_iters = prop_integer(min = 1L),
    log_interval = prop_integer(min = 1L),
    tensor_parallel_size = prop_integer(min = 1L),
    pipeline_parallel_size = prop_integer(min = 1L),
    context_parallel_size = prop_integer(min = 1L),
    precision = prop_string(),
    mixed_precision_recipe = prop_string(allow_null = TRUE),
    precision_aware_optimizer = prop_bool(FALSE),
    bf16_main_gradients = prop_bool(FALSE),
    gradient_reduce_fp32 = prop_bool(FALSE),
    activation_checkpointing = prop_string(),
    activation_checkpoint_layers = prop_integer(allow_null = TRUE, min = 1L),
    overlap_parameter_gather = prop_bool(FALSE),
    overlap_gradient_reduce = prop_bool(FALSE),
    subquadratic_ops = prop_bool(FALSE),
    clip_gradient = class_double,
    hidden_dropout = class_double,
    attention_dropout = class_double,
    checkpoint_async = prop_bool(FALSE),
    keep_checkpoints = class_integer,
    workers = prop_integer(min = 1L),
    seed = prop_integer(min = 0L),
    dataset_seed = prop_integer(allow_null = TRUE, min = 0L),
    extra = prop_list()
  )
)
