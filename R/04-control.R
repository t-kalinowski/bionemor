fit_control_flags <- c(
  sequence_length = "--seq-length",
  learning_rate = "--lr",
  minimum_learning_rate = "--min-lr",
  warmup_steps = "--warmup-steps",
  micro_batch_size = "--micro-batch-size",
  gradient_accumulation = "--grad-acc-batches",
  precision = "--fp8",
  clip_gradient = "--clip-grad",
  weight_decay = "--wd",
  attention_dropout = "--attention-dropout",
  hidden_dropout = "--hidden-dropout",
  validation_interval = "--val-check-interval",
  validation_batches = "--limit-val-batches",
  activation_checkpoint_layers = "--activation-checkpoint-recompute-num-layers",
  workers = "--workers",
  seed = "--seed",
  asynchronous_checkpoint = "--ckpt-async-save"
)

fit_argument_flags <- c(
  dataset = "--dataset-config",
  dataset = "-d",
  dataset_directory = "--dataset-dir",
  nodes = "--num-nodes",
  gpus = "--devices",
  model_size = "--model-size",
  output = "--result-dir",
  name = "--experiment-name",
  steps = "--max-steps",
  checkpoint = "--ckpt-dir",
  fit_control_flags
)

extra_arg_tokens <- function(extra_args) {
  tokens <- trimws(extra_args)
  tokens <- sub("[[:space:]].*$", "", tokens)
  sub("=.*$", "", tokens)
}

validate_extra_args <- function(extra_args) {
  stopifnot(
    "extra_args must be a character vector without missing values" =
      is.character(extra_args) && !anyNA(extra_args)
  )
  option_tokens <- extra_arg_tokens(extra_args)
  duplicated_fields <- unique(names(fit_argument_flags)[
    fit_argument_flags %in% option_tokens
  ])
  if (length(duplicated_fields) > 0L) {
    stop(
      "extra_args duplicates typed control ",
      paste(duplicated_fields, collapse = ", "),
      call. = FALSE
    )
  }
  extra_args
}

#' Construct typed Evo 2 fitting controls
#'
#' @param sequence_length,learning_rate,minimum_learning_rate,warmup_steps
#'   Optimization and schedule controls.
#' @param micro_batch_size,gradient_accumulation Batch controls.
#' @param precision Numerical precision.
#' @param clip_gradient,weight_decay,attention_dropout,hidden_dropout
#'   Regularization controls.
#' @param validation_interval,validation_batches Validation controls.
#' @param activation_checkpoint_layers Activation checkpointing layers.
#' @param workers,seed Data preparation controls.
#' @param split Named train, validation, and test proportions.
#' @param asynchronous_checkpoint Whether checkpoint writes are asynchronous.
#' @param extra_args Explicit additional `train_evo2` arguments.
#'
#' @return An S7 `Evo2FitControl`.
#' @export
evo2_fit_control <- function(
  sequence_length = 8192L,
  learning_rate = 1e-5,
  minimum_learning_rate = NULL,
  warmup_steps = NULL,
  micro_batch_size = 1L,
  gradient_accumulation = 1L,
  precision = c("bf16", "fp8"),
  clip_gradient = NULL,
  weight_decay = NULL,
  attention_dropout = NULL,
  hidden_dropout = NULL,
  validation_interval = NULL,
  validation_batches = NULL,
  activation_checkpoint_layers = NULL,
  workers = 1L,
  seed = 12342L,
  split = c(train = 0.9, validation = 0.05, test = 0.05),
  asynchronous_checkpoint = FALSE,
  extra_args = character()
) {
  precision <- match.arg(precision)
  stopifnot(
    "sequence_length must be a positive integer" =
      is_scalar_integerish(sequence_length, min = 1),
    "learning_rate must be positive" =
      is_scalar_number(learning_rate) && learning_rate > 0,
    "minimum_learning_rate must be NULL or non-negative" =
      is.null(minimum_learning_rate) ||
        is_scalar_number(minimum_learning_rate) && minimum_learning_rate >= 0,
    "warmup_steps must be NULL or a non-negative integer" =
      is.null(warmup_steps) || is_scalar_integerish(warmup_steps, min = 0),
    "micro_batch_size must be a positive integer" =
      is_scalar_integerish(micro_batch_size, min = 1),
    "gradient_accumulation must be a positive integer" =
      is_scalar_integerish(gradient_accumulation, min = 1),
    "clip_gradient must be NULL or non-negative" =
      is.null(clip_gradient) || is_scalar_number(clip_gradient) && clip_gradient >= 0,
    "weight_decay must be NULL or non-negative" =
      is.null(weight_decay) || is_scalar_number(weight_decay) && weight_decay >= 0,
    "attention_dropout must be NULL or between zero and one" =
      is.null(attention_dropout) ||
        is_scalar_number(attention_dropout) &&
          attention_dropout >= 0 && attention_dropout < 1,
    "hidden_dropout must be NULL or between zero and one" =
      is.null(hidden_dropout) ||
        is_scalar_number(hidden_dropout) &&
          hidden_dropout >= 0 && hidden_dropout < 1,
    "validation_interval must be NULL or a positive integer" =
      is.null(validation_interval) ||
        is_scalar_integerish(validation_interval, min = 1),
    "validation_batches must be NULL or a positive integer" =
      is.null(validation_batches) ||
        is_scalar_integerish(validation_batches, min = 1),
    "activation_checkpoint_layers must be NULL or a positive integer" =
      is.null(activation_checkpoint_layers) ||
        is_scalar_integerish(activation_checkpoint_layers, min = 1),
    "workers must be a positive integer" =
      is_scalar_integerish(workers, min = 1),
    "seed must be a non-negative integer" =
      is_scalar_integerish(seed, min = 0),
    "split must name train, validation, and test" =
      is.numeric(split) &&
        length(split) == 3L &&
        setequal(names(split), c("train", "validation", "test")),
    "split values must be finite and non-negative" =
      !anyNA(split) && all(is.finite(split)) && all(split >= 0),
    "split must sum to one" = abs(sum(split) - 1) < 1e-8,
    "asynchronous_checkpoint must be TRUE or FALSE" =
      is_scalar_logical(asynchronous_checkpoint)
  )
  extra_args <- validate_extra_args(extra_args)
  split <- split[c("train", "validation", "test")]

  Evo2FitControl(
    sequence_length = as.integer(sequence_length),
    learning_rate = as.double(learning_rate),
    minimum_learning_rate = if (is.null(minimum_learning_rate)) {
      NULL
    } else {
      as.double(minimum_learning_rate)
    },
    warmup_steps = if (is.null(warmup_steps)) NULL else as.integer(warmup_steps),
    micro_batch_size = as.integer(micro_batch_size),
    gradient_accumulation = as.integer(gradient_accumulation),
    precision = precision,
    clip_gradient = if (is.null(clip_gradient)) NULL else as.double(clip_gradient),
    weight_decay = if (is.null(weight_decay)) NULL else as.double(weight_decay),
    attention_dropout = if (is.null(attention_dropout)) {
      NULL
    } else {
      as.double(attention_dropout)
    },
    hidden_dropout = if (is.null(hidden_dropout)) {
      NULL
    } else {
      as.double(hidden_dropout)
    },
    validation_interval = if (is.null(validation_interval)) {
      NULL
    } else {
      as.integer(validation_interval)
    },
    validation_batches = if (is.null(validation_batches)) {
      NULL
    } else {
      as.integer(validation_batches)
    },
    activation_checkpoint_layers = if (is.null(activation_checkpoint_layers)) {
      NULL
    } else {
      as.integer(activation_checkpoint_layers)
    },
    workers = as.integer(workers),
    seed = as.integer(seed),
    split = as.double(split) |> stats::setNames(names(split)),
    asynchronous_checkpoint = asynchronous_checkpoint,
    extra_args = extra_args
  )
}
