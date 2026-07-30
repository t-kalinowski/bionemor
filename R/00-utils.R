`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

bionemor_condition_codes <- c(
  "BN_RECIPE_MISSING",
  "BN_RECIPE_MISMATCH",
  "BN_IMAGE_BUILD",
  "BN_RUNTIME_MISSING",
  "BN_NO_GPU",
  "BN_GPU_INCOMPATIBLE",
  "BN_GPU_MEMORY",
  "BN_PRECISION_INCOMPATIBLE",
  "BN_MODEL_UNKNOWN",
  "BN_CHECKPOINT_SOURCE",
  "BN_CHECKPOINT_FORMAT",
  "BN_CHECKPOINT_INCOMPLETE",
  "BN_BASE_CHECKPOINT_MISSING",
  "BN_TOKENIZER_MISMATCH",
  "BN_INVALID_SEQUENCE",
  "BN_CONTEXT_LIMIT",
  "BN_OUTPUT_SCHEMA",
  "BN_NONFINITE_OUTPUT",
  "BN_UPSTREAM",
  "BN_CANCELLED",
  "BN_TIMEOUT",
  "BN_PROTOCOL"
)

value_origin_codes <- c(
  "user_requested",
  "package_default",
  "adapter_default",
  "auto_resolved"
)

argument_origin_map <- function(
  values,
  call,
  argument_map = stats::setNames(names(values), names(values)),
  adapter_defaults = character(),
  auto_resolved = character()
) {
  stopifnot(
    "values must be a named list" =
      is.list(values) &&
        !is.null(names(values)) &&
        all(nzchar(names(values))),
    "call must be a matched call" = is.call(call),
    "argument map must name request fields" =
      is.character(argument_map) &&
        !is.null(names(argument_map)) &&
        all(nzchar(names(argument_map)))
  )
  supplied <- setdiff(names(call), c("", "..."))
  origins <- stats::setNames(
    rep("package_default", length(values)),
    names(values)
  )
  mapped <- intersect(names(argument_map), names(origins))
  origins[mapped] <- ifelse(
    unname(argument_map[mapped]) %in% supplied,
    "user_requested",
    "package_default"
  )
  origins[intersect(adapter_defaults, names(origins))] <- "adapter_default"
  origins[intersect(auto_resolved, names(origins))] <- "auto_resolved"
  stopifnot(
    "value origin is unsupported" =
      all(unlist(origins, use.names = FALSE) %in% value_origin_codes)
  )
  as.list(origins)
}

set_object_value_origins <- function(object, origins) {
  attr(object, "bionemor_value_origins") <- origins
  object
}

object_value_origins <- function(object, fallback = "user_requested") {
  attr(object, "bionemor_value_origins", exact = TRUE) %||%
    stats::setNames(
      as.list(rep(fallback, length(S7::prop_names(object)))),
      S7::prop_names(object)
    )
}

bionemor_abort <- function(code, message, ..., call = NULL) {
  stopifnot(
    "code must be a registered BioNeMo condition code" =
      is_scalar_string(code) && code %in% bionemor_condition_codes,
    "message must be one non-empty string" = is_scalar_string(message)
  )
  fields <- list(...)
  stopifnot(
    "condition fields must be named" =
      length(fields) == 0L ||
        !is.null(names(fields)) &&
          all(nzchar(names(fields)))
  )
  fields <- fields[!vapply(fields, is.null, logical(1))]
  condition <- structure(
    c(
      list(message = message, call = call, code = code),
      fields
    ),
    class = c(code, "bionemor_error", "error", "condition")
  )
  stop(condition)
}

is_scalar_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

is_scalar_logical <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

is_scalar_number <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
}

is_scalar_integerish <- function(x, min = NULL) {
  is_scalar_number(x) &&
    x == trunc(x) &&
    abs(x) <= .Machine$integer.max &&
    (is.null(min) || x >= min)
}

pluck_vec <- function(x, field, ptype) {
  stopifnot(
    "x must be a list" = is.list(x),
    "field must be one non-empty string" = is_scalar_string(field),
    "ptype must be one atomic value" = is.atomic(ptype) && length(ptype) == 1L
  )
  vapply(x, function(element) element[[field]], ptype)
}

pluck_chr <- function(x, field) {
  pluck_vec(x, field, "")
}

pluck_int <- function(x, field) {
  pluck_vec(x, field, 0L)
}

pluck_dbl <- function(x, field) {
  pluck_vec(x, field, 0)
}

normalize_path <- function(path, base = getwd()) {
  stopifnot(
    "path must be one non-empty string" = is_scalar_string(path),
    "base must be one non-empty string" = is_scalar_string(base)
  )
  path <- path.expand(path)
  absolute <- grepl("^/|^[A-Za-z]:[/\\\\]|^\\\\\\\\", path)
  if (!absolute) {
    path <- file.path(base, path)
  }

  suffix <- character()
  existing <- path
  while (!file.exists(existing)) {
    parent <- dirname(existing)
    if (identical(parent, existing)) {
      break
    }
    suffix <- c(basename(existing), suffix)
    existing <- parent
  }
  existing <- normalizePath(existing, mustWork = TRUE)
  if (length(suffix) == 0L) {
    existing
  } else {
    do.call(file.path, as.list(c(existing, suffix)))
  }
}

path_is_within <- function(path, directory) {
  path <- normalize_path(path)
  directory <- normalize_path(directory)
  identical(path, directory) ||
    startsWith(path, paste0(directory, .Platform$file.sep))
}

shell_join <- function(command, args = character()) {
  paste(vapply(c(command, args), shQuote, character(1)), collapse = " ")
}

credential_environment_variables <- c(
  "NGC_API_KEY",
  "NGC_CLI_API_KEY",
  "HF_TOKEN",
  "HUGGING_FACE_HUB_TOKEN"
)

process_environment <- function(allow = character()) {
  stopifnot(
    "allow contains an unsupported credential environment variable" =
      all(allow %in% credential_environment_variables)
  )
  environment <- Sys.getenv()
  excluded <- setdiff(credential_environment_variables, allow)
  environment[!names(environment) %in% excluded]
}

redact_credentials <- function(x) {
  secrets <- unique(unname(Sys.getenv(
    credential_environment_variables,
    unset = ""
  )))
  secrets <- secrets[nzchar(secrets)]
  for (secret in secrets) {
    x <- gsub(secret, "[REDACTED]", x, fixed = TRUE)
  }
  x
}

redact_text_file <- function(path) {
  if (!file.exists(path)) {
    return(invisible(path))
  }
  contents <- readLines(path, warn = FALSE)
  redacted <- redact_credentials(contents)
  if (!identical(contents, redacted)) {
    writeLines(redacted, path, useBytes = TRUE)
  }
  invisible(path)
}

prop_string <- function(default = NULL, allow_null = FALSE) {
  force(allow_null)
  new_property(
    class = if (allow_null) NULL | class_character else class_character,
    default = if (is.null(default) && !allow_null) {
      quote(stop("Required"))
    } else {
      default
    },
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return(NULL)
      }
      if (!is_scalar_string(value)) {
        "must be one non-empty string"
      }
    }
  )
}

prop_bool <- function(default) {
  new_property(
    class = class_logical,
    default = default,
    validator = function(value) {
      if (!is_scalar_logical(value)) {
        "must be TRUE or FALSE"
      }
    }
  )
}

prop_integer <- function(default = NULL, min = NULL, allow_null = FALSE) {
  force(min)
  force(allow_null)
  new_property(
    class = if (allow_null) NULL | class_integer else class_integer,
    default = if (is.null(default) && !allow_null) {
      quote(stop("Required"))
    } else {
      default
    },
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return(NULL)
      }
      if (!is_scalar_integerish(value, min = min)) {
        if (is.null(min)) "must be one integer" else paste0("must be at least ", min)
      }
    }
  )
}

prop_double <- function(default = NULL, allow_null = FALSE) {
  force(allow_null)
  new_property(
    class = if (allow_null) NULL | class_double else class_double,
    default = if (is.null(default) && !allow_null) {
      quote(stop("Required"))
    } else {
      default
    },
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return(NULL)
      }
      if (!is_scalar_number(value)) {
        "must be one finite number"
      }
    }
  )
}

prop_list <- function() {
  new_property(class = class_list, default = quote(list()))
}

atomic_write_lines <- function(text, path) {
  stopifnot(
    "path must be one non-empty string" = is_scalar_string(path),
    "text must be a character vector" = is.character(text)
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    paste0(".", basename(path), "-"),
    tmpdir = dirname(path)
  )
  on.exit(unlink(temporary), add = TRUE)
  writeLines(text, temporary, useBytes = TRUE)
  stopifnot(
    "failed to atomically replace output file" =
      file.rename(temporary, path)
  )
  invisible(path)
}

atomic_write_json <- function(
  value,
  path,
  auto_unbox = TRUE,
  pretty = TRUE
) {
  text <- jsonlite::toJSON(
    value,
    auto_unbox = auto_unbox,
    pretty = pretty,
    null = "null",
    na = "null",
    digits = NA
  )
  atomic_write_lines(text, path)
}

read_json_file <- function(path, simplify = TRUE) {
  stopifnot(
    "JSON file does not exist" = is_scalar_string(path) && file.exists(path)
  )
  jsonlite::read_json(path, simplifyVector = simplify)
}

safe_name <- function(name, prefix) {
  stopifnot(
    "prefix must be one safe name" =
      is_scalar_string(prefix) && grepl("^[A-Za-z0-9_.-]+$", prefix)
  )
  if (is.null(name)) {
    stamp <- format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")
    suffix <- sprintf("%06d", sample.int(999999L, 1L))
    return(paste(prefix, stamp, suffix, sep = "-"))
  }
  stopifnot(
    "name must be one safe name" =
      is_scalar_string(name) &&
        grepl("^[A-Za-z0-9_.-]+$", name) &&
        !(name %in% c(".", ".."))
  )
  name
}

path_digest <- function(path) {
  stopifnot(
    "path must exist" = is_scalar_string(path) && file.exists(path)
  )
  path <- normalize_path(path)
  if (dir.exists(path)) {
    files <- list.files(
      path,
      recursive = TRUE,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    files <- files[!dir.exists(files)]
    relative <- substring(files, nchar(path) + 2L)
    records <- paste(
      relative,
      as.character(tools::md5sum(files)),
      sep = ":"
    )
    temporary <- tempfile("bionemor-directory-digest-")
    on.exit(unlink(temporary), add = TRUE)
    writeLines(sort(records), temporary, useBytes = TRUE)
    return(unname(tools::md5sum(temporary)))
  }
  unname(tools::md5sum(path))
}

stable_partition_value <- function(seed, id) {
  stopifnot(
    "seed must be a non-negative integer" =
      is_scalar_integerish(seed, min = 0),
    "id must be one non-empty string" = is_scalar_string(id)
  )
  values <- utf8ToInt(paste0(as.integer(seed), "\r", id))
  hash <- 0
  for (value in values) {
    hash <- (hash * 131 + value) %% 2147483647
  }
  hash / 2147483647
}

as_nullable_integer <- function(x) {
  if (is.null(x)) NULL else as.integer(x)
}

as_nullable_double <- function(x) {
  if (is.null(x)) NULL else as.double(x)
}

model_checkpoint_path <- function(model, base = NULL) {
  checkpoint <- model@checkpoint
  path <- if (S7_inherits(checkpoint, BioNeMoCheckpoint)) {
    checkpoint@path
  } else {
    checkpoint
  }
  if (!is.null(path) && !is.null(base)) {
    normalize_path(path, base = base)
  } else {
    path
  }
}
