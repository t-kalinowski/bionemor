looks_like_path <- function(x) {
  is_scalar_string(x) &&
    (grepl("[/\\\\]", x) ||
      grepl(
        "\\.(fa|fasta|fna|fas|faa|pep)(\\.gz)?$",
        x,
        ignore.case = TRUE
      ))
}

abort_invalid_sequence <- function(message, request_id = NULL, path = NULL) {
  bionemor_abort(
    "BN_INVALID_SEQUENCE",
    message,
    request_id = request_id,
    operation = "sequence-input",
    path = path
  )
}

as_sequences <- function(x, column = "sequence") {
  if (inherits(x, "XStringSet")) {
    x <- as.character(x)
  }
  if (is.data.frame(x)) {
    if (!column %in% names(x)) {
      abort_invalid_sequence("data must contain the requested sequence column")
    }
    values <- x[[column]]
    if ("id" %in% names(x)) {
      names(values) <- as.character(x$id)
    }
    x <- values
  }
  if (!is.character(x) || length(x) == 0L) {
    abort_invalid_sequence(
      "sequences must be a non-empty character vector or XStringSet"
    )
  }
  ids <- names(x)
  if (is.null(ids)) {
    ids <- paste0("seq_", seq_along(x))
  } else if (anyNA(ids) || any(!nzchar(ids))) {
    abort_invalid_sequence("sequence IDs must not be missing or empty")
  }
  invalid_value <- which(is.na(x) | !nzchar(x))
  if (length(invalid_value)) {
    abort_invalid_sequence(
      "sequences must not contain missing or empty values",
      request_id = ids[[invalid_value[[1L]]]]
    )
  }
  duplicate <- which(duplicated(ids))
  if (length(duplicate)) {
    abort_invalid_sequence(
      "sequence IDs must be unique",
      request_id = ids[[duplicate[[1L]]]]
    )
  }
  invalid_id <- which(grepl("[\r\n]", ids))
  if (length(invalid_id)) {
    abort_invalid_sequence(
      "sequence IDs must not contain line breaks",
      request_id = ids[[invalid_id[[1L]]]]
    )
  }
  invalid_value <- which(grepl("[\r\n]", x))
  if (length(invalid_value)) {
    abort_invalid_sequence(
      "sequences must not contain line breaks",
      request_id = ids[[invalid_value[[1L]]]]
    )
  }
  names(x) <- ids
  x
}

normalize_sequence_values <- function(x, normalize) {
  normalize <- match.arg(normalize, c("dna", "evo2", "protein", "none"))
  utf8 <- enc2utf8(x)
  invalid <- which(!validEnc(x) | is.na(utf8))
  if (length(invalid)) {
    abort_invalid_sequence(
      "sequences must be valid UTF-8",
      request_id = names(x)[[invalid[[1L]]]]
    )
  }
  if (normalize == "none") {
    return(utf8)
  }
  x <- toupper(x)
  if (normalize == "protein") {
    x <- gsub("[ \t\f\v]", "", enc2utf8(x))
    invalid <- which(!grepl("^[LAGVSERTIDPKQNFYMHWCXBUZO.-]+$", x))
    if (length(invalid)) {
      abort_invalid_sequence(
        paste(
          "protein sequences may contain only amino-acid symbols",
          "supported by the ESM-2 tokenizer"
        ),
        request_id = names(x)[[invalid[[1L]]]]
      )
    }
    return(x)
  }
  if (normalize == "dna") {
    x <- gsub("[ \t\f\v]", "", enc2utf8(x))
    invalid <- which(!grepl("^[ACGTRYSWKMBDHVN]+$", x))
    if (length(invalid)) {
      abort_invalid_sequence(
        "DNA sequences may contain only IUPAC DNA symbols",
        request_id = names(x)[[invalid[[1L]]]]
      )
    }
    return(x)
  }
  values <- vapply(
    seq_along(x),
    function(index) {
      value <- x[[index]]
      request_id <- names(x)[[index]]
      tag <- ""
      sequence <- value
      if (startsWith(value, "|")) {
        closing <- regexpr("|", substring(value, 2L), fixed = TRUE)[[1L]]
        if (closing <= 0L) {
          abort_invalid_sequence(
            "Evo 2 phylogenetic prompt tag is not closed",
            request_id = request_id
          )
        }
        closing <- closing + 1L
        tag <- substring(value, 1L, closing)
        sequence <- substring(value, closing + 1L)
        valid_tag <- grepl(
          paste0(
            "^\\|D__[^;|]*;P__[^;|]*;C__[^;|]*;",
            "O__[^;|]*;F__[^;|]*;G__[^;|]*;S__[^;|]*\\|$"
          ),
          tag
        )
        if (!valid_tag) {
          abort_invalid_sequence(
            "Evo 2 phylogenetic prompt tag has an invalid rank layout",
            request_id = request_id
          )
        }
      }
      sequence <- gsub("[ \t\f\v]", "", sequence)
      if (
        nzchar(sequence) &&
          !grepl("^[ACGTRYSWKMBDHVN]+$", sequence)
      ) {
        abort_invalid_sequence(
          paste0(
            "Evo 2 prompts may contain a phylogenetic tag followed by IUPAC DNA"
          ),
          request_id = request_id
        )
      }
      paste0(tag, sequence)
    },
    character(1)
  )
  names(values) <- names(x)
  values
}

write_fasta <- function(sequences, path) {
  sequences <- as_sequences(sequences)
  lines <- unlist(
    Map(
      function(id, sequence) c(paste0(">", id), sequence),
      names(sequences),
      sequences
    ),
    use.names = FALSE
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  path
}

read_fasta <- function(path) {
  connection <- if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    gzfile(path, open = "rt")
  } else {
    file(path, open = "rt")
  }
  on.exit(close(connection), add = TRUE)
  lines <- readLines(connection, warn = FALSE)
  headers <- which(startsWith(lines, ">"))
  if (length(headers) == 0L) {
    abort_invalid_sequence(
      "FASTA data must contain at least one record",
      path = path
    )
  }
  if (headers[[1L]] != 1L) {
    abort_invalid_sequence(
      "FASTA data must start with a header",
      path = path
    )
  }
  ends <- c(headers[-1L] - 1L, length(lines))
  ids <- substring(lines[headers], 2L)
  if (any(!nzchar(ids))) {
    abort_invalid_sequence("FASTA record IDs must not be empty", path = path)
  }
  duplicate <- which(duplicated(ids))
  if (length(duplicate)) {
    abort_invalid_sequence(
      "FASTA record IDs must be unique",
      request_id = ids[[duplicate[[1L]]]],
      path = path
    )
  }
  sequences <- Map(
    function(start, end) {
      if (start == end) {
        ""
      } else {
        paste0(lines[seq.int(start + 1L, end)], collapse = "")
      }
    },
    headers,
    ends
  )
  as_sequences(stats::setNames(unlist(sequences, use.names = FALSE), ids))
}

prepare_sequence_input <- function(
  data,
  run_path,
  normalize = "dna",
  column = "sequence",
  filename = "sequences.fasta"
) {
  dir.create(
    file.path(run_path, "inputs"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  source <- "memory"
  source_path <- NULL
  if (is.character(data) && length(data) == 1L) {
    path_like <- looks_like_path(data)
    candidate <- if (
      path_like ||
        nchar(data, type = "bytes") <= 255L
    ) {
      normalize_path(data)
    } else {
      NULL
    }
    if (!is.null(candidate) && file.exists(candidate)) {
      source <- "fasta"
      source_path <- normalizePath(candidate, mustWork = TRUE)
      sequences <- read_fasta(source_path)
    } else {
      if (path_like) {
        bionemor_abort(
          "BN_INVALID_SEQUENCE",
          paste0("data path does not exist: ", data),
          operation = "sequence-input",
          path = data
        )
      }
      sequences <- as_sequences(data, column = column)
    }
  } else {
    source <- if (is.data.frame(data)) "data.frame" else "memory"
    sequences <- as_sequences(data, column = column)
  }
  source_digest <- if (is.null(source_path)) NULL else path_digest(source_path)
  sequences <- normalize_sequence_values(sequences, normalize)
  path <- write_fasta(
    sequences,
    file.path(run_path, "inputs", filename)
  )
  materialized_digest <- path_digest(path)
  input_source <- list(
    source = source,
    path = source_path,
    digest = source_digest %||% materialized_digest
  )
  list(
    path = normalize_path(path),
    ids = names(sequences),
    sequences = sequences,
    source = source,
    source_path = source_path,
    digest = input_source$digest,
    materialized_digest = materialized_digest,
    input_source = input_source,
    normalize = normalize
  )
}

write_jsonl_rows <- function(rows, path) {
  if (
    !is.list(rows) ||
      !all(vapply(
        rows,
        function(x) is.list(x) && !is.null(names(x)),
        logical(1)
      ))
  ) {
    stop("rows must be a list of named records")
  }
  lines <- vapply(
    rows,
    jsonlite::toJSON,
    character(1),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
  atomic_write_lines(lines, path)
  invisible(path)
}

read_jsonl_rows <- function(path) {
  if (!file.exists(path)) {
    stop("JSONL file does not exist")
  }
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  lapply(
    lines,
    jsonlite::fromJSON,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
}

pooled_embedding_paths <- function(prefix) {
  stopifnot("prefix must be one non-empty string" = is_scalar_string(prefix))
  c(
    data = paste0(prefix, ".f32.gz"),
    metadata = paste0(prefix, ".json")
  )
}

read_pooled_embedding_matrix <- function(prefix, ids, width = NULL) {
  stopifnot(
    "prefix must be one non-empty string" = is_scalar_string(prefix),
    "ids must be non-empty character values" = is.character(ids) &&
      length(ids) > 0L &&
      !anyNA(ids) &&
      all(nzchar(ids)),
    "width must be NULL or a positive integer" = is.null(width) ||
      is_scalar_integerish(width, min = 1L)
  )
  paths <- pooled_embedding_paths(prefix)
  if (!all(file.exists(paths))) {
    stop("pooled embedding output is incomplete")
  }
  metadata <- read_json_file(paths[["metadata"]])
  shape <- unlist(metadata$shape, use.names = FALSE)
  contract <- list(
    format = "bionemor-pooled-embeddings",
    version = 1L,
    storage_dtype = "float32",
    byte_order = "little",
    order = "row-major",
    compression = "gzip",
    compression_level = 1L
  )
  valid <- identical(metadata[names(contract)], contract) &&
    is.numeric(shape) &&
    length(shape) == 2L &&
    all(vapply(shape, is_scalar_integerish, logical(1), min = 1L)) &&
    is.character(metadata$ids) &&
    is_scalar_string(metadata$source_dtype) &&
    metadata$source_dtype %in% c("float16", "bfloat16", "float32") &&
    is_scalar_number(metadata$uncompressed_bytes) &&
    is_scalar_string(metadata$data_md5) &&
    grepl("^[0-9a-f]{32}$", metadata$data_md5)
  if (!valid) {
    stop("pooled embedding metadata is invalid")
  }
  shape <- as.integer(shape)
  expected <- prod(as.double(shape))
  if (
    !identical(metadata$ids, ids) ||
      shape[[1L]] != length(ids) ||
      !is.null(width) && shape[[2L]] != width ||
      metadata$uncompressed_bytes != expected * 4
  ) {
    stop("pooled embedding metadata does not match the requested matrix")
  }
  if (!identical(unname(tools::md5sum(paths[["data"]])), metadata$data_md5)) {
    stop("pooled embedding data checksum does not match its metadata")
  }

  connection <- gzfile(paths[["data"]], "rb")
  on.exit(close(connection))
  values <- readBin(
    connection,
    what = "numeric",
    n = expected,
    size = 4L,
    endian = "little"
  )
  trailing <- readBin(connection, what = "raw", n = 1L)
  if (length(values) != expected || length(trailing) != 0L) {
    stop("pooled embedding data has an invalid byte length")
  }
  if (any(!is.finite(values))) {
    stop("pooled embedding data contains non-finite values")
  }
  result <- matrix(values, nrow = shape[[1L]], byrow = TRUE)
  dimnames(result) <- list(ids, paste0("dim_", seq_len(shape[[2L]])))
  result
}
