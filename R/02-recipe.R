read_recipe_lock <- function(name, label) {
  stopifnot(
    "recipe lock name must be one safe identifier" = is_scalar_string(name) &&
      grepl("^[a-z][a-z0-9-]*$", name),
    "recipe label must be one non-empty string" = is_scalar_string(label)
  )
  path <- system.file(
    "recipes",
    paste0(name, ".json"),
    package = "bionemor",
    mustWork = TRUE
  )
  lock <- jsonlite::read_json(path, simplifyVector = TRUE)
  if (!identical(lock$schema_version, 1L)) {
    stop("unsupported ", label, " recipe lock schema")
  }
  if (
    !is_scalar_string(lock$uv_image) ||
      !is_scalar_string(lock$uv_image_digest) ||
      !grepl("^sha256:[0-9a-f]{64}$", lock$uv_image_digest)
  ) {
    stop(label, " recipe lock has an invalid uv image")
  }
  lock
}

recipe_descriptor <- function(
  lock,
  adapter,
  revision,
  repository,
  base_image,
  allow_mutable
) {
  stopifnot(
    "recipe lock must be a list" = is.list(lock),
    "adapter must be one safe identifier" = is_scalar_string(adapter) &&
      grepl("^[a-z][a-z0-9-]*$", adapter),
    "revision must be one non-empty string" = is_scalar_string(revision),
    "repository must be NULL or one non-empty string" = is.null(repository) ||
      is_scalar_string(repository),
    "base_image must be NULL or one non-empty string" = is.null(base_image) ||
      is_scalar_string(base_image),
    "allow_mutable must be TRUE or FALSE" = is_scalar_logical(allow_mutable)
  )

  revision <- if (identical(revision, "recommended")) {
    lock$revision
  } else {
    revision
  }
  if (!allow_mutable && !grepl("^[0-9a-fA-F]{40}$", revision)) {
    stop("revision must be a full commit SHA unless allow_mutable is TRUE")
  }
  if (grepl("^[0-9a-fA-F]{40}$", revision)) {
    revision <- tolower(revision)
  }

  repository <- repository %||% lock$repository
  base_image <- base_image %||% lock$base_image
  if (
    identical(
      base_image,
      paste0(lock$base_image, "@", lock$base_image_digest)
    )
  ) {
    base_image <- lock$base_image
  }
  base_image_digest <- if (identical(base_image, lock$base_image)) {
    lock$base_image_digest
  } else if (grepl("@sha256:[0-9a-fA-F]{64}$", base_image)) {
    tolower(sub("^.*@(sha256:[0-9a-fA-F]{64})$", "\\1", base_image))
  } else {
    NULL
  }

  BioNeMoRecipe(
    adapter = adapter,
    repository = repository,
    revision = revision,
    recipe_version = lock$recipe_version,
    subdirectory = lock$subdirectory,
    base_image = base_image,
    base_image_digest = base_image_digest,
    bridge_protocol = as.integer(lock$bridge_protocol),
    verified = identical(repository, lock$repository) &&
      identical(revision, lock$revision) &&
      identical(base_image, lock$base_image)
  )
}

recipe_runtime_provenance <- function(job) {
  recipe <- job@compute@recipe
  lock <- recipe_install_spec(recipe)$lock
  capabilities <- job@compute@config$capabilities %||% list()
  list(
    recipe = list(
      repository = recipe@repository,
      revision = recipe@revision,
      version = recipe@recipe_version,
      subdirectory = recipe@subdirectory,
      base_image = recipe@base_image,
      base_image_digest = recipe@base_image_digest,
      bridge_protocol = recipe@bridge_protocol,
      verified = recipe@verified
    ),
    dockerfile = list(
      path = file.path(recipe@subdirectory, "Dockerfile"),
      git_blob = if (recipe@verified) lock$dockerfile_blob else NULL
    ),
    helper = list(
      version = capabilities$helper_version %||% NULL,
      sha256 = capabilities$helper_sha256 %||% NULL
    )
  )
}
