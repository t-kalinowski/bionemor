test_that("unverified recipes require an explicit local container image", {
  workspace <- tempfile("bionemor-custom-recipe-install-")
  recipe <- evo2_recipe(
    revision = strrep("a", 40L),
    repository = "https://example.com/custom/bionemo-recipes",
    base_image = "example.com/custom/pytorch:26.06"
  )

  expect_error(
    bionemo_compute(workspace = workspace, recipe = recipe),
    "unverified recipes require an external runtime or an explicit container image"
  )

  container <- bionemo_compute(
    workspace = workspace,
    recipe = recipe,
    image = "example.com/custom/evo2:2.4"
  )
  expect_equal(container@image, "example.com/custom/evo2:2.4")
  expect_false(container@recipe@verified)

  external <- bionemo_compute(
    engine = "external",
    workspace = workspace,
    recipe = recipe
  )
  expect_null(external@image)
  expect_false(external@recipe@verified)
})

test_that("recipe descriptors retain known base-image digests", {
  locked <- evo2_recipe()
  custom_repository <- evo2_recipe(
    revision = strrep("c", 40L),
    repository = "https://example.com/custom/bionemo-recipes"
  )
  expect_equal(custom_repository@base_image, locked@base_image)
  expect_equal(
    custom_repository@base_image_digest,
    locked@base_image_digest
  )

  digest <- paste0("sha256:", strrep("d", 64L))
  custom_base <- evo2_recipe(
    base_image = paste0("example.com/custom/pytorch:26.06@", digest)
  )
  expect_equal(custom_base@base_image_digest, digest)
  expect_false(custom_base@verified)

  canonical <- evo2_recipe(
    base_image = paste0(locked@base_image, "@", locked@base_image_digest)
  )
  expect_equal(canonical@base_image, locked@base_image)
  expect_equal(canonical@base_image_digest, locked@base_image_digest)
  expect_true(canonical@verified)
})

test_that("installation plans verify explicit images without rebuilding them", {
  workspace <- tempfile("bionemor-explicit-image-install-")
  recipe <- evo2_recipe(
    revision = strrep("b", 40L),
    repository = "https://example.com/custom/bionemo-recipes",
    base_image = "example.com/custom/pytorch:26.06"
  )
  compute <- bionemo_compute(
    workspace = workspace,
    recipe = recipe,
    image = "example.com/custom/evo2:2.4"
  )

  plan <- bionemo_install_plan(compute)
  ids <- vapply(plan@steps, `[[`, character(1), "id")

  expect_equal(ids[[1L]], "image-inspect")
  expect_true("image-labels" %in% ids)
  expect_false(any(
    c(
      "source-init",
      "source-fetch",
      "source-checkout",
      "dockerfile-verify",
      "base-image-pull",
      "base-image-verify",
      "image-build"
    ) %in%
      ids
  ))
})

test_that("the NGC base image builds a distinct derived recipe image", {
  workspace <- tempfile("bionemor-base-image-install-")
  recipe <- evo2_recipe()
  compute <- bionemo_compute(
    workspace = workspace,
    recipe = recipe,
    image = recipe@base_image
  )
  derived <- paste0(
    "bionemor/evo2:",
    substr(recipe@revision, 1L, 12L)
  )

  plan <- bionemo_install_plan(compute)
  ids <- vapply(plan@steps, `[[`, character(1), "id")
  build <- plan@steps[[match("image-build", ids)]]$command
  tag <- build$args[[match("--tag", build$args) + 1L]]

  expect_equal(
    tag,
    derived
  )
  expect_equal(compute@image, derived)
  expect_false(identical(tag, recipe@base_image))

  digest_qualified <- bionemo_compute(
    workspace = tempfile("bionemor-base-digest-install-"),
    recipe = recipe,
    image = paste0(recipe@base_image, "@", recipe@base_image_digest)
  )
  expect_equal(digest_qualified@image, derived)
})

test_that("installation builds from locked source and immutable image inputs", {
  workspace <- tempfile("bionemor-locked-install-")
  bin <- tempfile("bionemor-locked-bin-")
  archive_source <- tempfile("bionemor-locked-archive-")
  install_log <- tempfile("bionemor-locked-log-")
  built_state <- tempfile("bionemor-locked-state-")
  captured_dockerfile <- tempfile("bionemor-locked-dockerfile-")
  labels_file <- tempfile("bionemor-locked-labels-")
  repo_digests_file <- tempfile("bionemor-locked-digests-")
  dir.create(workspace)
  dir.create(archive_source)
  fake_recipes_runtime(bin)
  suppressWarnings(fake_bionemo_runtime(bin))

  recipe <- evo2_recipe()
  base_reference <- paste0(recipe@base_image, "@", recipe@base_image_digest)
  image_id <- paste0("sha256:", strrep("e", 64L))
  source <- file.path(
    workspace,
    ".bionemor",
    "recipes",
    recipe@revision,
    "source"
  )
  recipe_source <- file.path(source, recipe@subdirectory)
  dir.create(recipe_source, recursive = TRUE)
  writeLines(
    c(
      paste("FROM", recipe@base_image),
      "RUN printf 'dirty source\\n'"
    ),
    file.path(recipe_source, "Dockerfile")
  )
  writeLines("must not enter the build", file.path(recipe_source, "dirty.txt"))
  writeLines(
    c(
      paste("FROM", recipe@base_image),
      "#COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/",
      "RUN printf 'locked source\\n'"
    ),
    file.path(archive_source, "Dockerfile")
  )
  writeLines("locked", file.path(archive_source, "clean.txt"))

  real_git <- Sys.which("git")
  helper <- system.file(
    "scripts",
    "materialize-evo2.py",
    package = "bionemor"
  )
  if (!nzchar(helper)) {
    helper <- testthat::test_path(
      "..",
      "..",
      "inst",
      "scripts",
      "materialize-evo2.py"
    )
  }
  helper_revision <- trimws(
    processx::run(
      real_git,
      c("hash-object", helper)
    )$stdout
  )
  labels <- as.list(c(
    "org.opencontainers.image.source" = recipe@repository,
    "org.opencontainers.image.revision" = recipe@revision,
    "org.opencontainers.image.version" = paste0(
      "evo2-recipe-",
      recipe@recipe_version
    ),
    "io.bionemor.helper.revision" = helper_revision,
    "io.bionemor.base.image" = recipe@base_image,
    "io.bionemor.base.digest" = recipe@base_image_digest,
    "io.bionemor.bridge.protocol" = as.character(recipe@bridge_protocol)
  ))
  jsonlite::write_json(labels, labels_file, auto_unbox = TRUE)
  jsonlite::write_json(
    paste0(recipe@base_image, "@", recipe@base_image_digest),
    repo_digests_file,
    auto_unbox = TRUE
  )

  write_executable(
    file.path(bin, "git"),
    c(
      "if [[ \"${1:-}\" == \"hash-object\" ]]; then",
      "  exec \"$BIONEMOR_REAL_GIT\" \"$@\"",
      "fi",
      "[[ \"${1:-}\" == \"-C\" ]]",
      "shift 2",
      "case \"${1:-}\" in",
      paste(
        "  rev-parse) printf '%s\\n'",
        shQuote(recipe@revision),
        ";;"
      ),
      paste(
        "  hash-object) printf '%s\\n'",
        shQuote("93ee109724fb44effb35262c0cd2279707c7c3a6"),
        ";;"
      ),
      "  archive)",
      "    output=",
      "    for argument in \"$@\"; do",
      "      case \"$argument\" in",
      "        --output=*) output=\"${argument#--output=}\" ;;",
      "      esac",
      "    done",
      "    [[ -n \"$output\" ]]",
      "    tar -cf \"$output\" -C \"$BIONEMOR_ARCHIVE_SOURCE\" .",
      "    ;;",
      "  *) exit 92 ;;",
      "esac"
    )
  )

  write_executable(
    file.path(bin, "docker"),
    c(
      "printf '%s\\n' \"$*\" >> \"$BIONEMOR_INSTALL_LOG\"",
      "if [[ \"${1:-}\" == \"pull\" ]]; then",
      "  [[ \"${2:-}\" == \"$BIONEMOR_BASE_REFERENCE\" ]]",
      "  exit",
      "fi",
      "if [[ \"${1:-}\" == \"build\" ]]; then",
      "  dockerfile=",
      "  previous=",
      "  for argument in \"$@\"; do",
      "    if [[ \"$previous\" == \"--file\" ]]; then dockerfile=\"$argument\"; fi",
      "    previous=\"$argument\"",
      "  done",
      "  context=\"${@: -1}\"",
      "  [[ -f \"$context/clean.txt\" ]]",
      "  [[ ! -e \"$context/dirty.txt\" ]]",
      "  grep -Fqx \"FROM $BIONEMOR_BASE_REFERENCE\" \"$dockerfile\"",
      "  cp \"$dockerfile\" \"$BIONEMOR_CAPTURED_DOCKERFILE\"",
      "  : > \"$BIONEMOR_BUILT_STATE\"",
      "  exit",
      "fi",
      "if [[ \"${1:-}\" == \"image\" && \"${2:-}\" == \"inspect\" ]]; then",
      "  if [[ \"${3:-}\" != \"--format\" ]]; then",
      "    [[ -f \"$BIONEMOR_BUILT_STATE\" ]]",
      "    printf '{}\\n'",
      "    exit",
      "  fi",
      "  format=\"$4\"",
      "  target=\"$5\"",
      "  case \"$format\" in",
      "    \"{{json .RepoDigests}}\")",
      "      [[ \"$target\" == \"$BIONEMOR_BASE_REFERENCE\" ]]",
      "      cat \"$BIONEMOR_REPO_DIGESTS_FILE\"",
      "      ;;",
      "    \"{{.Id}}\")",
      "      [[ -f \"$BIONEMOR_BUILT_STATE\" ]]",
      "      printf '%s\\n' \"$BIONEMOR_IMAGE_ID\"",
      "      ;;",
      "    \"{{json .Config.Labels}}\")",
      "      [[ \"$target\" == \"$BIONEMOR_IMAGE_ID\" ]]",
      "      cat \"$BIONEMOR_LABELS_FILE\"",
      "      ;;",
      "    *) exit 93 ;;",
      "  esac",
      "  exit",
      "fi",
      "if [[ \"${1:-}\" != \"run\" ]]; then exit 94; fi",
      "shift",
      "entrypoint=\"\"",
      "while [[ $# -gt 0 ]]; do",
      "  case \"$1\" in",
      "    --rm|--ipc=host) shift ;;",
      "    --gpus|--user|--name|-v|-w|-e) shift 2 ;;",
      "    --entrypoint) entrypoint=\"$2\"; shift 2 ;;",
      "    *) shift",
      "       if [[ -n \"$entrypoint\" && \"${1:-}\" == \"--help\" ]]; then printf 'help\\n'; exit; fi",
      "       if [[ -n \"$entrypoint\" ]]; then exec \"$entrypoint\" \"$@\"; fi",
      "       if [[ \"${2:-}\" == \"--help\" ]]; then printf 'help\\n'; exit; fi",
      "       exec \"$@\" ;;",
      "  esac",
      "done",
      "exit 95"
    )
  )

  withr::local_envvar(c(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_REAL_GIT = unname(real_git),
    BIONEMOR_ARCHIVE_SOURCE = archive_source,
    BIONEMOR_INSTALL_LOG = install_log,
    BIONEMOR_BUILT_STATE = built_state,
    BIONEMOR_CAPTURED_DOCKERFILE = captured_dockerfile,
    BIONEMOR_BASE_REFERENCE = base_reference,
    BIONEMOR_REPO_DIGESTS_FILE = repo_digests_file,
    BIONEMOR_LABELS_FILE = labels_file,
    BIONEMOR_IMAGE_ID = image_id
  ))
  compute <- bionemo_compute(workspace = workspace)

  installed <- bionemo_install(compute)

  expect_equal(installed@image_digest, image_id)
  expect_true(file.exists(captured_dockerfile))
  expect_match(
    readLines(captured_dockerfile, warn = FALSE)[[1L]],
    base_reference,
    fixed = TRUE
  )
  dockerfile <- readLines(captured_dockerfile, warn = FALSE)
  expect_true(
    "COPY --from=ghcr.io/astral-sh/uv:0.12.0@sha256:606e70c71c852d03f611b1e56a195d08648507018a7057fab82c4974c4eae105 /uv /uvx /bin/" %in%
      dockerfile
  )
  expect_false(
    "#COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/" %in%
      dockerfile
  )
  expect_false(file.exists(file.path(
    workspace,
    ".bionemor",
    "recipes",
    recipe@revision,
    "build-context",
    "dirty.txt"
  )))
})
