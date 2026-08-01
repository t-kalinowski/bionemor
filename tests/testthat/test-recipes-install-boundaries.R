test_that("unverified recipes require an explicit local container image", {
  workspace <- tempfile("bionemor-custom-recipe-install-")
  recipe <- evo2_recipe(
    revision = strrep("a", 40L),
    repository = "https://example.com/custom/bionemo-recipes",
    base_image = "example.com/custom/pytorch:26.06"
  )

  expect_error(
    bionemo_compute(recipe = recipe, workspace = workspace),
    "unverified recipes require an external runtime or an explicit container image"
  )

  container <- bionemo_compute(
    recipe = recipe,
    workspace = workspace,
    image = "example.com/custom/evo2:2.4"
  )
  expect_equal(container@image, "example.com/custom/evo2:2.4")
  expect_false(container@recipe@verified)

  external <- bionemo_compute(
    recipe = recipe,
    engine = "external",
    workspace = workspace
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

test_that("the NGC base image selects a distinct derived recipe image", {
  workspace <- tempfile("bionemor-base-image-install-")
  recipe <- evo2_recipe()
  compute <- bionemo_compute(
    recipe = recipe,
    workspace = workspace,
    image = recipe@base_image
  )
  derived <- paste0(
    "bionemor/evo2:",
    substr(recipe@revision, 1L, 12L)
  )

  expect_equal(compute@image, derived)
  expect_false(identical(compute@image, recipe@base_image))

  digest_qualified <- bionemo_compute(
    recipe = recipe,
    workspace = tempfile("bionemor-base-digest-install-"),
    image = paste0(recipe@base_image, "@", recipe@base_image_digest)
  )
  expect_equal(digest_qualified@image, derived)
})

locked_recipe_install <- function(
  recipe,
  helper_filename,
  dockerfile_blob,
  image_version,
  archive_dockerfile,
  setup_runtime,
  image = NULL,
  prebuilt = FALSE
) {
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
  setup_runtime(bin)
  if (prebuilt) {
    file.create(built_state)
  }

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
  writeLines(archive_dockerfile, file.path(archive_source, "Dockerfile"))
  writeLines("locked", file.path(archive_source, "clean.txt"))

  real_git <- Sys.which("git")
  helper <- system.file(
    "scripts",
    helper_filename,
    package = "bionemor"
  )
  if (!nzchar(helper)) {
    helper <- testthat::test_path(
      "..",
      "..",
      "inst",
      "scripts",
      helper_filename
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
    "org.opencontainers.image.version" = image_version,
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
        shQuote(dockerfile_blob),
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

  installed <- withr::with_envvar(
    c(
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
    ),
    {
      installed <- bionemo_install(bionemo_compute(
        recipe = recipe,
        workspace = workspace,
        image = image
      ))
      bionemo_install(installed)
    }
  )

  list(
    installed = installed,
    image_id = image_id,
    captured_dockerfile = captured_dockerfile,
    install_log = install_log,
    workspace = workspace
  )
}

test_that("installation builds from locked source and immutable image inputs", {
  recipe <- evo2_recipe()
  result <- locked_recipe_install(
    recipe = recipe,
    helper_filename = "materialize-evo2.py",
    dockerfile_blob = "93ee109724fb44effb35262c0cd2279707c7c3a6",
    image_version = paste0("evo2-recipe-", recipe@recipe_version),
    archive_dockerfile = c(
      paste("FROM", recipe@base_image),
      "#COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/",
      "RUN printf 'locked source\\n'"
    ),
    setup_runtime = function(bin) {
      fake_recipes_runtime(bin)
      suppressWarnings(fake_bionemo_runtime(bin))
    }
  )

  expect_equal(result$installed@image_digest, result$image_id)
  expect_true(file.exists(result$captured_dockerfile))
  expect_match(
    readLines(result$captured_dockerfile, warn = FALSE)[[1L]],
    paste0(recipe@base_image, "@", recipe@base_image_digest),
    fixed = TRUE
  )
  dockerfile <- readLines(result$captured_dockerfile, warn = FALSE)
  expect_true(
    "COPY --from=ghcr.io/astral-sh/uv:0.12.0@sha256:606e70c71c852d03f611b1e56a195d08648507018a7057fab82c4974c4eae105 /uv /uvx /bin/" %in%
      dockerfile
  )
  expect_false(
    "#COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/" %in%
      dockerfile
  )
  expect_false(file.exists(file.path(
    result$workspace,
    ".bionemor",
    "recipes",
    recipe@revision,
    "build-context",
    "dirty.txt"
  )))
})

test_that("ESM installation uses its locked native Transformers runtime", {
  recipe <- esm2_recipe()
  result <- locked_recipe_install(
    recipe = recipe,
    helper_filename = "embed-esm2.py",
    dockerfile_blob = "989ba6b853bcd31eeced5544e8e479361ef428c6",
    image_version = "esm2-transformers-5.14.1",
    archive_dockerfile = c(
      paste("FROM", recipe@base_image),
      "WORKDIR /workspace/bionemo",
      "COPY . .",
      "RUN pip install -r requirements.txt"
    ),
    setup_runtime = fake_esm2_runtime
  )

  expect_equal(result$installed@image_digest, result$image_id)
  dockerfile <- readLines(result$captured_dockerfile, warn = FALSE)
  expect_false(
    "COPY --from=ghcr.io/astral-sh/uv:0.12.0@sha256:606e70c71c852d03f611b1e56a195d08648507018a7057fab82c4974c4eae105 /uv /uvx /bin/" %in%
      dockerfile
  )
  expect_true(any(grepl(
    'transformers\\[torch\\]==5.14.1',
    dockerfile,
    fixed = FALSE
  )))
  expect_false(any(grepl("/workspace/vllm", dockerfile, fixed = TRUE)))

  invocation <- readLines(result$install_log, warn = FALSE)
  build <- invocation[startsWith(invocation, "build ")]
  expect_length(build, 1L)
  expected_image <- paste0(
    "bionemor/esm2-transformers:",
    substr(recipe@revision, 1L, 12L)
  )
  expect_identical(result$installed@image, expected_image)
  expect_match(build, paste("--tag", expected_image), fixed = TRUE)
  expect_no_match(build, "INSTALL_VLLM", fixed = TRUE)
  expect_no_match(build, "TORCH_CUDA_ARCH_LIST", fixed = TRUE)
})

test_that("an explicit ESM image remains verification-only", {
  recipe <- esm2_recipe()
  image <- paste0(
    "bionemor/esm2:",
    substr(recipe@revision, 1L, 12L),
    "-custom"
  )
  result <- locked_recipe_install(
    recipe = recipe,
    helper_filename = "embed-esm2.py",
    dockerfile_blob = "989ba6b853bcd31eeced5544e8e479361ef428c6",
    image_version = "esm2-transformers-5.14.1",
    archive_dockerfile = c(
      paste("FROM", recipe@base_image),
      "WORKDIR /workspace/bionemo",
      "COPY . .",
      "RUN pip install -r requirements.txt"
    ),
    setup_runtime = fake_esm2_runtime,
    image = image,
    prebuilt = TRUE
  )

  expect_identical(result$installed@image, image)
  invocation <- readLines(result$install_log, warn = FALSE)
  expect_false(any(startsWith(invocation, "build ")))
})
