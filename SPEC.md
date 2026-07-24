# `nimr` and `bionemor` package split

Status: proposed implementation specification

## Summary

Split the current `nimr` package into two independent R packages:

- `nimr` connects to and deploys NVIDIA NIM services.
- `bionemor` prepares checkpoints, trains models, and runs batch inference with
  NVIDIA BioNeMo Framework.

The split follows the runtime boundary. NIM is a long-running HTTP service.
BioNeMo runs Python commands that produce checkpoints and file-backed results.
Neither package should require the other. A checkpoint path is the handoff
between them.

The current code is unreleased and has version `0.0.0.9000`. The implementation
may make breaking changes without a deprecation period.

## Goals

- Give scientists a BioNeMo interface that does not imply NIM access.
- Keep NIM authentication, HTTP APIs, and deployment concerns out of
  `bionemor`.
- Keep Python training environments, schedulers, and file-backed jobs out of
  `nimr`.
- Support public checkpoints without requiring an NGC credential.
- Preserve an explicit path for NGC resources when the user has access.
- Use R and Bioconductor data structures at the public boundary.
- Keep execution versioned and reproducible.
- Start `bionemor` with Evo 2 without making its architecture specific to Evo
  2.

## Non-goals

- Do not provide one class hierarchy shared by both packages.
- Do not create a third package for shared utilities.
- Do not make reticulate the training or distributed-execution runtime.
- Do not expose Python trainer, model, or tensor objects as the main R API.
- Do not implement implicit fallback between NIM, BioNeMo, model sizes,
  precision modes, checkpoint sources, or GPU types.
- Do not expose LoRA, task-head fitting, or another fitting method until the
  selected BioNeMo profile implements and tests it.
- Do not claim support for BioNeMo models other than those with an implemented
  model adapter.
- Package submission to CRAN or Bioconductor is outside this split.

## Package boundaries

| Concern | `nimr` | `bionemor` |
| --- | ---: | ---: |
| NIM HTTP requests | Yes | No |
| NIM authentication | Yes | No |
| NIM health and readiness | Yes | No |
| Local NIM deployment | Yes | No |
| BioNeMo environment diagnostics | No | Yes |
| Checkpoint download and conversion | No | Yes |
| FASTA preprocessing | No | Yes |
| Training and fine-tuning | No | Yes |
| Batch inference | No | Yes |
| Local and Slurm jobs | No | Yes |
| Brev workflow | No | Yes |
| Optional reticulate use | Result decoding | Decoding and utilities |

## Package 1: `nimr`

### `nimr` purpose

`nimr` is an R client and local deployment interface for NVIDIA NIM services.
It treats a NIM as an HTTP service with a versioned API profile. It does not
model BioNeMo training or compute.

The initial release supports:

- self-hosted Evo 2 NIM version 2;
- NVIDIA-hosted Evo 2 40B generation;
- local Docker deployment of a supported Evo 2 NIM image;
- official and local checkpoints supported by that NIM image.

Support for another NIM requires a new profile implementation and public-API
tests. A generic request escape hatch does not count as profile support.

### `nimr` public types

Use S7 classes:

- `NimProfile`: service family, variant, service type, version, and profile
  configuration.
- `NimEndpoint`: URL, profile, redacted credentials, request timeout, TLS
  policy, and redacted headers.
- `NimDeployment`: a `NimEndpoint` with a local container ID and deployment
  metadata.
- `NimPrediction`: typed online prediction result.
- `NimEvaluation`: predictions and calculated metrics.
- `NimHealth`: readiness status and raw health metadata.
- `NimArtifact`: path and format for responses that should remain file-backed.
- `NimSetupPlan`: generated NIM deployment files and commands.
- `NimDoctor`: structured client or deployment checks.

Do not define `NimModel`, `NimCompute`, or `NimJob` in this package.

### Profile API

```r
nim_profile(
  family,
  variant = NULL,
  service = c("self-hosted", "nvidia-hosted"),
  version = NULL,
  config = list()
)
```

For the initial Evo 2 profile:

- `family` must be `"evo2"`.
- Self-hosted variants are `"7b"` and `"40b"`.
- The NVIDIA-hosted service supports `"40b"` only.
- `version` defaults to `"2"` for self-hosted Evo 2.
- Unsupported combinations fail in `nim_profile()`, before a request or
  deployment is attempted.

The profile owns endpoint paths, supported operations, request bodies, and
response decoding. These details must not be distributed across endpoint,
prediction, and deployment functions.

### Connection and request API

```r
nim_connect(
  url,
  profile,
  token = NULL,
  timeout = 600,
  verify_ssl = TRUE,
  headers = list()
)

nim_request(
  endpoint,
  operation,
  body = NULL,
  query = NULL,
  response = c("auto", "json", "text", "raw", "file"),
  path = NULL
)

nim_health(endpoint)
nim_capabilities(x)
```

`nim_request()` uses a profile operation name, such as `"generate"` or
`"forward"`, rather than accepting an arbitrary URL path. An explicitly named
low-level function may accept a path for debugging, but it must not be used by
the high-level API.

Tokens and sensitive headers:

- are never printed;
- are not serialized;
- are not written into generated setup files;
- are passed only to the HTTP request or container process that needs them.

Brev authentication and NGC authentication are separate. `nimr` must never
infer one from the other.

### Prediction and evaluation API

Implement `stats::predict()` for `NimEndpoint`:

```r
predict(
  object,
  newdata,
  type = c("response", "score", "representation", "raw"),
  num_tokens = 100L,
  temperature = 0.7,
  top_k = 3L,
  top_p = NULL,
  seed = NULL,
  logits = FALSE,
  probabilities = FALSE,
  timings = FALSE,
  reference = NULL,
  reduction = c("sum", "mean", "none"),
  layer = NULL,
  pooling = c("mean", "last", "position", "none"),
  position = NULL,
  output = c("auto", "raw", "file"),
  path = NULL,
  ...
)
```

The selected profile determines which types are supported. Unsupported types
fail before a request is sent.

Generation controls apply only to `"response"`; scoring controls apply only to
`"score"`; and layer, pooling, output, and path controls apply only to
`"representation"` or `"raw"`. Supplying an argument that does not apply to the
selected type is an error.

Return contracts, all wrapped in `NimPrediction`:

- `"response"`: a data frame with input ID, input sequence, generated sequence,
  elapsed time, and requested optional outputs.
- `"score"`: a data frame with input ID, score, and optional reference score.
- `"representation"`: an R matrix, array, or `NimArtifact` when the response is
  intentionally file-backed.
- `"raw"`: decoded R data when small, otherwise `NimArtifact`.

`generics::evaluate()` remains in `nimr` because it evaluates an online NIM
endpoint. The initial Evo 2 implementation supports AUC from sequence scores.

### Deployment API

```r
nim_deploy(
  profile,
  checkpoint = NULL,
  name = NULL,
  port = 8000L,
  image = NULL,
  cache = NULL,
  timeout = 600,
  poll = 5
)

nim_undeploy(deployment)
```

Deployment is local Docker only in the initial release.

- `checkpoint` is `NULL` for an image-managed checkpoint or one existing local
  directory.
- `checkpoint` may be the value returned by
  `bionemor::checkpoint_path()`, but `nimr` does not import `bionemor`.
- The profile validates whether the variant and checkpoint mode are supported.
- Generated services bind to loopback by default.
- A failed readiness check removes the container and includes bounded logs in
  the error.

NGC authentication is an external prerequisite for pulling the NIM image.
`nimr` uses the existing Docker credential store and does not implement registry
login. For an image-managed checkpoint, `nim_deploy()` reads `NGC_API_KEY` from
the process environment and passes it to the container without storing it. A
local checkpoint uses `NIM_DISABLE_MODEL_DOWNLOAD=1` and does not require the
key at container runtime, although the user must still have obtained the NIM
image. Image entitlement, acceptance of governing terms, and possession of an
API key are separate requirements.

Rename the current `deploy()` generic to `nim_deploy()`. Do not retain a broad
`deploy()` export.

### `nimr` setup and diagnostics

```r
nim_setup(
  profile,
  path = ".nimr",
  image = NULL,
  execute = FALSE
)

nim_doctor(
  target = c("client", "deployment"),
  endpoint = NULL,
  profile = NULL,
  image = NULL,
  verbose = TRUE
)
```

`nim_setup()` only creates NIM service files. It must not create BioNeMo
training or batch-inference files.

Successful diagnostics print a concise summary. Full command output is
included only when `verbose = TRUE` or when a check fails.

### `nimr` dependencies

Expected imports:

- `generics`
- `httr2`
- `jsonlite`
- `processx`
- `S7`
- `stats`
- `utils`

Reticulate may remain in `Suggests` only if a NIM response format requires
NumPy decoding. Ordinary HTTP prediction must not require Python.

Remove BioNeMo-only dependencies, scripts, Dockerfiles, and vignettes.

## Package 2: `bionemor`

### `bionemor` purpose

`bionemor` is an R interface to NVIDIA BioNeMo Framework for checkpoint
preparation, model fitting, and batch inference. It presents BioNeMo as a
versioned external compute system.

Evo 2 is the first model adapter. The package name reflects the intended
framework scope, while documentation must state exactly which models and
profiles are implemented.

The initial release supports:

- BioNeMo profile `bionemo-2.6.3`;
- Evo 2 sizes `"1b"`, `"7b"`, and `"40b"`;
- public Hugging Face checkpoint conversion;
- explicit NGC checkpoint resources when credentials are available;
- full-parameter training and fine-tuning;
- batch generation, scoring, and raw logits;
- local Python, local Docker, Slurm Python, and Slurm Apptainer execution;
- one node and one or more GPUs, subject to the model profile;
- the tested Brev L40S workflow.

### `bionemor` public types

Use S7 classes:

- `BioNeMoModel`: abstract model specification.
- `Evo2Model`: model size, checkpoint, task, configuration, and provenance.
- `BioNeMoCheckpoint`: path, format, family, variant, source, profile, and
  provenance.
- `BioNeMoCompute`: backend, engine, workspace, image, resources, scheduler
  fields, and profile.
- `BioNeMoJob`: external job ID, kind, state, compute, command, log, expected
  result, operation timeout, and process metadata.
- `BioNeMoPrediction`: typed batch prediction result.
- `BioNeMoArtifact`: file-backed artifact path, format, and metadata.
- `BioNeMoSetupPlan`: generated environment-check files and commands.
- `BioNeMoDoctor`: structured environment checks.
- `Evo2FitControl`: typed Evo 2 fitting controls.

Do not use the `Nim` prefix in `bionemor`.

### Model API

```r
evo2(
  size,
  checkpoint = NULL,
  pretrained = TRUE,
  config = list()
)
```

- `size` is required and must be `"1b"`, `"7b"`, or `"40b"`.
- Aliases may be accepted at input but are normalized immediately.
- `checkpoint` is `NULL`, one path, or a `BioNeMoCheckpoint`.
- The model does not store a compute backend.
- Fitting or prediction with pretrained weights requires an explicit
  checkpoint.
- `pretrained = FALSE` requires `checkpoint = NULL`.

Do not hide checkpoint downloads inside `fit()`. Checkpoint preparation is a
separate visible operation.

### Model adapter boundary

Compute, jobs, workspace handling, and artifact handling are framework-level
code. Checkpoint conversion, input preprocessing, fitting controls, command
construction, capabilities, and result decoding are model-adapter code.

Dispatch model behavior from the `BioNeMoModel` subclass and selected BioNeMo
profile. Framework-level functions must not accumulate branches such as
`if (family == "evo2")`. The initial release has one internal Evo 2 adapter and
no public adapter registry. Adding a second model family requires:

1. one exported model constructor;
2. one model-specific checkpoint constructor;
3. model-specific fitting controls where applicable;
4. mappings for every advertised profile and operation;
5. public-API unit tests and one opt-in integration smoke test; and
6. an entry in the supported model/profile table.

### Checkpoint API

```r
evo2_checkpoint(
  model,
  source,
  path,
  compute,
  overwrite = FALSE
)

checkpoint_path(x)
checkpoint_manifest(x)
```

`source` is one explicit URI:

- `hf://...` for a public Hugging Face source;
- `ngc://...` for an entitled NGC resource;
- a local path for an existing source checkpoint.

There is no source fallback. An `ngc://` source requires the relevant
credential in the execution environment and fails clearly when it is absent.
Credentials are never stored in the returned checkpoint object.

`evo2_checkpoint()`:

1. validates that `path` is in the compute workspace when required;
2. runs the converter defined by the selected profile;
3. verifies the expected checkpoint metadata files;
4. returns a `BioNeMoCheckpoint` with source and conversion provenance.

If a complete checkpoint already exists at `path`, `overwrite = FALSE` returns
it only when its manifest matches the requested family, size, source, and
profile. Otherwise it fails.

`checkpoint_path()` returns one normalized path and is the interoperability
boundary with `nimr`.

### Compute API

```r
bionemo_compute(
  backend = c("local", "slurm"),
  workspace = getwd(),
  profile = "bionemo-2.6.3",
  engine = c("python", "container"),
  image = NULL,
  gpus = 1L,
  nodes = 1L,
  queue = NULL,
  account = NULL,
  walltime = NULL,
  config = list()
)
```

Profile behavior is explicit:

- `engine = "python"` uses commands already installed in the execution
  environment.
- Local `engine = "container"` uses Docker.
- Slurm `engine = "container"` uses Apptainer.
- `profile = "bionemo-2.6.3"` maps model sizes and arguments to that exact
  command contract.
- A future BioNeMo version is a new profile implementation, not a silent change
  to the existing profile.
- Unsupported multi-node behavior fails during compute construction.

### Fitting control

```r
evo2_fit_control(
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
)
```

The constructor validates all fields and returns `Evo2FitControl`.
`extra_args` is an explicit advanced escape hatch. Typed arguments take
precedence, and passing the same option through `extra_args` is an error.

The package does not choose precision based on the GPU or checkpoint. The user
or documented workflow selects it explicitly.

### Fitting API

Implement `generics::fit()`:

```r
fit(
  object,
  data,
  compute,
  steps,
  control = evo2_fit_control(),
  name = NULL,
  output = NULL,
  timeout = Inf,
  async = FALSE,
  ...
)
```

Initial-release behavior:

- only full fitting is supported;
- `steps` is required and is a positive integer;
- `data` accepts a FASTA path, character vector, data frame with a `sequence`
  column, or `Biostrings::DNAStringSet`;
- input data is converted to FASTA in the compute workspace;
- preprocessing and training run as one job;
- `timeout` bounds the complete preprocessing and training job;
- a successful job identifies exactly one final checkpoint;
- synchronous fitting returns an `Evo2Model` with the fitted checkpoint;
- asynchronous fitting returns a `BioNeMoJob`;
- the fitted model records parent checkpoint, profile, precision, fitting
  control, and data provenance;
- a timeout or failed process never returns a model.

When `pretrained = TRUE`, `fit()` performs full-parameter fine-tuning from the
model checkpoint. When `pretrained = FALSE`, it trains from scratch and does
not pass a checkpoint argument to BioNeMo.

Do not expose `method = "lora"` or `method = "head"` in the initial release.

### Batch prediction API

Implement `stats::predict()` for `BioNeMoModel`:

```r
predict(
  object,
  newdata,
  type = c("response", "score", "raw"),
  compute,
  async = FALSE,
  output = NULL,
  name = NULL,
  reduction = c("sum", "mean"),
  num_tokens = 100L,
  temperature = 0.7,
  top_k = 3L,
  top_p = 0,
  precision = c("bf16", "fp8"),
  extra_args = character(),
  ...
)
```

Return contracts:

- `"response"`: `BioNeMoPrediction` containing a named character vector.
- `"score"`: `BioNeMoPrediction` containing a data frame with `id`, `score`,
  checkpoint provenance, and reduction.
- `"raw"`: `BioNeMoPrediction` containing a `BioNeMoArtifact`.

Large tensors remain file-backed. Scores and generated sequences must not
require users to load a PyTorch `.pt` file themselves.

Representation extraction is not advertised until a BioNeMo profile implements
it. NIM-only representation behavior remains in `nimr`.

### Job API

```r
job_status(x, refresh = TRUE)
job_wait(x, poll = 10, timeout = Inf)
job_result(x)
job_logs(x, tail = NULL)
job_cancel(x)
```

Rules:

- Local jobs run as external processes with separate logs.
- Slurm jobs use `sbatch`, `sacct`, and `scancel`.
- `job_wait()` returns the same typed result that synchronous execution would
  return.
- An operation-level timeout stored on a local job terminates the full process
  or container, not only the parent R process.
- A `job_wait()` timeout stops waiting but does not cancel the job. Cancellation
  is explicit through `job_cancel()`.
- Unknown scheduler states fail with their original value.
- Logs and commands must not contain credentials.

Do not export job functions with a `nim_` prefix.

### `bionemor` setup and diagnostics

```r
bionemo_setup(
  compute,
  model = NULL,
  target = c("training", "inference"),
  path = ".bionemo",
  execute = FALSE
)

bionemo_doctor(
  compute,
  model = NULL,
  target = c("training", "inference"),
  verbose = TRUE
)

bionemo_capabilities(x)
```

Diagnostics check the exact commands required by the selected profile. They do
not check for NIM images or NIM endpoints.

Successful checks print a concise summary. Full `--help` output is retained in
the structured result but printed only in verbose mode or for a failed check.

### R and Python execution

The default execution path is:

```text
R API
  -> validated FASTA and configuration files
  -> versioned BioNeMo CLI command
  -> external Python process on the selected compute
  -> checkpoint or prediction artifact
  -> typed R result
```

CLI entrypoints remain the execution contract for Evo 2:

- `evo2_convert_to_nemo2`
- `preprocess_evo2`
- `train_evo2`
- `predict_evo2`
- `infer_evo2`

Reticulate policy:

- `reticulate` is in `Suggests`, not `Imports`.
- Training, distributed launch, Docker, Apptainer, and Slurm do not run through
  reticulate.
- Reticulate may decode NumPy results and call documented, side-effect-free
  BioNeMo Python utilities.
- PyTorch artifacts that require the remote Python environment are converted
  by a packaged helper process in that environment.
- Public results contain R objects or artifact paths, never Python objects.
- The package does not export a general Python-module accessor in the initial
  release. Advanced users can use reticulate directly.

### Primary user workflow

The first-release vignette and integration test use this public API:

```r
library(bionemor)

compute <- bionemo_compute(
  backend = "local",
  engine = "container",
  image = "nvcr.io/nvidia/clara/bionemo-framework:2.6.3",
  workspace = "/home/ubuntu/bionemor-workspace"
)

spec <- evo2("1b")
base_checkpoint <- evo2_checkpoint(
  spec,
  source = "hf://arcinstitute/savanna_evo2_1b_base",
  path = "checkpoints/evo2-1b-8k",
  compute = compute
)
model <- evo2("1b", checkpoint = base_checkpoint)

baseline <- predict(
  model,
  "ACGTACGTACGT",
  type = "response",
  compute = compute,
  num_tokens = 8L
)

fitted <- fit(
  model,
  data = c("ACGTACGTACGT", "TGCATGCATGCA"),
  compute = compute,
  steps = 3L,
  timeout = 300
)

after <- predict(
  fitted,
  "ACGTACGTACGT",
  type = "response",
  compute = compute,
  num_tokens = 8L
)
```

Paths relative to `workspace` are resolved there. Absolute paths must already
be visible to the selected engine. This workflow must not read
`NGC_API_KEY`.

### `bionemor` dependencies

Expected imports:

- `generics`
- `jsonlite`
- `processx`
- `S7`
- `stats`
- `utils`
- `yaml`

Expected suggests:

- `Biostrings`
- `knitr`
- `reticulate`
- `rmarkdown`
- `testthat`
- `withr`

The R package must install and load without Python or a GPU. Runtime checks
belong in `bionemo_doctor()` and the operation that requires them.

## Interoperability

The packages exchange plain values, not shared classes.

The supported fitted-checkpoint handoff is:

```r
checkpoint <- bionemor::checkpoint_path(fitted)

profile <- nimr::nim_profile(
  "evo2",
  variant = "7b",
  service = "self-hosted"
)

deployment <- nimr::nim_deploy(
  profile,
  checkpoint = checkpoint
)
```

The user selects the NIM profile explicitly. `nimr` validates that the selected
NIM supports the checkpoint family and variant. It does not infer a profile
from a serialized `bionemor` object.

Neither package imports the other. Cross-package examples may appear in
documentation with evaluation disabled.

## Repository and migration plan

Keep the existing `mlverse/nimr` repository for the focused NIM package.
Initially publish `t-kalinowski/bionemor` using the current split commit as its
starting source. Transfer it to `mlverse/bionemor` when the organization
repository is ready. Preserve the MIT license and existing authorship.

Do not create compatibility wrappers for unreleased APIs.

### Code ownership

Move or rewrite these areas for `bionemor`:

- model and checkpoint classes;
- `evo2()`;
- compute specifications;
- local and Slurm jobs;
- fitting;
- batch prediction;
- BioNeMo setup and diagnostics;
- Docker assets;
- Brev scripts and vignettes;
- Python artifact helpers.

Keep or rewrite these areas in `nimr`:

- endpoint and profile classes;
- HTTP request handling;
- online prediction;
- online evaluation;
- health checks;
- local NIM deployment;
- NIM setup and diagnostics;
- secret redaction.

Mixed files must be split by responsibility rather than copied unchanged.
Each package should contain its own small validation and path helpers. Do not
introduce a shared internal package.

### Public API migration

| Current API | Destination |
| --- | --- |
| `evo2()` | `bionemor::evo2()` |
| `nim_compute()` | `bionemor::bionemo_compute()` |
| `fit()` | `bionemor::fit()` method |
| Checkpoint-backed `predict()` | `bionemor::predict()` method |
| `nim_status()` | `bionemor::job_status()` |
| `nim_wait()` | `bionemor::job_wait()` |
| `nim_result()` | `bionemor::job_result()` |
| `nim_logs()` | `bionemor::job_logs()` |
| `nim_cancel()` | `bionemor::job_cancel()` |
| `nim_connect()` | `nimr::nim_connect()` with a `NimProfile` |
| Endpoint-backed `predict()` | `nimr::predict()` method |
| `evaluate()` | `nimr::evaluate()` method |
| `deploy()` | `nimr::nim_deploy()` |
| `nim_undeploy()` | `nimr::nim_undeploy()` |
| Training `nim_setup()` | `bionemor::bionemo_setup()` |
| Inference `nim_setup()` | `nimr::nim_setup()` |
| Training/inference compute diagnostics | `bionemor::bionemo_doctor()` |
| Client/deployment diagnostics | `nimr::nim_doctor()` |

Update package titles:

- `nimr`: “NVIDIA NIM Services from R”
- `bionemor`: “NVIDIA BioNeMo Framework Workflows from R”

## Documentation

### `nimr` tests

Provide:

- an endpoint-client vignette using mocked or user-supplied service details;
- a local deployment vignette that clearly states NIM image and entitlement
  requirements;
- a profile support table;
- an explanation that Brev authentication does not grant NGC or NIM image
  access;
- an interoperability example using a plain checkpoint path.

Do not include BioNeMo training, Brev training, FASTA preprocessing, or
checkpoint-conversion instructions.

### `bionemor` tests

Provide:

- a local or container environment guide;
- the tested Brev L40S guide;
- public Evo 2 checkpoint conversion from Hugging Face;
- full fitting and post-fit batch inference;
- Slurm execution;
- a supported model/profile table;
- an explanation of the R, CLI, and Python process boundary;
- an optional handoff to `nimr` for users with NIM access.

The primary guide must work without an NGC key.

## Versioned upstream contracts

Implement profile behavior against versioned documentation and commands
observed in integration tests. Do not implement a pinned profile from the
unversioned `latest` documentation.

- The `bionemo-2.6.3` profile follows the
  [BioNeMo 2.6.3 Evo 2 command contract](https://docs.nvidia.com/bionemo-framework/2.6.3/main/developer-guide/bionemo-evo2/bionemo-evo2-Overview/index.html).
- Public checkpoint conversion follows NVIDIA's documented
  [Hugging Face to NeMo2 conversion](https://docs.nvidia.com/bionemo-framework/2.6.3/main/developer-guide/bionemo-evo2/bionemo-evo2-Overview/index.html#checkpoint-conversion-from-hugging-face-to-nemo2).
- The NIM Evo 2 version 2 profile follows the documented
  [generation, forward, and readiness endpoints](https://docs.nvidia.com/nim/bionemo/evo2/2.0.0/endpoints.html).
- Local NIM deployment follows the current
  [Evo 2 NIM quickstart](https://docs.nvidia.com/nim/bionemo/evo2/latest/quickstart-guide.html)
  for registry and runtime authentication. The implementation must pin the
  tested image tag rather than infer one from this unversioned page.

## Testing

Follow red/green TDD and test public APIs only.

### `nimr`

Required tests:

- profile validation for supported and unsupported Evo 2 combinations;
- URL, credential, header, and serialization behavior;
- operation-to-path and request-body mapping through mocked HTTP;
- response decoding for every advertised prediction type;
- unsupported operations fail before HTTP;
- local deployment command construction;
- readiness success, timeout, and early container exit;
- cleanup after failed deployment;
- generated setup files contain no credential values;
- package installation and tests do not require Python.

### `bionemor`

Required tests:

- Evo 2 size and checkpoint validation;
- public checkpoint command construction and manifest validation;
- input conversion from character, data frame, FASTA, and optional
  `DNAStringSet`;
- typed fitting-control validation;
- exact BioNeMo 2.6.3 training command mapping;
- exact batch generation and score command mapping;
- local, Docker, Slurm, and Apptainer job construction;
- job state, wait, result, logs, cancellation, and timeout behavior;
- score and generation materialization into R objects;
- raw tensor results remain file-backed;
- setup and doctor behavior for each supported engine;
- no NGC credential is required for an explicit public checkpoint;
- no credential value appears in commands, logs, objects, or generated files.

### Integration acceptance

The split is complete when:

1. Both packages pass `R CMD check` with no errors, warnings, or notes.
2. `nimr` installs and runs mocked client tests without Python.
3. `bionemor` installs and loads without Python, then reports missing runtime
   requirements through `bionemo_doctor()`.
4. The existing NIM HTTP and deployment tests pass in `nimr` under the new
   profile API.
5. The public Evo 2 7B framework smoke test passes in `bionemor`.
6. The Evo 2 1B Brev workflow completes checkpoint conversion, baseline
   inference, a three-step full fit, and fitted inference without an NGC key.
7. The Brev training phase enforces its 300-second limit at the container
   boundary.
8. The Brev instance is stopped after the integration run.
9. The fitted checkpoint path can be passed explicitly to `nimr::nim_deploy()`
   for a supported NIM profile; command construction is tested even when the
   NIM image is unavailable to CI.

## Implementation order

1. Create `bionemor` and move the compute-side public API and tests.
2. Rename compute, job, setup, diagnostics, and result classes and functions.
3. Add first-class checkpoint preparation and typed Evo 2 fitting controls.
4. Materialize batch scores and generated sequences into R objects.
5. Remove compute-side code and dependencies from `nimr`.
6. Introduce `NimProfile` and refactor endpoint behavior behind profiles.
7. Rename NIM deployment and split setup and diagnostics.
8. Add the plain checkpoint-path interoperability example.
9. Run both package checks and the opt-in Brev integration workflow.
