FROM nvcr.io/nvidia/clara/bionemo-framework:2.6.3

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        libcurl4-openssl-dev \
        libssl-dev \
        r-base-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -q -e 'install.packages(c("generics", "jsonlite", "processx", "S7", "yaml"), repos = "https://cloud.r-project.org")'

COPY . /opt/bionemor
RUN R CMD INSTALL /opt/bionemor

WORKDIR /workspace
