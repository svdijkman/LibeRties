FROM r-base:4.4.0

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/liberties

COPY LibeRtAD /opt/LibeRtAD
COPY LibeRation /opt/LibeRation
COPY LibeRties /opt/LibeRties

RUN R -e "install.packages(c('Rcpp','jsonlite','plumber','digest','callr'), repos='https://cloud.r-project.org')" && \
    R CMD INSTALL /opt/LibeRtAD && \
    R CMD INSTALL /opt/LibeRation && \
    R CMD INSTALL /opt/LibeRties

ENV LIBERTIES_LAUNCHER=local
ENTRYPOINT ["Rscript", "/opt/LibeRties/inst/worker/run_job.R"]
