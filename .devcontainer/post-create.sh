#!/usr/bin/env bash
set -euxo pipefail

# Relax hardening flags for source builds (yaml etc.)
mkdir -p ~/.R
{
  echo 'CFLAGS += -Wno-error=format-security'
  echo 'CXXFLAGS += -Wno-error=format-security'
} >> ~/.R/Makevars

# Restore environment
R -q -e "if (!requireNamespace('renv', quietly=TRUE)) install.packages('renv')"
R -q -e "renv::restore(prompt=FALSE)"

# Trace
R -q -e "cat('yaml:', as.character(utils::packageVersion('yaml')), '\nquarto:', as.character(utils::packageVersion('quarto')), '\n')"
