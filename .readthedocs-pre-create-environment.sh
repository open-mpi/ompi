#!/bin/bash

set -euxo pipefail

# The ReadTheDocs build process does not run autogen/configure/make.
# Hence, we have to copy the PRRTE RST files (from the 3rd-party/prrte
# tree) to our docs/ tree manually.

# Ensure that we're in the RTD CI environment

if [[ "${READTHEDOCS:-no}" == "no" ]]; then
    echo "This script is only intended to be run in the ReadTheDocs CI environment"
    exit 1
fi

SCHIZO_SRC_DIR=3rd-party/prrte/src/mca/schizo/ompi
SCHIZO_TARGET_DIR=docs/schizo-ompi-rst-content

PRRTE_RST_SRC_DIR=3rd-party/prrte/src/docs/prrte-rst-content
PRRTE_RST_TARGET_DIR=docs/prrte-rst-content

# Copy the OMPI schizo file from PRRTE
#
# See lengthy comment in docs/Makefile.am about copying in RST files
# from PRRTE for a longer explanation of what is happening here.

cp -rp $SCHIZO_SRC_DIR $SCHIZO_TARGET_DIR
cp -rp $PRRTE_RST_SRC_DIR $PRRTE_RST_TARGET_DIR
