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

cp -rp $SCHIZO_SRC_DIR $SCHIZO_TARGET_DIR

# Only copy the PRRTE RST source files in prrte-rst-content that are
# referenced by ".. include::" in the schizo-ompi-cli.rst file.  We do
# this because Sphinx complains if there are .rst files that are not
# referenced.  :-(

mkdir -p $PRRTE_RST_TARGET_DIR
files=`fgrep '.. include::' $SCHIZO_TARGET_DIR/schizo-ompi-cli.rstxt | awk '{ print $3 }'`
for file in $files; do
    filename=`basename $file`
    cp -pf $PRRTE_RST_SRC_DIR/$filename $PRRTE_RST_TARGET_DIR
done
