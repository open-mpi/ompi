#! /usr/bin/bash

VERSION_FILES="opal/include/opal/version.h orte/include/orte/version.h ompi/include/ompi/version.h"
CONFIG_FILES="opal/include/opal_config.h orte/include/orte_config.h ompi/include/ompi_config.h"
MISC_FILES="opal/mca/installdirs/config/install_dirs.h contrib/platform/win32/generate_windows_patch.sh contrib/platform/win32/ompi_install.sh"
MPI_FILES="ompi/include/mpi.h ompi/include/mpif-common.h ompi/include/mpif.h"
STATIC_COMP_FILES=`find . -name static-components.h`

ALL_FILES="${VERSION_FILES} ${CONFIG_FILES} ${MISC_FILES} ${MPI_FILES} ${STATIC_COMP_FILES}"

OUTPUT="../ompi_patch`date +'%m%d%y'`"

rm -f ${OUTPUT}

echo Preparing SVN diffs
svn diff > ompi_patch

echo "Preparing the tar file ${OUTPUT}.tar"
tar -c `svn status | grep ".vcproj$" | awk '{print $2}'` ompi_patch ompi-trunk.sln ${ALL_FILES} > ${OUTPUT}.tar

echo "Compressing the tar file"
bzip2 -9 ${OUTPUT}.tar

echo "Cleaning up ..."
rm -f ompi_patch

echo "Done"
