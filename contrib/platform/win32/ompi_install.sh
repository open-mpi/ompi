#!/bin/sh

SOURCE=x64/debug
DEST=/cygdrive/z/ompi/

mkdir -p ${DEST}/bin
mkdir -p ${DEST}/etc
mkdir -p ${DEST}/include
mkdir -p ${DEST}/lib
mkdir -p ${DEST}/lib/openmpi
mkdir -p ${DEST}/share
mkdir -p ${DEST}/share/openmpi

#
# Copy all includes
#
cp opal/include/opal_config.h ${DEST}/share/openmpi/
cp opal/include/opal_config_bottom.h ${DEST}/share/openmpi/
cp orte/include/orte_config.h ${DEST}/share/openmpi/
cp ompi/include/mpi.h ${DEST}/include
cp ompi/include/mpif.h ${DEST}/include
cp ompi/include/mpif-config.h ${DEST}/share/openmpi/
cp ompi/include/mpif-common.h ${DEST}/share/openmpi/
cp ompi/include/ompi_config.h ${DEST}/share/openmpi/

#
# Copy the helpers .txt files
#
cp AUTHORS ${DEST}/
#
# OPAL
#
cp opal/etc/openmpi-mca-params.conf ${DEST}/etc
cp opal/mca/base/help-mca-base.txt ${DEST}/share/openmpi
cp opal/mca/base/help-mca-param.txt ${DEST}/share/openmpi
cp opal/runtime/help-opal-runtime.txt ${DEST}/share/openmpi
cp opal/tools/wrappers/help-opal-wrapper.txt ${DEST}/share/openmpi
cp opal/tools/wrappers/opalc++-wrapper-data.txt ${DEST}/share/openmpi
cp opal/tools/wrappers/opalcc-wrapper-data.txt ${DEST}/share/openmpi
#
# ORTE
#
cp orte/mca/odls/default/help-odls-default.txt ${DEST}/share/openmpi
cp orte/mca/odls/process/help-odls-process.txt ${DEST}/share/openmpi
cp orte/mca/pls/base/help-pls-base.txt ${DEST}/share/openmpi
cp orte/mca/pls/bproc/help-pls-bproc.txt ${DEST}/share/openmpi
cp orte/mca/pls/gridengine/help-pls-gridengine.txt ${DEST}/share/openmpi
cp orte/mca/pls/rsh/help-pls-rsh.txt ${DEST}/share/openmpi
cp orte/mca/pls/slurm/help-pls-slurm.txt ${DEST}/share/openmpi
cp orte/mca/pls/tm/help-pls-tm.txt ${DEST}/share/openmpi
cp orte/mca/ras/gridengine/help-ras-gridengine.txt ${DEST}/share/openmpi/
cp orte/mca/ras/slurm/help-ras-slurm.txt ${DEST}/share/openmpi/
cp orte/mca/rds/hostfile/help-rds-hostfile.txt ${DEST}/share/openmpi
cp orte/mca/rmaps/base/help-orte-rmaps-base.txt ${DEST}/share/openmpi
cp orte/mca/rmaps/round_robin/help-orte-rmaps-rr.txt ${DEST}/share/openmpi
cp orte/mca/rmgr/base/help-rmgr-base.txt ${DEST}/share/openmpi
cp orte/runtime/help-orte-runtime.txt ${DEST}/share/openmpi
cp orte/tools/console/help-orteconsole.txt ${DEST}/share/openmpi/
cp orte/tools/orte-clean/help-orte-clean.txt ${DEST}/share/openmpi
cp orte/tools/orte-clean/orte-clean.1 ${DEST}/share/openmpi
cp orte/tools/orte-ps/help-orte-ps.txt ${DEST}/share/openmpi/
cp orte/tools/orte-ps/orte-ps.1 ${DEST}/share/openmpi
cp orte/tools/orted/help-orted.txt ${DEST}/share/openmpi
cp orte/tools/orteprobe/help-orteprobe.txt ${DEST}/share/openmpi
cp orte/tools/orterun/help-orterun.txt ${DEST}/share/openmpi
cp orte/tools/orterun/orterun.1 ${DEST}/share/openmpi/
cp orte/tools/wrappers/ortec++-wrapper-data.txt ${DEST}/share/openmpi
cp orte/tools/wrappers/ortecc-wrapper-data.txt ${DEST}/share/openmpi
#
# OMPI
#
cp ompi/mca/bml/r2/help-mca-bml-r2.txt ${DEST}/share/openmpi/
cp ompi/mca/btl/base/help-mpi-btl-base.txt ${DEST}/share/openmpi/
cp ompi/mca/btl/mvapi/help-mpi-btl-mvapi.txt ${DEST}/share/openmpi/
cp ompi/mca/btl/openib/help-mpi-btl-openib.txt ${DEST}/share/openmpi
cp ompi/mca/btl/mvapi/help-mpi-btl-mvapi.txt ${DEST}/share/openmpi
cp ompi/mca/coll/base/help-mca-coll-base.txt ${DEST}/share/openmpi
cp ompi/mca/coll/sm/help-coll-sm.txt ${DEST}/share/openmpi
cp ompi/mpi/help-mpi-api.txt ${DEST}/share/openmpi
cp ompi/runtime/help-mpi-runtime.txt ${DEST}/share/openmpi/
cp ompi/tools/ompi_info/help-ompi_info.txt ${DEST}/share/openmpi
cp ompi/tools/wrappers/mpic++-wrapper-data.txt ${DEST}/share/openmpi/
cp ompi/tools/wrappers/mpicc-wrapper-data.txt ${DEST}/share/openmpi
cp ompi/tools/wrappers/mpicxx-wrapper-data.txt ${DEST}/share/openmpi
cp ompi/tools/wrappers/mpif77-wrapper-data.txt ${DEST}/share/openmpi/
cp ompi/tools/wrappers/mpif90-wrapper-data.txt ${DEST}/share/openmpi

#
# Copy the libraries
#
cp ${SOURCE}/liborte.lib ${DEST}/lib/
cp ${SOURCE}/libompi.lib ${DEST}/lib/
cp ${SOURCE}/libopal.lib ${DEST}/lib/
cp ${SOURCE}/liborte.dll ${DEST}/lib/
cp ${SOURCE}/libompi.dll ${DEST}/lib/
cp ${SOURCE}/libopal.dll ${DEST}/lib/
cp ${SOURCE}/liborte.dll ${DEST}/bin/
cp ${SOURCE}/libompi.dll ${DEST}/bin/
cp ${SOURCE}/libopal.dll ${DEST}/bin/
cp ${SOURCE}/mca_*.dll ${DEST}/lib/openmpi
cp ${SOURCE}/mca_*.lib ${DEST}/lib/openmpi

#
# Copy the executables
#
cp ${SOURCE}/*.exe            ${DEST}/bin/
cp ${SOURCE}/opal_wrapper.exe ${DEST}/bin/
cp ${SOURCE}/opal_wrapper.exe ${DEST}/bin/ortecc.exe
cp ${SOURCE}/opal_wrapper.exe ${DEST}/bin/ortec++.exe
cp ${SOURCE}/opal_wrapper.exe ${DEST}/bin/ortecxx.exe
cp ${SOURCE}/opal_wrapper.exe ${DEST}/bin/mpicc.exe
cp ${SOURCE}/opal_wrapper.exe ${DEST}/bin/mpic++.exe
cp ${SOURCE}/opal_wrapper.exe ${DEST}/bin/mpicxx.exe
cp ${SOURCE}/opal_wrapper.exe ${DEST}/bin/mpif77.exe
cp ${SOURCE}/opal_wrapper.exe ${DEST}/bin/mpif90.exe
cp ${SOURCE}/orterun.exe      ${DEST}/bin/mpirun.exe
cp ${SOURCE}/orterun.exe      ${DEST}/bin/mpiexec.exe

#cp  ${DEST}/share/openmpi
