#!/bin/sh
#
# Self-contained configure script, that does not rely
# on cross-compilation, aka no need for a platforms-file.
#
# If the env flags SRCDIR and PREFIX are not set, initialize to default...
#

# Compilation should be done as VPATH
if [ -d .svn -o -f AUTHORS ] ; then
    echo WARNING: Should not compile in source directory
    echo Please create a directory and adapt SRCDIR in this script
    return
fi

[[ -z ${XTOS_VERSION} ]] && echo "XTOS_VERSION is not set.  Please load the xt-os module"
[[ -z ${CATAMOUNT_DIR} ]] && echo "CATAMOUNT_DIR is not set.  Please load the xt-catamount module"
[[ -z ${PE_DIR} ]] && echo "PE_DIR is not set.  Please load the xt-pe module"
[[ -z ${SE_DIR} ]] && echo "SE_DIR is not set.  Please load the xt-service module"
[[ -z ${MPT_DIR} ]] && echo "MPT_DIR is not set.  Please load the xt-mpt module"
[[ -z ${PE_ENV} ]] && echo "PE_ENV is not set.  Please load the PrgEnv module"

if test "$PE_ENV" != "GNU" ; then
    echo "PrgEnv module is not GNU"
    return
fi

###################################################################

# If the env flags SRCDIR and PREFIX are not set, initialize to default...
SRCDIR=${SRCDIR:-..}

INSTALL_ROOT=/sw/xt5/ompi
VERSION=`${SRCDIR}/config/ompi_get_version.sh ${SRCDIR}/VERSION`
BUILD=gnu
SVER=cnl`echo "${XTOS_VERSION}" | cut -c1-3`
CVER=${BUILD}`gcc --version  | awk '/gcc/{print $3}'`
INSTALL_DIR=${INSTALL_ROOT}/${VERSION}/${SVER}_${CVER}

PREFIX=${PREFIX:-$INSTALL_DIR}

###################################################################

$SRCDIR/configure \
   --prefix=$PREFIX \
   --enable-static --disable-shared --disable-dlopen --disable-pretty-print-stacktrace --disable-pty-support \
   --with-threads --with-memory-manager=none \
   --without-tm --with-alps --with-portals --with-portals-config=cnl_modex \
   --enable-mca-no-build=timer-catamount,maffinity-first_use,maffinity-libnuma,ess-cnos,filem-rsh,grpcomm-cnos,pml-dr \
   --with-wrapper-ldflags="-static -L${PE_DIR}/lib/snos64 -L${SE_DIR}/lib/snos64 -L${MPT_DIR}/util/lib" \
   --with-wrapper-libs="-lpct -lalpslli -lalpsutil -lportals -lpthread -lm" \
   CPPFLAGS="-I${CATAMOUNT_DIR}/catamount/linux/include -I${PE_DIR}/include" \
   FFLAGS="-I${PE_DIR}/include" \
   FCFLAGS="-I${PE_DIR}/include" \
   LDFLAGS="-L${PE_DIR}/lib/snos64 -L${SE_DIR}/lib/snos64 -L${MPT_DIR}/util/lib" \
   LIBS="-lrt -lpct -lalpslli -lalpsutil -lportals -lpthread -lm" | tee build.log

#
# To build orted static, use the libtool-flag -all-static
#
make -s -j4 orted_LDFLAGS=-all-static all | tee -a build.log

# make -s orted_LDFLAGS=-all-static install | tee -a install.log

