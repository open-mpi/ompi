#!/bin/sh
#
# Self-contained configure script, that does not rely
# on cross-compilation, aka not need for a platforms-file.
#

# Compilation should be done as VPATH
if [ -d .svn -o -f AUTHORS ] ; then
    echo WARNING: Should not compile in source directory
    echo Please create a directory and adapt SRCDIR in this script
    return
fi

SRCDIR=..
PREFIX=`pwd`/usr

$SRCDIR/configure \
   --prefix=$PREFIX \
   --enable-static --disable-shared \
   --with-alps=yes --with-portals=yes --with-portals-config=cnl_modex \
   --enable-mca-no-build=maffinity-first_use,maffinity-libnuma,ess-cnos,filem-rsh,grpcomm-cnos,pml-dr \
   --with-wrapper-ldflags='-L/opt/xt-service/default/lib/snos64/ -lportals \
                           -L/opt/xt-pe/default/lib/snos64/ -lpct \
                           -L/opt/xt-mpt/default/lib/snos64/ -lalpslli -lalpsutil' \
   CPPFLAGS='-I/opt/xt-pe/default/include/' \
   FFLAGS='-I/opt/xt-pe/default/include/' \
   FCFLAGS='-I/opt/xt-pe/default/include/' \
   LDFLAGS='-L/opt/xt-service/default/lib/snos64/ -L/opt/xt-mpt/default/lib/snos64/' \
   LIBS='-lportals -lalpslli -lalpsutil' | tee build.log

#
# To build orted static, use the libtool-flag -all-static
#
make -s -j2 orted_LDFLAGS=-all-static all | tee -a build.log

