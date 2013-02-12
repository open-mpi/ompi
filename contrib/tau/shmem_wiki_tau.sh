#!/bin/bash
# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved.

if [[ -n "$1" ]]; then
    OSHMEM_INST=$1
else
    echo "Path to OpenShmem install should be passed as an argument and built with --with-pmi"
fi

export PATH=$OSHMEM_INST/bin:$PATH

OSHMEM_TAU_PATCH=`pwd`/tau_openshmem.patch

# TAU expects having shmcc compiler name for Open SMEM
#ln -s $OSHMEM_INST/bin/shmemcc $OSHMEM_INST/bin/oshcc
#ln -s $OSHMEM_INST/share/openshmem/shmemcc-wrapper-data.txt $OSHMEM_INST/share/openshmem/oshcc-wrapper-data.txt


# download PDT sources
wget -nc http://tau.uoregon.edu/pdt_releases/pdtoolkit-3.17.tar.gz

# build PDT
tar -xzf pdtoolkit-3.17.tar.gz
cd pdtoolkit-*
PDT_INST=$PWD
./configure
make install
cd ..


function install_openshmem_tau_patch
{
cat > ${OSHMEM_TAU_PATCH} <<END_MSG
*** tau-2.21.2/src/Profile/TauShmemOpenShmemC.c	2012-01-27 20:43:12.000000000 +0200
--- new/src/Profile/TauShmemOpenShmemC.c	2012-05-21 14:14:51.000000000 +0300
***************
*** 6,11 ****
--- 6,99 ----
  #define TAU_SHMEM_TAGID tau_shmem_tagid_f=tau_shmem_tagid_f%250
  #define TAU_SHMEM_TAGID_NEXT (++tau_shmem_tagid_f) % 250 
  
+ 
+ /* This section contains old API that are not part of openshmem.org specification
+  *
+  */
+ void pshmem_init (void)
+ {
+     fprintf(stderr, "Dummy %s\n", __FUNCTION__);
+     return ;
+ }  
+ 
+ void pshmem_finalize (void)
+ {
+     fprintf(stderr, "Dummy %s\n", __FUNCTION__);
+     return ;
+ }  
+ 
+ char *pshmem_nodename (void)
+ {
+     fprintf(stderr, "Dummy %s\n", __FUNCTION__);
+     return NULL;
+ }  
+ 
+ int pshmem_version (int *major, int *minor)
+ {
+     fprintf(stderr, "Dummy %s\n", __FUNCTION__);
+     return 0;
+ }  
+ 
+ void *pshmem_malloc (size_t size)
+ {
+     fprintf(stderr, "Dummy %s\n", __FUNCTION__);
+     return NULL;
+ }  
+ 
+ void pshmem_free (void *ptr)
+ {
+     fprintf(stderr, "Dummy %s\n", __FUNCTION__);
+     return ;
+ }  
+ 
+ void *pshmem_realloc (void *ptr, size_t size)
+ {
+     fprintf(stderr, "Dummy %s\n", __FUNCTION__);
+     return NULL;
+ }  
+ 
+ void *pshmem_memalign (size_t alignment, size_t size)
+ {
+     fprintf(stderr, "Dummy %s\n", __FUNCTION__);
+     return NULL;
+ }  
+ 
+ char *psherror (void)
+ {
+     fprintf(stderr, "Dummy %s\n", __FUNCTION__);
+     return NULL;
+ }  
+ 
+ char *pshmem_error (void)
+ {
+     fprintf(stderr, "Dummy %s\n", __FUNCTION__);
+     return NULL;
+ }  
+ 
+ void pshmem_sync_init (long *pSync)
+ {
+     fprintf(stderr, "Dummy %s\n", __FUNCTION__);
+     return ;
+ }  
+ 
+ #ifdef __cplusplus
+ # include <complex>
+ # define COMPLEXIFY(T) std::complex<T>
+ #else /* _cplusplus */
+ # include <complex.h>
+ # define COMPLEXIFY(T) T complex
+ #endif /* __cplusplus */
+ void pshmem_complexd_put (COMPLEXIFY (double) * dest,
+                                  const COMPLEXIFY (double) * src,
+                                  size_t nelems, int pe)
+ {
+     fprintf(stderr, "Dummy %s\n", __FUNCTION__);
+     return ;
+ }  
+ 
+ /* Old API */
+ 
+ 
  /**********************************************************
     start_pes
   **********************************************************/
END_MSG

patch --dry-run -p1 -i ${OSHMEM_TAU_PATCH}
patch -p1 -i ${OSHMEM_TAU_PATCH}

    return 0
}


# download TAU sources
wget -nc http://www.cs.uoregon.edu/research/paracomp/tau/tauprofile/dist/tau_latest.tar.gz
#wget http://tau.uoregon.edu/tau.tgz

tar -xzf tau_latest.tar.gz
cd tau-*
install_openshmem_tau_patch
TAU_INST=$PWD/inst-tau-shmem
./configure -prefix=$TAU_INST -shmem -tag=oshmem -cc=gcc -pdt=$PDT_INST -PROFILEPARAM -useropt="-g" -shmemlib=$OSHMEM_INST/lib -shmemlibrary="-lshmem -lpmi"
make install
cd ..

export PATH=$TAU_INST/x86_64/bin:$PATH
export TAU_MAKEFILE=$TAU_INST/x86_64/lib/Makefile.tau-oshmem-param-shmem-pdt
export TAU_OPTIONS=


# Example
# Note:
# srun reports error message as ORTE_ERROR_LOG: A message is attempting to be sent to a process whose contact information is unknown in file
# in case shmem library is built w/o option --with-pmi
mkdir example
cd example
if [[ -n "$2" ]]; then
    tau_cc.sh $2/test/shmem/vs/osu_latency.c -DOSHMEM -o osu_latency-tau.out
    env LD_LIBRARY_PATH=$OSHMEM_INST/lib:$LD_LIBRARY_PATH srun -n 2 osu_latency-tau.out
    cd ..
else
    tau_cc.sh $TAU_INST/../examples/shmem/c/simple.c -o simple-tau.out
    env LD_LIBRARY_PATH=$OSHMEM_INST/lib:$LD_LIBRARY_PATH srun -n 2 simple-tau.out
fi
pprof
paraprof --pack example_shmem_tau.ppk
cd ..
