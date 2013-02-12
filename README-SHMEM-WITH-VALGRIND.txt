Copyright (c) 2012      Mellanox Technologies, Inc.
                        All rights reserved.
In order to enable valgrind support:
1) download and build (into the same dir for convenience)  two libraries: libmlx4 and libibverbs with valgrind support.
	- http://www.openfabrics.org/downloads/libmlx4/
	- ./configure --prefix=/PATH_TO_LIBS --with-valgrind && make && make install
	- http://www.openfabrics.org/downloads/libibverbs/
	- ./configure --prefix=/PATH_TO_LIBS --with-valgrind && make && make install

NOTE: libmlx4 and libibvers should match your ofed version

2) build shmem with valgrind, memchecker module and these two libs:
./autogen.sh && ./configure --prefix=$PWD/install --with-openib=/usr --with-oshmem --with-openib-libdir=PATH_TO_LIBS --with-valgrind=PATH_TO_VALGRIND --enable-memchecker --disable-dlopen && make  clean && make  && make install 

	- --with-openib=/usr - the place where to look for infiniband
	- --with-openib-libdir=PATH_TO_LIBS  - two libs previously installed
	- --with-valgrind - the --prefix  to the valgrind install. that must contain $prefix/include/valgrind/valgrind.h and $prefix/valgrind/memchecker.h. Should be /usr if valgrind installed from rpm 

==8511== Conditional jump or move depends on uninitialised value(s)
==8511==    at 0x3849A17486: index (in /lib64/ld-2.12.so)
==8511==    by 0x3849A06254: expand_dynamic_string_token (in /lib64/ld-2.12.so)
==8511==    by 0x3849A07CAF: _dl_map_object (in /lib64/ld-2.12.so)
==8511==    by 0x3849A016EA: map_doit (in /lib64/ld-2.12.so)
==8511==    by 0x3849A0E0A5: _dl_catch_error (in /lib64/ld-2.12.so)
==8511==    by 0x3849A015EE: do_preload (in /lib64/ld-2.12.so)
==8511==    by 0x3849A03BAA: dl_main (in /lib64/ld-2.12.so)
==8511==    by 0x3849A15A7D: _dl_sysdep_start (in /lib64/ld-2.12.so)
==8511==    by 0x3849A01493: _dl_start (in /lib64/ld-2.12.so)
==8511==    by 0x3849A00AF7: ??? (in /lib64/ld-2.12.so)
==8511==    by 0x2: ???
==8511==    by 0x7FF0000A2: ???
==8511==  Uninitialised value was created by a stack allocation
==8511==    at 0x3849A0328D: dl_main (in /lib64/ld-2.12.so)

In order to get rid of them use an extra command line option to valgrind: --suppression=${prefix}/share/openshmem/openmpi-valgrind.supp --suppressions=shmem_suppressions, where shmem_suppressions is the file in the root of the shmem code and ${prefix} is shmem install prefix as set by configure.

