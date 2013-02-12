Copyright (c) 2012      Mellanox Technologies, Inc.
                        All rights reserved.


Build SHMEM
-----------

./autogen.sh
./configure --prefix=$PWD/install --with-oshmem -enable-contrib-no-build=libnbc,vt --with-openib
make 
make install
export SHMEM_HOME=$PWD/install

Run SHMEM
---------

$SHMEM_HOME/bin/shmemrun -np 10 --host amd1,amd5,amd7 -mca btl openib,self hello_shmem.exe


