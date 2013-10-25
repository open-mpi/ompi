C *
C * Copyright (c) 2013      Mellanox Technologies, Inc.
C *                         All rights reserved.
C * $COPYRIGHT$
C * 
C * Additional copyrights may follow
C * 
C * $HEADER$
C *

        program main
        integer proc, nproc
        call START_PES(0)
        proc = MY_PE()
        nproc = NUM_PES()
        
        write(*, '("Hello, world, I am ", i2, " of ", i2)')
     &        proc, nproc
        end 
