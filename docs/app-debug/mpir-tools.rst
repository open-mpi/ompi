.. _using-mpir-based-tools-label:

Using MPIR-based tools with Open MPI
====================================

The `legacy MPIR specification
<https://www.mpi-forum.org/docs/mpir-specification-03-01-2018.pdf>`_
is the specification of an interface that debuggers and others tools
can use to obtain the PID, hostname, and executable name for each MPI
application process.

This specification has been replaced by the PMIx tools API that is part of the
`OpenPMIx project <https://openpmix.github.io/>`_.

Debuggers and tools which use the legacy MPIR specification can
continue functioning correctly with the `MPIR shim module
<https://github.com/hpc/mpir-to-pmix-guide>`_ described in this
section.

Obtaining Application Process Mapping Information
-------------------------------------------------

The `MPIR shim module <https://github.com/hpc/mpir-to-pmix-guide>`_ is
a stand-alone module which can be used by debuggers and tools that
still utilize the legacy MPIR interface to attach to parallel MPI
applications.  The debugger or tool launches the shim module as an
intermediate process between it and the application's :ref:`mpirun(1)
<man1-mpirun>` command.

The MPIR shim module implements the ``MPIR_Breakpoint`` function, as a
hook where debuggers and tools can set a breakpoint to read the MPIR
processing mapping when it is available.

Instructions for use of the shim module are available at
https://github.com/hpc/mpir-to-pmix-guide/blob/master/README.md.

You may need to build this shim module before you can use it. The
source code for this module can be downloaded from
`MPIR to PMIx Shim repository <https://github.com/hpc/mpir-to-pmix-guide>`_.

Once you have the MPIR shim module built and installed, follow the
instructions included with the debugger or tool to tell it where to
find the MPIR shim module.

/////////////////////////////////////////////////////////////////////////

Building the MPIR Shim Module
-----------------------------

Here is an example shell script that builds the MPIR shim module
|mdash| you may need to customize it for your specific environment:

.. code-block:: bash

    #!/usr/bin/env bash

    set -euxo pipefail

    cd $HOME
    mkdir -p git
    cd git

    git clone https://github.com/hpc/mpir-to-pmix-guide.git
    cd mpir-to-pmix-guide

    # PMIX_ROOT must be set to your PMIx install root directory
    # (which may be the same as your Open MPI installation directory)
    if test -z "$PMIX_ROOT" -o ! -d "$PMIX_ROOT"; then
        echo "Set the env variable PMIX_ROOT to the location of your PMIx installation"
        exit 1
    fi
    
    ./autogen.sh
    ./configure --prefix=$HOME/MPIR --with-pmix=$PMIX_ROOT
    make
    make install


/////////////////////////////////////////////////////////////////////////

Verification of MPIR shim functionality using Open MPI
------------------------------------------------------

Once installed, you may want to verify that the MPIR shim is
functioning correctly.  Below is a shell script that builds and runs a
trivial MPI application, and tests the MPIR shim with it.  Just like
the prior shell script example, this script may need to be adapted for
your environment.

.. code-block:: bash

    #!/usr/bin/env bash

    set -euxo pipefail

    # This script assumes the same directories and locations as the
    # prior script.
    BUILD_DIR=$HOME/git/mpir-to-pmix-guide

    # Add the previously-installed MPIR shim into PATH and
    # LD_LIBRARY_PATH (we assume that all relevant Open MPI / PMIx /
    # etc. values already exist in PATH and LD_LIBRARY_PATH)
    export PATH=$HOME/MPIR/bin:$PATH
    export LD_LIBRARY_PATH=$HOME/MPIR/lib:$LD_LIBRARY_PATH

    cat > testprog.c <<EOF
    #include <mpi.h>
    #include <unistd.h>
    int main(int argc, char **argv) {
        MPI_Init(&argc, &argv);
        sleep(60);
        MPI_Finalize();
        return 0;
    }
    EOF
    mpicc -o testprog testprog.c

    # Test the shim in proxy mode.
    # Manually verify that displayed process mapping is correct.
    $BUILD_DIR/test/mpirshim_test mpirun -n 2 ./testprog
    
    # Launch mpirun for attach test and get its PID
    mpirun -n 2 ./testprog &
    PID=$!
    # Test shim attach mode.
    # Manually verify that displayed process mapping is correct.
    $BUILD_DIR/test/mpirshim_test -c $PID
