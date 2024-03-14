Unusual jobs
============

Open MPI can run many types of applications, including non-MPI programs.
This section describes some of the less common kinds of programs that can
be executed.


Running non-MPI programs with :ref:`mpirun(1) <man1-mpirun>`
------------------------------------------------------------

Non-MPI programs can be launched with :ref:`mpirun(1) <man1-mpirun>`,
for example:

.. code-block::

   shell$ mpirun -n 2 --host a,b uptime

This will launch a copy of the Unix command ``uptime`` on the hosts ``a``
and ``b``.

:ref:`mpirun(1) <man1-mpirun>` works equally well for MPI and non-MPI
applications.

Running GUI applications
------------------------

Running GUI applications depends on your local setup and may require additional
setup.

You will need to have graphics forwarding (e.g., X11
forwarding) enabled from the remote processes to the display where you
want output to appear.  In a secure environment, you can simply allow
all X requests to be shown on the target display and set the
``DISPLAY`` environment variable in all MPI processes' environments to
the target display, perhaps something like this:

.. code-block::

   shell$ hostname
   my_desktop.secure-cluster.example.com
   shell$ xhost +
   shell$ mpirun -n 4 -x DISPLAY=my_desktop.secure-cluster.example.com a.out

However, this technique is not generally suitable for unsecure
environments (because it allows anyone to read and write to your
display).  A slightly more secure way is to only allow X connections
from the nodes where your application will be running:

.. code-block::

   shell$ hostname
   my_desktop.secure-cluster.example.com
   shell$ xhost +compute1 +compute2 +compute3 +compute4
   compute1 being added to access control list
   compute2 being added to access control list
   compute3 being added to access control list
   compute4 being added to access control list
   shell$ mpirun -n 4 -x DISPLAY=my_desktop.secure-cluster.example.com a.out

(assuming that the four nodes you are running on are ``compute1``
through ``compute4``).

Other methods are available, but they involve sophisticated X
forwarding through :ref:`mpirun(1) <man1-mpirun>` and are generally
more complicated than desirable.

Running curses-based applications
---------------------------------

Open MPI provides fairly sophisticated stdin / stdout / stderr
forwarding.  However, it does not work well with curses, ncurses,
readline, or other sophisticated I/O packages that generally require
direct control of the terminal.

Every application and I/O library is different |mdash| you should try to
see if yours is supported.  But chances are that it won't work.

Launching an MPMD MPI job
-------------------------

Open MPI supports multiple program, multiple data (MPMD) style launches,
either from the command line or from a file.  For example:

.. code-block::

   shell$ mpirun -n 2 a.out : -n 2 b.out

This will launch a single parallel application, but the first two
processes will be instances of the ``a.out`` executable, and the
second two processes will be instances of the ``b.out`` executable.
In MPI terms, this will be a single ``MPI_COMM_WORLD``, but the
``a.out`` processes will be ranks 0 and 1 in ``MPI_COMM_WORLD``, while
the ``b.out`` processes will be ranks 2 and 3 in ``MPI_COMM_WORLD``.

:ref:`mpirun(1) <man1-mpirun>` can also accept a parallel application
specified in a file instead of on the command line.  For example:

.. code-block::

   shell$ mpirun --app my_appfile

where the file ``my_appfile`` contains the following:

.. code-block:: sh

   # Comments are supported; comments begin with #
   # Application context files specify each sub-application in the
   # parallel job, one per line.  The first sub-application is the 2
   # a.out processes:
   -n 2 a.out
   # The second sub-application is the 2 b.out processes:
   -n 2 b.out

This will result in the same behavior as running ``a.out`` and ``b.out``
from the command line.

Connecting independent MPI applications
---------------------------------------

In certain environments, Open MPI supports connecting multiple,
independent MPI applications using mechanism defined in the MPI
specification such as ``MPI_Comm_connect() / MPI_Comm_accept()`` and
publishing connection information using ``MPI_Publish_name() /
MPI_Lookup_name()``. These mechanisms require a centralized service
to exchange contact information across multiple jobs.

Beginning with Open MPI v5.0.0 this can be achieved by starting an
instance of the prte server with the ``report-uri`` option to
display the contact information of the prte server. This information
can then be used for launching subsequent MPI applications.

The following commands show an example for launching two MPI jobs
that will connect to each other at runtime using the MPI-2 based
functionality.


Step 1: start the standalone prte server

.. code-block::

   user@myhost:~/ompi-install/bin$ ./prte --report-uri <filename>
   DVM ready

Step 2: Launch the first MPI application providing the uri of the
prte server

.. code-block::

   user@myhost:~/app1-dir$ mpiexec --dvm file:<filename> -np 4 ./mpi_app_1

Step 3: Launch the second MPI application providing the uri of the
prte server again

.. code-block::

   user@myhost:~/app2-dir$ mpiexec --dvm file:<filename> -np 4 ./mpi_app_2


In case the prte server has been started as a system server using the
``--system-server`` argument (e.g. the nodes used by the MPI
applications are not shared by multiple jobs), the sequence can be
simplified by using ``mpiexec --dvm system`` or ``mpiexec --dvm
system-first`` instead of the uri of the prte server.
