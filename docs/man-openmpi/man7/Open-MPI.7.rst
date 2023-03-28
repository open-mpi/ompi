.. _open-mpi:
.. _mpi:

Open MPI
========

.. include_body

Open MPI - General information

OPEN MPI
--------

Open MPI is an open source implementation of MPI (message-passing
interface), the industry-standard specification for writing
message-passing programs. Message passing is a programming model that
gives the programmer explicit control over interprocess communication.

The MPI specification was developed by the MPI Forum, a group of
software developers, computer vendors, academics, and computer-science
researchers whose goal was to develop a standard for writing
message-passing programs that would be efficient, flexible, and
portable.

The outcome, known as the MPI Standard, was first published in 1993; its
most recent version (MPI-4.0) was published in June 2021. Open MPI
includes all MPI |mpi_standard_version|-compliant routines.

For more information about Open MPI, see https://www.open-mpi.org.

The MPI standards are available at https://www.mpi-forum.org.

MAN PAGE SYNTAX
---------------

Man pages for Open MPI and Open MPI I/O routines are named according to
C syntax, that is, they begin with the prefix ``MPI_``, all in
uppercase, and the first letter following the ``MPI_`` prefix is also
uppercase. The rest of the letters in the routine are all lowercase, for
example, :ref:`MPI_Comm_get_attr`.

ENVIRONMENT
-----------

To fine-tune your Open MPI environment, you can either use arguments
to the :ref:`mpirun <man1-mpirun>` or :ref:`mpiexec <man1-mpiexec>`
commands, or you can use MCA parameters.

For more information on arguments, see the :ref:`mpirun(1)
<man1-mpirun>` man page.

For a complete listing of MCA parameters and their descriptions, issue
the command ``ompi_info --all``.  See :ref:`ompi_info(1)
<man1-ompi_info>` for more information.


ERRORS
------

See :ref:`MPI_Errors` for details about error values.


.. seealso:: :ref:`MPI_T` :ref:`mpirun(1) <man1-mpirun>` :ref:`mpiexec(1)
             <man1-mpiexec>` :ref:`ompi_info(1) <man1-ompi_info>`
