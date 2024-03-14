.. _man1-mpisync:


mpisync
=======

.. include_body

mpisync |mdash| Open MPI timing tools


SYNTAX
------

``mpisync [options]``

``mpirun_prof [options]``

``ompi_timing_post [<timing-file>] [<processed-file>]``


DESCRIPTION
-----------

``mpisync``: determines clock offsets relative to Head Node Process
(HNP). It accepts the following options:

* ``-o``, ``--output``: The name of output file where offsets related
  to HNP will be written

* ``-h``, ``--help``: Print help information

``ompi_timing_post`` takes the timing output file as input parameter.
The events are sorted by the timestamps. Next, the timestamps are
replaced with time offsets relative to the ``first`` : ``previous``
event.

``mpirun_prof`` is a wrapper around :ref:`mpirun <man1-mpirun>` that
performs clock synchronisation and post-processing of the timing
output file.


NOTES
-----

The mpisync code was derived from MPIPerf project:
http://mpiperf.cpct.sibsutis.ru/index.php/Main/Documentation


FILES
-----

The output file has following format:

.. code::

   <hostname> <round-trip-time> <offset-from-hnp>
