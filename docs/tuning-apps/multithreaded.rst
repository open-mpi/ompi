.. _label-running-multithreaded-apps:

Running multi-threaded MPI applications
=======================================

Support for ``MPI_THREAD_MULTIPLE`` (i.e., multiple threads
executing within the MPI library) and asynchronous message passing
progress (i.e., continuing message passing operations even while no
user threads are in the MPI library) has been designed into Open MPI
from its first planning meetings.

``MPI_THREAD_MULTIPLE`` was included in the first version of
Open MPI, but it only became robust around v3.0.0.  Subsequent
releases continually improve reliability and performance of
multi-threaded MPI applications.
