Supported fault tolerance techniques
====================================

Open MPI is a vehicle for research in fault tolerance and over the years
provided support for a wide range of resilience techniques:

* Currently supported
  
    * User Level Fault Mitigation techniques similar to those defined
      in the context of the MPI Forum (this is the closest match when
      migrating from FT-MPI); :ref:`see its documentation section
      <ulfm-label>`.

* Only for research / non-production usage
      
    * Message logging techniques. Similar to those implemented in
      MPICH-V.

* Deprecated / no longer available
  
    * Coordinated and uncoordinated process checkpoint and
      restart. Similar to those implemented in LAM/MPI and MPICH-V,
      respectively.
    * Data Reliability and network fault tolerance. Similar to those
      implemented in LA-MPI.

Current fault tolerance development
-----------------------------------

The only active work in resilience in Open MPI targets the User Level Fault
Mitigation (ULFM) approach, a technique discussed in the context of the MPI
standardization body.

For information on the Fault Tolerant MPI prototype in Open MPI see the
links below:

*  :ref:`Open MPI's ULFM documentation section <ulfm-label>`
* `MPI Forum's Fault Tolerance Working Group <https://github.com/mpiwg-ft/ft-issues/wiki>`_
* `Information, examples, and support <https://fault-tolerance.org/>`_

Support for other types of resilience (e.g., :ref:`data reliability <ft-data-reliability-label>`,
:ref:`checkpoint <ft-checkpoint-restart-label>`) has been deprecated over the
years due to lack of adoption and lack of maintenance. If you are interested
in doing some archeological work, traces are still available on the main
repository.
