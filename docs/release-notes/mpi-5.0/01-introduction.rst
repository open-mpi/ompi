.. _release-notes-mpi-5.0-ch01:

Introduction to MPI
===================


.. admonition:: tl;dr
   :class: tip

   Chapter 1 is descriptive (it inventories what the Standard includes). Open MPI implements the large majority of the listed components. Two substantive caveats: (1) the build self-reports MPI 4.1 via MPI_VERSION/MPI_SUBVERSION and MPI_Get_version (VERSION:23-24), so it does not yet \*advertise\* MPI-5.0 even though most MPI-5.0 features are present; (2) the MPI Application Binary Interface (the single largest MPI-5.0 addition, listed in 1.13) is absent on main and in-flight via PR #13280.

Conformance summary
-------------------


**6** reconciled requirement(s)/behavior(s): 4 supported, 0 conditional, 1 partial, 0 unsupported, 1 missing-but-in-flight, 0 N/A (66.7% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


MPI Application Binary Interface listed as an included Standard component (1.13)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §1.13
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=silent (reviewers disagreed)
:Gate: in-flight: PR #13280
:Evidence: ``No MPI_Abi_*, MPI_ABI_VERSION/SUBVERSION, MPI_ERR_ABI, or *_toint/*_fromint converters in ompi/include/mpi.h.in (see Chapter 20)``
:Notes: b1 marked UNSUPPORTED only because its rubric lacked an in-flight bucket (it still tagged PR #13280). Per the in-flight tiebreaker the whole ABI surface is MISSING_INFLIGHT, not UNSUPPORTED.

Implementation advertises its supported MPI standard level (MPI_VERSION / MPI_SUBVERSION macros + MPI_Get_version)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §1.10
:Reviewers: b1=PARTIAL, b2=SUPPORTED, b3=silent (reviewers disagreed)
:Evidence: VERSION:23-24 (mpi_standard_version=4, mpi_standard_subversion=1); config/autogen_found_items.m4:12-13 (MPI_VERSION_NUM=4, MPI_SUBVERSION_NUM=1); ompi/include/mpi.h.in:211-212 templated from these; ompi/mpi/c/get_version.c.in returns those macros
:Notes: The macros and MPI_Get_version work correctly and are thread-safe, but the configured values are (4,1), not (5,0). So a strict standards check (e.g. #if MPI_VERSION>=5) fails and the library advertises MPI-4.1 conformance. b1's PARTIAL is correct; b2's SUPPORTED only checked that the mechanism exists. This is the single most visible 'not yet MPI-5.0' signal in the tree.

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - Component inventory of 1.13 (point-to-point, collective, RMA, I/O, dynamic processes, datatypes, topologies, tools, etc.) maps to subsystems present in the tree
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Canonical data representation (external32) for MPI_Pack_external/Unpack_external and MPI I/O interoperability is included (1.15)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Backward compatibility: any valid MPI-4.1 program is a valid MPI-5.0 program
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - Conformance to base Standard absent explicit side-document opt-ins (1.14)
     - b1=sile/b2=SUPP/b3=sile (unique)
