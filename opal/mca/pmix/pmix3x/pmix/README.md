[![Build Status](https://travis-ci.org/openpmix/openpmix.svg?branch=master)](https://travis-ci.org/openpmix/openpmix)

The Process Management Interface (PMI) has been used for quite some time as a means of exchanging wireup information needed for interprocess communication. Two versions (PMI-1 and PMI-2) have been released as part of the MPICH effort. While PMI-2 demonstrates better scaling properties than its PMI-1 predecessor, attaining rapid launch and wireup of the roughly 1M processes executing across 100k nodes expected for exascale operations remains challenging.

PMI Exascale (PMIx) represents an attempt to resolve these questions by providing an extended version of the PMI standard specifically designed to support clusters up to and including exascale sizes. The overall objective of the project is not to branch the existing pseudo-standard definitions - in fact, PMIx fully supports both of the existing PMI-1 and PMI-2 APIs - but rather to (a) augment and extend those APIs to eliminate some current restrictions that impact scalability, and (b) provide a reference implementation of the PMI-server that demonstrates the desired level of scalability.

The charter of the PMIx community is to:

> Develop an open source (non-copy-left licensed) and independent (i.e., not affiliated with any specific programming model code base) standalone library to support application interactions with Resource Managers (RMs)

> Retain transparent compatibility with the existing PMI-1 and PMI-2 definitions, and any future PMI releases

> Support the _Instant On_ initiative for rapid startup of applications at exascale and beyond

> Work with the HPC community to define and implement new APIs that support evolving programming model requirements for application-RM interactions.

PMIx is designed to be particularly easy for resource managers to adopt, thus facilitating a rapid uptake into that community for application portability. Both client and server libraries are included, along with reference examples of client usage and server-side integration. A list of supported environments and versions is provided [here](etc) - please check regularly as the list is changing!

PMIx targets support for the Linux operating system.  A reasonable effort is made to support all major, modern Linux distributions; however, validation is limited to the most recent 2-3 releases of RedHat Enterprise Linux (RHEL), Fedora, CentOS, and SUSE Linux Enterprise Server (SLES). Support for vendor-specific operating systems is included as provided by the vendor.
