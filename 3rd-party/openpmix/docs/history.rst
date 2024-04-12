History
========

The Process Management Interface (PMI) has been used for quite some
time as a means of exchanging wireup information needed for
interprocess communication. Two versions (PMI-1 and PMI-2) have been
released as part of the MPICH effort. While PMI-2 demonstrates better
scaling properties than its PMI-1 predecessor, attaining rapid launch
and wireup of the roughly 1M processes executing across 100k nodes
expected for exascale operations remains challenging.

PMI Exascale (PMIx) represents an attempt to resolve these questions
by providing an extended version of the PMI standard specifically
designed to support clusters up to and including exascale sizes. The
overall objective of the project is not to branch the existing
pseudo-standard definitions - in fact, PMIx fully supports both of the
existing PMI-1 and PMI-2 APIs - but rather to (a) augment and extend
those APIs to eliminate some current restrictions that impact
scalability, and (b) provide a reference implementation of the
PMI-server that demonstrates the desired level of scalability.
