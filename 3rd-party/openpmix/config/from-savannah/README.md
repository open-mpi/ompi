These files downloaded from
https://git.savannah.gnu.org/gitweb/?p=config.git at git hash
6faca61810d335c7837f320733fe8e15a1431fc2 on 26 Jan 2021.

They were stashed here in the OpenPMIx repository in response to
https://github.com/open-mpi/ompi/issues/8410, where it was determined
that the responses from `config.*` installed by Autoconf were not
sufficient for some modern platforms (e.g., Apple M1 Macs).

`autogen.pl` will copy in these files if they are, in fact, newer than
the corresponding files installed by Autoconf.
