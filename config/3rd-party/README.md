# 3rd-party Build Behavior

This document outlines modifications we have made to the OpenPMIX and
PRRTE build systems in order to support building Open MPI and its
dependencies in one shot.  There may be other places these techniques
are helpful for custom builds of Open MPI.

## Intercepting Macros

The motivation for this work was to allow Open MPI to ship OpenPMIx
and PRRTE in a way that customers would follow the traditional
`./configure ; make ; make install` flow, even when Open MPI is
installing dependencies (like libevent and HWLOC for PMIX and lievent,
HWLOC, and PMIX for PRRTE).  For development velocity reasons, the
goal was to 1) not fork either project, 2) be able to use submodules
to track upstream, and 3) not require any patches that would dirty the
submodule (from a git standpoint).  The OpenPMIX and PRRTE communities
were against supporting this feature natively in the upstream, which
would have been the simplest path forward.

Our solution is to inject customized macros for the Libevent, HWLOC,
and PMIx checks in both libraries.  We have to stay relatively in sync
with upstream, in terms of the `AC_DEFINE()`s and `AM_CONDITIONAL()`s
that are set, but otherwise can change the tests to fit our needs.  We
do this by taking advantage of aclocal's search pathing.  Aclocal will
search through the user-defined path, from the start, for each macro
it finds referenced in configure (or recursively referenced from
configure).  By injecting a custom search path for aclocal and
autoconf, we are able to ensure that the macros in this directory are
found before the macros in PMIX and PRRTE.

To inject the paths, Open MPI's autogen.pl modifies the environment
for calling PMIX and PRRTE's autogen.pl, setting both the ACLOCAL and
AUTOCONF environment variables .  Both are set slightly differently.
Aclocal will always write its output to the current working directory
from which it is invoked.  So we set `ACLOCAL="$aclocal -I
../../config/3rd-party/<project>"`.  The path must be relative because
there is some relative vs. absolute path behavior changes in aclocal.
`$aclocal` is either `aclocal` or the current value of the `ACLOCAL`
environment variable, allowing user customizations to still work.
Likewise, we set `AUTOCONF="$autoconf -B
../../config/3rd-party/<project>"`  The `-B` option is because
Autoconf puts its build artifacts in the directory specified by the
first `-I`.  In retrospect, this was probably a bad decision, so the
`-B` option acts like `-I`, but does not count towards the output
directory logic.  We need to set both `AUTOCONF` and `ACLOCAL`,
because both can cause macro searches, depending on configuration.

## Our Injected Macros

The injected macros (those in this directory) have the same logic for
external builds as the upstream configure macros.  The one exception
is the thread checks in Libevent; upstream uses `AC_CHECK_LIB`, which
requires a library to test against.  These macros use `AC_TRY_COMPILE`
to check preprocessor macros which provide the same information.

For cobuild cases (ie, running one big stream of configures before we
build any packages), we can't call CHECK_PACKAGE, because there's no
library yet (but we also know it will be there.  We also need to be
careful with libraries.  We won't have a libtool archive to link
against until make time, so we also don't want to add the dependency
libraries into the PMIX or PRRTE LIBS until the very end of its
configure, or any library linking test will fail (and that's no good).
