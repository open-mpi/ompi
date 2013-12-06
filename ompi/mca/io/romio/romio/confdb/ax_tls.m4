# ===========================================================================
#             http://www.nongnu.org/autoconf-archive/ax_tls.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_TLS
#
# DESCRIPTION
#
#   Provides a test for the compiler support of thread local storage (TLS)
#   extensions. Defines TLS if it is found. Currently only knows about GCC
#   and MSVC. I think SunPro uses the same as GCC, and Borland apparently
#   supports either.
#
# LICENSE
#
#   Copyright (c) 2008 Alan Woodland <ajw05@aber.ac.uk>
#
#   This program is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the
#   Free Software Foundation, either version 3 of the License, or (at your
#   option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
#   Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program. If not, see <http://www.gnu.org/licenses/>.
#
#   As a special exception, the respective Autoconf Macro's copyright owner
#   gives unlimited permission to copy, distribute and modify the configure
#   scripts that are the output of Autoconf when processing the Macro. You
#   need not follow the terms of the GNU General Public License when using
#   or distributing such scripts, even though portions of the text of the
#   Macro appear in them. The GNU General Public License (GPL) does govern
#   all other use of the material that constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the Autoconf
#   Macro released by the Autoconf Archive. When you make and distribute a
#   modified version of the Autoconf Macro, you may extend this special
#   exception to the GPL to apply to your modified version as well.

AC_DEFUN([AX_TLS], [
  AC_MSG_CHECKING(for thread local storage specifier)
  AC_CACHE_VAL(ac_cv_tls, [
    ax_tls_keywords="__thread __declspec(thread) none"
    for ax_tls_keyword in $ax_tls_keywords; do
       case $ax_tls_keyword in
          none) ac_cv_tls=none ; break ;;
	  *)
             # MPICH modification: This was an AC_TRY_COMPILE before, but
             # Darwin with non-standard compilers will accept __thread at
             # compile time but fail to link due to an undefined
             # "__emutls_get_address" symbol unless -lgcc_eh is added to the
             # link line.
             AC_LINK_IFELSE(
                 [AC_LANG_PROGRAM([$ax_tls_keyword int bar = 5;],[++bar;])],
                 [ac_cv_tls=$ax_tls_keyword ; break],
                 [ac_cv_tls=none])
          esac
    done
])

  if test "$ac_cv_tls" != "none"; then
    # MPICH modification: this was "TLS" before instead of
    # "MPIU_TLS_SPECIFIER", but TLS had a reasonably high chance of conflicting
    # with a system library.
    AC_DEFINE_UNQUOTED([MPIU_TLS_SPECIFIER], $ac_cv_tls, [If the compiler supports a TLS storage class define it to that here])
  fi
  AC_MSG_RESULT($ac_cv_tls)
])
