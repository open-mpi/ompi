dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

AC_DEFUN([LAM_C_GET_ALIGNMENT],[
# Determine datatype alignment. 
# First arg is type, 2nd arg is config var to define.
AC_MSG_CHECKING([alignment of $1])
AC_TRY_RUN([
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

struct foo { char c; $1 x; };
int main(int argc, char* argv[])
{
    struct foo *p = (struct foo *) malloc(sizeof(struct foo));
    int diff;
    FILE *f=fopen("conftestval", "w");
    if (!f) exit(1);
    diff = ((char *)&p->x) - ((char *)&p->c);
    fprintf(f, "%d\n", (diff >= 0) ? diff : -diff);
    return 0;
}],[lam_ac_align=`cat conftestval`],[lam_ac_align=-1],[lam_ac_align=-1])

if test "`expr $lam_ac_align \<= 0`" = "1"; then
    AC_MSG_WARN([*** Problem running configure test!])
    AC_MSG_WARN([*** See config.log for details.])
    AC_MSG_ERROR([*** Cannot continue.])
fi

AC_MSG_RESULT([$lam_ac_align])
AC_DEFINE_UNQUOTED($2, $lam_ac_align, [Alignment of type $1])
eval "$2=$lam_ac_align"
unset lam_ac_align

/bin/rm -f conftest*])dnl
