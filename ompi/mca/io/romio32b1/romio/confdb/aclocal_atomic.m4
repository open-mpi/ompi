dnl /*D PAC_C_MEMATOMIC - Try and determine how to implement memory-atomic
dnl   operations with the selected C compiler
dnl
dnl Synopsis:
dnl PAC_C_MEMATOMIC
dnl
dnl Notes:
dnl Defines names of the following form
dnl + HAVE_GCC_ASM_AND_X86_{MFENCE,LFENCE,SFENCE} - gcc __asm__ will issue
dnl    mfence, lfence, or sfence
dnl . HAVE___ASM_AND_X86_{MFENCE,LFENCE,SFENCE} - __asm _emit will issue
dnl    mfence, lfence, or sfence
dnl . HAVE_ASM_AND_X86_{MFENCE,LFENCE,SFENCE} - asm("...") will issue
dnl    mfence, lfence, or sfence
dnl . HAVE__INTERLOCKEDEXCHANGE - _InterlockedExchange intrinsic is available
dnl    (IA64)
dnl . HAVE_GCC_ASM_SPARC_MEMBAR - gcc __asm__ will issue SPARC architecture
dnl    memory barrier instruction
dnl . HAVE_SOLARIS_ASM_SPARC_MEMBAR - Solaris asm() will issue SPARC 
dnl    architecture memory barrier instruction
dnl . HAVE_GCC_ASM_SPARC_STBAR - gcc __asm__ will issue stbar
dnl - HAVE_SOLARIS_ASM_SPARC_STBAR - Solaris __asm() will issue stbar
dnl 
dnl D*/
AC_DEFUN([PAC_C_MEMATOMIC],[
AC_CACHE_CHECK([for x86 mfence instruction using __asm__],
    pac_cv_have_gcc_asm_and_x86_mfence,[
AC_TRY_RUN([
int main(int argc, char **argv)
{
    __asm__ __volatile__  ( ".byte 0x0f, 0xae, 0xf0" ::: "memory" );
    exit(0);
}
],
pac_cv_have_gcc_asm_and_x86_mfence=yes,pac_cv_have_gcc_asm_and_x86_mfence=no)])

if test "$pac_cv_have_gcc_asm_and_x86_mfence" = "yes" ; then
    AC_DEFINE(HAVE_GCC_ASM_AND_X86_MFENCE, 1, [Define if using gcc on a x86 system with the mfence instruction])
fi

AC_CACHE_CHECK([for x86 sfence instruction using __asm__],
    pac_cv_have_gcc_asm_and_x86_sfence,[
AC_TRY_RUN([
int main(int argc, char **argv)
{
    __asm__ __volatile__  ( ".byte 0x0f, 0xae, 0xf8" ::: "memory" );
    exit(0);
}
],
pac_cv_have_gcc_asm_and_x86_sfence=yes,pac_cv_have_gcc_asm_and_x86_sfence=no)])

if test "$pac_cv_have_gcc_asm_and_x86_sfence" = "yes" ; then
    AC_DEFINE(HAVE_GCC_ASM_AND_X86_SFENCE, 1, [Define if using gcc on a x86 system with the sfence instruction])
fi

AC_CACHE_CHECK([for x86 lfence instruction using __asm__],
    pac_cv_have_gcc_asm_and_x86_lfence,[
AC_TRY_RUN([
int main(int argc, char **argv)
{
    __asm__ __volatile__  ( ".byte 0x0f, 0xae, 0xe8" ::: "memory" );
    exit(0);
}
],
pac_cv_have_gcc_asm_and_x86_lfence=yes,pac_cv_have_gcc_asm_and_x86_lfence=no)])

if test "$pac_cv_have_gcc_asm_and_x86_lfence" = "yes" ; then
    AC_DEFINE(HAVE_GCC_ASM_AND_X86_LFENCE, 1, [Define if using gcc on a x86 system with the lfence instruction])
fi

dnl Some compilers, like icc, may want __asm _emit
AC_CACHE_CHECK([for x86 mfence instruction using __asm],
     pac_cv_have___asm_and_x86_mfence,[
AC_TRY_RUN([
int main(int argc, char **argv)
{
    __asm _emit 0x0f __asm _emit 0xae __asm _emit 0xf0 ;
    exit(0);
}
],
pac_cv_have___asm_and_x86_mfence=yes,pac_cv_have___asm_and_x86_mfence=no)])

if test "$pac_cv_have___asm_and_x86_mfence" = "yes" ; then
    AC_DEFINE(HAVE___ASM_AND_X86_MFENCE, 1, [Define if using __asm on a x86 system with the mfence instruction])
fi

AC_CACHE_CHECK([for x86 sfence instruction using __asm],
    pac_cv_have___asm_and_x86_sfence,[
AC_TRY_RUN([
int main(int argc, char **argv)
{
    __asm sfence ;
    exit(0);
}
],
pac_cv_have___asm_and_x86_sfence=yes,pac_cv_have___asm_and_x86_sfence=no)])

if test "$pac_cv_have___asm_and_x86_sfence" = "yes" ; then
    AC_DEFINE(HAVE___ASM_AND_X86_SFENCE, 1, [Define if using __asm on a x86 system with the sfence instruction])
fi

AC_CACHE_CHECK([for x86 lfence instruction using __asm],
    pac_cv_have___asm_and_x86_lfence,[
AC_TRY_RUN([
int main(int argc, char **argv)
{
    __asm _emit 0x0f __asm _emit 0xae __asm _emit 0xe8 ;
    exit(0);
}
],
pac_cv_have___asm_and_x86_lfence=yes,pac_cv_have___asm_and_x86_lfence=no)])

if test "$lac_cv_have___asm_and_x86_lfence" = "yes" ; then
    AC_DEFINE(HAVE___ASM_AND_X86_LFENCE, 1, [Define if using __asm on a x86 system with the lfence instruction])
fi

dnl 
dnl Some compilers, such as pgcc, may require additional arguments.
dnl pgcc may need -Masmkeyword flag.  We may want to try this with and 
dnl without adding -Masmkeyword to CFLAGS

AC_CACHE_CHECK([for x86 mfence instruction using asm()],
    pac_cv_have_asm_and_x86_mfence,[
AC_TRY_RUN([
int main(int argc, char **argv)
{
    asm("_emit 0x0f __asm _emit 0xae __asm _emit 0xf0");
    exit(0);
}
],
pac_cv_have_asm_and_x86_mfence=yes,pac_cv_have_asm_and_x86_mfence=no)])

if test "$pac_cv_have_asm_and_x86_mfence" = "yes" ; then
    AC_DEFINE(HAVE_ASM_AND_X86_MFENCE, 1, [Define if using asm() on a x86 system with the mfence instruction])
fi

AC_CACHE_CHECK([for x86 sfence instruction using asm()],
    pac_cv_have_asm_and_x86_sfence,[
AC_TRY_RUN([
int main(int argc, char **argv)
{
    asm("sfence");
    exit(0);
}
],
pac_cv_have_asm_and_x86_sfence=yes,pac_cv_have_asm_and_x86_sfence=no)])

if test "$pac_cv_have_asm_and_x86_sfence" = "yes" ; then
    AC_DEFINE(HAVE_ASM_AND_X86_SFENCE, 1, [Define if using asm() on a x86 system with the sfence instruction])
fi

AC_CACHE_CHECK([for x86 lfence instruction using asm()],
    pac_cv_have_asm_and_x86_lfence,[
AC_TRY_RUN([
int main(int argc, char **argv)
{
    asm("_emit 0x0f __asm _emit 0xae __asm _emit 0xe8");
    exit(0);
}
],
pac_cv_have_asm_and_x86_lfence=yes,pac_cv_have_asm_and_x86_lfence=no)])

if test "$pac_cv_have_asm_and_x86_lfence" = "yes" ; then
    AC_DEFINE(HAVE_ASM_AND_X86_LFENCE, 1, [Define if using asm() on a x86 system with the lfence instruction])
fi

AC_CACHE_CHECK([for _InterlockedExchange intrinsic],
    pac_cv_have__InterlockedExchange,[
AC_TRY_RUN([
int main(int argc, char **argv)
{
    unsigned long lock, *lock_ptr;
    lock_ptr = &lock;
    _InterlockedExchange(lock_ptr, 1);
    exit(0);
}
],
pac_cv_have__InterlockedExchange=yes,pac_cv_have__InterlockedExchange=no)])

if test "$pac_cv_have__InterlockedExchange" = "yes" ; then
    AC_DEFINE(HAVE__INTERLOCKEDEXCHANGE, 1, [Define if _InterlockedExchange intrinsic is available])
fi

AC_CACHE_CHECK([for SPARC membar instruction with gcc],
    pac_cv_gcc_sparc_membar,[
AC_TRY_RUN([
int main(int argc, char **argv){
    __asm__ __volatile__ ( "membar #StoreLoad | #StoreStore" : : : "memory" );
    exit(0);
}],pac_cv_gcc_sparc_membar=yes,pac_cv_gcc_sparc_membar=no)])
if test "$pac_cv_gcc_sparc_membar" = yes ; then
    AC_DEFINE(HAVE_GCC_ASM_SPARC_MEMBAR,1,[Define if gcc asm membar supported])
fi

AC_CACHE_CHECK([for SPARC membar instruction with Solaris C],
    pac_cv_solaris_sparc_membar,[
AC_TRY_RUN([
int main(int argc, char **argv){
    __asm ( "membar #StoreLoad | #StoreStore");
    exit(0);
}],pac_cv_solaris_sparc_membar=yes,pac_cv_solaris_sparc_membar=no)])
if test "$pac_cv_solaris_sparc_membar" = yes ; then
    AC_DEFINE(HAVE_SOLARIS_ASM_SPARC_MEMBAR,1,[Define if solaris asm membar supported])
fi

AC_CACHE_CHECK([for SPARC stbar instruction with gcc],
    pac_cv_gcc_sparc_stbar,[
AC_TRY_RUN([
int main(int argc, char **argv){
    __asm__ __volatile__ ( "stbar" : : : "memory" );
    exit(0);
}],pac_cv_gcc_sparc_stbar=yes,pac_cv_gcc_sparc_stbar=no)])
if test "$pac_cv_gcc_sparc_stbar" = yes ; then
    AC_DEFINE(HAVE_GCC_ASM_SPARC_STBAR,1,[Define if gcc asm stbar supported])
fi

AC_CACHE_CHECK([for SPARC stbar instruction with Solaris C],
    pac_cv_solaris_sparc_stbar,[
AC_TRY_RUN([
int main(int argc, char **argv){
    __asm ( "stbar" );
    exit(0);
}],pac_cv_solaris_sparc_stbar=yes,pac_cv_solaris_sparc_stbar=no)])
if test "$pac_cv_solaris_sparc_stbar" = yes ; then
    AC_DEFINE(HAVE_SOLARIS_ASM_SPARC_STBAR,1,[Define if solaris asm stbar supported])
fi
])