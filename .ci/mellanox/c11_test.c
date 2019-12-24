
#include <shmem.h>
#ifdef ENABLE_PSHMEM
#include <pshmem.h>
#endif

int main(int argc, char** argv)
{
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)
#if (defined(SHMEM_MAJOR_VERSION) && SHMEM_MAJOR_VERSION >= 1) && \
    (defined(SHMEM_MINOR_VERSION) && SHMEM_MINOR_VERSION >= 4)
    char *char_ptr = 0;
    double *double_ptr = 0;
    int *int_ptr = 0;
    unsigned int *uint_ptr = 0;
    short *short_ptr = 0;
    long *long_ptr = 0;
    unsigned long *ulong_ptr = 0;
    long long *long_long_ptr = 0;
    unsigned long long *ulong_long_ptr = 0;
    float *float_ptr = 0;
    shmem_ctx_t ctx;

    shmem_p(char_ptr, 0, 0);
    shmem_p(ctx, char_ptr, 0, 0);

    shmem_g(long_ptr, 0);
    shmem_g(ctx, long_ptr, 0);

    shmem_put(double_ptr, double_ptr, 0, 0);
    shmem_put(ctx, double_ptr, double_ptr, 0, 0);

    shmem_get(long_long_ptr, long_long_ptr, 0, 0);
    shmem_get(ctx, long_long_ptr, long_long_ptr, 0, 0);

    shmem_iput(int_ptr, int_ptr, 0, 0, 0, 0);
    shmem_iput(ctx, int_ptr, int_ptr, 0, 0, 0, 0);

    shmem_iget(float_ptr, float_ptr, 0, 0, 0, 0);
    shmem_iget(ctx, float_ptr, float_ptr, 0, 0, 0, 0);

    shmem_put_nbi(short_ptr, short_ptr, 0, 0);
    shmem_put_nbi(ctx, short_ptr, short_ptr, 0, 0);

    shmem_get_nbi(short_ptr, short_ptr, 0, 0);
    shmem_get_nbi(ctx, short_ptr, short_ptr, 0, 0);

    shmem_atomic_swap(int_ptr, 0, 0);
    shmem_atomic_swap(ctx, int_ptr, 0, 0);

    shmem_atomic_set(int_ptr, 0, 0);
    shmem_atomic_set(ctx, int_ptr, 0, 0);

    shmem_atomic_fetch(int_ptr, 0);
    shmem_atomic_fetch(ctx, int_ptr, 0);

    shmem_atomic_compare_swap(long_ptr, 0, 0, 0);
    shmem_atomic_compare_swap(ctx, long_ptr, 0, 0, 0);

    shmem_atomic_fetch_add(long_ptr, 0, 0);
    shmem_atomic_fetch_add(ctx, long_ptr, 0, 0);

    shmem_atomic_fetch_and(ulong_ptr, 0, 0);
    shmem_atomic_fetch_and(ctx, ulong_ptr, 0, 0);

    shmem_atomic_fetch_or(ulong_long_ptr, 0, 0);
    shmem_atomic_fetch_or(ctx, ulong_long_ptr, 0, 0);

    shmem_atomic_fetch_xor(uint_ptr, 0, 0);
    shmem_atomic_fetch_xor(ctx, uint_ptr, 0, 0);

    shmem_atomic_fetch_inc(int_ptr, 0);
    shmem_atomic_fetch_inc(ctx, int_ptr, 0);

    shmem_atomic_add(long_ptr, 0, 0);
    shmem_atomic_add(ctx, long_ptr, 0, 0);

    shmem_atomic_inc(int_ptr, 0);
    shmem_atomic_inc(ctx, int_ptr, 0);

    shmem_atomic_and(ulong_ptr, 0, 0);
    shmem_atomic_and(ctx, ulong_ptr, 0, 0);

    shmem_atomic_or(uint_ptr, 0, 0);
    shmem_atomic_or(ctx, uint_ptr, 0, 0);

    shmem_atomic_xor(ulong_long_ptr, 0, 0);
    shmem_atomic_xor(ctx, ulong_long_ptr, 0, 0);

    shmem_wait_until(short_ptr, 0, 0);
    shmem_test(int_ptr, 0, 0);

#ifdef ENABLE_PSHMEM
    pshmem_p(char_ptr, 0, 0);
    pshmem_p(ctx, char_ptr, 0, 0);

    pshmem_g(long_ptr, 0);
    pshmem_g(ctx, long_ptr, 0);

    pshmem_put(double_ptr, double_ptr, 0, 0);
    pshmem_put(ctx, double_ptr, double_ptr, 0, 0);

    pshmem_get(long_long_ptr, long_long_ptr, 0, 0);
    pshmem_get(ctx, long_long_ptr, long_long_ptr, 0, 0);

    pshmem_iput(int_ptr, int_ptr, 0, 0, 0, 0);
    pshmem_iput(ctx, int_ptr, int_ptr, 0, 0, 0, 0);

    pshmem_iget(float_ptr, float_ptr, 0, 0, 0, 0);
    pshmem_iget(ctx, float_ptr, float_ptr, 0, 0, 0, 0);

    pshmem_put_nbi(short_ptr, short_ptr, 0, 0);
    pshmem_put_nbi(ctx, short_ptr, short_ptr, 0, 0);

    pshmem_get_nbi(short_ptr, short_ptr, 0, 0);
    pshmem_get_nbi(ctx, short_ptr, short_ptr, 0, 0);

    pshmem_atomic_swap(int_ptr, 0, 0);
    pshmem_atomic_swap(ctx, int_ptr, 0, 0);

    pshmem_atomic_set(int_ptr, 0, 0);
    pshmem_atomic_set(ctx, int_ptr, 0, 0);

    pshmem_atomic_fetch(int_ptr, 0);
    pshmem_atomic_fetch(ctx, int_ptr, 0);

    pshmem_atomic_compare_swap(long_ptr, 0, 0, 0);
    pshmem_atomic_compare_swap(ctx, long_ptr, 0, 0, 0);

    pshmem_atomic_fetch_add(long_ptr, 0, 0);
    pshmem_atomic_fetch_add(ctx, long_ptr, 0, 0);

    pshmem_atomic_fetch_and(ulong_ptr, 0, 0);
    pshmem_atomic_fetch_and(ctx, ulong_ptr, 0, 0);

    pshmem_atomic_fetch_or(ulong_long_ptr, 0, 0);
    pshmem_atomic_fetch_or(ctx, ulong_long_ptr, 0, 0);

    pshmem_atomic_fetch_xor(uint_ptr, 0, 0);
    pshmem_atomic_fetch_xor(ctx, uint_ptr, 0, 0);

    pshmem_atomic_fetch_inc(int_ptr, 0);
    pshmem_atomic_fetch_inc(ctx, int_ptr, 0);

    pshmem_atomic_add(long_ptr, 0, 0);
    pshmem_atomic_add(ctx, long_ptr, 0, 0);

    pshmem_atomic_inc(int_ptr, 0);
    pshmem_atomic_inc(ctx, int_ptr, 0);

    pshmem_atomic_and(ulong_ptr, 0, 0);
    pshmem_atomic_and(ctx, ulong_ptr, 0, 0);

    pshmem_atomic_or(uint_ptr, 0, 0);
    pshmem_atomic_or(ctx, uint_ptr, 0, 0);

    pshmem_atomic_xor(ulong_long_ptr, 0, 0);
    pshmem_atomic_xor(ctx, ulong_long_ptr, 0, 0);

    pshmem_wait_until(short_ptr, 0, 0);
    pshmem_test(int_ptr, 0, 0);
#endif
#endif
#endif
    return 0;
}
