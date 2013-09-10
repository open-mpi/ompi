#include "shmem.h"
#include "stdio.h"

#define N 100
static int target[N];

static int source[N];

#define STATIC_CHECK 1
#define DYNAMIC_CHECK 1
#define ATOMIC 1
#define PEER 1

int main()
{
    int  *source_d,*target_d;
    int i;

    start_pes(0);

    source_d = shmalloc(sizeof(*source_d)*N);
    target_d = shmalloc(sizeof(*target_d)*N);

    for (i = 0; i < N; i++)
    {
	source_d[i] = source[i] = 1;
	target[i] = target_d[i] = 9;
    }

    int peer = PEER;
    if (_my_pe() == 0)
    {
#if STATIC_CHECK
	int c, f;
	int a = c, b = f;
#if ATOMIC
	for (i = 0; i < N; i++)
	    target[i] = shmem_int_g(source + i, peer);
#else
	shmem_int_get(target, source, N, PEER);
#endif
#endif

#if DYNAMIC_CHECK
#if ATOMIC
	for (i = 0; i < N; i++)
	{
	    target_d[i] = shmem_int_g(source_d + i, peer);
	}
#else
	shmem_int_get(target_d, source_d, N, PEER);
#endif

#endif
    }
    if(_my_pe() == 0)
    {
	for (i = 0; i < N; i++)
	{
#if DYNAMIC_CHECK
	    if(target_d[i] != 1)
	    {
		printf("Get dynamic error %d, target + i = %p, target[0] = %d, target[1] = %d\n",i, target_d + i,target_d[0], target_d[1]);
		fflush(stdout);
		return 1;
#endif
#if STATIC_CHECK
		if (target[i] != 1)
		{
		    printf("Get static error %d, target + i = %p, target[i] = %d\n",i, target + i,target[i]);
		    fflush(stdout);
		    return 1;
		}
#endif
	    }
	}
    }
    
/*put check*/

    for (i = 0; i < N; i++)
    {
	source_d[i] = source[i] = 1;
	target[i] = target_d[i] = -9;
    }

    shmem_barrier_all();

    if (_my_pe() == 0)
    {
#if STATIC_CHECK
	shmem_int_put(target, source, N, PEER);
#endif
#if DYNAMIC_CHECK
	shmem_int_put(target_d, source_d, N, PEER);
#endif
    }

    shmem_barrier_all();

    if(_my_pe() == PEER)
    {
	for (i = 0; i < N; i++)
	{
#if DYNAMIC_CHECK
	    if(target_d[i] != 1)
	    {
		printf("Put dynamic error\n");
		fflush(stdout);
		return 1;
	    }
#endif
#if STATIC_CHECK
	    if (target[i] != 1)
	    {
		printf("Put static error\n");
		fflush(stdout);
		return 1;
	    }
#endif
	}
    }
    printf("All test passed\n");fflush(stdout);
    shmem_finalize();

    return 0;
}

