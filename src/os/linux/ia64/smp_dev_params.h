/*
 * $HEADER$
 */

static const int LogBase2PageSize = 14;
static const int SMPPageSize = 1 << LogBase2PageSize;
static const int LogBase2ChunkSize = 16;
static const int NumMemoryBuckets = 3;
static const int MProt = MMAP_SHARED_PROT;
static const int MFlags = MMAP_SHARED_FLAGS;
static const int PagesPerDevice = 2048;
static const int LogBase2MaxPoolSize = 32;
static const int MaxStkElements = 32768;
static const bool ZeroAllocBase = false;
