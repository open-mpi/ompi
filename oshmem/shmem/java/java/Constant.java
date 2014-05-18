package shmem;

class Constant
{
    protected int CMP_EQ, CMP_GE, CMP_GT, CMP_LE, CMP_LT, CMP_NE;

    protected int BARRIER_SYNC_SIZE,
                  BCAST_SYNC_SIZE,
                  COLLECT_SYNC_SIZE,
                  REDUCE_SYNC_SIZE,
                  REDUCE_MIN_WRKDATA_SIZE,
                  SYNC_VALUE;

    protected Constant()
    {
	setConstant();
    }

    private native void setConstant();

} // Constant
