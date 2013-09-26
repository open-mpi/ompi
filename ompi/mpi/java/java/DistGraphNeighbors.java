package mpi;

/**
 * Adjacency information for a distributed graph topology.
 */
public final class DistGraphNeighbors
{
private final int[] sources, sourceWeights, destinations, destWeights;
private final boolean weighted;

protected DistGraphNeighbors(
        int[] sources, int[] sourceWeights,
        int[] destinations, int[] destWeights, boolean weighted)
{
    this.sources       = sources;
    this.sourceWeights = sourceWeights;
    this.destinations  = destinations;
    this.destWeights   = destWeights;
    this.weighted      = weighted;
}

/**
 * Gets the number of edges into this process.
 * @return number of edges into this process
 */
public int getInDegree()
{
    return sources.length;
}

/**
 * Gets the number of edges out of this process.
 * @return number of edges out of this process
 */
public int getOutDegree()
{
    return destinations.length;
}

/**
 * Returns false if {@code MPI_UNWEIGHTED} was supplied during creation.
 * @return false if {@code MPI_UNWEIGHTED} was supplied, true otherwise
 */
public boolean isWeighted()
{
    return weighted;
}

/**
 * Gets a process for which the calling processs is a destination.
 * @param i source index
 * @return process for which the calling processs is a destination
 */
public int getSource(int i)
{
    return sources[i];
}

/**
 * Gets the weight of an edge into the calling process.
 * @param i source index
 * @return weight of the edge into the calling process
 */
public int getSourceWeight(int i)
{
    return sourceWeights[i];
}

/**
 * Gets a process for which the calling process is a source
 * @param i destination index
 * @return process for which the calling process is a source
 */
public int getDestination(int i)
{
    return destinations[i];
}

/**
 * Gets the weight of an edge out of the calling process.
 * @param i destination index
 * @return weight of an edge out of the calling process
 */
public int getDestinationWeight(int i)
{
    return destWeights[i];
}

} // DistGraphNeighbors
