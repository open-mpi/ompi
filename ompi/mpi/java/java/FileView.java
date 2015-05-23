/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

package mpi;

/**
 * This class represents file views.
 */
public final class FileView
{
private final long disp;
private final Datatype etype, filetype;
private final String datarep;

/**
 * Constructs a file view.
 * @param disp     displacement
 * @param etype    elementary datatype
 * @param filetype file type
 * @param datarep  data representation
 */
public FileView(long disp, Datatype etype, Datatype filetype, String datarep)
{
    this.disp     = disp;
    this.etype    = etype;
    this.filetype = filetype;
    this.datarep  = datarep;
}

/**
 * Gets the displacement.
 * @return displacement
 */
public long getDisp()
{
    return disp;
}

/**
 * Gets the elementary datatype.
 * @return elementary datatype
 */
public Datatype getEType()
{
    return etype;
}

/**
 * Gets the file type.
 * @return file type
 */
public Datatype getFileType()
{
    return filetype;
}

/**
 * Gets the data representation.
 * @return data representation
 */
public String getDataRep()
{
    return datarep;
}

} // FileView
