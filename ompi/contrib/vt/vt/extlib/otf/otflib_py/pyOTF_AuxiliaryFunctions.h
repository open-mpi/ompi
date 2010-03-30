/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef PYOTF_AUXILIARYFUNCTIONS_H
#define PYOTF_AUXILIARYFUNCTIONS_H

uint32_t* createInt32ArrayFromSequence( PyObject* list );

/* conversion from python sequence to uint32_t array
CREATES AN ARRAY - you have to free it yourself */
uint32_t* createInt32ArrayFromSequence( PyObject* list ) {

	int i;
	int dim;
	uint32_t* ret;

	if (!PySequence_Check( list )) {
#ifdef OTF_VERBOSE
		PyErr_SetString(PyExc_TypeError,"Expecting a sequence");
#endif /* OTF_VERBOSE */
		return NULL;
	}
	
	dim= PyObject_Length( list );

	ret= (uint32_t*) malloc( sizeof(uint32_t) * dim );

	for ( i =0; i < dim; ++i ) {
	
		PyObject *o = PySequence_GetItem( list ,i );
		
		if (!PyInt_Check(o)) {
			Py_XDECREF(o);
#ifdef OTF_VERBOSE
			PyErr_SetString(PyExc_ValueError,"Expecting a sequence of integers");
#endif /* OTF_VERBOSE */
			free( ret );
			return NULL;
		}
		
		ret[i] = PyInt_AsLong(o);
		Py_DECREF(o);
	}
	
	return ret;
}



#endif /* PYOTF_AUXILIARYFUNCTIONS_H */
