#ifndef OMPI_DECLSPEC_H
#define OMPI_DECLSPEC_H

#ifdef OMPI_BUILDING
#define OMPI_DECLSPEC __declspec(dllexport)
#else
#define OMPI_DECLSPEC __declspec(dllimport)
#endif

#endif
