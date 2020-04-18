/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*   File:         kmeans.h   (an OpenMP version)                            */
/*   Description:  header file for a simple k-means clustering program       */
/*                                                                           */
/*   Author:  Wei-keng Liao                                                  */
/*            ECE Department Northwestern University                         */
/*            email: wkliao@ece.northwestern.edu                             */
/*                                                                           */
/*   Copyright (C) 2005, Northwestern University                             */
/*   See COPYRIGHT notice in top-level directory.                            */
/*                                                                           */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef _H_KMEANS
#define _H_KMEANS

#include <assert.h>

int omp_kmeans(int, float**, int, int, int, float, int*, float**);
int seq_kmeans(float**, int, int, int, float, int*, float**);

float** file_read(int, char*, int*, int*);
int     file_write(char*, int, int, int, float**, int*, int);

int read_n_objects(int, char*, int, int, float**);

int check_repeated_clusters(int, int, float**);

double  wtime(void);

extern int _debug;

#endif
