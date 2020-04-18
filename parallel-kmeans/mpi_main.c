/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*   File:         mpi_main.c   (an MPI version)                             */
/*   Description:  This program shows an example on how to call a subroutine */
/*                 that implements a simple k-means clustering algorithm     */
/*                 based on Euclid distance.                                 */
/*   Input file format:                                                      */
/*                 ascii  file: each line contains 1 data object             */
/*                 binary file: first 4-byte integer is the number of data   */
/*                 objects and 2nd integer is the no. of features (or        */
/*                 coordinates) of each object                               */
/*                                                                           */
/*   Author:  Wei-keng Liao                                                  */
/*            ECE Department Northwestern University                         */
/*            email: wkliao@ece.northwestern.edu                             */
/*                                                                           */
/*   Copyright (C) 2005, Northwestern University                             */
/*   See COPYRIGHT notice in top-level directory.                            */
/*                                                                           */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>     /* strtok() */
#include <sys/types.h>  /* open() */
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>     /* getopt() */

#include <mpi.h>
int      _debug;
#include "kmeans.h"

#ifdef _PNETCDF_BUILT
#include <pnetcdf.h>
float** pnetcdf_read(char*, char*, int*, int*, MPI_Comm);
int     pnetcdf_write(char*, int, int, int, int, float**, int*, int, MPI_Comm,
                      int verbose);
int     pnetcdf_read_centers(char*, char*, int, int, float**, MPI_Comm);
#endif

int     mpi_kmeans(float**, int, int, int, float, int*, float**, MPI_Comm);
float** mpi_read(int, char*, int*, int*, MPI_Comm);
int     mpi_write(int, char*, int, int, int, float**, int*, int, MPI_Comm,
                  int verbose);


/*---< usage() >------------------------------------------------------------*/
static void usage(char *argv0, float threshold) {
    char *help =
"Usage: %s [switches] -i filename -n num_clusters\n"
"       -i filename    : file containing data to be clustered\n"
"       -c centers     : file containing initial centers. default: filename\n"
"       -b             : input file is in binary format (default no)\n"
"       -r             : output file in binary format (default no)\n"
"       -n num_clusters: number of clusters (K must > 1)\n"
"       -t threshold   : threshold value (default %.4f)\n"
"       -o             : output timing results (default no)\n"
"       -v var_name    : using PnetCDF for file input and output and var_name\n"
"                        is variable name in the netCDF file to be clustered\n"
"       -k var_name    : name of variable in the netCDF to be used as the\n"
"                        initial cluster centers. If skipped, the variable\n"
"                        name from the option \"-v\" is used\n"
"       -q             : quiet mode\n"
"       -d             : enable debug mode\n"
"       -h             : print this help information\n";
    fprintf(stderr, help, argv0, threshold);
}

/*---< main() >-------------------------------------------------------------*/
int main(int argc, char **argv) {
           int     opt;
    extern char   *optarg;
    extern int     optind;
           int     i, j;
           int     isInFileBinary, isOutFileBinary, do_pnetcdf;
           int     is_output_timing, is_print_usage, verbose;

           int     numClusters, numCoords, numObjs, totalNumObjs;
           int    *membership; /* [numObjs] */
           char   *filename, *centers_filename;
           char   *var_name, *centers_name;
           float **objects;    /* [numObjs][numCoords] data objects */
           float **clusters;   /* [numClusters][numCoords] cluster centers */
           float   threshold;
           double  timing, io_timing, clustering_timing;

           int     rank, nproc, mpi_namelen;
           char    mpi_name[MPI_MAX_PROCESSOR_NAME];

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &nproc);
    MPI_Get_processor_name(mpi_name,&mpi_namelen);

    /* some default values */
    _debug           = 0;
    verbose          = 1;
    threshold        = 0.001;
    numClusters      = 0;
    isInFileBinary   = 0;
    isOutFileBinary  = 0;
    is_output_timing = 0;
    is_print_usage   = 0;
    filename         = NULL;
    do_pnetcdf       = 0;
    var_name         = NULL;
    centers_filename = NULL;
    centers_name     = NULL;

    while ( (opt=getopt(argc,argv,"i:n:t:v:c:abdorhq"))!= EOF) {
        switch (opt) {
            case 'i': filename=optarg;
                      break;
            case 'c': centers_filename=optarg;
                      break;
            case 'b': isInFileBinary = 1;
                      break;
            case 'r': isOutFileBinary = 1;
                      break;
            case 't': threshold=atof(optarg);
                      break;
            case 'n': numClusters = atoi(optarg);
                      break;
            case 'o': is_output_timing = 1;
                      break;
            case 'v': do_pnetcdf = 1;
                      var_name = optarg;
                      break;
            case 'k': centers_name = optarg;
                      break;
            case 'q': verbose = 0;
                      break;
            case 'd': _debug = 1;
                      break;
            case 'h':
            default: is_print_usage = 1;
                      break;
        }
    }

    if (filename == 0 || numClusters <= 1 || is_print_usage == 1 ||
        (do_pnetcdf && var_name == NULL)) {
        if (rank == 0) usage(argv[0], threshold);
        MPI_Finalize();
        exit(1);
    }
    if (centers_filename == NULL) centers_filename = filename;
    if (centers_name     == NULL) centers_name     = var_name;

    if (_debug) printf("Proc %d of %d running on %s\n", rank, nproc, mpi_name);

#ifndef _PNETCDF_BUILT
    if (do_pnetcdf) {
        if (rank == 0) printf("Error: PnetCDF feature is not built\n");
        MPI_Finalize();
        exit(1);
    }
#endif

    MPI_Barrier(MPI_COMM_WORLD);
    io_timing = MPI_Wtime();

    /* read data points from file ------------------------------------------*/
    if (rank == 0)
        printf("reading data points from file %s\n",filename);

#ifdef _PNETCDF_BUILT
    if (do_pnetcdf)
        objects = pnetcdf_read(filename, var_name, &numObjs, &numCoords,
                               MPI_COMM_WORLD);
    else
#endif
        objects = mpi_read(isInFileBinary, filename, &numObjs, &numCoords,
                           MPI_COMM_WORLD);

    if (_debug) { /* print the first 4 objects' coordinates */
        int n_objs = (numObjs   < 4) ? numObjs   : 4;
        int n_coor = (numCoords < 4) ? numCoords : 4;
        for (i=0; i<n_objs; i++) {
            char strline[1024], strfloat[16];
            sprintf(strline,"%d: objects[%d]= ",rank,i);
            for (j=0; j<n_coor; j++) {
                sprintf(strfloat,"%10f",objects[i][j]);
                strcat(strline, strfloat);
            }
            strcat(strline, "\n");
            printf("%s",strline);
        }
    }

    /* get the total number of data points */
    MPI_Allreduce(&numObjs, &totalNumObjs, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (totalNumObjs < numClusters) {
        if (rank == 0)
            printf("Error: number of clusters must be larger than the number of data points to be clustered.\n");
        free(objects[0]);
        free(objects);
        MPI_Finalize();
        return 1;
    }

    /* allocate a 2D space for clusters[] (coordinates of cluster centers)
       this array should be the same across all processes                  */
    clusters    = (float**) malloc(numClusters *             sizeof(float*));
    assert(clusters != NULL);
    clusters[0] = (float*)  malloc(numClusters * numCoords * sizeof(float));
    assert(clusters[0] != NULL);
    for (i=1; i<numClusters; i++)
        clusters[i] = clusters[i-1] + numCoords;

    /* checking if numObjs < nproc is done in the I/O routine */

    /* read the first numClusters elements from file centers_filename as the
     * initial cluster centers*/
#ifdef _PNETCDF_BUILT
    if (do_pnetcdf) {
        int err, minErr;
        err = pnetcdf_read_centers(centers_filename, centers_name, numClusters,
                                   numCoords, clusters, MPI_COMM_WORLD);
        MPI_Allreduce(&err, &minErr, 1, MPI_INT, MPI_MIN, MPI_COMM_WORLD);
        if (err != 1) {
            MPI_Finalize();
            return 1;
        }
    }
    else {
#endif
        if (rank == 0) {
            if (numObjs < numClusters || centers_filename != filename) {
                printf("reading initial %d centers from file %s\n", numClusters,
                       centers_filename);
                /* read the first numClusters data points from file */
                read_n_objects(isInFileBinary, centers_filename, numClusters,
                               numCoords, clusters);
            }
            else {
                printf("selecting the first %d elements as initial centers\n",
                       numClusters);
                /* copy the first numClusters elements in feature[] */
                for (i=0; i<numClusters; i++)
                    for (j=0; j<numCoords; j++)
                        clusters[i][j] = objects[i][j];
            }
        }
        MPI_Bcast(clusters[0], numClusters*numCoords, MPI_FLOAT, 0, MPI_COMM_WORLD);
#ifdef _PNETCDF_BUILT
    }
#endif

    /* check initial cluster centers for repeatition */
    if (check_repeated_clusters(numClusters, numCoords, clusters) == 0) {
        printf("Error: some initial clusters are repeated. Please select distinct initial centers\n");
        MPI_Finalize();
        return 1;
    }

    if (_debug && rank == 0) {
        printf("Sorted initial cluster centers:\n");
        for (i=0; i<numClusters; i++) {
            printf("clusters[%d]=",i);
            for (j=0; j<numCoords; j++)
                printf(" %6.2f", clusters[i][j]);
            printf("\n");
        }
    }

    timing            = MPI_Wtime();
    io_timing         = timing - io_timing;
    clustering_timing = timing;

    /* membership: the cluster id for each data object */
    membership = (int*) malloc(numObjs * sizeof(int));
    assert(membership != NULL);

    /* start the core computation -------------------------------------------*/
    mpi_kmeans(objects, numCoords, numObjs, numClusters, threshold, membership,
               clusters, MPI_COMM_WORLD);

    free(objects[0]);
    free(objects);

    timing            = MPI_Wtime();
    clustering_timing = timing - clustering_timing;

    /* output: the coordinates of the cluster centres ----------------------*/
#ifdef _PNETCDF_BUILT
    if (do_pnetcdf)
        pnetcdf_write(filename, 1, numClusters, numObjs, numCoords, clusters,
                      membership, totalNumObjs, MPI_COMM_WORLD, verbose);
    else
#endif
        mpi_write(isOutFileBinary, filename, numClusters, numObjs, numCoords,
                  clusters, membership, totalNumObjs, MPI_COMM_WORLD, verbose);

    free(membership);
    free(clusters[0]);
    free(clusters);

    /*---- output performance numbers ---------------------------------------*/
    if (is_output_timing) {
        double max_io_timing, max_clustering_timing;

        io_timing += MPI_Wtime() - timing;

        /* get the max timing measured among all processes */
        MPI_Reduce(&io_timing, &max_io_timing, 1, MPI_DOUBLE,
                   MPI_MAX, 0, MPI_COMM_WORLD);
        MPI_Reduce(&clustering_timing, &max_clustering_timing, 1, MPI_DOUBLE,
                   MPI_MAX, 0, MPI_COMM_WORLD);

        if (rank == 0) {
            printf("\nPerforming **** Simple Kmeans  (MPI) ****\n");
            printf("Num of processes = %d\n", nproc);
            printf("Input file:        %s\n", filename);
            printf("numObjs          = %d\n", totalNumObjs);
            printf("numCoords        = %d\n", numCoords);
            printf("numClusters      = %d\n", numClusters);
            printf("threshold        = %.4f\n", threshold);

            printf("I/O time           = %10.4f sec\n", max_io_timing);
            printf("Computation timing = %10.4f sec\n", max_clustering_timing);
        }
    }

    MPI_Finalize();
    return(0);
}

