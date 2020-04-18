/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*   File:         bin2nc.c                                                  */
/*   Description:  A parallel tool to convert a binary file to netCDF file   */
/*                                                                           */
/*   Author:  Wei-keng Liao                                                  */
/*            EECS Department, Northwestern University                       */
/*            email: wkliao@eecs.northwestern.edu                            */
/*                                                                           */
/*   Copyright (C) 2005, Northwestern University                             */
/*   See COPYRIGHT notice in top-level directory.                            */
/*                                                                           */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
extern int errno;

#include <mpi.h>
#include <pnetcdf.h>

/* Handle errors by printing an error message and exiting with a
 * non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", ncmpi_strerror(e)); exit(ERRCODE);}

/*----< main() >------------------------------------------------------------*/
int main(int argc, char **argv) {

    int fd, rank, nprocs, retval, debug=0;
    int ncid, varid, dimids[2];
    int num_elements, num_coords;
    char *var_name, *path, *in_filename, out_filename[256];
    float *buf;
    MPI_Offset offset, len, start[2], count[2];
    MPI_File fh;
    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

    if (argc != 2) {
        printf("Usage: %s filename\n",argv[0]);
        MPI_Finalize();
        return 1;
    }

    /* read from binary file to get num_elements and num_coords */
    if (rank == 0) {
        if (-1 == (fd = open(argv[1], O_RDONLY, 0666))) {
            printf("Error! open() failed %s (error: %s)\n", argv[1],strerror(errno));
            num_elements = -1; /* indicating failed open */
        } else {
            read(fd, &num_elements, sizeof(int));
            read(fd, &num_coords,   sizeof(int));
            close(fd);
            printf("input file: num_elements=%d num_coords=%d\n",num_elements,num_coords);
        } 
    }
    MPI_Bcast(&num_elements, 1, MPI_INT, 0, MPI_COMM_WORLD);
    MPI_Bcast(&num_coords,   1, MPI_INT, 0, MPI_COMM_WORLD);
    if (num_elements == -1) {
        if (errno == 0 && rank == 0)
            printf("Error: illegal num_elements (%d)\n",num_elements);
        MPI_Finalize();
        return 1;
    }

    /* parallel read from binary file */
    MPI_File_open(MPI_COMM_WORLD, argv[1], MPI_MODE_RDONLY, MPI_INFO_NULL, &fh);

    len     = num_elements/nprocs;
    offset  = len * rank;
    offset += (rank < num_elements % nprocs) ? rank : num_elements % nprocs;
    if (rank < num_elements % nprocs) len++;

    start[0] = offset;
    start[1] = 0;
    count[0] = len;
    count[1] = num_coords;
    if (debug) printf("%d: start=%lld %lld count=%lld %lld\n",rank,start[0],start[1],count[0],count[1]);

    offset *= num_coords * sizeof(float);
    offset += 4 + 4; /* the two integers at the beginning */

    buf = (float*) malloc(len*num_coords * sizeof(float));

    /* read in parallel */
    MPI_File_read_at_all(fh, offset, buf, len*num_coords, MPI_FLOAT, &status);
    MPI_File_close(&fh);

    /* define variable by taking the input file name as its name */
    in_filename = strrchr(argv[1], '/');
    if (in_filename != NULL) {
        *in_filename = '\0';
        in_filename++;
        path = argv[1];
    }
    else {
        in_filename = argv[1];
        path = ".";
    }
    var_name = strtok(in_filename, ".");
    sprintf(out_filename, "%s/%s.nc", path, var_name);

    /* Create the file. The NC_CLOBBER parameter tells netCDF to
     * overwrite this file, if it already exists.*/
    if ((retval = ncmpi_create(MPI_COMM_WORLD, out_filename, NC_CLOBBER | NC_64BIT_DATA,
                               MPI_INFO_NULL, &ncid)))
        ERR(retval);

    /* Define dimensions. NetCDF will return an ID for each. */
    if ((retval = ncmpi_def_dim(ncid, "num_elements", num_elements, &dimids[0])))
        ERR(retval);
    if ((retval = ncmpi_def_dim(ncid, "num_coordinates", num_coords, &dimids[1])))
        ERR(retval);

    /* Define the variable. Data type in this case is * NC_FLOAT (4-byte float). */
    if ((retval = ncmpi_def_var(ncid, var_name, NC_FLOAT, 2, dimids, &varid)))
        ERR(retval);

    /* End define mode. This tells netCDF we are done defining metadata. */
    if ((retval = ncmpi_enddef(ncid)))
        ERR(retval);

    /* write in parallel */
    if ((retval = ncmpi_put_vara_float_all(ncid, varid, start, count, buf)))
        ERR(retval);

    free(buf);
    /* Close the file */
    if ((retval = ncmpi_close(ncid)))
       ERR(retval);

    if (rank ==0) printf("Successfully create file %s\n",out_filename);
    MPI_Finalize();
    return 0;
}

