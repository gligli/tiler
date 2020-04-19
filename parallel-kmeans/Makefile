#  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#    File:         Makefile                                                  */
#    Description:  Makefile for programs running a simple k-means clustering */
#                  algorithm                                                 */
#                                                                            */
#    Author:  Wei-keng Liao                                                  */
#             ECE Department Northwestern University                         */
#             email: wkliao@ece.northwestern.edu                             */
#                                                                            */
#    Copyright (C) 2005, Northwestern University                             */
#    See COPYRIGHT notice in top-level directory.                            */
#                                                                            */
#  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

.KEEP_STATE:

all: seq omp mpi

ENABLE_PNETCDF = no
PNETCDF_DIR    = $(HOME)/PnetCDF

CC             = gcc
OMPCC          = gcc
MPICC          = mpicc

INCFLAGS    = -I.
OPTFLAGS    = -O4 -DNDEBUG -flto -fuse-linker-plugin
LDFLAGS     = $(OPTFLAGS)

ifeq ($(ENABLE_PNETCDF), yes)
INCFLAGS   += -I$(PNETCDF_DIR)/include
DFLAGS     += -D_PNETCDF_BUILT
LIBS       += -L$(PNETCDF_DIR)/lib -lpnetcdf
endif

CFLAGS      = $(OPTFLAGS) $(DFLAGS) $(INCFLAGS)


# please check the compile manual for the openmp flag
# Here, I am using gcc and the flag is -fopenmp
# If icc is used, it is -opnemp
#
OMPFLAGS    = -fopenmp

ifeq ($(ENABLE_PNETCDF), yes)
OMPCC       = $(MPICC)
endif

H_FILES     = kmeans.h

COMM_SRC = file_io.c util.c

#------   OpenMP version -----------------------------------------
OMP_SRC     = omp_main.c \
	      omp_kmeans.c

OMP_OBJ     = $(OMP_SRC:%.c=%.o) $(COMM_SRC:%.c=%.o)

ifeq ($(ENABLE_PNETCDF), yes)
OMP_OBJ    += pnetcdf_io.o
endif

omp_main.o: omp_main.c $(H_FILES)
	$(OMPCC) $(CFLAGS) $(OMPFLAGS) -c $*.c

omp_kmeans.o: omp_kmeans.c $(H_FILES)
	$(OMPCC) $(CFLAGS) $(OMPFLAGS) -c $*.c

omp: omp_main
omp_main: $(OMP_OBJ)
	$(OMPCC) $(LDFLAGS) $(OMPFLAGS) -o $@ $(OMP_OBJ) $(LIBS)

#------   MPI version -----------------------------------------
MPI_SRC     = mpi_main.c   \
              mpi_kmeans.c \
              mpi_io.c

ifeq ($(ENABLE_PNETCDF), yes)
MPI_SRC    += pnetcdf_io.c
endif

MPI_OBJ     = $(MPI_SRC:%.c=%.o) $(COMM_SRC:%.c=%.o)

mpi_main.o: mpi_main.c $(H_FILES)
	$(MPICC) $(CFLAGS) -c $*.c

mpi_kmeans.o: mpi_kmeans.c $(H_FILES)
	$(MPICC) $(CFLAGS) -c $*.c

mpi_io.o: mpi_io.c $(H_FILES)
	$(MPICC) $(CFLAGS) -c $*.c

pnetcdf_io.o: pnetcdf_io.c $(H_FILES)
	$(MPICC) $(CFLAGS) -c $*.c

mpi: mpi_main
mpi_main: $(MPI_OBJ) $(H_FILES)
	$(MPICC) $(LDFLAGS) -o $@ $(MPI_OBJ) $(LIBS)

bin2nc: bin2nc.c
	$(MPICC) $(CFLAGS) $(LDFLAGS) $< -o $@ $(LIBS)

#------   sequential version -----------------------------------------
SEQ_SRC     = seq_main.c   \
              seq_kmeans.c \
	      wtime.c

SEQ_OBJ     = $(SEQ_SRC:%.c=%.o) $(COMM_SRC:%.c=%.o)

$(SEQ_OBJ): $(H_FILES)

seq_main.o: seq_main.c $(H_FILES)
	$(CC) $(CFLAGS) -c $*.c

seq_kmeans.o: seq_kmeans.c $(H_FILES)
	$(CC) $(CFLAGS) -c $*.c

wtime.o: wtime.c
	$(CC) $(CFLAGS) -c $*.c

seq: seq_main
seq_main: $(SEQ_OBJ) $(H_FILES)
	$(CC) $(LDFLAGS) -o $@ $(SEQ_OBJ) $(LIBS)

IMAGE_FILES =   color100.txt   color17695.bin   color17695.nc \
                 edge100.txt    edge17695.bin    edge17695.nc \
              texture100.txt texture17695.bin texture17695.nc

INPUTS = $(IMAGE_FILES:%=Image_data/%)

PACKING_LIST = $(COMM_SRC) $(SEQ_SRC) $(OMP_SRC) $(MPI_SRC) $(H_FILES) \
               Makefile README COPYRIGHT sample.output bin2nc.c

dist:
	dist_dir=parallel-kmeans \
	&& rm -rf $$dist_dir $$dist_dir.tar.gz\
	&& mkdir -p $$dist_dir/Image_data \
	&& cp $(PACKING_LIST) $$dist_dir \
	&& cp $(INPUTS) $$dist_dir/Image_data \
	&& tar -cf - $$dist_dir | gzip > $$dist_dir.tar.gz \
	&& rm -rf $$dist_dir

clean:
	rm -rf *.o omp_main seq_main mpi_main \
		bin2nc core* .make.state              \
		*.cluster_centres *.membership \
		*.cluster_centres.nc *.membership.nc \
		Image_data/*.cluster_centres Image_data/*.membership \
		Image_data/*.cluster_centres.nc Image_data/*.membership.nc

check: all
	# sequential K-means ---------------------------------------------------
	seq_main -q -b -n 4 -i Image_data/color17695.bin
	seq_main -q    -n 4 -i Image_data/color100.txt
	# OpenMP K-means using pragma atomic -----------------------------------
	omp_main -q -a -b -n 4 -i Image_data/color17695.bin
	omp_main -q -a    -n 4 -i Image_data/color100.txt
	# MPI K-means ----------------------------------------------------------
	mpiexec -n 4 mpi_main -q -b -n 4 -i Image_data/color17695.bin
	mpiexec -n 4 mpi_main -q    -n 4 -i Image_data/color100.txt
ifeq ($(ENABLE_PNETCDF), yes)
	# MPI K-means using PnetCDF --------------------------------------------
	mpiexec -n 4 mpi_main -q -n 4 -i Image_data/color17695.nc -v color17695
	# MPI+OpenMP using PnetCDF ---------------------------------------------
	mpiexec -n 1 omp_main -q -a -n 4 -i Image_data/color17695.nc -v color17695
endif

