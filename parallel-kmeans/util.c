/*
 * This program sorts a list of N-dimensional data points starting from the
 * most significant dimension to the least. It also reports whether there
 * are repeated data points.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int col;

static int compare(const void *a, const void *b)
{
    if (((float*)a)[col] > ((float*)b)[col]) return  1;
    if (((float*)a)[col] < ((float*)b)[col]) return -1;
    return 0;
}

/*
 * sort the array and return 0 when repeated elements are found.
 * Otherwise, 1 is returned.
 */
static
int sort_array(int    nElements,
               int    nDims,
               float *array)  /* [nElements * nDims] */
{
    int i, isGroup=0, start=-1, ret, found_repeat=0;

    if (nElements == 1) return 1;

    /* sort array */
    qsort(array, nElements, nDims*sizeof(float), compare);

    for (i=1; i<nElements; i++) {
        if (array[i*nDims+col] == array[(i-1)*nDims+col]) {
            if (col == nDims-1) return 0; /* found a repeat */

            /* mark the start of a possible repeated group */
            if (start < 0) start = i-1;
            isGroup = 1;
        }
        else if (isGroup) {
            /* the group starts from start to i-1 */
            col++;
            ret = sort_array(i-start, nDims, &array[start*nDims]);
            if (ret == 0) found_repeat = 1;
            col--;
            isGroup = 0;
            start = -1;
        }
    }
    return (found_repeat == 1) ? 0 : 1;
}

int check_repeated_clusters(int     numClusters,
                            int     numCoords,
                            float **clusters)  /* [numClusters][numCoords] */
{
    col = 0;
    return sort_array(numClusters, numCoords, clusters[0]);
}

