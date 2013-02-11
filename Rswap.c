#include "swap.h"
#include <stdio.h>

void Rswap(int *len, int *currX, int *currY,
           int *nextX, int *nextY,
           int *M, double *grid){
	swap(len, currX, currY, nextX, nextY, M, grid);
}
