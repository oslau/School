#include "swap.h"
#include <stdio.h>

void swap(int *len, int *currX, int *currY,                     
            int *nextX, int *nextY,                  
            int *M, double *grid){

	int currVal, nextVal;                     
	for(int i = 0; i < len[0]; i++){
		currVal = grid[((currY[i] - 1) * M[0]) + (currX[i]-1)];
		nextVal = grid[((nextY[i] - 1) * M[0]) + (nextX[i]-1)];
		if(nextVal == 0){
			grid[((currY[i] - 1) * M[0]) + (currX[i]-1)] = 0;
			grid[((nextY[i] - 1) * M[0]) + (nextX[i]-1)] = currVal;
		}
	}
}
