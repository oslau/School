library(ggplot2)
library(reshape)

#################
###USER INPUTS###
#################
grid.r = 10 ##number of rows
grid.c = 10 ##number of columns
rho = .5	##proportion of grid filled

##################
###SIMPLE CALCS###
##################
ncars = round(rho * grid.r * grid.c, 0)	##total number of cars
totcells = grid.r * grid.c	##total number of cells

##############################
###GENERATE RANDOM CAR GRID###
##############################
colors = c(rep(0, totcells-ncars), rep(1, ncars/2), rep(2, ncars/2))
grid = matrix(sample(colors, totcells), nrow = grid.r)
image(t(grid[grid.r:1,]), axes = FALSE, col = c("white", "blue", "red"))
#grid.coords = melt(grid)
#names(grid.coords) <- c("i", "j", "col")

###################
###MOVE THAT CAR###
###################
General idea:
#move = function(grid, time) 
#if t= odd
##look at blue cars
### move cars by checking if space is available
#if t = even
##look at red cars
##move cars by checking if space is available

##This function changes the coordinates according to specified direction
next.pos = function(i, j, direction){
	ij = switch(direction,
		right = c(i, (j+1)),
		up = c((i+1), j),
		c(i, j)
	)
	ij = off.grid(ij)
	return(ij)}

##This function checks if the move generated aboce is off the grid
##If so, it will wrap around.
off.grid = function(ij){
	i = ij[1]
	j = ij[2]
	if(i > grid.r)
		i = 1
	else
		if(i < 1)
			i = grid.r
	if(j > grid.c)
		j = 1
	else
		if(i < 1)
			i = grid.c
	c(i, j)
}

##This function implements the car movement at time t
move = function(grid, direction){
	i = dim(grid[1])
	j = dim(grid[2])
	curr.pos = dat[i,j]
	next.pos = next.move(curr.pos, direction)
	next.pos = off.grid(next.pos)
	check = any(dat[,"x"] == next.pos[,"x"] & dat[,"y"] == next.pos[,"y"])
	if(check == FALSE){
		curr.pos = next.pos
	}
	return(curr.pos)
}

move = function(grid, time){
	if(x %% 2 == 0){		##red cars
		direction = "right"
		
	}
	else{		##blue cars
		direction = "up"
		blue = which(grid == 2, arr.ind = TRUE)
		
	}
	
}