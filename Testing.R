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
next.pos = function(ij, direction){
	pos = switch(direction,
		right = cbind(ij[,1], ij[,2]+1),
		up = cbind(ij[,1]-1, ij[,2])
	)
	pos = off.grid(pos)
	return(pos)}

##This function checks if the move generated aboce is off the grid
##If so, it will wrap around.
off.grid = function(ij){
	ij[ij[ , 1] > grid.r, 1] = 1
	ij[ij[ , 1] == 0, 1] = grid.r
	ij[ij[ , 2] > grid.c, 2] = 1
	ij[ij[ , 2] == 0, 2] = grid.c
	return(ij)
}

#GENERAL FUNCTION
#swap = function(empty.spot, car.spot){
#	grid[empty.spot[1], empty.spot[2]] = grid[car.spot[1], car.spot[2]]
#	grid[car.spot[1], car.spot[2]] = 0
#	return(grid)
#}

move = function(grid, time){
	if(time %% 2 == 0){		##red cars
		direction = "right"
		car.pos = which(grid == 2, arr.ind = TRUE)
		val = 2
	}
	else{		##blue cars
		direction = "up"
		car.pos = which(grid == 1, arr.ind = TRUE)
		val = 1
	}
	potential = next.pos(car.pos, direction)
	##check next pos
	new.grid = swap(car.pos, potential)
	velocity = totcells - sum(grid == new.grid)
}

swap = function(car.pos, potential){
	values = apply(potential, 1, function(x) grid[x[1], x[2]])
	t = which(values == 0)
	swap = cbind(empty = potential[t,], hasCar = car.pos[t,])
	for(i in 1:length(t)){
		grid[swap[i,1], swap[i,2]] = grid[swap[i,3], swap[i,4]]
		grid[swap[i,3], swap[i,4]] = 0
	}
	return(grid)
}