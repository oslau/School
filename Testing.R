library(ggplot2)
library(reshape)

#################
###USER INPUTS###
#################
grid.r = 10 ##number of rows
grid.c = 10 ##number of columns
rho = .5	##proportion of grid filled

##############################
###GENERATE RANDOM CAR GRID###
##############################

genGrid = function(grid.r, grid.c, rho = .5){
	###SIMPLE CALCS###
	ncars = round(rho * grid.r * grid.c, 0)	##total number of cars
	totcells = grid.r * grid.c	##total number of cells
	###GENERATE GRID###
	colors = c(rep(0, totcells-ncars), rep(1, ncars/2), rep(2, ncars/2))
	grid = matrix(sample(colors, totcells), nrow = grid.r)
	class(grid) = "Grid"
	return(grid)
}

plot.Grid = function(x){
	rows = nrow(x)
	image(t(x[rows:1,]), axes = FALSE, col = c("white", "blue", "red"))
	}
#grid.coords = melt(grid)
#names(grid.coords) <- c("i", "j", "col")

###################
###MOVE THAT CAR###
###################
##This function changes the coordinates next potential position
next.pos = function(ij, n.row){
	up = cbind(ij[,1]-1, ij[,2])
	up[up[ , 1] == 0, 1] = n.row
	return(up)}
	
swap = function(car.pos, potential, grid){
	values = apply(potential, 1, function(x) grid[x[1], x[2]])
	t = which(values == 0)
	swap = cbind(empty = potential[t,], hasCar = car.pos[t,])
	for(i in 1:length(t)){
		grid[swap[i,1], swap[i,2]] = grid[swap[i,3], swap[i,4]]
		grid[swap[i,3], swap[i,4]] = 0
	}
	return(grid)
}

move = function(grid, time){
	d = dim(grid)
	if(time %% 2 == 0){		##red cars
		grid = t(grid[ , ncol(grid):1])
		car.pos = which(grid == 2, arr.ind = TRUE)
		potential = next.pos(car.pos, nrow(grid))
		new.grid = swap(car.pos, potential, grid)
		new.grid = t(new.grid[nrow(new.grid):1,])
	}
	else{		##blue cars
		car.pos = which(grid == 1, arr.ind = TRUE)
		potential = next.pos(car.pos, nrow(grid))
		new.grid = swap(car.pos, potential, grid)
	}
	return(new.grid)
}

simTraffic = function(grid, t = 100){
	velocity = integer(t)
	png(file = "BML%03d.png", width = 500, height = 500)
	for(i in 1:t){
		new.grid = move(grid, i)
		velocity[i] = length(grid) - sum(new.grid == grid)
		grid = new.grid
		class(grid) = "Grid"
		plot(grid)
	}
	system("convert -delay 0.5 *.png myMovie.gif")
	invisible(file.remove(list.files(pattern = ".png")))
	graphics.off()
	plot(grid)
	return(velocity)
}