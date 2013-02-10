library(ggplot2)

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
	if(rho > 1 | rho < 0){
		cat("Warning: This is not a valid proportion (rho). \n Proceeding with default rho = 0.5. \n")
		rho = 0.5
	}
	if(rho == 0){
		cat("Cool, you made an empty parking lot...\n\n")
	}
	if(rho == 1){
		cat("Looks like LA rush hour. \n\n")
	}
	
	###SIMPLE CALCS###
	ncars = round(rho * grid.r * grid.c, 0)	##total number of cars
	totcells = grid.r * grid.c	##total number of cells
	
	###GENERATE GRID###
	colors = c(rep(0, totcells-ncars), rep(1, round(ncars/2)), rep(2, round(ncars/2)))
	grid = matrix(sample(colors, totcells), nrow = grid.r)
	class(grid) = "Grid"
	return(grid)
}

summary.Grid = function(x){
	cat("Current grid state:\n")
	print(x)
	cat("Grid dimensions:", dim(x), "\n")
	cat("Total number of spaces", length(x), "\n")
	cat("Number of blue cars:", sum(x==1), "\n")
	cat("Number of red cars:", sum(x==2), "\n")
	cat("Number of open spaces:", sum(x==0), "\n")
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
	up = cbind((ij[,1]-1), ij[,2])	##change coords
	up[up[ , 1] == 0, 1] = n.row	##check coords
	return(up)						##return coords
}

swap = function(car.pos, potential, grid){
	values = apply(potential, 1, function(x) grid[x[1], x[2]])
	t = which(values == 0)
	if(length(t) == 0){
		return(grid)
	}
	else{
		if(length(t) == 1){
			swap = c(empty = potential[t,], hasCar = car.pos[t,])
			for(i in 1:length(t)){
				grid[swap[1], swap[2]] = grid[swap[3], swap[4]]
				grid[swap[3], swap[4]] = 0
			}
		}
		else{
			swap = cbind(empty = potential[t,], hasCar = car.pos[t,])
			for(i in 1:length(t)){
				grid[swap[i,1], swap[i,2]] = grid[swap[i,3], swap[i,4]]
				grid[swap[i,3], swap[i,4]] = 0
			}	
		}
		return(grid)
	}
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

simTraffic = function(grid, t = 100, plotLast = TRUE){
        velocity = integer(t)
        for(i in 1:t){
                new.grid = move(grid, i)
                velocity[i] = length(grid) - sum(new.grid == grid)
                grid = new.grid
                class(grid) = "Grid"
                if(i > 5){
                        if(all(velocity[(i - 5):i] == 0)){
                        cat("No movement for 5 time periods. \n Stopping now at time = ", i, "\n\n")
                        break}
                }
        }
        if(plotLast == TRUE){
                plot(grid)}
        return(velocity)
}

#myList = c("genGrid.R", "move.R", "next.pos.R", "plot.Grid.R", "simTraffic.R", "swap.R", "summary.Grid.R")
#package.skeleton(name = "LauBML", code_files = myList)

#library("LauBML", lib.loc = "~/Documents/STA 242/Assignment2/")
#library(profr)
