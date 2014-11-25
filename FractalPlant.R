# This script draws a plant in R using fractals. Load the functions below into your
# workspace and then run the lines of code to below to produce the graphic.
# =================================================================================

## Create drawing functions
## ========================

createVector <- function(vec, dir, len, col)
{
    vec2 <- c((vec[1] + len * cos(dir)), (vec[2] + len * sin(dir)))
    lines(x = c(vec[1],vec2[1]), y = c(vec[2],vec2[2]), type = "l", col = col)
    return(vec2)
}

drawPlant <- function(vec, direction, length, n, direction2) 
{
    if(n > 0)
    {
        colour <- ifelse(n > 6,
                         "brown",
                         ifelse(n > 2, "dark green", "green")
                         )

        vec <- createVector(vec = vec, dir = direction, len = length, col = colour)
	drawPlant(vec = vec, direction = (direction + direction2 * pi/4),
                  length = (length * 1.12/2), n = (n-1), direction2 = direction2)
		
	vec <- createVector(vec = vec, dir = (direction - pi/10), len = length, col = colour)
	drawPlant(vec = vec, direction = (direction - direction2 * pi/4),
                  length = (length * 1.12/3), n = (n-1), direction2 = -direction2)
		
    	vec <- createVector(vec = vec, dir = direction, len = length, col = colour)
	drawPlant(vec = vec, direction = direction,
                  length = (length * 1.12/2), n = (n-1), direction2 = direction2)
    }	
}

## Draw the plant
## ==============

# Create empty plot to draw into
plot(c(0,0), c(0,0), type="l", col = "green", ylim = c(-5,5), xlim = c(0,10),
     xlab = NA, ylab = NA, xaxt = "n", yaxt = "n", main = "Fractal plant")

# Draw plant
drawPlant(vec = c(5,-5), direction = (pi/2+pi/20), length = 1.45, n = 10, direction2 = 1)
