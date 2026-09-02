library(sp)
library(gstat)

# Field Parametric Geostatistics (FPG)

extendfun <- function(x)
{
	n <- length(x)
	# weights include:
	# 1) frequency of value
	# 2) spatial weight (any method including volume)
	# 3) representitive weight (density, sample size or weight)
	w <- rep(1, n)
	o <- order(x)

	w <- tapply(w[o], x[o], sum)
	wx <- as.numeric(names(w))
	wy <- cumsum(w) / sum(w)
	if (!any(x==0))
	{
		wy <- c(0, wy)
		wx <- c(0, wx)
	}

	# could use splinefun with method="monoH.FC"
	ex.fun <- approxfun(wx, wy, method='linear', rule=2)
	ex.inv <- approxfun(wy, wx, method='linear', rule=2)

	return(list(fun=ex.fun, inv=ex.inv))
}

# ---

#data(walker)
#walker <- read.csv("walker.csv")
#plot(walker$X, walker$Y)

#ex.fun <- extendfun(walker$U)
#walker$U.ex <- ex.fun$fun(walker$U)
#g <- gstat(id="U.ex", formula=U.ex~1, locations=~X+Y, data=walker)
#vgm1 <- variogram(g)
#vgm1 <- variogram(U.ex~1, ~X+Y, walker)
#vgm1.fit <- fit.variogram(vgm1, vgm(1,"Sph",300,1))
#plot(vgm1, model=vgm1.fit)

# ---


data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y

ex.fun <- extendfun(log(meuse$zinc))
meuse$zinc.ex <- ex.fun$fun(log(meuse$zinc))
vgm1 <- variogram(zinc.ex~1, meuse)
vgm1.fit <- fit.variogram(vgm1, vgm(.4,"Sph",954,.06))

x <- krige(zinc.ex~1, meuse, meuse.grid, model=vgm1.fit)
x$var1.pred <- ex.fun$inv(x$var1.pred)
spplot(x["var1.pred"], main="ordinary kriging predictions")

y <- krige(log(zinc)~1, meuse, meuse.grid, model=vgm(.4,"Sph",954,.06))
spplot(y["var1.pred"], main="ordinary kriging predictions")
