dyn.load("Rdm.dll")

read.dm <- function(filename)
{
	return(.Call('Rdmread', as.character(filename)))
}

df <- read.dm('_geology.dm')

dyn.unload("Rdm.dll")

