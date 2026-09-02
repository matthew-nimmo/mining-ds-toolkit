dyn.load("Rdm.dll")

read.dm <- function(filename)
{
	return(.Call('Rdmread', as.character(filename)))
}

cuts <- read.dm('_estcut.dm')
dh <- read.dm('_eholes.dm')

dyn.unload("Rdm.dll")

