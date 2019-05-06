## ------------------------------------------------------------------------
library("wrapr")

variable <- as.name("angle")

sinterp(
  'variable name is .(variable)'
)

## ------------------------------------------------------------------------
angle = 1:10
variable <- as.name("angle")

evalb(
  
  plot(x = .(variable), 
       y = sin(.(variable)))
  
  )

## ------------------------------------------------------------------------
print(evalb)

## ------------------------------------------------------------------------
bquote(
  
  plot(x = .(variable), 
       y = sin(.(variable)))
  
  )

## ------------------------------------------------------------------------
plotb <- bquote_function(graphics::plot)

plotb(x = .(variable), 
      y = sin(.(variable)))

## ------------------------------------------------------------------------
f <- function() { 
  sin
}

# pipe 5 to the value of f()
# the .() says to evaluate f() before the
# piping
5 %.>% .(f())

# evaluate "f()"" with . = 5
# not interesting as "f()"" is "dot free" 
5 %.>% f()

