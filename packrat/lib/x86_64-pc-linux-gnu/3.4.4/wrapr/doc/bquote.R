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

