# Save a point symbol as a temporary file.

Save a point symbol as a temporary file.

## Usage

``` r
getTexture(pch = 16, cex = 10, ...)
```

## Arguments

- pch:

  Point number/symbol.

- cex:

  Point size

- ...:

  Further arguments passed to `plot`.

## Value

The file name.

## Examples

``` r
# \donttest{
# Pch shapes
generateRPointShapes<-function(){
   oldPar<-par()
   par(font=2, mar=c(0.5,0,0,0))
   y=rev(c(rep(1,6),rep(2,5), rep(3,5), rep(4,5), rep(5,5)))
   x=c(rep(1:5,5),6)
   plot(x, y, pch = 0:25, cex=1.5, ylim=c(1,5.5), xlim=c(1,6.5),
        axes=FALSE, xlab="", ylab="", bg="blue")
   text(x, y, labels=0:25, pos=3)
   par(mar=oldPar$mar,font=oldPar$font )
}
generateRPointShapes()


getTexture()
#> [1] "/tmp/Rtmpiz48iS/file294473f5648f.png"
# }
```
