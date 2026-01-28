# Convert LaTeX to a png file

Convert LaTeX to a png file

## Usage

``` r
texToPng(
  tex,
  width = NULL,
  height = NULL,
  dpi = 72,
  viewPng = FALSE,
  fontsize = 12,
  calcM = FALSE,
  crop = FALSE
)
```

## Arguments

- tex:

  TeX string. Remember to escape backslash with \\

- width:

  Width of the png.

- height:

  Height of the png (`width` are ignored).

- dpi:

  Dpi of the png. Not used if `width` or `height` are specified.

- viewPng:

  View the result in the plots window.

- fontsize:

  Front size used in the LaTeX document.

- calcM:

  Estimate 1 em in pixels in the resulting png.

- crop:

  Call command line program `pdfcrop` (must be installed).

## Value

The filename of the png or a list if `calcM = TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
tex <- "$\\mathbb{R}_{\\geqq}$"
texToPng(tex, viewPng = TRUE)
texToPng(tex, fontsize = 20, viewPng = TRUE)
texToPng(tex, height = 50, fontsize = 10, viewPng = TRUE)
texToPng(tex, height = 50, fontsize = 50, viewPng = TRUE)
tex <- "MMM"
texToPng(tex, dpi=72, calcM = TRUE)
texToPng(tex, width = 100, calcM = TRUE)
f <- texToPng(tex, dpi=300)
pngSize(f)
} # }
```
