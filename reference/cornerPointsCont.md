# Calculate the corner points for the polytope Ax\<=b assuming all variables are continuous.

Calculate the corner points for the polytope Ax\<=b assuming all
variables are continuous.

## Usage

``` r
cornerPointsCont(A, b, nonneg = rep(TRUE, ncol(A)))
```

## Arguments

- A:

  Constraint matrix.

- b:

  Right hand side.

- nonneg:

  A boolean vector of same length as number of variables. If entry k is
  TRUE then variable k must be non-negative.

## Value

A data frame with a corner point in each row.

## Author

Lars Relund <lars@relund.dk>
