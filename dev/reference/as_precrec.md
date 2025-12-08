# Convert to 'precrec' Format

Converts to a format which is understood by
[`precrec::evalmod()`](https://rdrr.io/pkg/precrec/man/evalmod.html) of
package [precrec](https://CRAN.R-project.org/package=precrec).

## Usage

``` r
as_precrec(object)

# S3 method for class 'PredictionClassif'
as_precrec(object)

# S3 method for class 'ResampleResult'
as_precrec(object)

# S3 method for class 'BenchmarkResult'
as_precrec(object)
```

## Arguments

- object:

  (`any`)  
  Object to convert.

## Value

Object as created by
[`precrec::mmdata()`](https://rdrr.io/pkg/precrec/man/mmdata.html).

## References

Saito T, Rehmsmeier M (2017). “Precrec: fast and accurate
precision-recall and ROC curve calculations in R.” *Bioinformatics*,
**33**(1), 145-147.
[doi:10.1093/bioinformatics/btw570](https://doi.org/10.1093/bioinformatics/btw570)
.
