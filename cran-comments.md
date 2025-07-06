## Summary

This is a new package designed to allow users to dynamically generate quarto syntax (e.g., for tabsets, section headers, divs, etc) from within R. It provides user facing functions like `quarto_tabset()`, `quarto_section()` that hold the user-specified content. Most of the work is done by the `format()`, `print()`, and `knitr::knit_print()` methods supplied for the objects returned by the `quarto_*()` functions.

Tests run on github, Rhub, and win-builder did not produce errors or warnings, and only the new release note. I hope I have not missed any checks required! Thank you for your consideration.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
