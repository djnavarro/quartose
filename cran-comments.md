## Summary

This is a new package designed to allow users to dynamically generate quarto syntax (e.g., for tabsets, section headers, divs, etc) from within R. It provides user facing functions like `quarto_tabset()`, `quarto_section()` that hold the user-specified content. Most of the work is done by the `format()`, `print()`, and `knitr::knit_print()` methods supplied for the objects returned by the `quarto_*()` functions.

Tests run on github, Rhub, and win-builder generally did not produce errors or warnings, and only the new release note. The one case where Rhub failures appeared is noted below, and is innocuous as far as I can tell. I hope I have not missed any checks required! Thank you for your consideration.

Kind regards
Danielle Navarro

## R CMD check results (local)

0 errors | 0 warnings | 1 note

* This is a new release.

## Rhub platforms tested

Checked on all 30 platforms currently available via Rhub. Passes on 29 with no warnings or errors:

https://github.com/djnavarro/quartose/actions/runs/16098155304

 [1] "linux"          "m1-san"         "macos"         
 [4] "macos-arm64"    "windows"        "atlas"         
 [7] "c23"            "clang-asan"     "clang-ubsan"   
[10] "clang16"        "clang17"        "clang18"       
[13] "clang19"        "clang20"        "donttest"      
[16] "gcc-asan"       "gcc13"          "gcc14"         
[19] "gcc15"          "intel"          "mkl"           
[22] "nold"           "noremap"        "nosuggests"    
[25] "ubuntu-clang"   "ubuntu-gcc12"   "ubuntu-next"   
[28] "ubuntu-release" "valgrind" 

The one failure is "rchk":

https://github.com/djnavarro/quartose/actions/runs/16098119543

As far as I can tell this failure is innocuous. If I understand correctly rchk looks for memory errors in included C code; there is no compiled code in this package so my guess is that the run failure is for other reasons? Very happy to make any necessary changes if I have misunderstood.
 
## Win-builder platforms tested

- `devtools::check_win_devel()`: https://win-builder.r-project.org/nHF1okW4QowR/
- `devtools::check_win_release()`: https://win-builder.r-project.org/OpvYiCZ51PVu

R CMD check logs look okay.
