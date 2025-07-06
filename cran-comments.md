## Summary

This is a new package designed to allow users to dynamically generate quarto syntax (e.g., for tabsets, section headers, divs, etc) from within R. It provides user facing functions like `quarto_tabset()`, `quarto_section()` that hold the user-specified content. Most of the work is done by the `format()`, `print()`, and `knitr::knit_print()` methods supplied for the objects returned by the `quarto_*()` functions.

Tests run on github, Rhub, and win-builder generally did not produce errors or warnings, and only the new release note. The two cases where Rhub failures appeared are noted below, and are innocuous as far as I can tell. I hope I have not missed any checks required! Thank you for your consideration.

Kind regards
Danielle Navarro

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Rhub platforms tested

 [1] "linux"          "m1-san"         "macos"         
 [4] "macos-arm64"    "windows"        "atlas"         
 [7] "c23"            "clang-asan"     "clang-ubsan"   
[10] "clang16"        "clang17"        "clang18"       
[13] "clang19"        "clang20"        "donttest"      
[16] "gcc-asan"       "gcc13"          "gcc14"         
[19] "gcc15"          "intel"          "mkl"           
[22] "nold"           "noremap"        "nosuggests"    
[25] "rchk"           "ubuntu-clang"   "ubuntu-gcc12"  
[28] "ubuntu-next"    "ubuntu-release" "valgrind"  

Passes on all platforms with the following two exceptions:

 - Run fails on "nosuggests" due to tests that use suggested packages; package build looks okay 
 - Run fails on "rchk". I think this is innocuous? If I understand correctly rchk looks for memory errors in included C code; as there is no compiled code at all in this package my guess is that the run failure is for other reasons

 https://github.com/djnavarro/quartose/actions/runs/16097438053
 
## Win-builder platforms tested

- `devtools::check_win_devel()`
- `devtools::check_win_release()`

R CMD check logs look okay.
