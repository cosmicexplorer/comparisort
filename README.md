comparisort
===========

Because comparison sorting shouldn't be that hard. Currently provides merge sorting and insertion sorting, which can be seen in [sort.R](R/sort.R).

# Install

For now, `devtools::install_github("cosmicexplorer/comparisort")`.

# Documentation

This is just a dump of the same info that's in the function documentation. Let me know if this is incomplete.

```
merge_sort             package:comparisort             R Documentation

Sort a List of Objects By Comparison

Description:

     ‘merge_sort’ uses a merge sorting algorithm to produce a sorted
     list from the input given a pairwise comparison function.

     ‘insertion_sort’ uses a merge sorting algorithm to produce a
     sorted list given a pairwise comparison function.

Usage:

     merge_sort(v, lt = `<`, bail_depth = 50, bail_size = 20,
       bail_func = insertion_sort)

     insertion_sort(v, lt = `<`)

Arguments:

       v: list of objects to sort.

      lt: function accepting two arguments and returning a logical -
          whether the first argument is less than the second (e.g.
          ‘<’()).

bail_depth: nonnegative scalar integer, the number of recursions after
          which to bail out.

bail_size: nonnegative scalar integer, the size of ‘v’ beneath which to
          bail out.

bail_func: a function accepting two arguments, a list to sort and a
          comparison function, which produces a sorted list.

Details:

     "bailing out" refers to calling ‘bail_func(v, lt)’. Setting
     ‘bail_depth = Inf’ and ‘bail_size = 0’ will ensure no bail outs
     occur, but becomes extremely slow.

     These functions perform a stable sort.

Value:

     Sorted version of the input list ‘v’.

Examples:
```

``` R
     ## defaults to `<`
     unlist(merge_sort(list(4, 1, 2, 1, 6, 2)))
     ## [1] 1 1 2 2 4 6

     ## can perform complex sortings -- this sorts alphabetically, but also
     ## ensures superstrings always come before substrings
     unlist(merge_sort(list("bb", "asdf", "cbbc", "www", "awwwa", "b"),
         function (a, b) {
             if (grepl(b, a, fixed = TRUE)) { T }
             else if (grepl(a, b, fixed = TRUE)) { F }
             else { a < b }
         }))
     ## [1] "asdf"  "awwwa" "cbbc"  "bb"    "b"     "www"

     unlist(insertion_sort(as.list(runif(14))))
     ##  [1] 0.01849342 0.09089005 0.10954994 0.29853601 0.35190103 0.40293052
     ##  [7] 0.49709814 0.57571824 0.60270605 0.69433286 0.73642727 0.76766467
     ## [13] 0.88342148 0.99029094

     ## this is a stable sort
     unlist(insertion_sort(list("a", "ww", "nwaw", "bs", "z"), function (a, b) {
         nchar(a) < nchar(b)
     }))
     ## [1] "a"    "z"    "ww"   "bs"   "nwaw"
```

# License
[GPL 3.0+](LICENSE)
