#' @import magrittr

is_counting_num <- function (x, allow_zero = TRUE) {
    is.vector(x, 'integer') &&
        (length(x) == 1) &&
        ((x > 0) || (allow_zero && (x == 0)))
}

safe_int_seq <- function (from, to) {
    stopifnot(is_counting_num(from),
              is_counting_num(to, allow_zero = TRUE))
    stopifnot(to >= (from - 1L))
    if (to == from - 1L) {
        integer(0)
    } else {
        from:to
    }
}

## TODO: hierarchical merging?
insert_merge <- function (base, add, lt) {
    ## TODO: add check for base being sorted already? optimization if add is
    ## already sorted?
    Reduce(x = add, init = base, function (ret, el) {
        for (idx in safe_int_seq(1L, length(ret))) {
            if (lt(el, ret[[idx]])) {
                return(c(ret[safe_int_seq(1L, idx - 1L)],
                         list(el),
                         ret[safe_int_seq(idx, length(ret))]))
            }
        }
        c(ret, list(el))
    })
}

## speed of merging
## T(n) = kT(n/k)

do_merge <- function (to_merge, lt) {
    Reduce(x = to_merge, f = function (cur, merges) {
        insert_merge(cur, merges, lt)
    })
}

merge_sort_recur <- function
(
    v, lt, bail_depth, cur_depth, bail_size, bail_func
) {
    if (cur_depth >= bail_depth) { return(bail_func(v, lt)) }
    n <- length(v)
    if (n <= bail_size) { return(bail_func(v, lt)) }
    ## optimal branching factor is n^(1/2)
    inc <- n %>% sqrt %>% round
    ## start indices
    seq(1, n, by = inc) %>%
        ## split into ranges of v
        lapply((. %>% { v[.:min(. + inc - 1, n)] })) %>%
        ## sort each individually
        lapply((. %>% merge_sort_recur(
            lt, bail_depth, cur_depth + 1, bail_size, bail_func))) %>%
        do_merge(lt)
}

## T(n) = n + kT(n/k) => (a = k, b = k, f(n) = n) =>
##                       (p = log_{k}(k) = 1, g(n) = n/n^p = 1)
##      = n + 2T(n/2)
## T(n) = n + k(n/k + kT(n/k^2))
## n log_k (n) = n + k(n/k log_k (n/k))
##             = n + n log_k (n/k) = n + n (log_k (n) - log_k (k))
##             = n + n (log_k (n) - 1) = n log_k (n)
## => n = kT(n/k) => n = k (n/k log_k (n/k)) => 1 = log_k (n / k)
##                => k = n/k => k^2 = n => k = n^(1/2)
## therefore optimal branching factor k = n^(1/2)
## (can prove this is superior to k = 2)

#' @title Sort a List of Objects By Comparison
#'
#' @description `merge_sort` uses a merge sorting algorithm to produce a sorted
#'     list from the input given a pairwise comparison function.
#'
#' @param v list of objects to sort.
#' @param lt function accepting two arguments and returning a logical -- whether
#'     the first argument is less than the second (e.g. [`<`()]).
#' @param bail_depth nonnegative scalar integer, the number of recursions after
#'     which to bail out.
#' @param bail_size nonnegative scalar integer, the size of `v` beneath which to
#'     bail out.
#' @param bail_func a function accepting two arguments, a list to sort and a
#'     comparison function, which produces a sorted
#'     list.
#'
#' @return Sorted version of the input list `v`.
#'
#' @details "bailing out" refers to calling `bail_func(v, lt)`. Setting
#'     `bail_depth = Inf` and `bail_size = 0` will ensure no bail outs occur,
#'     but becomes extremely slow.
#'
#'     These functions perform a stable sort.
#'
#' @examples
#' ## defaults to `<`
#' unlist(merge_sort(list(4, 1, 2, 1, 6, 2)))
#' ## [1] 1 1 2 2 4 6
#'
#' ## can perform complex sortings -- this sorts alphabetically, but also
#' ## ensures superstrings always come before substrings
#' unlist(merge_sort(list("bb", "asdf", "cbbc", "www", "awwwa", "b"),
#'     function (a, b) {
#'         if (grepl(b, a, fixed = TRUE)) { T }
#'         else if (grepl(a, b, fixed = TRUE)) { F }
#'         else { a < b }
#'     }))
#' ## [1] "asdf"  "awwwa" "cbbc"  "bb"    "b"     "www"
#'
#' @rdname comparison_sort
#'
#' @export
merge_sort <- function (v, lt = `<`,
                        bail_depth = 50, bail_size = 20,
                        bail_func = insertion_sort) {
    bail_depth <- as.integer(bail_depth)
    bail_size <- as.integer(bail_size)
    stopifnot(is.list(v), is.function(lt),
              is_counting_num(bail_depth), is_counting_num(bail_size),
              is.function(bail_func))
    merge_sort_recur(v, lt, bail_depth, 0L, bail_size, bail_func)
}

#' @title Insertion Sort a List of Objects By Comparison
#'
#' @description `insertion_sort` uses a merge sorting algorithm to produce a
#'     sorted list given a pairwise comparison function.
#'
#' @inheritParams merge_sort
#'
#' @rdname comparison_sort
#'
#' @examples
#' unlist(insertion_sort(as.list(runif(14))))
#' ##  [1] 0.01849342 0.09089005 0.10954994 0.29853601 0.35190103 0.40293052
#' ##  [7] 0.49709814 0.57571824 0.60270605 0.69433286 0.73642727 0.76766467
#' ## [13] 0.88342148 0.99029094
#'
#' ## this is a stable sort
#' unlist(insertion_sort(list("a", "ww", "nwaw", "bs", "z"), function (a, b) {
#'     nchar(a) < nchar(b)
#' }))
#' ## [1] "a"    "z"    "ww"   "bs"   "nwaw"
#'
#' @export
insertion_sort <- function (v, lt = `<`) {
    stopifnot(is.list(v), is.function(lt))
    insert_merge(list(), v, lt)
}
