CHANGES IN htmltable VERSION 0.5

NEW FEATURES

o added a function tidy_app() to replace tidy.gui() in previous versions:
  tidy_app() launches a Shiny app in the browser to reformat R code. The
gWidgets interface (e.g. GTK+) is no longer supported. See
https://yihui.shinyapps.io/formatR/ for a live demo.

BUG FIXES

o the shebang #! is no longer treated as an R comment (thanks, Mirko
Ebert, #36)

MAJOR CHANGES

o three functions were renamed (from the `foo.bar` style to `foo_bar`):
  `tidy.source()` (`tidy_source()`), `tidy.dir()` (`tidy_dir()`), and
`tidy.eval()` (`tidy_eval()`)

o the arguments of tidy_source() were renamed: `keep.comment` was renamed to
`comment`, `keep.blank.line` -> `blank`, `replace.assign` -> `arrow`,
`left.brace.newline` -> `brace.newline`, and `reindent.spaces` -> `indent`;
similarly, the corresponding global options were also renamed: now you should
use `options(formatR.comment)` instead of `options(keep.comment)`,
`keep.blank.line` -> `formatR.blank`, and so on; see `?tidy_source` and
http://yihui.name/formatR for details

MINOR CHANGES

o the usage() function returns the source code of the usage of a function
now; in previous versions, it only returns NULL

o added a new argument 'tidy' in usage() to make it possible not to
reformat the usage code

o tidy_source() may not work for R 3.0.0 or 3.0.1 if the code only
contains comments due to a bug in base R, which has been fixed; if you use
R 3.0, please upgrade to at least R 3.0.2 (R 2.15.x is not affected)
