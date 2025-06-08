
Human vs exact matching:
  bbt default behavior is "human match", that is ignoring differences
  in casing, ignoring consecutive spaces, and ignoring blank lines.
  The opposite behavior, to make strict compare, is set with:
  -em  | --exact_match
  exact_match may be altered if **followed** by one or more of:
  -iw  | --ignore_whitespaces (default)
  -ic  | --ignore_casing      (default)
  -ibl | --ignore_blank_lines (default)
  For example, "-em -iw" will take into account blank lines and
  casing but ignore whitespaces
  Note that -iw, -ic, and -ibl are useless if not preceded by -em, 
  because they are the default setting.
  There is also a
  -hm  | --human_match
  option, equivalent to defaults "-iw -ic -ibl", if you want to
  assert on the command line that this is the required behavior.
