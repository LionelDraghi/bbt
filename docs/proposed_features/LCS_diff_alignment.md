<!-- omit from toc -->
# LCS based diff alignment

## Problem

When a comparison fails, *bbt* now displays expected and actual side by side,
with hunk headers to locate the problem
(cf. [B105](../features/B105_Expected_vs_Actual_Output.md)).

But the comparison is **positional**: expected line N is compared with
actual line N. This is simple and fast, but an insertion or deletion
shifts all the following pairings, and produces misleading output:

```
@@ -7,3 +7,3 @@
                           |   Each line is unique
  Each line is unique      |   and clearly identified
  and clearly identified   |   
```

Here the actual file has one blank line more at the end than the expected
one. A human (or `diff`, or `git diff`) would report a single line insertion,
whereas the positional comparison turns it into three "changed" lines, none
of which is really changed.

## Proposed solution

Align the two texts with a **Longest Common Subsequence** algorithm before
displaying them, as `diff` and `git diff` do. The classic reference is
Myers' algorithm (*An O(ND) Difference Algorithm and Its Variations*, 1986),
which is what GNU diff uses.

With a LCS based alignment, the example above would become:

```
@@ -10,0 +11 @@
            >   
```

Benefits:
- the displayed differences are the minimal set of real differences
- the `|`, `<`, `>` separators become exact: `<` is a line present only in
  the expected text, `>` only in the actual one
- hunk line numbers point directly at the real problem
- `Index_Of` could also locate multi-line patterns on the aligned match
  instead of on the first line occurrence

## Impacted code

- `Text_Utilities.Side_By_Side` is the main impacted function; the
  comparison criteria (`Case_Insensitive`, `Ignore_Whitespaces`) should
  remain those of the current match settings
- the `Is_Equal` / `Contains` verdicts are *not* impacted: only the display
  of the differences changes

## Note on size

Files compared by *bbt* are usually small (test fixtures, command outputs),
so even a naive dynamic programming LCS (O(N*M)) would be acceptable.
A Myers implementation is only needed for very large files.
