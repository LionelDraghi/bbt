## Feature: readers and writers organization

Status: parked. The need is recorded here so that it is not lost; the
design will be arbitrated later. When decided, it will be recorded in
docs/design_decisions.md.

### Motivations

bbt reads Markdown (the MDG reader) and a close AsciiDoc subset, and
writes Markdown, AsciiDoc and text - the text writer being in fact a
second Markdown writer, as B140 shows: the verbose output is equal to
the Markdown index file.

The Markdown specific knowledge is scattered: the readers (mdg, adoc)
and the lexer know how to read, the writers know how to write, and
Markdown_Utilities (2026-10) centralizes the shared helpers (Web_Path,
Link, Checkbox, Hard_Break), but:

- Text_Writer duplicates the Markdown knowledge of Markdown_Writer:
  checkboxes, links, hard breaks, and the results summary table are
  written twice;
- AsciiDoc_Writer embeds Markdown conventions (the "  " hard break,
  whereas AsciiDoc uses " +");
- a future format that is not a Markdown flavor (reST, org mode...)
  would need its own reader and its own writer, and would reuse
  almost nothing of the existing writers.

### The balance to work

Maximize the factorization - one results model, one writer hierarchy,
the format syntax grouped per format utility package - while keeping
the door open to formats that are not Markdown flavors: where does
the abstraction line go between the writer hierarchy and the format
utilities? Should Text_Writer be a Markdown_Writer? Should the
markdown and asciidoc writers share a common intermediate
representation? The trade off is between the factorization gain and
the cost of an abstraction layer for a set of formats that is, today,
all Markdown derived.

### References

- docs/design_decisions.md: the decision log entry to write when this
  design is arbitrated;
- B140_Index_File.md: the text writer output is the Markdown index;
- markdown_utilities.ads/.adb: the first factorization step.
