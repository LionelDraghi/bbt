# gnoga 2.2.3 / Alire 2.x PRJ_TARGET fix (see README in this directory).
#
# Alire 2.1.1 exports PRJ_TARGET="Win64" (the win32ada crate's
# [gpr-set-externals]), while gnoga's gpr files branch on a typed
# string restricted to "Windows" etc. — the build dies at
# settings.gpr:7 with:
#    error: value "Win64" is illegal for typed string "Build_Target"
#
# Fix: extend the typed string and accept Win32/Win64 wherever the
# Windows branch is expected. Applied by tools/prepare_gnoga.sh
# (idempotent: it checks whether the type already carries Win64).
s|   type Target_Type is ("Windows", "Windows_NT", "Linux", "UNIX", "OSX", "FreeBSD");|   type Target_Type is ("Windows", "Windows_NT", "Linux", "UNIX", "OSX", "FreeBSD", "Win32", "Win64");|
s|      when "Windows" =>|      when "Windows" \| "Win32" \| "Win64" =>|
# Also: settings.gpr compiles the library with -gnat2012 while its
# own dependency uxstrings 0.9.4 requires Ada 2022 (Aggregate
# aspect) — the release profile build fails. Both rules are
# self-idempotent: each one matches the unpatched spelling only.
s|"-gnat2012"|"-gnat2022"|
