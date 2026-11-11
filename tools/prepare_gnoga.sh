#!/usr/bin/env bash
# Prepare the gnoga crate copies in the local Alire cache so that
# they build on every machine:
#
# 1. Trim everything not needed to compile:
#    - deps/zanyblue/src/test : zanyblue's own test suite fixtures,
#      with pathological path depths (deps/zanyblue/src/test/...) that
#      break Alir's build dir sync on Windows (MAX_PATH);
#    - deps/zanyblue/{doc,examples}, demo/, test/ : documentation and
#      demo trees, dead weight for the sync as well.
# 2. Apply the patches/ directory rules (cf. patches/README.md).
#
# The crate copies are rebuildable cache state: trimming and patching
# are safe, idempotent, and re-applied at each build by the root
# Makefile, so that it holds on every machine and the CI. A no-op
# when no gnoga release is in the cache.

set -u

# Plain 'find' may resolve to Windows FIND.EXE in some environments:
Find_Cmd="/usr/bin/find"
[ -x /usr/bin/find ] || Find_Cmd="find"

Script_Dir="$(cd "$(dirname "$0")" && pwd)"
Patch_Rules="$Script_Dir/../patches/gnoga-*.sed"

Trimmed=0
Patched=0
# LOCALAPPDATA is only set on Windows, hence the default: pick up
# the gnoga copies wherever the Alire cache lives.
for Cache in "${LOCALAPPDATA:-}/alire/cache" "$HOME/.local/share/alire/cache"; do
   [ -d "$Cache" ] || continue
   for Gnoga in $("$Find_Cmd" "$Cache/releases" "$Cache/builds" \
                    -maxdepth 1 -type d -name 'gnoga_*' 2>/dev/null); do
      for Trim in deps/zanyblue/src/test deps/zanyblue/doc \
                  deps/zanyblue/examples demo test ; do
         if [ -e "$Gnoga/$Trim" ]; then
            rm -rf "$Gnoga/$Trim"
            Trimmed=$((Trimmed + 1))
         fi
      done

      # Note: Settings.gpr is patched through a marker test to stay
      # idempotent (sed alone would duplicate the case branches on
      # each build).
      for Gpr in $("$Find_Cmd" "$Gnoga" -maxdepth 2 -name 'settings.gpr' \
                     2>/dev/null) ; do
         if ! grep -qs '"Win64"' "$Gpr" || grep -qs '"-gnat2012"' "$Gpr"; then
            sed -i -f $Patch_Rules "$Gpr" 2>/dev/null \
              && echo "prepare_gnoga: patched $Gpr"
            Patched=$((Patched + 1))
         fi
      done
      for Gpr in $("$Find_Cmd" "$Gnoga/src" -maxdepth 1 -name 'gnoga.gpr' \
                     2>/dev/null); do
         if ! grep -qs '"Win64"' "$Gpr" || grep -qs '"-gnat2012"' "$Gpr"; then
            sed -i -f $Patch_Rules "$Gpr" 2>/dev/null \
              && echo "prepare_gnoga: patched $Gpr"
            Patched=$((Patched + 1))
         fi
      done
   done
done

if [ "$Trimmed" -gt 0 ] || [ "$Patched" -gt 0 ]; then
   echo "prepare_gnoga: done ($Trimmed trimmed dir(s), $Patched patched file(s))"
fi

exit 0
