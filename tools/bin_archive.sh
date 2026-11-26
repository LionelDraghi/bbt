#!/usr/bin/env bash
# -----------------------------------------------------------------------------
# bbt, the black box tester (https://github.com/LionelDraghi/bbt)
# SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
# SPDX-FileCopyrightText: 2026, Lionel Draghi
# -----------------------------------------------------------------------------
#
# The packaging of the bbt_bin crate (cf. docs/proposed/bin_crate.md):
# the archive of the current platform, in the shape the crate declares
# as its origin, and the check of that manifest against a private index.
#
#   bin_archive.sh package   build the archive, print its sha256, and
#                            check it by extracting it in a clean
#                            directory and running a scenario with it
#   bin_archive.sh check     generate the manifest of the current
#                            platform, resolve it through a private
#                            index, check the hash it carries, then
#                            install the crate in a throwaway prefix and
#                            run a scenario with the installed bbt
#   bin_archive.sh fragment  write the (download URL, sha256) pair of
#                            this platform, for the bbt_bin manifest
#
# The first two are driven by `make bin` and `make bin-check`; the CI
# packaging job calls `package` on each runner, and a release calls
# `fragment` on each platform. The archive goes to obj/bin/, which is
# git-ignored and removed by make clean.

set -eu

ROOT=$(cd "$(dirname "$0")/.." && pwd)
BIN_DIR="$ROOT/obj/bin"
INDEX_NAME=bbtbinlocal
TMP_DIR=

cleanup() {
   if [ -n "$TMP_DIR" ]; then
      rm -rf "$TMP_DIR"
   fi
   alr index --del "$INDEX_NAME" >/dev/null 2>&1 || true
}
trap cleanup EXIT

usage() {
   echo "usage: $0 package|check|fragment" >&2
   exit 2
}

# The executable suffix, the architecture and the platform tag of the
# archive name: the three conventions the bbt_bin manifest declares.
case "$(uname -s)" in
   *NT* | CYGWIN* | MINGW*) EXE=.exe; OS_TAG=windows64 ;;
   Darwin) EXE=; OS_TAG=darwin ;;
   *) EXE=; OS_TAG=linux ;;
esac

case "$(uname -m)" in
   x86_64 | amd64) ARCH=x86_64 ;;
   aarch64 | arm64) ARCH=aarch64 ;;
   *) echo "error: unsupported host architecture $(uname -m)" >&2; exit 1 ;;
esac

BBT="$ROOT/bbt$EXE"

sha256_of() {
   if command -v sha256sum >/dev/null 2>&1; then
      sha256sum "$1" | cut -d' ' -f1
   elif command -v shasum >/dev/null 2>&1; then
      shasum -a 256 "$1" | cut -d' ' -f1
   else
      echo "error: no sha256 tool (sha256sum or shasum)" >&2
      exit 1
   fi
}

# The version comes from the binary, never from alire.toml: a stale
# src/Alire_config/bbt_config.ads would otherwise name the archive with
# a version its content does not carry.
version_of() {
   "$1" --version | sed -e 's/^bbt version //'
}

require_bbt() {
   [ -x "$BBT" ] || {
      echo "error: $BBT is missing or not executable; run make build-release" >&2
      exit 1
   }
}

# One scenario, generated here: the documents of docs/examples need the
# sut tool of tests/, and the smoke test must depend on nothing but the
# extracted bbt.
smoke_scenario() {
   cat > "$1" <<'EOF'
## Scenario: smoke

- When I run `echo hello`
- Then the output is `hello`
EOF
}

package() {
   require_bbt
   local version archive name got
   version=$(version_of "$BBT")
   name="bbt-$version-$ARCH-$OS_TAG"
   archive="$BIN_DIR/$name.tar.gz"

   mkdir -p "$BIN_DIR"
   rm -rf "$BIN_DIR/stage"
   mkdir -p "$BIN_DIR/stage/$name/bin"
   cp "$BBT" "$BIN_DIR/stage/$name/bin/"
   cp "$ROOT/LICENSE" "$ROOT/README.md" "$BIN_DIR/stage/$name/"
   # One single top-level directory, holding bin/, LICENSE and
   # README.md: Alire deploys a binary archive by checking that it
   # expands to exactly one directory, then moving that directory up as
   # CRATE_ROOT (alire-origins-deployers-source_archive.adb,
   # Check_And_Move_Up). An archive with a flat bin/ at its root is
   # rejected there — « Unexpected contents where a single directory was
   # expected » — and the installation fails. ${CRATE_ROOT}/bin is that
   # bin/, as [environment] PATH.prepend = ${CRATE_ROOT}/bin declares.
   ( cd "$BIN_DIR/stage" && tar -czf "$archive" "$name" )
   rm -rf "$BIN_DIR/stage"

   echo "  archive: $archive"
   echo "  sha256 : $(sha256_of "$archive")"
   echo "  checking the archive in a clean directory"

   TMP_DIR=$(mktemp -d)
   tar -xzf "$archive" -C "$TMP_DIR"
   got=$(version_of "$TMP_DIR/$name/bin/bbt$EXE")
   if [ "$got" != "$version" ]; then
      echo "error: the archive announces $version, its binary reports $got" >&2
      exit 1
   fi
   smoke_scenario "$TMP_DIR/smoke.md"
   # bbt writes a .out file next to the scenario: running from the
   # temporary directory keeps that residue out of the repository.
   if ! ( cd "$TMP_DIR" && "$TMP_DIR/$name/bin/bbt$EXE" smoke.md > smoke.log 2>&1 ); then
      cat "$TMP_DIR/smoke.log" >&2
      exit 1
   fi
   if ! grep -q '## Summary : \*\*Success\*\*, 1 scenarios OK' "$TMP_DIR/smoke.log"; then
      cat "$TMP_DIR/smoke.log" >&2
      exit 1
   fi
   echo "  extracted archive runs a scenario: OK"
}

# The manifest of the current platform, with the local archive as
# origin: it checks what packaging cannot — that Alire accepts the
# manifest, resolves the origin, and reads the same hash.
check() {
   local version archive hash idx archive_url prefix installed got
   version=$(version_of "$BBT")
   archive="$BIN_DIR/bbt-$version-$ARCH-$OS_TAG.tar.gz"
   [ -f "$archive" ] || {
      echo "error: $archive is missing; run make bin first" >&2
      exit 1
   }
   hash=$(sha256_of "$archive")

   # A `file:` URL of the archive just built: the origin is local, so
   # the check needs no network and no publication. The mixed path form
   # (C:/...) is the one Alire resolves here; -m gives it on Windows.
   case "$(uname -s)" in
      *NT* | CYGWIN* | MINGW*) archive_url="file:$(cygpath -m "$archive")" ;;
      *) archive_url="file:$archive" ;;
   esac

   idx="$BIN_DIR/index"
   rm -rf "$idx"
   mkdir -p "$idx/index/bb/bbt_bin"
   printf 'version = "1.4.0"\n' > "$idx/index/index.toml"
   # One platform, since this index only knows the archive just built;
   # the published manifest carries one origin case per shipped
   # platform (cf. docs/proposed/bin_crate.md).
   cat > "$idx/index/bb/bbt_bin/bbt_bin-$version.toml" <<EOF
name = "bbt_bin"
description = "Black box test runner (prebuilt executable) - local check"
version = "$version"
authors = ["Lionel Draghi"]
maintainers = ["Lionel Draghi <lionel.draghi@free.fr>"]
maintainers-logins = ["LionelDraghi"]
licenses = "MIT OR Apache-2.0 WITH LLVM-exception"
website = "https://github.com/LionelDraghi/bbt"

[environment]
PATH.prepend = "\${CRATE_ROOT}/bin"

[origin]
url = "$archive_url"
hashes = ["sha256:$hash"]
binary = true
EOF

   alr index --del "$INDEX_NAME" >/dev/null 2>&1 || true
   alr --no-color index --add "$idx" --name "$INDEX_NAME" >/dev/null

   TMP_DIR=$(mktemp -d)
   if ! ( cd "$TMP_DIR" && alr --no-color show bbt_bin > show.log 2>&1 ); then
      cat "$TMP_DIR/show.log" >&2
      exit 1
   fi
   if ! grep -q "sha256:$hash" "$TMP_DIR/show.log"; then
      echo "error: the manifest does not carry the hash of the archive" >&2
      cat "$TMP_DIR/show.log" >&2
      exit 1
   fi
   echo "  the manifest resolves through a private index, hash included: OK"

   # The installation itself, into a throwaway prefix: it is the only
   # step that proves the archive deploys as the crate declares — one
   # top-level directory moved up as CRATE_ROOT, bin/ reachable from the
   # environment, the executable runnable. `binary = true` is what makes
   # Alire take the binary deployment path; without it the archive is
   # deployed as a source crate, which fails.
   prefix="$TMP_DIR/prefix"
   if ! alr --no-color install bbt_bin --prefix="$prefix" \
            > "$TMP_DIR/install.log" 2>&1
   then
      cat "$TMP_DIR/install.log" >&2
      exit 1
   fi
   installed="$prefix/bin/bbt$EXE"
   [ -x "$installed" ] || {
      echo "error: $installed is missing after the installation" >&2
      exit 1
   }
   got=$(version_of "$installed")
   if [ "$got" != "$version" ]; then
      echo "error: the installed bbt is announced $version," \
           "its binary reports $got" >&2
      exit 1
   fi
   smoke_scenario "$TMP_DIR/smoke.md"
   if ! ( cd "$TMP_DIR" && "$installed" smoke.md > installed.log 2>&1 ); then
      cat "$TMP_DIR/installed.log" >&2
      exit 1
   fi
   if ! grep -q '## Summary : \*\*Success\*\*, 1 scenarios OK' \
        "$TMP_DIR/installed.log"
   then
      cat "$TMP_DIR/installed.log" >&2
      exit 1
   fi
   echo "  the installed bbt runs a scenario: OK"
}

# The pair (download URL, sha256) of this platform's archive, written to
# obj/bin/<arch>-<os>.origin and printed: the release assembles the
# [origin] cases of bbt_bin/alire.toml from those records, one per
# platform, so that no hash is transcribed by hand. The manifest syntax
# itself is the one `alr publish --skip-build` generates, not a guess of
# this script.
fragment() {
   local version archive hash url name
   version=$(version_of "$BBT")
   name="bbt-$version-$ARCH-$OS_TAG"
   archive="$BIN_DIR/$name.tar.gz"
   [ -f "$archive" ] || {
      echo "error: $archive is missing; run make bin first" >&2
      exit 1
   }
   hash=$(sha256_of "$archive")
   url="https://github.com/LionelDraghi/bbt/releases/download/$version/$name.tar.gz"
   mkdir -p "$BIN_DIR"
   {
      echo "# bbt_bin origin, $OS_TAG / $ARCH, version $version"
      echo "archive = \"$name.tar.gz\""
      echo "url     = \"$url\""
      echo "sha256  = \"$hash\""
   } > "$BIN_DIR/$ARCH-$OS_TAG.origin"
   cat "$BIN_DIR/$ARCH-$OS_TAG.origin"
}

case "${1:-}" in
   package) package ;;
   check) check ;;
   fragment) fragment ;;
   *) usage ;;
esac
