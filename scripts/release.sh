#!/usr/bin/env bash
# Build Leksah's four downloads and publish them as the assets of GitHub
# release v<version> — where the website's download links point (see the
# #install section of docs/website/index.html).  They are release assets, not
# files in the site: the GTK tarball alone is far over GitHub's 100 MB limit
# for a file in a git repository, which rules out the Pages repo.
#
#   scripts/release.sh [--dry-run] [--publish]
#
#   --dry-run  check everything and show what would happen: evaluates the four
#              artifacts but builds, tags, pushes and uploads nothing.
#   --publish  publish the release immediately.  Without it the release is
#              created as a DRAFT, to be looked over and published on GitHub.
#
# The version is leksah/leksah.cabal's.  Needs: a clean, committed tree; nix
# with builders for aarch64-darwin (the .dmg; its hdiutil step needs the daemon's
# `sandbox = false`) and x86_64-linux (the rest); gpg to sign the tag; and gh
# authenticated for leksah/leksah.
set -euo pipefail
cd "$(dirname "$0")/.."

dry=0 publish=0
for a in "$@"; do
  case "$a" in
    --dry-run) dry=1 ;;
    --publish) publish=1 ;;
    *) echo "usage: $0 [--dry-run] [--publish]" >&2; exit 2 ;;
  esac
done

die() { echo "release: $*" >&2; exit 1; }
run() { if [ "$dry" = 1 ]; then echo "  would run: $*"; else "$@"; fi; }

version=$(sed -n 's/^version:[[:space:]]*//p' leksah/leksah.cabal)
[ -n "$version" ] || die "no version in leksah/leksah.cabal"
tag="v$version"
echo "Leksah $version (tag $tag)"

# --- checks --------------------------------------------------------------
# The artifacts are built from the working tree (the flake reads it dirty),
# so an uncommitted edit would ship without being in the tagged commit.
[ -z "$(git status --porcelain --untracked-files=no)" ] \
  || die "uncommitted changes to tracked files — commit or stash them first"

# The website hard-codes the version its links use.
grep -q "var v = '$version';" docs/website/index.html \
  || die "docs/website/index.html's download version is not $version"

# Pushing the tag pushes every commit it reaches that no remote has yet; all
# of them must be signed.  G/U/E are fine (E: key not in the local keyring).
unsigned=$(git log --format='%h %G? %s' HEAD --not --remotes \
             | awk '$2 !~ /^[GUE]$/')
[ -z "$unsigned" ] || die "these commits would be pushed unsigned:
$unsigned"

if git rev-parse -q --verify "refs/tags/$tag" >/dev/null; then
  [ "$(git rev-parse "$tag^{commit}")" = "$(git rev-parse HEAD)" ] \
    || die "tag $tag exists but is not HEAD"
  have_tag=1
else
  have_tag=0
fi

# --- artifacts -----------------------------------------------------------
# attr  system  file-inside-the-output  (names as the website expects them)
artifacts=(
  "leksah-linux-tarball      x86_64-linux   leksah-$version-x86_64-linux.tar.gz"
  "leksah-warp-linux-tarball x86_64-linux   leksah-warp-$version-x86_64-linux-musl.tar.gz"
  "leksah-windows-installer  x86_64-linux   LeksahSetup.exe"
  "leksah-macos-dmg          aarch64-darwin Leksah-$version.dmg"
)
files=()
for a in "${artifacts[@]}"; do
  read -r attr sys file <<<"$a"
  ref=".#packages.$sys.$attr"
  grep -q "$file" docs/website/index.html \
    || die "the website does not link $file"
  if [ "$dry" = 1 ]; then
    out=$(nix eval --raw "$ref.outPath")
    echo "  $file <- $ref ($out)"
    continue
  fi
  echo "building $ref"
  out=$(nix build --no-link --print-out-paths "$ref")
  [ -f "$out/$file" ] || die "$ref did not produce $file"
  files+=("$out/$file")
done

# --- tag, push, release --------------------------------------------------
if [ "$have_tag" = 0 ]; then
  run git tag -s "$tag" -m "Leksah $version"
fi
run git push origin "refs/tags/$tag"

draft=(--draft)
[ "$publish" = 1 ] && draft=()
run gh release create "$tag" --repo leksah/leksah --verify-tag \
  --title "Leksah $version" --generate-notes "${draft[@]}" "${files[@]}"

if [ "$dry" = 1 ]; then
  echo "dry run: nothing was built, tagged, pushed or uploaded"
elif [ "$publish" = 1 ]; then
  echo "published: https://github.com/leksah/leksah/releases/tag/$tag"
else
  echo "draft created — review and publish at https://github.com/leksah/leksah/releases"
fi
