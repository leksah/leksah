#!/usr/bin/env bash
# Serve the website (docs/website — the leksah.github.io replacement) for
# local testing.  The site root must be served as / : the app's generated CSS
# uses absolute /pics/… and /fonts/… paths, matching GitHub Pages layout.
# The demo lives at /try/.
cd "$(dirname "$0")/.."
exec python3 -m http.server "${1:-8000}"
