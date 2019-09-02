#!/usr/bin/env bash

set -eo pipefail
set -x

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

NPM="$(command -v npm || true)"
YARN="$(command -v yarn || true)"

if [[ -n "${YARN}" ]]; then
    cd "${ROOT}"
    exec "${YARN}" install
elif [[ -n "${NPM}" ]]; then
    cd "${ROOT}"
    exec "${NPM}" install
else
    echo "Fatal error: either 'npm' or 'yarn' must be installed" >&2
    exit 1
fi
