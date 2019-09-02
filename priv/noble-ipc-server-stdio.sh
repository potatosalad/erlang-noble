#!/usr/bin/env bash
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
(cat && kill 0) | exec "${ROOT}/node_modules/.bin/noble-ipc-server-stdio" $@
