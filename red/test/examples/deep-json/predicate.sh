#!/usr/bin/env bash
jq 'any(.hello? | any(. == true))' $1
