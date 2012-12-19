#!/bin/bash
tr -d '\r' | egrep -o ',(open|not a real question|not constructive|off topic|too localized)$'

