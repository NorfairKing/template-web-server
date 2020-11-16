#!/usr/bin/env bash


killall foo-bar-web-server || true

foo-bar-web-server &
