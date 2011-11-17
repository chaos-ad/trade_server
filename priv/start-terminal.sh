#!/bin/sh
cd `dirname $0`
mkdir -p logs
WINEDEBUG=-all wine ./trade_terminal.exe $@ 1> /dev/null 2> logs/terminal.log
