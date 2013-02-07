#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s resource_manager -config sys.config -name resource_manager@virtualhold.com -setcookie cookie
