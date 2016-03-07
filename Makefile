all:
	./rebar compile

run:
	ERL_LIBS=apps:deps erl +MBas aobf +MBlmbcs 512 -name crest_erl@127.0.0.1 -pa ./ebin -boot start_sasl -eval "application:start(crest_erl)"  -sasl errlog_type error
