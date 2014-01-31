PSEUDO: run background

run:
	erl -pa deps/**/ebin -pa ebin

background:
	redis-server & && echo "Start postgres too"
