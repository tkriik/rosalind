SRC=	src/rosalind.erl
BIN=	ebin/rosalind.beam

all: $(SRC)
	erlc -o ebin/ $(SRC)

shell: all
	erl -pa ebin/
