Pill=Pill.json Pill.xpm

all: $(Pill) Makefile

Pill.json: Pill.raw.json
	jq ".meta.image |= \"Pill.xpm\"" Pill.raw.json > Pill.json

Pill.xpm: Pill.png
	convert Pill.png Pill.xpm

Pill.raw%json Pill%png: src/Pill.ase
	aseprite -b src/Pill.ase --sheet Pill$*png --data Pill.raw$*json
