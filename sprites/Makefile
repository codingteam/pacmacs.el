PILL=Pill.json Pill.xpm
BIG_PILL=Big-Pill.json Big-Pill.xpm

all: $(PILL) $(BIG_PILL) Makefile

Pill.json: Pill.raw.json
	jq ".meta.image |= \"Pill.xpm\"" Pill.raw.json > Pill.json

Pill.xpm: Pill.png
	convert Pill.png Pill.xpm

Pill.raw%json Pill%png: src/Pill.ase
	aseprite -b src/Pill.ase --sheet Pill$*png --data Pill.raw$*json

Big-Pill.json: Big-Pill.raw.json
	jq ".meta.image |= \"Big-Pill.xpm\"" Big-Pill.raw.json > Big-Pill.json

Big-Pill.xpm: Big-Pill.png
	convert Big-Pill.png Big-Pill.xpm

Big-Pill.raw%json Big-Pill%png: src/Big-Pill.ase
	aseprite -b src/Big-Pill.ase --sheet Big-Pill$*png --data Big-Pill.raw$*json
