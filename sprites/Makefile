PILL=Pill.json Pill.xpm
BIG_PILL=Big-Pill.json Big-Pill.xpm
PACMAN_DEATH=Pacman-Death.json Pacman-Death.xpm

all: $(PILL) $(BIG_PILL) $(PACMAN_DEATH) Makefile

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

Pacman-Death.json: Pacman-Death.raw.json
	jq ".meta.image |= \"Pacman-Death.xpm\"" Pacman-Death.raw.json > Pacman-Death.json

Pacman-Death.xpm: Pacman-Death.png
	convert Pacman-Death.png Pacman-Death.xpm

Pacman-Death.raw%json Pacman-Death%png: src/Pacman-Death.ase
	aseprite -b src/Pacman-Death.ase --sheet Pacman-Death$*png --data Pacman-Death.raw$*json
