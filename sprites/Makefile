PILL=Pill.json Pill.xpm
BIG_PILL=Big-Pill.json Big-Pill.xpm
PACMAN_DEATH=Pacman-Death.json Pacman-Death.xpm
TERRIFIED_GHOST=Terrified-Ghost.json Terrified-Ghost.xpm
BLINKING_TERRIFIED_GHOST=Blinking-Terrified-Ghost.json Blinking-Terrified-Ghost.xpm

all: $(PILL) $(BIG_PILL) $(PACMAN_DEATH) $(TERRIFIED_GHOST) $(BLINKING_TERRIFIED_GHOST) Makefile

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

Terrified-Ghost.json: Terrified-Ghost.raw.json
	jq ".meta.image |= \"Terrified-Ghost.xpm\"" Terrified-Ghost.raw.json > Terrified-Ghost.json

Terrified-Ghost.xpm: Terrified-Ghost.png
	convert Terrified-Ghost.png Terrified-Ghost.xpm

Terrified-Ghost.raw%json Terrified-Ghost%png: src/Terrified-Ghost.ase
	aseprite -b src/Terrified-Ghost.ase --sheet Terrified-Ghost$*png --data Terrified-Ghost.raw$*json

Blinking-Terrified-Ghost.json: Blinking-Terrified-Ghost.raw.json
	jq ".meta.image |= \"Blinking-Terrified-Ghost.xpm\"" Blinking-Terrified-Ghost.raw.json > Blinking-Terrified-Ghost.json

Blinking-Terrified-Ghost.xpm: Blinking-Terrified-Ghost.png
	convert Blinking-Terrified-Ghost.png Blinking-Terrified-Ghost.xpm

Blinking-Terrified-Ghost.raw%json Blinking-Terrified-Ghost%png: src/Blinking-Terrified-Ghost.ase
	aseprite -b src/Blinking-Terrified-Ghost.ase --sheet Blinking-Terrified-Ghost$*png --data Blinking-Terrified-Ghost.raw$*json
