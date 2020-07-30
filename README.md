# coren

This package contains a collection of tools to enhance your solo roleplaying games.

## Installation Instructions

Run these commands in your R console:

```
if(!'devtools'%in%installed.packages()){
        install.packages('devtools')
    }
library(devtools)
install_github('rpg-tips/coren')
```
## Contained Functions

There are functions for several solo tools. Each family function has a common prefix.

- Mythic Game Master Emulator: prefix `gme`. 

Functions: `gme_fate_check`, `gme_detail_check`,`gme_event_check`, `gme_description_table`, `gme_actions_table`, `gme_behavior_check` (requires Shiny installed).

- Conjecture Games BOLD and UNE: prefixes `bold` and `une`.

- Instant Game: prefix `inst`.

- Ironsworn: prefix `iron`.




