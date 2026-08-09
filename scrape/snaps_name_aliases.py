"""FantasyPros -> CBS canonical player name aliases for the `snaps` table.

FantasyPros has used different display names than CBS for the same real
player at various points (nicknames, legal name changes, suffix handling).
Discovered by cross-referencing a 2020-2025 FantasyPros CSV export against
td.main.weekly (Season+NFL team+Pos matched, manually reviewed to exclude
coincidental same-team/position/season pairings of two different real
players - see conversation history for the excluded candidates and why).
"""

NAME_ALIASES = {
    "Bam Knight": "Zonovan Knight",
    "Cedrick Wilson": "Ced Wilson",
    "Chig Okonkwo": "Chigoziem Okonkwo",
    "Chris Brooks": "Christopher Brooks",
    "DeMario Douglas": "Demario Douglas",
    "Dee Eskridge": "D'Wayne Eskridge",
    "Drew Ogletree": "Andrew Ogletree",
    "Hollywood Brown": "Marquise Brown",
    "Irv Charles": "Irvin Charles",
    "Joshua Palmer": "Josh Palmer",
    "Kenny Gainwell": "Kenneth Gainwell",
    "KhaDarel Hodge": "Khadarel Hodge",
    "La'Mical Perine": "Lamical Perine",
    "Mitch Tinsley": "Mitchell Tinsley",
    "Mitchell Trubisky": "Mitch Trubisky",
    "Nyheim Miller-Hines": "Nyheim Hines",
    "Robbie Chosen": "Chosen Anderson",
    "Scotty Miller": "Scott Miller",
    "Tre' Harris": "Tre Harris",
    "Tyron Billy-Johnson": "Tyron Johnson",
}


def apply_name_aliases(names):
    return names.replace(NAME_ALIASES)
