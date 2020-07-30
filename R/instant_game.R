#' @export
inst_setting <- function(){
  r <- c("Alien Occupied Earth","All Supers World","Alternate History","Ancient Egypt","Ape World",
         "Arabian Nights","Archaic/Biblical","Arkship","Artificial World","Atlantis","Barbarian Europe",
         "Bizarro World","Boston"," 1770s","Camping Out","City Under Siege","Classical (Greek/Roman/Peloponnesian)",
         "Cloud City","Cold War","Contemporary","Correctional System","Cyberpunk","Darkworld","Dawn of civilization",
         "Depopulated Earth","Desert Planet","Dreamworld","Dungeon","Dying World","Dystopia","Early Space Exploration",
         "Elizabethan England","Fallen Civilization","Farm","Festival","First Alien Contact","Forest",
         "French Revolution","Frontier","Frozen Wasteland","Future Earth","Galactic Empires","Golden Age of Comics",
         "Grimm Fairy Tales","Historical","Industrial Revolution","Institution/Institutionalized",
         "Isolated Space Probe","Jungle World","Limbo","London","Lost Civilization","Lost/Stranded","Mars",
         "Mechanized Planet","Medieval","Microworld","Military Installation","Modern Day Las Vegas",
         "Modern-Day Amusement Park","Modern-Day Milwaukee","Moon","Mystical Orient","Near Future",
         "Near Future World Government HQ","Neverland/Childworld","Orwellian Future","Outpost","Post-Apocalyptic",
         "Pre-European Americas","Psychic Near-Future","Realm of the Gods","Remote Future","Renaissance","Resort/Spa",
         "School of Magic","Sea Adventures","Skyscraper","Snowed-In Town","Space Colony","Steampunk","Stone Age",
         "Suburban Retail Center","Sword & Sorcery","Tiny People","Tokyo/Near-Future","Traveling Circus",
         "Tree-top Civilization","Tropical Paradise","Underground","Underwater","Viking explorers",
         "Virtual Reality","War","Wild West","Wilderness","Wildlife Preserve","WWI","WWII","WWIII","Your Place")
  return(sample(r,1))
}
#' @export
inst_tones <- function(){
  r <- c("Action/Adventure","B-movie","Campy","Conspiracy","Despair","Epic","Fantasy","Gothic",
         "Heroic","Horror","Intrigue","Mystery","Mythic","Parable/Moralistic","Pulp","Realistic",
         "Romance","Suspense","Swashbuckling","Tactical")
  return(sample(r,1))
}

