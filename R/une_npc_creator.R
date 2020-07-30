#' @export
une_power <- function (rlevel = 3){

  power_level <- c("Much Weaker","Slightly Weaker","Comparable","Slightly Stronger","Much Stronger")

  if(rlevel == 1){
    #print("Order")
    return(sample(power_level,size = 1, prob = c(2,8,80,8,2)))
  }
  if(rlevel == 2){
    #print("Calm")
    return(sample(power_level,size = 1, prob = c(4,11,70,11,4)))
  }
  if(rlevel == 3){
    #print("Standard")
    return(sample(power_level,size = 1, prob = c(5,15,60,15,5)))
  }
  if(rlevel == 4){
    #print("Disarray")
    return(sample(power_level,size = 1, prob = c(8,17,50,17,8)))
  }
  if(rlevel == 5){
    #print("Chaos")
    return(sample(power_level,size = 1, prob = c(12,18,40,18,12)))
  }
}

une_npc_modifier <- function(){
    mods <- c("superfluous","inept","pleasant","lethargic","jovial","addicted",
              "banal","insensitive","defiant","shrewd","conformist","logical",
              "titled","obnoxious","liberal","nefarious","subtle","inexperienced",
              "insightful","compliant","sensible","reputable","prying","tactless",
              "destitute","untrained","wicked","oblivious","fanatic","conniving",
              "romantic","lazy","refined","plebeian","careful","unreasonable",
              "pessimistic","indispensable","childish","alluring","skilled",
              "solemn","scholarly","pious","defective","neglectful","habitual",
              "conservative","uneducated","optimistic","lively","meek","uncouth",
              "inconsiderate","affluent","forthright","helpful","willful","cultured",
              "despondent","idealistic","unconcerned","indifferent","revolting",
              "mindless","unsupportive","generous","fickle","curious","passionate",
              "rational","docile","elderly","touchy","devoted","coarse","cheery",
              "sinful","needy","established","foolish","pragmatic","naive",
              "dignified","unseemly","cunning","serene","privileged","pushy",
              "dependable","delightful","thoughtful","glum","kind","righteous",
              "miserly","hopeless","likable","corrupt","confident")
    sample(mods, 1)
}

une_npc_noun <- function(){
  nouns <- c("gypsy","missionary","villager","mediator","performer","witch",
             "outcast","magus","crook","magister","merchant","mercenary",
             "conscript","civilian","serf","expert","caretaker","worker",
             "activist","brute","commoner","hermit","actor","hero","inquisitor",
             "judge","orator","herald","champion","lord","ranger","chieftain",
             "highwayman","cleric","villain","occultist","pioneer","fortune-hunter",
             "slave","professor","reverend","burglar","governor","gunman","servant",
             "thug","vicar","scrapper","clairvoyant","charmer","drifter","officer",
             "monk","patriarch","globetrotter","journeyman","explorer","homemaker",
             "shopkeeper","sniper","statesman","warden","recluse","crone","courtier",
             "astrologer","outlaw","steward","adventurer","priest","duelist","adept",
             "polymath","soldier","tradesman","jack-of-all-trades","bum","magician",
             "entertainer","hitman","aristocrat","sorcerer","traveler","craftsman",
             "wizard","preacher","laborer","vagrant","scientist","beggar","artisan",
             "master","apprentice","ascetic","tradesman","rogue","ascendant",
             "politician","superior","warrior")
  sample(nouns, 1)
}

#' @export
une_npc <- function(){
  tools::toTitleCase(paste(une_npc_modifier(),
        une_npc_noun()))
}

une_npc_motivation_verb <- function(){
  verbs <- c("advise","shepherd","take","work","manage","obtain","abuse",
             "discover","accompany","suppress","attempt","indulge","deter",
             "offend","proclaim","spoil","chronicle","acquire","guide","operate",
             "oppress","fulfill","damage","learn","access","interact","drive",
             "publicize","persecute","refine","create","review","burden",
             "communicate","compose","abduct","aid","advocate","process",
             "undermine","promote","follow","implement","report","explain",
             "conceive","advance","understand","develop","discourage","blight",
             "guard","collaborate","steal","attend","progress","conquer","strive",
             "suggest","detect","distress","hinder","complete","weaken","execute",
             "possess","plunder","compel","achieve","maintain","record","construct",
             "join","secure","realize","embrace","encourage","assist","inform",
             "convey","contact","agonize","defile","patronize","rob","pursue",
             "comprehend","produce","depress","establish","associate","administer",
             "institute","determine","overthrow","prepare","relate","account",
             "seek","support")
  sample(verbs, 1)
}

une_npc_motivation_noun <- function(parts = 1:5,
                                    complete  = FALSE){
  nouns <- list()
  nouns[[1]] <- c("wealth", "hardship", "affluence", "resources",
                  "prosperity", "poverty", "opulence", "deprivation",
                  "success", "distress", "contraband", "music",
                  "literature", "technology", "alcohol", "medicines",
                  "beauty", "strength", "intelligence", "force")
  nouns[[2]] <- c("the wealthy", "the populous", "enemies",
                  "the public", "religion", "the poor", "family",
                  "the elite", "academia", "the forsaken", "the law",
                  "the government", "the oppressed", "friends",
                  "criminals", "allies", "secret societies",
                  "the world", "military", "the church")
  nouns[[3]] <- c("dreams", "discretion", "love", "freedom",
                  "pain", "faith", "slavery", "enlightenment",
                  "racism", "sensuality", "dissonance", "peace",
                  "discrimination", "disbelief", "pleasure", "hate",
                  "happiness", "servitude", "harmony", "justice")
  nouns[[4]] <- c("gluttony", "lust", "envy", "greed", "laziness",
                  "wrath", "pride", "purity", "moderation",
                  "vigilance", "zeal", "composure", "charity",
                  "modesty", "atrocities", "cowardice", "narcissism",
                  "compassion", "valor", "patience")
  nouns[[5]] <- c("advice", "propaganda", "science", "knowledge",
                  "communications", "lies", "myths", "riddles",
                  "stories", "legends", "industry", "new religions",
                  "progress", "animals", "ghosts", "magic", "nature",
                  "old religions", "expertise", "spirits")
  part <- sample(parts, 1)
  if(complete){
    return(list(sample(nouns[[part]], 1), part))
  }else{
    return(sample(nouns[[part]], 1))
  }
}
#' @export
une_npc_motivation <- function(parts = 1:5, complete = FALSE){
  if(!complete){
    return(tools::toTitleCase(paste(une_npc_motivation_verb(),
          une_npc_motivation_noun(parts))))
  }else{
    mot <- une_npc_motivation_noun(parts, complete)
    return(list(paste(une_npc_motivation_verb(),
                      mot[[1]]),
                mot[[2]]))
  }

}
#' @export
une_npc_complete <- function(){
 result <- paste(une_npc(), "\nMotivations:")
 parts <- 1:5
 mots <- list()
 mots[[1]] <- une_npc_motivation(parts, TRUE)
 parts <- parts[-which(parts == mots[[1]][[2]])]
 mots[[2]] <- une_npc_motivation(parts, TRUE)
 parts <- parts[-which(parts == mots[[2]][[2]])]
 mots[[3]] <- une_npc_motivation(parts, TRUE)
 cat(tools::toTitleCase(paste(result,
                              mots[[1]][[1]],
                              mots[[2]][[1]],
                              mots[[3]][[1]],
                              sep = "\n")))
}
#' @export
une <- function(power = F, power_r = 3){
  cat("NPC:\n\n")
  une_npc_complete()
  cat("\n\nConversation:\n\n")
  une_conv()
  if(power){
    cat("\n\nPower Level:\n\n")
    une_power(power_r)
  }
}
