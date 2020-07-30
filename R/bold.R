# Functions from Conjecture Games, BOLD

# Features ----

bold_waylay_descriptor <- function(){
  sample(c("Easy Foes", "Hard Foes", "Knowledge", "Physical",
           "Factional", "Haven", "Party", "Personal", "Epic",
           "Natural"), 1)
}

bold_waylay_noun <- function(descriptor = bold_waylay_descriptor()){
  if(descriptor == "Easy Foes"){
    return(sample(c("Animals",
                    "Mooks",
                    "Mob",
                    "Bandits",
                    "Deputies"),
                  1))
  }
  if(descriptor == "Hard Foes"){
    return(sample(c("Bounty hunter",
                    "Soldiers",
                    "Monster",
                    "Villain",
                    "Horror"),
                  1))
  }
  if(descriptor == "Knowledge"){
    return(sample(c("Ascetic",
                    "research",
                    "occult ",
                    "enigma",
                    "science"),
                  1))
  }
  if(descriptor == "Physical"){
    return(sample(c("pursuit",
                    "trap",
                    "struggle",
                    "illness",
                    "labor"),
                  1))
  }
  if(descriptor == "Factional"){
    return(sample(c("army",
                    "invader",
                    "holdings",
                    "authority",
                    "rebels"),
                  1))
  }
  if(descriptor == "Haven"){
    return(sample(c("festival",
                    "hermit",
                    "tavern ",
                    "hamlet",
                    "conclave"),
                  1))
  }
  if(descriptor == "Party"){
    return(sample(c("misunderstanding",
                    "accusations",
                    "power play",
                    "friend-in-need",
                    "disappearance"),
                  1))
  }
  if(descriptor == "Personal"){
    return(sample(c("traitor",
                    "lover",
                    "death",
                    "relative",
                    "rival"),
                  1))
  }
  if(descriptor == "Epic"){
    return(sample(c("heaven / hell",
                    "afterlife",
                    "myth",
                    "otherworldly",
                    "the strange"),
                  1))
  }
  if(descriptor == "Natural"){
    return(sample(c("weather",
                    "straying /lost",
                    "social environ",
                    "deprivation",
                    "the wild"),
                  1))
  }
}

bold_waylay_modifier <- function(){
  res <- c("futile","harsh","binding","unreliable","abnormal","impassioned","leeching","noble","altruistic","abstract","hesitant","tranquil","copious","storied","irritating","benign","inclusive","retired","tapped","hidden","revered","righteous","provoking","tedious","hallowed","pedantic","attentive","ordinary","quiet","illusory","grim","inexplicable","prolonged","exotic","legendary","common","corrupt","deceiving","impervious","prolonged","bitter","roaring","savage","fledgling","meek","impassioned","unmistakable","drowsy","fixated","proficient","perceiving","sudden","mundane","illuminating","forbidden","selfish","impending","abrupt","exhausting","fantastic","prohibited","fragile","unforeseen","honorable","accidental","brusque","painless","peaceful","exclusive","malevolent","prosperous","haphazard","steady","apparent","unlikely","depraved","foreign","abetting","valuable","problematic","comforting","ravenous","stale","haunting","eccentric","hopeless","adept","regular","migrant","lethargic","waning","barbaric","dubious","pleasant","amusing","regimental","disputable","exclusive","incompetent","afflicting")
  sample(res, 1)
}

#' @export
bold_waylay_solution <- function(){
  res <- c("legendary help","act of nature","the people","enemy help","avoidance","scarce-used ability","personal resources","close friend","strong attribute","favored ability","favored skill","on accident","weak attribute","counteraction","faction intervention","the authority","fate","change of heart","deus ex")
  sample(res, 1)
}

#' @export
bold_waylay_theme <- function(){
  tools::toTitleCase(paste(bold_waylay_modifier(), bold_waylay_noun()))
}

#' @export
bold_waylay <- function(){
  tools::toTitleCase(paste(bold_waylay_modifier(), bold_waylay_noun(),"overcome by", bold_waylay_solution()))
}

#' @export
bold_arced_waylay <- function(n = "auto"){
  cat("- Waylay Theme:\n\n")
  cat(bold_waylay_theme())
  cat("\n\n- Nested Waylays:\n\n")
  if(n == 'auto'){
    cat('Exposition:',
        bold_waylay(),
        'Rising Action and Conflict:',
        bold_waylay(),
        'Climax and Resolution:',
        bold_waylay(),
        sep="\n")
  }else if(is.numeric(n)){
    for(i in 1:n){
      cat(bold_waylay())
      cat('\n')
    }
  }else{
    stop('`n` must be either a natural number or "auto".')
  }
}

bold_connections_action <- function(){
  res <- c("proclaim","protest","resolve","interject","demand","impart","commend","muse","examine","digress","assure","reminisce","ponder","scrutinize","address","deceive","negotiate","guide","endorse","speculate")
  sample(res,1)
}

bold_connections_gerund <- function(){
  res <- c("exposing","weakening","divulging","lying","confronting","detailing","avoiding","working","negating","preparing","training","enjoying","discussing","obsessing","repairing","improving","understanding","connecting","concluding","excluding")
  sample(res,1)
}

bold_connections_subject <- function(){
  res <- c("a dislike","an attribute","a desire","friends or family","an annoyance","a skill","a fear","wealth","home or hearth","an ability","a decision","a love","personal qualities","your nature","a like","enemies","party member","a core belief","a possession","allies")
  sample(res,1)
}

#' @export
bold_connections <- function(){
  cat("- Connections grid:\n\n")
  cat(bold_connections_action(),bold_connections_gerund(),bold_connections_subject(),"\n",sep="\t\t\t")
  cat(bold_connections_action(),bold_connections_gerund(),bold_connections_subject(),"\n",sep="\t\t\t")
  cat(bold_connections_action(),bold_connections_gerund(),bold_connections_subject(),"\n",sep="\t\t\t")
}