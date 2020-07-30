# GME Variations 2 Fate Check ----

#' Mythic Variations 2 Fate Check
#'
#' Use it to generate yes / no answers, with
#' the possibility of generating exceptional results and random events.
#' Please consult Mythic Variations II to understand exactly how these results
#'  are generated.
#'
#' @param chaosFactor A number between 3 and 6. Defaults to 4. Higher values
#' correspond to situations in which your characters are disfavoured and
#' viceversa. Use \code{chaosAdjust} to control it if your Chaos Factor is 3 or 6.
#' @param chaosAdjust A number, one of -2 or +2, modifying the likelihood of a
#' yes in a positive (CF 3) or negative (CF 6) way for the character/s.
#' @param prob A number: 0 (50/50 or unsure), +2/-2 (likely / unlikely),
#'  +4/-4 (very likely / very unlikely), +6/-6 (sure thing / no way) or
#'   +8/-8 (has to be / impossible).
#' @export
gme_fate_check <- function(chaosFactor = 4,
                           chaosAdjust = 2,
                           prob = 0){
  throw1 <- sample(1:10,1)
  throw2 <- sample(1:10,1)
  throw <-  throw1 + throw2 + prob
  if(chaosFactor %in% c(3,6)){
    throw <- throw + chaosAdjust
  }
  answer <- ifelse(test = throw >= 11, yes = "Yes", no = "No")
  chaosDice <- sample(1:10,1)
  if(chaosDice <= chaosFactor){
    if(throw1 == throw2){
      answer <- paste("Random Event and Exceptional", answer)
      return(answer)
    }
    if(throw1%%2==0 & throw2%%2==0){
      answer <- paste("Random Event and", answer)
    }
    if(throw1%%2!=0 & throw2%%2!=0){
      answer <- paste("Exceptional", answer)
    }
  }
  return(answer)
}

# GME Variations 2 Detail Check ----

#' Mythic Variations 2 Detail Check
#'
#' Use it to answer questions that need more
#' detail than just a yes or no, such as "How does the tavern look?" or
#' "What does the cache contain?".
#'
#' @param cf A number between 3 and 6. Defaults to 4. Mythic Chaos Factor.
#' Higher values correspond to situations in which your characters are
#' disfavoured and viceversa.
#' @param debug A logical. If \code{TRUE}, the output is a list with the die roll
#' result included. If \code{FALSE}, only the result is shown. Defaults to \code{FALSE}.
#' @param themes A logical. If \code{TRUE}, results related to NPCs are changed
#' to results related to Themes. Defaults to \code{FALSE}.
#' @export
gme_detail_check <- function(cf = 4,
                             debug = F,
                             themes = F){
  throw <- sample(1:10,1) + sample(1:10,1)
  if(cf == 3) throw <- throw + 2
  if(cf == 6) throw <- throw - 2
  results <- c("Anger", "Sadness", "Fear", "Disfavors Thread", "Disfavors PC", "Focus NPC", "Favors NPC",
               "Focus PC", "Disfavors NPC", "Focus Thread", "Favors PC", "Favors Thread", "Courage",
               "Happiness", "Calm")
  if(themes){
    results[c(6,7)] <- 'Focus Theme'
    results[9] <- 'Disfavors Theme'
  }
  if(throw < 4){
    result <- "Anger"
  }else if(throw > 18){
      result <- "Calm"
  }else{
      result <- results[throw-3]
    }
  if(debug){
    return(list(throw, result))
  }else{
      return(result)
    }
}

# GME Variations 2 Event Check ----

#' Mythic Event Check
#'
#' Including themes from Variations I. Use it when
#' the \code{\link{Fate Check}} generates a Random Event, when your scene is
#' Interrupted or whenever you want to throw some randomness.
#'
#' @param theme A character. One of "standard" (default), "horror", "mystery" or
#' "chekhov".
#' @export
#' @seealso \code{\link{gme_plot_point()}} and \code{\link{gme_turning_point()}}
gme_event_check <- function(theme = "standard"){
  if(theme == 'standard'){
    result <- c(rep("Remote Event", 7),
                rep("NPC Action", 21),
                rep("Introduce a new NPC", 7),
                rep("Move towards a thread", 10),
                rep("Move away from a thread", 7),
                rep("Close a thread", 3),
                rep("PC Negative", 12),
                rep("PC Positive", 8),
                rep("Ambiguous Event", 8),
                rep("NPC Negative", 9),
                rep("NPC Positive", 8))
  }
  if(theme == 'horror'){
    result <- c(rep("Horror - PC", 10),
                rep("Horror - NPC", 13),
                rep("Remote event", 7),
                rep("NPC Action", 19),
                rep("New NPC", 3),
                rep("Move toward a thread", 3),
                rep("Move away from a thread", 7),
                rep("PC Negative", 10),
                rep("PC Positive", 3),
                rep("Ambiguous event", 7),
                rep("NPC negative", 15),
                rep("NPC positive", 3))
  }
  if(theme == 'mystery'){
    result <- c(rep("Remote event", 8),
                rep("NPC Action", 12),
                rep("New NPC", 12),
                rep("Move toward a thread", 20),
                rep("Move away from a thread", 12),
                rep("PC Negative", 8),
                rep("PC Positive", 8),
                rep("Ambiguous event", 8),
                rep("NPC negative", 8),
                rep("NPC positive", 4))
  }
  if(theme == 'chekhov'){
    result <- c(rep("Remote Event", 7),
                rep("NPC Action", 21),
                rep("Introduce a new NPC", 7),
                rep("Move towards a thread", 10),
                rep("Move away from a thread", 7),
                rep("Close a thread", 3),
                rep("PC Negative", 12),
                rep("PC Positive", 8),
                rep("Chekhov's gun is shot / Ambiguous event", 8),
                rep("NPC Negative", 9),
                rep("NPC Positive", 8))
  }
  return(sample(result, 1))
}

# GME Meaning Tables: Descriptions ----

#' Mythic Meaning Tables: Descriptions
#'
#' Use it to generate two word descriptions, such as Kindly Delightful or
#' Delightfully Bizarre.
#' @export
#' @seealso \code{\link{gme_actions_table}},
#' \code{\link{gme_actions_table2}} and
#' \code{\link{rand_gma}}
gme_description_table <- function(){
  word1 <- c("Abnormally","Adventurously","Aggressively","Angrily","Anxiously","Awkwardly","Beautifully",
             "Bleakly","Boldly","Bravely","Busily","Calmly","Carefully","Carelessly","Cautiously","Ceaselessly",
             "Cheerfully","Combatively","Coolly","Crazily","Fully","Generously","Gently","Gladly","Gracefully",
             "Gratefully","Happily","Hastily","Healthily","Helpfully","Helplessly","Hopelessly","Innocently",
             "Intensely","Interestingly","Irritatingly","Jovially","Joyfully","Judgementally","Kindly",
             "Peacefully","Perfectly","Playfully","Politely","Positively","Powerfully","Quaintly","Quarrelsomely",
             "Quietly","Roughly","Rudely","Ruthlessly","Slowly","Softly","Swiftly","Threateningly","Very","Violently",
             "Wildly","Yieldingly","Curiously","Daintily","Dangerously","Defiantly","Deliberately","Delightfully",
             "Dimly","Efficiently","Energetically","Enormously","Enthusiastically","Excitedly","Fearfully",
             "Ferociously","Fiercely","Foolishly","Fortunately","Frantically","Freely","Frighteningly","Kookily",
             "Lazily","Lightly","Loosely","Loudly","Lovingly","Loyally","Majestically","Meaningfully","Mechanically",
             "Miserably","Mockingly","Mysteriously","Naturally","Neatly","Nicely","Oddly","Offensively","Officially",
             "Partially")
  word2 <- c("Abandoned","Abnormal","Amusing","Ancient","Aromatic","Average","Beautiful","Bizarre","Classy","Clean",
             "Cold","Colorful","Creepy","Cute","Damaged","Dark","Defeated","Delicate","Delightful","Dirty",
             "Graceful","Hard","Harsh","Healthy","Heavy","Historical","Horrible","Important","Interesting",
             "Juvenile","Lacking","Lame","Large","Lavish","Lean","Less","Lethal","Lonely","Lovely","Macabre",
             "Remarkable","Rotten","Rough","Ruined","Rustic","Scary","Simple","Small","Smelly","Smooth","Soft",
             "Strong","Tranquil","Ugly","Valuable","Warlike","Warm","Watery","Weak","Young","Disagreeable",
             "Disgusting","Drab","Dry","Dull","Empty","Enormous","Exotic","Faded","Familiar","Fancy","Fat",
             "Feeble","Feminine","Festive","Flawless","Fresh","Full","Glorious","Good","Magnificent","Masculine",
             "Mature","Messy","Mighty","Military","Modern","Extravagant","Mundane","Mysterious","Natural",
             "Nondescript","Odd","Pale","Petite","Poor","Powerful","Quaint","Rare","Reassuring")
  return(paste(sample(word1,1), sample(word2,1)))
}

# GME Meaning Tables: Actions ----

#' Mythic Meaning Tables: Actions
#'
#' Use it to generate two word actions, such as Agree Magic or Fight Adversities.
#' @export
#' @seealso \code{\link{gme_descriptions_table}},
#' \code{\link{gme_actions_table2}} and
#' \code{\link{rand_gma}}
gme_actions_table <- function(){
  word1 <- c("Attainment","Starting","Neglect","Fight","Recruit","Triumph","Violate","Oppose","Malice",
             "Communicate","Persecute","Increase","Decrease","Abandon","Gratify","Inquire","Antagonize",
             "Move","Waste","Truce","Expose","Haggle","Imprison","Release","Celebrate","Develop","Travel",
             "Block","Harm","Debase","Overindulge","Adjourn","Adversity","Kill","Disrupt","Usurp","Create",
             "Betray","Agree","Abuse","Excitement","Activity","Assist","Care","Negligence","Passion","Work",
             "Control","Attract","Failure","Pursue","Vengeance","Proceedings","Dispute","Punish","Guide",
             "Transform","Overthrow","Oppress","Change","Release","Befriend","Judge","Desert","Dominate",
             "Procrastinate","Praise","Separate","Take","Break","Heal","Delay","Stop","Lie","Return","Imitate",
             "Struggle","Inform","Bestow","Postpone","Oppress","Inspect","Ambush","Spy","Attach","Carry","Open",
             "Carelessness","Ruin","Extravagance","Trick","Arrive","Propose","Divide","Refuse","Mistrust",
             "Deceive","Cruelty","Intolerance","Trust")
  word2 <- c("Goals","Dreams","Environment","Outside","Inside","Reality","Allies","Enemies","Evil","Good",
             "Emotions","Opposition","War","Peace","Innocent","Love","Spirit","Intellect","Ideas","Joy",
             "Advice","Plot","Competition","Prison","Illness","Food","Attention","Success","Failure","Travel",
             "Jealousy","Dispute","Home","Investment","Suffering","Wishes","Tactics","Stalemate","Randomness",
             "Misfortune","Victory","Dispute","Riches","Normal","Technology","Hope","Magic","Illusions","Portals",
             "Danger","Weapons","Animals","Weather","Elements","Nature","Masses","Leadership","Fame","Anger",
             "Information","Messages","Energy","Balance","Tension","Friendship","Physical","Project","Pleasures",
             "Pain","Possessions","Benefits","Plans","Lies","Expectations","Legal","Bureaucracy","Business",
             "Path","News","Exterior","Death","Disruption","Power","Burden","Intrigues","Fears","Ambush","Rumor",
             "Wounds","Extravagance","Representative","Adversities","Opulence","Liberty","Military","Mundane",
             "Trials","Masses","Vehicle","Art")
  return(paste(sample(word1,1),sample(word2,1)))
}

# GME Meaning Tables: Actions Variation ----

#' Mythic Meaning Tables: Extended Actions
#'
#' A homebrewed modification of the original \link[=gme_actions_table]{Actions Table}
#'adding the first word from the \link[=gme_descriptions_table]{Descriptions Table}
#'in the middle, to generate results such as Persecute Damaged Fears and
#' Abuse Graceful Bureaucracy.
#' @export
#' @seealso \code{\link{gme_descriptions_table}},
#' \code{\link{gme_actions_table2}} and
#' \code{\link{rand_gma}}
#' @export
gme_actions_table2 <- function(){
  word1 <- c("Attainment","Starting","Neglect","Fight","Recruit","Triumph","Violate","Oppose","Malice",
             "Communicate","Persecute","Increase","Decrease","Abandon","Gratify","Inquire","Antagonize",
             "Move","Waste","Truce","Expose","Haggle","Imprison","Release","Celebrate","Develop","Travel",
             "Block","Harm","Debase","Overindulge","Adjourn","Adversity","Kill","Disrupt","Usurp","Create",
             "Betray","Agree","Abuse","Excitement","Activity","Assist","Care","Negligence","Passion","Work",
             "Control","Attract","Failure","Pursue","Vengeance","Proceedings","Dispute","Punish","Guide",
             "Transform","Overthrow","Oppress","Change","Release","Befriend","Judge","Desert","Dominate",
             "Procrastinate","Praise","Separate","Take","Break","Heal","Delay","Stop","Lie","Return","Imitate",
             "Struggle","Inform","Bestow","Postpone","Oppress","Inspect","Ambush","Spy","Attach","Carry","Open",
             "Carelessness","Ruin","Extravagance","Trick","Arrive","Propose","Divide","Refuse","Mistrust",
             "Deceive","Cruelty","Intolerance","Trust")
  word2 <- c("Abandoned","Abnormal","Amusing","Ancient","Aromatic","Average","Beautiful","Bizarre","Classy","Clean",
             "Cold","Colorful","Creepy","Cute","Damaged","Dark","Defeated","Delicate","Delightful","Dirty",
             "Graceful","Hard","Harsh","Healthy","Heavy","Historical","Horrible","Important","Interesting",
             "Juvenile","Lacking","Lame","Large","Lavish","Lean","Less","Lethal","Lonely","Lovely","Macabre",
             "Remarkable","Rotten","Rough","Ruined","Rustic","Scary","Simple","Small","Smelly","Smooth","Soft",
             "Strong","Tranquil","Ugly","Valuable","Warlike","Warm","Watery","Weak","Young","Disagreeable",
             "Disgusting","Drab","Dry","Dull","Empty","Enormous","Exotic","Faded","Familiar","Fancy","Fat",
             "Feeble","Feminine","Festive","Flawless","Fresh","Full","Glorious","Good","Magnificent","Masculine",
             "Mature","Messy","Mighty","Military","Modern","Extravagant","Mundane","Mysterious","Natural",
             "Nondescript","Odd","Pale","Petite","Poor","Powerful","Quaint","Rare","Reassuring")
  word3 <- c("Goals","Dreams","Environment","Outside","Inside","Reality","Allies","Enemies","Evil","Good",
             "Emotions","Opposition","War","Peace","Innocent","Love","Spirit","Intellect","Ideas","Joy",
             "Advice","Plot","Competition","Prison","Illness","Food","Attention","Success","Failure","Travel",
             "Jealousy","Dispute","Home","Investment","Suffering","Wishes","Tactics","Stalemate","Randomness",
             "Misfortune","Victory","Dispute","Riches","Normal","Technology","Hope","Magic","Illusions","Portals",
             "Danger","Weapons","Animals","Weather","Elements","Nature","Masses","Leadership","Fame","Anger",
             "Information","Messages","Energy","Balance","Tension","Friendship","Physical","Project","Pleasures",
             "Pain","Possessions","Benefits","Plans","Lies","Expectations","Legal","Bureaucracy","Business",
             "Path","News","Exterior","Death","Disruption","Power","Burden","Intrigues","Fears","Ambush","Rumor",
             "Wounds","Extravagance","Representative","Adversities","Opulence","Liberty","Military","Mundane",
             "Trials","Masses","Vehicle","Art")
  return(paste(sample(word1,1),
               sample(word2,1),
               sample(word3,1)))
}
