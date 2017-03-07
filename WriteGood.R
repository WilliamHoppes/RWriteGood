#R writegood
#This script opens up a text document and UPPERCASES weak words or phrases.
#Based on http://rs.io/software-writers-tools-improve-writing/
#Based on https://github.com/robertseaton/writegood-mode/blob/master/writegood-mode.el
WriteGood<-function(FileName) {
  
  #Added in "ly" words from http://www.techtoolsforwriters.com/hunt-down-adverbs-with-a-macro/
  WeaselWords<-c("many", "various", "very", "fairly", "several", "extremely",  
                 "exceedingly", "quite", "remarkably", "few", "surprisingly",  
                 "mostly", "largely", "huge", "tiny", "are a number", "is a number",  
                 "excellent", "interestingly", "significantly", "substantially",  
                 "clearly", "vast", "relatively", "completely", "literally",  
                 "not rocket science", "outside the box", "angrily", "cautiously", "happily", 
                 "sadly", "unhappily")
  
  #Based on the emacs, this is all combinations of passive "be" verbs 
  PassiveVoiceIrregulars<-c("awoken", "been", "born", "beat", "become", "begun", "bent", "beset",
                            "bet", "bid", "bidden", "bound", "bitten", "bled", "blown", "broken",
                            "bred", "brought", "broadcast", "built", "burnt", "burst", "bought",
                            "cast", "caught", "chosen", "clung", "come", "cost", "crept", "cut",
                            "dealt", "dug", "dived", "done", "drawn", "dreamt", "driven", "drunk",
                            "eaten", "fallen", "fed", "felt", "fought", "found", "fit", "fled",
                            "flung", "flown", "forbidden", "forgotten", "foregone", "forgiven",
                            "forsaken", "frozen", "gotten", "given", "gone", "ground", "grown",
                            "hung", "heard", "hidden", "hit", "held", "hurt", "kept", "knelt", "knit",
                            "known", "laid", "led", "leapt", "learnt", "left", "lent", "let", "lain",
                            "lighted", "lost", "made", "meant", "met", "misspelt", "mistaken", "mown",
                            "overcome", "overdone", "overtaken", "overthrown", "paid", "pled", "proven",
                            "put", "quit", "read", "rid", "ridden", "rung", "risen", "run", "sewn",
                            "said", "seen", "sought", "sold", "sent", "set", "sewn", "shaken", "shaven",
                            "shorn", "shed", "shone", "shod", "shot", "shown", "shrunk", "shut",
                            "sung", "sunk", "sat", "slept", "slain", "slid", "slung", "slit",
                            "smitten", "sown", "spoken", "sped", "spent", "spilt", "spun", "spit",
                            "split", "spread", "sprung", "stood", "stolen", "stuck", "stung",  
                            "stunk", "stridden", "struck", "strung", "striven", "sworn", "swept",  
                            "swollen", "swum", "swung", "taken", "taught", "torn", "told", "thought",  
                            "thrived", "thrown", "thrust", "trodden", "understood", "upheld", "upset",  
                            "woken", "worn", "woven", "wed", "wept", "wound", "won", "withheld",  
                            "withstood", "wrung", "written")
  PassivePreceders<-c("am", "are", "were", "being", "is", "been", "was", "be")
  PassivePhrase<-expand.grid(PassivePreceders, PassiveVoiceIrregulars)
  PassivePhrase<-data.frame(Var1=as.character(PassivePhrase$Var1),
                            Var2=as.character(PassivePhrase$Var2), stringsAsFactors = F)
  PassivePhrase<-apply(PassivePhrase, 1, paste, collapse=" ")
  
  #Cliches
  #Also based on http://cliche.theinfo.org/cliches
  Cliches<-c("accidentally on purpose", "accident waiting to happen", "actions speak louder than words",
             "act of contrition", "absolutely conclusive", "agricultural crops", "awkward dilemma",
             "close proximity", "complete monopoly", "completely full", "divisive quarrel", 
             "end result", "entirely absent", "exact counterpart", "future plan", "general public", 
             "grateful thanks", "hired mercenary", "irreducible minimum", "lonely hermit", 
             "lifeless corpse", "meaningless gibberish", "mutual cooperation", "new record",
             "old adage", "organic life", "original founder", "patently obvious", "personal friend", 
             "personal friend", "personal opinion", "pragmatic realist", "present incumbent",
             "sworn affidavit", "true facts", "ultimate outcome", "violent explosion", 
             "determined the truth of", "gave permission to", "held a meeting", "proved of benefit to",
             "put in an appearance", "reached an agreement", "submitted his resignation",
             "take into consideration", "established conclusive evidence of", "take into custody", 
             "accommodations", "ameliorate", "approximately", "assistance", "commence", "deactivate", 
             "endeavor", "finalize", "implement", "in consequence of", "initiate", "methodology", 
             "motivation", "objective", "peruse", "prior to", "proliferation", "purchase", 
             "remuneration", "replicate", "socialize", "substantial proportion", "underprivileged", 
             "utilize", "armed to the teeth", "banker's hours", "battle royal", "beat a hasty retreat",
             "beauty and the beast", "bewildering variety", "beyond the shadow of a doubt", 
             "bite the dust", "blazing inferno", "blessed event", "blessing in disguise", 
             "blissful ignorance", "bull in a china shop", "burn one's bridges", "burn your bridges", 
             "burn the midnight oil", "burning issue", "bury the hatchet", "buried the hatchet", 
             "calm before the storm", "cherished belief", "clear the decks", "club-wielding police", 
             "colorful scene", "conspicuous by its absence", "coveted award", "crack troops", 
             "curvaceous blonde", "dramatic new move", "dread disease", "dream come true", 
             "drop in the bucket", "fame and fortune", "feast or famine", "fickle fortune", 
             "gentle hint", "glaring omission", "glutton for punishment", "gory detail", 
             "grief-stricken", "Grim Reaper", "hand in glove", "hammer out", "happy couple", 
             "hook, line and sinker", "hook, line, and sinker", "head over heels in love", 
             "heart of gold", "heavily armed troops", "iron out", "intensive investigation", 
             "Lady Luck", "lashed out", "lashes out", "last-ditch stand", "leave no stone unturned", 
             "leaps and bounds", "light at the end of the tunnel", "lightning speed", "limp into port",
             "limps into port", "lock, stock and barrel", "lock, stock, and barrel", 
             "long arm of the law", "man in the street", "marvels of science", "matrimonial bliss", 
             "meager pension", "miraculous escape", "Mother Nature", "moves into high gear", 
             "never a dull moment", "Old Man Winter", "paint a grim picture", "paints a grim picture",
             "pay a supreme penalty", "picture of health", "pillar of society", "pillar of the church",
             "pinpoint the cause", "police dragnet", "pool of blood", "posh resort", "powder keg", 
             "pre-dawn darkness", "prestigious law firm", "proud heritage", "proud parents", 
             "pursuit of excellence", "radiant bride", "red faces", "red-faced", "reins of government",
             "rushed to the scene", "scantily clad", "scintilla of evidence", "scurried to shelter",
             "spotlessly clean", "sprawling base", "sprawling facility", "spreading like wildfire",
             "steaming jungle", "stick out like a sore thumb", "sticks out like a sore thumb", 
             "stranger than fiction", "storm of protest", "supreme sacrifice", "surprise move", 
             "sweep under the rug", "sweet harmony", "sweetness and light", "tender mercies",
             "terror-stricken", "tie the knot", "tip of the iceberg", "true colors", 
             "vanish in thin air", "walking encyclopedia", "wealth of information", 
             "whirlwind campaign", "wouldn't touch with a ten foot pole", "last but not least",
             "beck and call", "betwixt and between", "bits and pieces", "blunt and brutal", 
             "bound and determined", "clear and simple", "confused and bewildered", 
             "death and destruction", "disgraced and dishonored", "each and every", "fair and just", 
             "few and far between", "nervous and distraught", "nook and cranny", "pick and choose",
             "ready and willing", "right and proper", "safe and sound", "shy and withdrawn", 
             "smooth as silk", "various and sundry", "very unique")
  
  #Taken from http://www.techtoolsforwriters.com/omit-needless-words-with-a-macro/
  NeedlessWords<-c("then", "almost", "about", "begin", "start", "decided", "planned", "very", "sat",
                   "truly", "rather", "fairly", "really", "somewhat", "up", "over", "together",
                   "behind", "in order", "around", "only", "just", "even")
  
  #Taken from http://www.techtoolsforwriters.com/showing-vs-telling-macro/
  TellingWords<-c("was", "were", "when", "as", "the sound of", "could see", "saw", "notice",
                  "noticed", "noticing", "consider", "considered", "considering", "smell", "smelled",
                  "heard", "felt", "tasted", "knew", "realize", "realized", "realizing", "think",
                  "thought", "thinking", "believe", "believed", "believing", "wonder", "wondered",
                  "wondering", "recognize", "recognized", "recognizing", "hope", "hoped", "hoping",
                  "supposed", "pray", "prayed", "praying", "angrily")
  
  AutoReplace<-data.frame(
    bad=c("\".", "\","),
    good=c(".\"", ",\"")
  )
  
  #Imports text, includes grammar notation eg. /n for new line
  #http://stackoverflow.com/questions/9068397/import-text-file-as-single-character-string
  text<-readChar(FileName, file.info(FileName)$size)
  
  #processing, need to function-alize, scoping screws me up
  AllWords<-c(WeaselWords, PassivePhrase, Cliches, NeedlessWords, TellingWords)
  AllWords<-paste(" ", AllWords, sep="")
  AllWords<-paste(AllWords, " ", sep="")
  
  for (i in 1:length(AllWords)) {
    text<-gsub(AllWords[i], toupper(AllWords[i]), text, ignore.case = T)
  }
  for (i in 1:length(AutoReplace)) {
    text<-gsub(AutoReplace[i, 1], AutoReplace[i, 2], text, ignore.case = T)
  }
  
  #Get credit
  if (grepl(".Rmd", FileName)==T) {
    text<-paste(text, "\n \nEdited with [RWriteGood](https://williamhoppes.github.io)")
  } else {
    text<-paste(text, "\n \nEdited with RWriteGood")
  }
  #Write it back out !Warning, overwrite old file
  writeChar(text, FileName)
}