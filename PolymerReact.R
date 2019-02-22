PolymerReact <- function(polymer){ 
  #funkce polymer react odstranuje jednotky stejneho typu, ale jine polarity z retezce polymeru
  #vstup...retezec polymeru
  #vystup...stabilni retezec polymeru
  smazano=0                                        #inicializace promenne
  asc <- function(x) { strtoi(charToRaw(x),16L) }  #funkce pro prevod znaku na ciselny kod v ascii
  while(1){                                        
    L=nchar(polymer)-1                             #delka polymeru                     
    smazano=0                                      #pri kazdem opakovani smycky se promenna smazano nuluje
    for (i in 1:L){                                #pro vsechny jednotky polymeru
      b=substring(polymer,i,i)                     #prvek polymeru i
      c=substring(polymer,(i+1),(i+1))             #prvek polymeru i+1 
      if (abs((asc(b)-asc(c)))==32){               #pokud jsou od sebe prvky v ascii tabulce vzdaleny 32
        substr(polymer,i,(i+1))<-'11'               #oba se prepisi na jednicky 
        smazano=smazano+2                           #k poctu smazanych se pricte 2 
      }}              
    polymer=gsub('1',"",polymer)                   #na konci kazde iterace se jednicky vymazou
    if ((smazano==0) | ((nchar(polymer)<2))) break} #pokud je delka polymeru mensi nez 2 nebo uz neprobihaji zadne dalsi reakce, smycka konci
  polymer
}