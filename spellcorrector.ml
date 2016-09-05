module MyHash = struct
  type t = string
  let equal a b = String.compare a b = 0
  let hash = Hashtbl.hash
end
module WordDict = Hashtbl.Make(MyHash)

let work_time = ref 0.
let work_count = ref 0
let add_time f x =
  incr work_count;
  let start = Sys.time() in
  let a = f x in
  work_time := !work_time +. Sys.time() -. start;
  a
let alphabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';
                'j';'k';'l';'m';'n';'o';'p';'q';'r';
                's';'t';'u';'v';'w';'x';'y';'z']

let soc c = String.make 1 c
let alphabet = List.map soc alphabet

(** Return a [String] representing the content of the specified [filename] **)
let read_file filename =
  let channel = open_in filename in
  let channel_length = in_channel_length channel in
  let result = Bytes.create channel_length in
  really_input channel result 0 channel_length;
  result

(** Return the integer value associated with the specified [key] in the specified
    [dictionary] if present or 1 if not present **)
let find_elem_one elem dict =
  try WordDict.find dict elem with Not_found -> 1

let r = Str.regexp "[^a-z]+"

(** Dictionary that maps words in a text to the number of occurrences of those
    words in the file "big.txt" **)
let nwords =
  let words text = Str.split r (String.lowercase_ascii text) in
  let dict = WordDict.create 1000 in
  let train features =
    List.iter
      (fun elem -> WordDict.replace dict elem @@ find_elem_one elem dict + 1)
      features
  in
  train @@ words @@ read_file "big.txt";
  dict

(** Return the list of all possible strings obtained from the specified [word] by:
    1. deleting any one character of [word];
    2. inserting any one character of the English alphabet in [word];
    3. replacing any one character in [word] with any character of the English alphabet;
    4. transposing any pair of adjacent characters in [word]. **)

let edits1 word =
  let splits =
    let rec all_splits word n =
      let s = Str.string_before word n, Str.string_after word n in
      if n == 0 then [s]
      else s :: all_splits word (n - 1)
    in
    all_splits word (String.length word)
  in
  let replaces =
    let filtered =
      List.map (fun (a,b) -> (a, Str.string_after b 1)) @@
      List.filter (fun (_,s) -> String.length s > 0) splits in
    List.fold_left (fun acc c ->
      List.fold_left (fun acc2 (a,b) -> (a^c^b)::acc2) acc filtered)
      [] alphabet
  in
  let filtered = List.filter (fun (_,s) -> String.length s >= 1) splits in
  let deletes =
    List.map
      (fun (a,b) -> a ^ Str.string_after b 1)
      filtered
  in
  let filtered = List.filter (fun (_,s) -> String.length s > 1) filtered in
  let transposes =
    let transpose (a,b) =
      a ^ (soc @@ b.[1]) ^ (soc @@ b.[0]) ^ (Str.string_after b 2)
    in
    List.map transpose filtered
  in
  let inserts =
    let insert c = List.map (fun (a,b) -> a^c^b) splits in
    List.concat (List.map insert alphabet)
  in
  inserts @ replaces @ transposes @ deletes

(** Return the list of string result of removing from the specified [words] all
    the strings that are not part of an implementation defined list of known words
**)
let known words =
  List.filter (fun word -> WordDict.mem nwords word) words

(** Return the list of all possible strings obtained from applying twice the
    function [edits1] to the specified [word] **)
let known_edits2 word =
  let rec uniq = function
    | a :: (b :: _ as t) -> if a = b then uniq t else a :: uniq t
    | smaller -> smaller
  in
  let distance1 = uniq (edits1 word) in
  let distance2 = distance1 @ List.concat (List.map edits1 distance1) in
  let distance2 = uniq @@ known distance2 in
  distance2

let (|-) f g x = f @@ g x
let (|||) x f = if x = [] then f () else x

(** Return the string [c] the maximizes the probability that the specified
    [word] was written when [c] was intended, according to an implementation defined
    dictionary **)
let correct word =
  (* This is the equivalent of
      candidates =
          known(word) or known(edits1(word)) or known_edits2(word) or word'
  *)
  let candidates =
    known [word] ||| fun () -> known @@ edits1 word |||
      fun () -> known_edits2 word ||| fun () -> [word]
  in
  let pairs =
    List.map (fun word -> word, find_elem_one word nwords) candidates
  in
  let max pairs =
    List.fold_left (fun ((max, best_cand) as curr) cand ->
      let rank = find_elem_one cand nwords in
      if rank > max then (rank, cand) else curr)
      (0, List.hd candidates)
      candidates
  in
  snd @@ max pairs

let r_sep = Str.regexp_string " "

let spelltest tests ?(bias = 0) ?(verbose = false) =
  let process_tokenized_tests filter tokenized_tests =
    List.iter
      (fun (target, wrongs) -> List.iter (filter target) wrongs)
      tokenized_tests
  in
  let tokenize str = Str.split r_sep str in
  let n = ref 0 in
  let bad = ref 0 in
  let unknown = ref 0 in
  let tokenized_tests = List.map (fun (a, b) -> a, tokenize b) tests in
  let update_bias (a, b) =
    WordDict.replace nwords a (find_elem_one a nwords + bias)
  in
  let start = Sys.time() in
  if bias <> 0 then List.iter update_bias tests;
  let filter target wrong =
    let w = correct wrong in
    incr n;
    if w <> target then begin
      incr bad;
      unknown := !unknown + if WordDict.mem nwords target then 0 else 1;
    end
  in
  process_tokenized_tests filter tokenized_tests;
  (!bad, !n, bias,
    int_of_float(100. -. 100. *. float_of_int(!bad)/. float_of_int(!n)),
    !unknown,
    Sys.time() -. start,
    !work_time,
    !work_count)

let tests1 = [( "access", "acess"); ( "accessing", "accesing"); ( "accommodation",
"accomodation acommodation acomodation"); ( "account", "acount"); ( "address",
"adress adres"); ( "addressable", "addresable"); ( "arranged", "aranged arrainged"); (
"arrangeing", "aranging"); ( "arrangement", "arragment"); ( "articles", "articals"); (
"aunt", "annt anut arnt"); ( "auxiliary", "auxillary"); ( "available", "avaible"); (
"awful", "awfall afful"); ( "basically", "basicaly"); ( "beginning", "begining"); (
"benefit", "benifit"); ( "benefits", "benifits"); ( "between", "beetween"); ( "bicycle",
"bicycal bycicle bycycle"); ( "biscuits",
"biscits biscutes biscuts bisquits buiscits buiscuts"); ( "built", "biult"); (
"cake", "cak"); ( "career", "carrer"); ("cemetery", "cemetary semetary"); ( "centrally", "centraly"); ( "certain", "cirtain"); (
"challenges", "chalenges chalenges"); ( "chapter", "chaper chaphter chaptur"); (
"choice", "choise"); ( "choosing", "chosing"); ( "clerical", "clearical"); (
"committee", "comittee"); ( "compare", "compair"); ( "completely", "completly"); (
"consider", "concider"); ( "considerable", "conciderable"); ( "contented",
"contenpted contende contended contentid"); ( "curtains",
"cartains certans courtens cuaritains curtans curtians curtions"); ( "decide", "descide"); ( "decided",
"descided"); ( "definitely", "definately difinately"); ( "definition", "defenition"); (
"definitions", "defenitions"); ( "description", "discription"); ( "desiccate",
"desicate dessicate dessiccate"); ( "diagrammatically", "diagrammaticaally"); (
"different", "diffrent"); ( "driven", "dirven"); ( "ecstasy", "exstacy ecstacy"); (
"embarrass", "embaras embarass"); ( "establishing", "astablishing establising"); (
"experience", "experance experiance"); ( "experiences", "experances"); ( "extended",
"extented"); ( "extremely", "extreamly"); ( "fails", "failes"); ( "families", "familes"); (
"february", "febuary"); ( "further", "futher"); ( "gallery", "galery gallary gallerry gallrey"); (
"hierarchal", "hierachial"); ( "hierarchy", "hierchy"); ( "inconvenient",
"inconvienient inconvient inconvinient"); ( "independent", "independant independant"); (
"initial", "intial"); ( "initials", "inetials inistals initails initals intials"); (
"juice", "guic juce jucie juise juse"); ( "latest", "lates latets latiest latist"); (
"laugh", "lagh lauf laught lugh"); ( "level", "leval"); (
"levels", "levals"); ( "liaison", "liaision liason"); ( "lieu", "liew"); ( "literature",
"litriture"); ( "loans", "lones"); ( "locally", "localy"); ( "magnificent",
"magnificnet magificent magnifcent magnifecent magnifiscant magnifisent magnificant"); (
"management", "managment"); ( "meant", "ment"); ( "minuscule", "miniscule"); (
"minutes", "muinets"); ( "monitoring", "monitering"); ( "necessary",
"neccesary necesary neccesary necassary necassery neccasary"); ( "occurrence",
"occurence occurence"); ( "often", "ofen offen offten ofton"); ( "opposite",
"opisite oppasite oppesite oppisit oppisite opposit oppossite oppossitte"); ( "parallel",
"paralel paralell parrallel parralell parrallell"); ( "particular", "particulaur"); (
"perhaps", "perhapse"); ( "personnel", "personnell"); ( "planned", "planed"); ( "poem",
"poame"); ( "poems", "poims pomes"); ( "poetry", "poartry poertry poetre poety powetry"); (
"position", "possition"); ( "possible", "possable"); ( "pretend",
"pertend protend prtend pritend"); ( "problem", "problam proble promblem proplen"); (
"pronunciation", "pronounciation"); ( "purple", "perple perpul poarple"); (
"questionnaire", "questionaire"); ( "really", "realy relley relly"); ( "receipt",
"receit receite reciet recipt"); ( "receive", "recieve"); ( "refreshment",
"reafreshment refreshmant refresment refressmunt"); ( "remember", "rember remeber rememmer rermember"); (
"remind", "remine remined"); ( "scarcely", "scarcly scarecly scarely scarsely"); (
"scissors", "scisors sissors"); ( "separate", "seperate"); (
"singular", "singulaur"); ( "someone", "somone"); ( "sources", "sorces"); ( "southern",
"southen"); ( "special", "speaical specail specal speical"); ( "splendid",
"spledid splended splened splended"); ( "standardizing", "stanerdizing"); ( "stomach",
"stomac stomache stomec stumache"); ( "supersede", "supercede superceed"); ( "there", "ther"); (
"totally", "totaly"); ( "transferred", "transfred"); ( "transportability",
"transportibility"); ( "triangular", "triangulaur"); ( "understand", "undersand undistand"); (
"unexpected", "unexpcted unexpeted unexspected"); ( "unfortunately",
"unfortunatly"); ( "unique", "uneque"); ( "useful", "usefull"); ( "valuable", "valubale valuble"); (
"variable", "varable"); ( "variant", "vairiant"); ( "various", "vairious"); (
"visited", "fisited viseted vistid vistied"); ( "visitors", "vistors"); (
"voluntary", "volantry"); ( "voting", "voteing"); ( "wanted", "wantid wonted"); (
"whether", "wether"); ( "wrote", "rote wote")]

let tests2 = [("forbidden", "forbiden"); ( "decisions", "deciscions descisions"); (
"supposedly", "supposidly"); ( "embellishing", "embelishing"); ( "technique",
"tecnique"); ( "permanently", "perminantly"); ( "confirmation", "confermation"); (
"appointment", "appoitment"); ( "progression", "progresion"); ( "accompanying",
"acompaning"); ( "applicable", "aplicable"); ( "regained", "regined"); ( "guidelines",
"guidlines"); ( "surrounding", "serounding"); ( "titles", "tittles"); ( "unavailable",
"unavailble"); ( "advantageous", "advantageos"); ( "brief", "brif"); ( "appeal",
"apeal"); ( "consisting", "consisiting"); ( "clerk", "cleark clerck"); ( "component",
"componant"); ( "favourable", "faverable"); ( "separation", "seperation"); ( "search",
"serch"); ( "receive", "recieve"); ( "employees", "emploies"); ( "prior", "piror"); (
"resulting", "reulting"); ( "suggestion", "sugestion"); ( "opinion", "oppinion"); (
"cancellation", "cancelation"); ( "criticism", "citisum"); ( "useful", "usful"); (
"humour", "humor"); ( "anomalies", "anomolies"); ( "would", "whould"); ( "doubt",
"doupt"); ( "examination", "eximination"); ( "therefore", "therefoe"); ( "recommend",
"recomend"); ( "separated", "seperated"); ( "successful", "sucssuful succesful"); (
"apparent", "apparant"); ( "occurred", "occureed"); ( "particular", "paerticulaur"); (
"pivoting", "pivting"); ( "announcing", "anouncing"); ( "challenge", "chalange"); (
"arrangements", "araingements"); ( "proportions", "proprtions"); ( "organized",
"oranised"); ( "accept", "acept"); ( "dependence", "dependance"); ( "unequalled",
"unequaled"); ( "numbers", "numbuers"); ( "sense", "sence"); ( "conversely",
"conversly"); ( "provide", "provid"); ( "arrangement", "arrangment"); (
"responsibilities", "responsiblities"); ( "fourth", "forth"); ( "ordinary",
"ordenary"); ( "description", "desription descvription desacription"); (
"inconceivable", "inconcievable"); ( "data", "dsata"); ( "register", "rgister"); (
"supervision", "supervison"); ( "encompassing", "encompasing"); ( "negligible",
"negligable"); ( "allow", "alow"); ( "operations", "operatins"); ( "executed",
"executted"); ( "interpretation", "interpritation"); ( "hierarchy", "heiarky"); (
"indeed", "indead"); ( "years", "yesars"); ( "through", "throut"); ( "committee",
"committe"); ( "inquiries", "equiries"); ( "before", "befor"); ( "continued",
"contuned"); ( "permanent", "perminant"); ( "choose", "chose"); ( "virtually",
"vertually"); ( "correspondence", "correspondance"); ( "eventually", "eventully"); (
"lonely", "lonley"); ( "profession", "preffeson"); ( "they", "thay"); ( "now", "noe"); (
"desperately", "despratly"); ( "university", "unversity"); ( "adjournment",
"adjurnment"); ( "possibilities", "possablities"); ( "stopped", "stoped"); ( "mean",
"meen"); ( "weighted", "wagted"); ( "adequately", "adequattly"); ( "shown", "hown"); (
"matrix", "matriiix"); ( "profit", "proffit"); ( "encourage", "encorage"); ( "collate",
"colate"); ( "disaggregate", "disaggreagte disaggreaget"); ( "receiving",
"recieving reciving"); ( "proviso", "provisoe"); ( "umbrella", "umberalla"); ( "approached",
"aproached"); ( "pleasant", "plesent"); ( "difficulty", "dificulty"); ( "appointments",
"apointments"); ( "base", "basse"); ( "conditioning", "conditining"); ( "earliest",
"earlyest"); ( "beginning", "begining"); ( "universally", "universaly"); (
"unresolved", "unresloved"); ( "length", "lengh"); ( "exponentially",
"exponentualy"); ( "utilized", "utalised"); ( "set", "et"); ( "surveys", "servays"); (
"families", "familys"); ( "system", "sysem"); ( "approximately", "aproximatly"); (
"their", "ther"); ( "scheme", "scheem"); ( "speaking", "speeking"); ( "repetitive",
"repetative"); ( "inefficient", "ineffiect"); ( "geneva", "geniva"); ( "exactly",
"exsactly"); ( "immediate", "imediate"); ( "appreciation", "apreciation"); ( "luckily",
"luckeley"); ( "eliminated", "elimiated"); ( "believe", "belive"); ( "appreciated",
"apreciated"); ( "readjusted", "reajusted"); ( "were", "wer where"); ( "feeling",
"fealing"); ( "and", "anf"); ( "false", "faulse"); ( "seen", "seeen"); ( "interrogating",
"interogationg"); ( "academically", "academicly"); ( "relatively", "relativly relitivly"); (
"traditionally", "traditionaly"); ( "studying", "studing"); (
"majority", "majorty"); ( "build", "biuld"); ( "aggravating", "agravating"); (
"transactions", "trasactions"); ( "arguing", "aurguing"); ( "sheets", "sheertes"); (
"successive", "sucsesive sucessive"); ( "segment", "segemnt"); ( "especially",
"especaily"); ( "later", "latter"); ( "senior", "sienior"); ( "dragged", "draged"); (
"atmosphere", "atmospher"); ( "drastically", "drasticaly"); ( "particularly",
"particulary"); ( "visitor", "vistor"); ( "session", "sesion"); ( "continually",
"contually"); ( "availability", "avaiblity"); ( "busy", "buisy"); ( "parameters",
"perametres"); ( "surroundings", "suroundings seroundings"); ( "employed",
"emploied"); ( "adequate", "adiquate"); ( "handle", "handel"); ( "means", "meens"); (
"familiar", "familer"); ( "between", "beeteen"); ( "overall", "overal"); ( "timing",
"timeing"); ( "committees", "comittees commitees"); ( "queries", "quies"); (
"econometric", "economtric"); ( "erroneous", "errounous"); ( "decides", "descides"); (
"reference", "refereence refference"); ( "intelligence", "inteligence"); (
"edition", "ediion ediition"); ( "are", "arte"); ( "apologies", "appologies"); (
"thermawear", "thermawere thermawhere"); ( "techniques", "tecniques"); (
"voluntary", "volantary"); ( "subsequent", "subsequant subsiquent"); ( "currently",
"curruntly"); ( "forecast", "forcast"); ( "weapons", "wepons"); ( "routine", "rouint"); (
"neither", "niether"); ( "approach", "aproach"); ( "available", "availble"); (
"recently", "reciently"); ( "ability", "ablity"); ( "nature", "natior"); (
"commercial", "comersial"); ( "agencies", "agences"); ( "however", "howeverr"); (
"suggested", "sugested"); ( "career", "carear"); ( "many", "mony"); ( "annual",
"anual"); ( "according", "acording"); ( "receives", "recives recieves"); (
"interesting", "intresting"); ( "expense", "expence"); ( "relevant",
"relavent relevaant"); ( "table", "tasble"); ( "throughout", "throuout"); ( "conference",
"conferance"); ( "sensible", "sensable"); ( "described", "discribed describd"); (
"union", "unioun"); ( "interest", "intrest"); ( "flexible", "flexable"); ( "refered",
"reffered"); ( "controlled", "controled"); ( "sufficient", "suficient"); (
"dissension", "desention"); ( "adaptable", "adabtable"); ( "representative",
"representitive"); ( "irrelevant", "irrelavent"); ( "unnecessarily", "unessasarily"); (
"applied", "upplied"); ( "apologised", "appologised"); ( "these", "thees thess"); (
"choices", "choises"); ( "will", "wil"); ( "procedure", "proceduer"); ( "shortened",
"shortend"); ( "manually", "manualy"); ( "disappointing", "dissapoiting"); (
"excessively", "exessively"); ( "comments", "coments"); ( "containing", "containg"); (
"develop", "develope"); ( "credit", "creadit"); ( "government", "goverment"); (
"acquaintances", "aquantences"); ( "orientated", "orentated"); ( "widely", "widly"); (
"advise", "advice"); ( "difficult", "dificult"); ( "investigated", "investegated"); (
"bonus", "bonas"); ( "conceived", "concieved"); ( "nationally", "nationaly"); (
"compared", "comppared compased"); ( "moving", "moveing"); ( "necessity",
"nessesity"); ( "opportunity", "oppertunity oppotunity opperttunity"); ( "thoughts",
"thorts"); ( "equalled", "equaled"); ( "variety", "variatry"); ( "analysis",
"analiss analsis analisis"); ( "patterns", "pattarns"); ( "qualities", "quaties"); ( "easily",
"easyly"); ( "organization", "oranisation oragnisation"); ( "the", "thw hte thi"); (
"corporate", "corparate"); ( "composed", "compossed"); ( "enormously", "enomosly"); (
"financially", "financialy"); ( "functionally", "functionaly"); ( "discipline",
"disiplin"); ( "announcement", "anouncement"); ( "progresses", "progressess"); (
"except", "excxept"); ( "recommending", "recomending"); ( "mathematically",
"mathematicaly"); ( "source", "sorce"); ( "combine", "comibine"); ( "input", "inut"); (
"careers", "currers carrers"); ( "resolved", "resoved"); ( "demands", "diemands"); (
"unequivocally", "unequivocaly"); ( "suffering", "suufering"); ( "immediately",
"imidatly imediatly"); ( "accepted", "acepted"); ( "projects", "projeccts"); (
"necessary", "necasery nessasary nessisary neccassary"); ( "journalism",
"journaism"); ( "unnecessary", "unessessay"); ( "night", "nite"); ( "output",
"oputput"); ( "security", "seurity"); ( "essential", "esential"); ( "beneficial",
"benificial benficial"); ( "explaining", "explaning"); ( "supplementary",
"suplementary"); ( "questionnaire", "questionare"); ( "employment", "empolyment"); (
"proceeding", "proceding"); ( "decision", "descisions descision"); ( "per", "pere"); (
"discretion", "discresion"); ( "reaching", "reching"); ( "analysed", "analised"); (
"expansion", "expanion"); ( "although", "athough"); ( "subtract", "subtrcat"); (
"analysing", "aalysing"); ( "comparison", "comparrison"); ( "months", "monthes"); (
"hierarchal", "hierachial"); ( "misleading", "missleading"); ( "commit", "comit"); (
"auguments", "aurgument"); ( "within", "withing"); ( "obtaining", "optaning"); (
"accounts", "acounts"); ( "primarily", "pimarily"); ( "operator", "opertor"); (
"accumulated", "acumulated"); ( "extremely", "extreemly"); ( "there", "thear"); (
"summarys", "sumarys"); ( "analyse", "analiss"); ( "understandable",
"understadable"); ( "safeguard", "safegaurd"); ( "consist", "consisit"); (
"declarations", "declaratrions"); ( "minutes", "muinutes muiuets"); ( "associated",
"assosiated"); ( "accessibility", "accessability"); ( "examine", "examin"); (
"surveying", "servaying"); ( "politics", "polatics"); ( "annoying", "anoying"); (
"again", "agiin"); ( "assessing", "accesing"); ( "ideally", "idealy"); ( "scrutinized",
"scrutiniesed"); ( "simular", "similar"); ( "personnel", "personel"); ( "whereas",
"wheras"); ( "when", "whn"); ( "geographically", "goegraphicaly"); ( "gaining",
"ganing"); ( "requested", "rquested"); ( "separate", "seporate"); ( "students",
"studens"); ( "prepared", "prepaired"); ( "generated", "generataed"); ( "graphically",
"graphicaly"); ( "suited", "suted"); ( "variable", "varible vaiable"); ( "building",
"biulding"); ( "required", "reequired"); ( "necessitates", "nessisitates"); (
"together", "togehter"); ( "profits", "proffits")];;

  let bad, n, bias, pct, unknown, secs, work_secs, work_count =
    spelltest tests1 ~bias:0 ~verbose:false in
  Printf.printf
    "{'bad': %d, 'bias': %d, 'unknown': %d, 'secs': %f, 'pct': %d, 'n': %d, 'work_secs': %f, 'workcount': %d}"
    bad bias unknown secs pct n work_secs work_count
