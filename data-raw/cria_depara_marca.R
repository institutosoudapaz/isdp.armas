# Esse script pega um código legado enviado pelo ISDP e transforma as regras
# em uma planilha Excel de-para de marca.

texto <- 'reclassifica_marca <- function(coluna) {
  dplyr::case_when(
    coluna %in% c(
      "ARTESANAL","CASEIRA","CASEIRA-", "Artesanal", "caseira", "caseiro", "FABRICAÇÃO CASEIRA",
      "artesenal"
    )  ~ "artesanal",
    coluna %in% c(
      "AKDAL ARMS", "akdal", "akdal arms", "Akdal Arms"
    ) ~ "akdal",
    coluna %in% c(
      "ANCIENS ETABLISSEMENTS PIEPER", "h. pieper (origem belga)", "henri pieper"
    ) ~ "anciens etablissements pieper",
    coluna %in% c(
      "AREX", "Arex", "arex"
    ) ~ "arex",
    coluna %in% c(
      "ARMI JAGER", "Armi Jager", "armi jager"
    ) ~ "armi jager",
    coluna %in% c(
      "BEEMAN", "Beeman", "beeman",
      "beeman (fabricado na china)", "beeman (fabricado nos eua)",
      "beeman (fabricada na china)","beeman"
    ) ~ "beeman",
    coluna %in% c(
      "BERETTA", "beretta", "Beretta", "beretta (fabricada no brasil sob concessao)",
      "beretta (fabricada no brasil)", "BERETA", "bereta", "beretta (origem italiana)",
      "beretta (produzida no brasil)", "beretta", "beretta (fabricado no brasil)","BARETTA",
      "BERETA 2T","BERETA 765","BERETA 950","BERETA BER","BERETA BR","BERETA GAR","BERETA MOD",
      "BERETA THU","BERETA95C","BERETA950B","BERETTA",""BERETTA","BERETTA -","BERETTA 40",
      "BERETTA 95","BERETTA CA","BERETTA FN","BERETTA IT","BERETTA MC","BERETTA635","BERETTA765",
      "BERETTA-92","BERETTA-95","BERETTA950","BERETTTA","BERRETA"
    ) ~ "beretta",
    coluna %in% c(
      "BERSA", "Bersa", "bersa", "bersa (industria argentina)", "bersa (origem argentina)",
      "bersa s. a. (origem argentina)","bersa", "BERSA S/A", "BERSA SA","BERSA -","BERSA .S.A",
      "BERSA 380","BERSA MD85","BERSA S A","BERSA S.A","BERSA S.A.","BERSA S/A","BERSA SA",
      "BERSA THUN","BERSA-ARG.","thunder45"
    ) ~ "bersa",
    coluna %in% c(
      "boito (origem brasileira)", "boito", "BOITO", "boito", "Boito", "Boito Pump","BOITO",
      "BOITO -","BOITO - MA","BOITO B300","BOITO BRAS","BOITO BRAZ","BOITO/CARTURCHO","BOITO MIURA","boite","boita","BOITO MADE","BOITO MOD","BOITO PUMP","BOITTO","BOITU","BOIUTO"
    ) ~ "boito",
    coluna %in% c(
      "browning", "BROWNING ARMS COMPANY", "Browning", "BROWNING","BRONWINGS","BRONWNING",
      "BROWENING","BROWHING","BROWING","BROWINGS","BROWINIG","BROWN NG","BROWNI","BROWNI BGS",
      "BROWNIE","BROWNIG","BROWNIMG","BROWNIN","BROWNING","BROWNING F","BROWNING M","BROWNING´S",
      "BROWNINGS","BROWNING"S","BROWNINO","BROWNIXG","BROWNNING","BROWNY","BROYNING"
    ) ~ "browning arms",
    coluna %in% c(
      "BUL TRANSMARK", "Bul Transmark", "bultransmark", "bul (origem israelense)",
      "bul transmark (origem israelense)", "bul","BUL","BUL - CHER","BUL ARMORY","BUL CHERO",
      "BUL CHEROK","BUL GCHER","BUL TRANSM","BUL-CHEROK","BULCHEROKE","BULDOGUE","BULGHEROKE",
      "BULL","CHEROKEE","G-CHEROKEE"
    ) ~ "bul transmark",
    coluna %in% c(
      "canik (origem turca)", "canik", "canik (origem turca)	", "CANIK", "Canik", "canik",
      "CANIK","KANIK","CAMIK","CANIC","CANIK","CANIK -","CANIK /380" ,"CANIK 55","CANIK L120",
      "CANIK TP9","CANK"
    ) ~ "canik",
    coluna %in% c(
      "CARAMURU", "Caramuru", "caramuru", "caramuru (origem brasileira)", "caramuru","CAMURU","CARAMURO","FAM"
    ) ~ "caramuru",
    coluna %in% c(
      "CBC", "CBC ", "cbc (origem brasileira)", "cbc (origem norte americana)",
      "cbc", "CBC", "companhia brasileira de cartuchos - cbc (origem brasileira)",
      "companhia brasileira de cartuchos (origem brasileira)", "APAREN CBC", "CBC.40 W&W",
      "CBC -151", "VELOX- CBC", "CBC ROSSI", "CBC/WINCHE", "CBC 651", "CBC 151", "CBC PUMP",
      "APAREN CBC","C B C	18","C BC	1","CBC -151","CBC 151","CBC 651","CBC PUMP","CBC ROSSI",
      "CBC.40 W&W","CBC/WINCHE","COMPANHIA","C.B.C	13","C.B.C.	29","C.B.C. SP", "cbc", "C B C", "Cbc",
      "Cbc", "cbc m", "CBC/651"
    ) ~ "CBC",
    coluna %in% c(
      "COLT", "colt", "Colt", "COLTS", "colt caval", "Colti", "COLTPOLICE", "COLT"S",
      "colt (origem norte-americana)", "colt (origem norte americana)", "colt","COLT",
      "COLT CAVAL","COLT"S","COLTI","COLTPOLICE","COLTS","COLT","COLT  AUTO","COLT .223","COLT 38",
      "COLT 45","COLT AGENT","COLT AR","COLT AR 15","COLT AR15","COLT AUTOM","COLT CAVAL",
      "COLT DEFEN","COLT DETEC","COLT H G","COLT H.BAR","COLT ITT","COLT M04","COLT M16",
      "COLT M1991","COLT M4","COLT -M4","COLT MK","COLT MK IV","COLT MKIV","COLT MODM4",
      "COLT POLIC","COLT PYTHO","COLT USA","COLT\"S", "MFG","COLT/AR15","COLT/USA","COLT´S",
      "COLT-DETCT","COLTE","COLTE 357","COLTE ALTO","COLTI","COLTIS","COLTPOLICE","COLTS","COLT"S",
      "COLT"S PT","COLTS PT F"
    ) ~ "colt",
    coluna %in% c(
      "CROSMAN STINGER", "Crosman Stinger", "crosman stinger", "crosman",
      "crosman air guns (origem norte americana)"
    ) ~ "crosman air guns",
    coluna %in% c(
      "CZ", "cz", "Cz","CZ 52", "CZ 83", "CZ 75", "CZ  P-10 C", "CZEEH SMAL", "cz",
      "cz - republica tcheca", "cz (ceska zbrojovka) - origem republica tcheca",
      "cz (czech republic)", "cz (origem republica checa)", "cz (origem tcheca)",
      "cz (origem republica tcheca)", "cz ceska zbrojovka - praha (origem checoslovaquia)",
      "czech republic","CESKA","CESKA ZBRO","CESKAZ","CZ  P-10 C","CZ 52","CZ 75" ,"CZ 83",
      "ZBROJOVIKA","CESKA BROJ","CESKÁ ZBRO","CESKAZ","CESKAZBROJ","CESKAZORO"
    ) ~ "cz",
    coluna %in% c(
      "DOBERMAN", "Doberman", "doberman (origem argentina)", "doberman","DOBERMAM"
    )~ "doberman",
    coluna %in% c(
      "ERMA WERKE", "erma werke", "Erma Werke"
    ) ~ "erma werke",
    coluna %in% c(
      "FIXXAR", "Fixxar", "fixxar (importadora)", "fixxar"
    ) ~ "fixxar",
    coluna %in% c(
      "FN HERSTAL","FN", "fn - fabrique nationale d"armes de guerre herstal (origem belga)",
      "fn - herstal (origem belga)", "fn herstal (origem belga)",
      "fn  (fabrique nationale) -origem belga", "fn (origem belga)", "fn herstal", "fn",
      "FN BELGIUM","FN HERSTAI","FN HERSTAL","FN SCAL","FN SCAR","FN SCAR H","fn scar l",
      "FN SCAR-H","FN SCARH","FN-SCAR-L","FNH","FNSCARH762","SCAR 762","SCAR H","SCAR-H"
    ) ~ "fn herstal",
    coluna %in% c(
      "gamo (origem brasileira)", "gamo (origem espanha)", "GAMO", "Gamo", "gamo",
      "gamo (origem espanhola)","gamo"
    ) ~ "gamo",
    coluna %in% c(
      "girsan", "GIRSAN", "Girsan", "girsan (origem turca)"
    ) ~ "girsan",
    coluna %in% c(
      "glock (fabricada na austria)", "glock (fabricada nos eua)", "Glock .40", "Glock",
      "glock (origem austria)", "glock (origem austriaca)", "GLOCK (AUSTRIA)",
      "GLOCK (EUA)", "glock (origem austriaca, fabricada nos eua)", "GLOCK ",
      "glock (origem estrangeira)", "glock (origem norte americana)",
      "glock", "GLOCK AUSTRIA ", " GLOCK", "GLOCK", "GLOCK G19", "GLOCK-G22", ".40 GLOCK",
      "GLOCK GES.", "GLOCK G22", "GLOCK .40", "Glock G 22", "GLOCK G 25", "Glock G22",
      "GLOCK 22", "GLOCK G44", "GLOCK -", "GLOCK22GEN", "GLOCK GEN", "GLOCK-G22",
      ".40 GLOCK", "GLOCK 19X", "GLOCK AUST", "Glock -G22", "GLOCK G 22", "GLOCK G-25",
      "GLOCK 40", "GLOCK G21",".40 GLOCK","CLOCK","GLCOK","GLOC","GLOC G22","GLOCK","GLOCK -",
      "Glock -G22","GLOCK .40","GLOCK 19X","GLOCK 22","GLOCK 40","GLOCK AUST","GLOCK G 22",
      "GLOCK G 25","GLOCK G-25","GLOCK G21","GLOCK G25","GLOCK G44","GLOCK GEN","GLOCK GES.",
      "GLOCK-G22","GLOCK22GEN","GLOK G19","GLOOCK","GLOOK", "grock", "GLOK"
    ) ~ "glock",
    coluna %in% c(
      "HK - HECKLER & KOCH", "HECKLER & KOCH", "HK", "H&K","heckler & koch (origem alema)",
      "hk (heckler & koch gmbh licenciada pela hk)", "HK - HECKLER & KOCH ", "HK- HECKLER & KOCH",
      "HK-HECKLER & KOCH", "HK -HECKLER & KOCH", "HK - HECKLER & KOCH","HECKLER&KO","HK"
    ) ~ "hk (heckler & koch)",
    coluna %in% c(
      "HS PRODUCT", "HS PRODUKT", "HS-PRODUKT"
    ) ~ "hs produkt",
    coluna %in% c(
      "fabrica de itajuba (origem brasileira)", "fabrica de itajuba", "IMBEL", "Imbel", "imbel",
      "imbel (fabrica de itajuba - brasil)", "imbel (origem brasileira)", "imbel","IMBEL MD1",
      "IMBEL IA2", "IMBEL-IA2", "IMBELMD6AD", "IMBEL IA-2",".380 MD1A2","EMBEL","IMBEL",
      "IMBEL IA-2","IMBEL IA2","IMBEL MD1","IMBEL-IA2","IMBELMD6AD","INBEL","ITAJUBA","ITJUBABRAS"
    ) ~ "imbel",
    coluna %in% c(
      "IMI", "I.M.I", "i.m.i", "imi (israel military industries)", "imi (origem israelense)",
      "IMI","IWI","IWI ARAD","JERICHO"
    ) ~ "imi",
    coluna %in% c(
      "I.N.A.", "INA", "ina", "i.n.a", "i. n. a. (origem brasileira)", "i.n.a.", "ina",
      "i.n.a. (fabricacao brasileira)", "i.n.a. (origem brasileira)", "I.N.A",
      "industria nacional de armas - ina (origem brasileira)","INA TIGRE",
      "industria nacional de armas - ina","I.N.A","I.N.A.","I.S.A","INA"
    ) ~ "ina",
    coluna %in% c(
      "LLAMA", "Llama", "llama", "gabilondo & cia (origem espanhola)", "gabilondo & cia",
      "gabilondo y cia (espanha)", "gabilondo y cia (origem espanhola)",
      "llama gabilondo y cia (origem espanhola)","llama, gabilondo y cia (origem espanhola)"
    ) ~ "llama-gabilondo",
    coluna %in% c(
      "MOSSBERG", "mosserberg", "Mosserberg", "mossberg (origem norte-americana)", "mossberg",
      "maverick - mosserberg", "maverick mossberg", "mossberg (origem norte americana)",
      "mossberg (origem norte americana)", "mossberg (origem norte-americana)"
    ) ~ "mossberg",
    coluna %in% c(
      "KRICO", "Krico"
    ) ~ "krico",
    coluna %in% c(
      "KWC", "Kwc", "kwc", "kwc - origem taiwan", "kwc (fabricada em  taiwan)",
      "kwc (fabricada em taiwan)", "kwc (kien well corporation) - fabricado em  taiwan",
      "kwc (origem taiwan)", "kwc (kien well corporation) - fabricado em taiwan", "kwc",
      "kwc fabricado em taiwan", "kwc (fabricado em taiwan)"
    ) ~ "kwc",
    coluna %in% c(
      "REMINGTON", "Remington", "remington (origem norte americana)",
      "remington (origem norte-americana)", "remington"
    ) ~ "remington",
    coluna %in% c(
      "amadeo rossi & cia (origem brasileira)", "amadeo rossi (origem brasileira)",
      "amadeo rossi s. a. (origem brasileira)", "amadeo rossi s.a.", "rossi", "ROSSI",
      "rossi (fabricada na china)", "rossi (origem braileira)", "rossi (origem brasileira)",
      "rossi (origem brasileira", "rossi, modificada artesanalmente", "rossi",
      "rossi (origem brasileira)", "rossi (origem brasileira)", "ROSSI ESPE", "A. ROSSI",
      "amadeo rossi s.a. (origem brasileira)", "Rossi", "AMADEROSSI", "ROSSI&CIA",
      "Amad Rossi", "ROSSI 38", "AMAD ROSSI", "rossi  .22", "CBC ROSSI", "ROSSI 4\"",
      "rossi (fabricado na china)", "rossi 32", "rossi 28", "rossi 22", "ROSSI PUMA","A. ROSSI",
      "AMAD ROSSI","AMADEO","AMADEO ROS","AMADEOROSS","AMADEROSSI","RO0SSI","ROSSI","rossi  .22",
      "rossi 22","rossi 28","ROSSI 32","ROSSI 38","ROSSI 4","ROSSI ESPE","ROSSI PUMA","ROSSI&CIA",
      "ROSSSI", "ROSSE", "rosse", "AMADEU ROSSI", "ROSSI (AMADEO ROSSI S.A.)"
    ) ~ "rossi",
    coluna %in% c(
      "RUGER","ruger (fabricacao norte americana)", "ruger (origem norte americana)",
      "ruger (origem norte-americana)", "ruger", "Ruger"
    ) ~ "ruger",
    coluna %in% c(
      "sarsilmaz (origem turca)", "sarsilmaz", "SARSILMAZ", "sarsilmaz", "Sarsilmaz",
      "sarsilmaz (origem turquia)"
    ) ~ "sarsilmaz",
    coluna %in% c(
      "sig", "sig sauer", "SIG SAUER", "Sig sauer", "Sig Sauer", "Sig", "SIG"
    ) ~ "sig sauer",
    coluna %in% c(
      "SMITH E WESSON","smith & wesson (fabricacao norte americana)", "smith & wesson", "smith&wesson",
      "smith & wesson (origem norte americana)", "S&WESSON", "S&W", "S&W LONG",
      "smith & wesson (origem norte-americana)", "S&Wesson", "SMITH & WESSON",
      "SMITH&W...", "smith/wess", "SMITH WESO", "smith & w.", "SMITH WESO", "smith wes",
      "Smith/Wess", "smithweson", "SMITH WESN", "SMITHWESSO", "smith", "smith&wes",
      "SMITH WEST", "smith W", "smithwesso", "SMITH & W.", "SMITH&WESS", "smithwesle",
      "smith wess", "Smith W.", "SMITH & W", "Smith & We", "SMITH WES.", "SMITH W",
      "smithewess", "SmithWesso", "SMITH", "smith w", "SMITH & WE", "Smith&W.",
      "SMITH WESS", "SMITH & WESSON ","emith wer.","S & W","S & W CTG","S & WESSON","S E W",
      "S W","S. WESSON" ,"S.W,","S[.]WESSON","S&W","S&W LONG","S&WESSON","SM","SM. WESSON",
      "SMIT&WESSO","SMITH","SMITH & W","SMITH & W","SMITH & WE","SMITH W","SMITH W.","SMITH WES",
      "SMITH WES.","SMITH WESN" ,"SMITH WESO","SMITH WESS","SMITH WEST","SMITH/WESS","Smith&W.",
      "SMITH&W...","SMITH&WES","SMITH&WESS","SMITHEWESS","smithwesle","SMITHWESON","SMITHWESSO",
      "SMITWESSON","SW", "SMITH AND WESSON", "smith wesson", "smith weysson","smith wesson .357 magnum",
      "smith&wesson","smith wesson .357 magnum","SMITH & WESSON","Smith & Wesson", "SMITH &WESSON",
      "SMITH WESSON","smiter welce","SW","Smith wesson","SMITH&WESSON","smith","SMITH WELSON",
      "SMITH8 WESSON","SMITE", "SMIT WEST", "smit & wesson"

    ) ~ "smith & wesson",
    coluna %in% c("SOCA, SOCA", "soca-soca","SOCA SOCA")~ "soca soca" ,

    coluna %in% c(
      "^springfiel$", "SPRINGFIELD ARMORY", "SPRINGFIELD", "Springfield Armory", "springfield armory"
    ) ~ "springfield armory",
    coluna %in% c(
      "STANLEY", "Stanley"
    ) ~ "stanley",
    coluna %in% c(
      "STOEGER", "Stoeger"
    ) ~ "stoeger",
    coluna %in% c(
      "TANFOGLIO", "Tanfoglio", "tanfoglio", "tanfoglio (origem italiana)"
    ) ~ "tanfoglio",
    coluna %in% c(
      "forjas taurus (origem brasileira)", "forjas taurus s. a. (origem brasileira)",
      "forjas taurus s.a. - brasil", "forjas taurus s.a. (origem brasileira)",
      "forjas taurus s.a.", "forjas taurus", "taurus - brasil", "taurus - famae",
      "taurus (fabricacao brasileira)", "taurus (miami, fl. - usa)", "TAURUS 380",
      "taurus (origem brasil)", "taurus (origem brasileira)", "taurus", "TAURUS TS9",
      "taurus (origem norte americana)", "taurus", "TAURUS", "Taurus", "Taurus .32",
      "TAURUS .38", "TAURUS G22", "TAURUS G2C", "TAURUS 765", "TAURUSPT58", "TAURUS PT1",
      "taurus 765", "TAURUS PT", "taurus g2c", "taurus ts9", "FOR TAURUS", "TAURUS 838",
      "TAURUS,G2C", "Taurus 9mm", "838 TAURUS", "TAURUS TH9", "F. TAURUS", "TAURUS G3",
      "TAURUS-938", "TAURUS T4", "TAURUS 38", "TAURUS 32", "TAURUS40","838 TAURUS","F. TAURUS",
      "F.T.","FOR TAURUS","FORJAS","PUMA","TARURUS" ,"TATURUS","TAURIS","TAUROS","TAURP","TAURS",
      "TAURUS","TAURUS  PT","Taurus .32","TAURUS .38","TAURUS 32","TAURUS 38","TAURUS 380",
      "TAURUS 765","TAURUS 838","Taurus 9mm","TAURUS G22","TAURUS G2C","TAURUS G3","TAURUS PT",
      "TAURUS PT1","TAURUS T4","TAURUS TH9","TAURUS TS9","TAURUS-938","TAURUS,G2C","TAURUS40",
      "TAURUSPT58","TUARUS","TURUS", "TAU", "tauros", "TAURuS", "taurus/pt58 hc", "TAURUS ARMAS S.A.",
      "taurus oxidado", "FORJAS TAUROS", "TAURUS  g2c", "TAURUS PT  740", "TAURUS 6 tiros", "TAURUS 9 MM"
      , "TAUROS", "tauros", "Tauros") ~ "taurus",
    coluna %in% c(
      "tisas (marca turca)", "tisas (origem turca)", "tisas (turquia)",
      "tisas", "TISAS", "Tisas", "tisas"
    ) ~ "tisas",
    coluna %in% c(
      "URKO", "urko", "Urko","urko (origem brasileira)", "urko", "urko"
    ) ~ "urko",
    coluna %in% c(
      "WAFFEN FABRIK MAUSER", "waffen fabrik mauser", "WAFFENFABRIK MAUSER",
      "waffenfabrik mauser"
    ) ~ "waffenfabrik mauser",
    coluna %in% c(
      "WALTHER", "walther", "Walther","walther", "WALTHER PPQ",
      "walther (manurhin - origem francesa)", "walther (origem alema)"
    ) ~ "walther",
    coluna %in% c(
      "WINCHESTER", "Winchester", "winchester", "wincheste", "Wincheeste",
      "winchester (origem norte americana)", "winchester (origem norte-americana)",
      "winchester repeating arms (origem norte-americana)", "winchester"
    ) ~ "winchester",
    coluna %in% c(
      "WINGUN", "win gun", "wingun - rossi", "wingun (fabricado em taiwan importado pela rossi)",
      "wingun e inscricao no cano licenciado pela rossi"
    ) ~ "wingun",
    coluna %in% c(
      "INBRATERRESTRE", "InbraTerrestre","Inbraterrestre", "Imbraterrestre"
    ) ~"inbraterrestre",
    coluna %in% c (
      "ITAJUBA","fabrica de itajuba (origem brasileira)", "fabrica de itajuba",
      "imbel (fabrica de itajuba - brasil)", "imbel (origem brasileira)", "imbel"
    ) ~ "imbel",
    coluna %in% c(
      "J.G. ANSCHUTZ", "J G ANSCHUTZ","JG ANSCHUTZ", "j.g anschutz", "j.g. anschutz")~ "jg anschtz",
    coluna %in% c(
      "JAGUAR", "Jaguar", "Wild Jaguar", "WILD JAGUAR") ~"wild jaguar",
    coluna %in% c(
      "JERICHO","IWI JERICHO", "Jericho", "Jerico", "JERICO")~ "jericho",
    coluna %in% c(
      "KEL-TEC", "kel tec", "KEL TEC", "Kel Tec")~ "kel tec",
    coluna %in% c(
      "LAPORT", "Laport")~ "laport",
    coluna %in% c(
      "MANNLICHER", "Mannlicher", "Manlicher", "MANLICHER")~ "mannlicher",
    coluna %in% c(
      "MARLIN FIREARMS", "Marlin Firearms", "Marlin", "MARLIN","marlin (origem norte-americana)"
    ) ~"marlin",
    coluna %in% c(
      "MAUSER", "Mauser", "waffenfabrik mauser (origem alema)"
    ) ~ "mauser",
    coluna %in% c(
      "o. h. (origem espanhola)", "o.h. (orbea hermanos - origem espanhola)",
      "o.h. (orbea hermanos) - origem espanhola", "o.h. (orbea hermanos)",
      "o.h. (origem espanhola)", "o.h.", "oh (origem espanhola)",
      "oh", "ORBEA HERMANOS"
    ) ~ "o.h. (orbea hermanos)",
    coluna %in% c(
      "POWERLINE", "DAISY", "powerline", "Powerline", "Powerline", "Daisy Powerline"
    ) ~"daisy",
    coluna %in% c("PUCARA", "Pucara"
    ) ~"pucara",
    coluna %in% c(
      "QGK OUTDOOR", "qgk (origem chinesa)", "QGK", "Qgk"
    ) ~ "qgk",
    coluna %in% c(
      "RIVOLTELLA", "Rivoltella", "Rivoltela", "RIVOLTELA"
    ) ~"rivoltella",
    coluna %in% c(
      "fabricado pela rpc e importado pela cbc", "fabricado por narpo e importado por cbc",
      "fabricado por narpc e importado por cbc", "rpc (origem chinesa) importada por cbc",
      "RPC", "Rpc"
    ) ~ "rpc",
    coluna %in% c("RUBY", "Ruby"
    ) ~"ruby",
    coluna %in% c("SAIGA", "Saiga"
    ) ~ "saiga",
    coluna %in% c(
      "SPORTSMAN FIFTEEN", "Sportsman Fifteen"
    ) ~"sportsman fifteen",
    coluna %in% c(
      "STANLEY","Stanley"
    ) ~"stanley",
    coluna %in% c(
      "STURM RUGER", "Sturm Ruger"
    ) ~ "sturm ruger",
    coluna %in% c(
      "tank (origem espanhola)", "tanke (origem espanhola)", "tanke", "tanque (origem espanhola)",
      "tanque", "TANQUE", "leandro redaelli", "L.R."
    ) ~ "tanque (leandro redaelli)",
    coluna %in% c(
      "tara (fabricada em montenegro)", "tara (origem montenegrina)", "tara", "TARA"
    ) ~ "tara",
    coluna %in% c(
      "TARZAN", "Tarzan"
    ) ~"tarzan",
    coluna %in% c(
      "astra-unceta cia s.a. (origem espanhola)", "UNCETA Y COMAÑIA", "Unceta y Comañia",
      "astra unceta","astra-unceta"
    ) ~ "astra arms",
    coluna %in% c(
      "VALMET", "Valmet"
    ) ~"valmet",
    coluna %in% c(
      "VICTOR SARASQUETA EIBAR", "Victor Sarasqueta Eibar", "Victor Sarasqueta", "Eibar", "EIBAR"
    ) ~ "victor sarasqueta",
    coluna %in% "ZABALA HERMANOS" ~ "zabala hermanos",
    coluna %in% "ZIGANA" ~ "zigana",
    coluna %in% c("semmarca", "null", "n/c", "nãoconsta", "s/marca", "nãoaparen", "ignorada", "nãopossui", "desconheci", "nc", "s/m", "naoaparen") ~ "Sem informação"
  )
}'

combinacoes <- texto |>
  stringr::str_replace_all("\n", "") |>
  stringr::str_extract_all("%in% c(.+)") |>
  stringr::str_remove_all("coluna") |>
  stringr::str_split("%in%") |>
  purrr::pluck(1) |>
  stringr::str_squish() |>
  stringr::str_remove_all("^c\\(|\\(|\\)|\\{|\\}")

tibble::tibble(
  combinacao = combinacoes
) |>
  dplyr::filter(stringr::str_detect(combinacao, "~")) |>
  tidyr::separate_wider_delim(
    cols = combinacao,
    delim = "~",
    names = c("marca", "marca_formatada"),
    cols_remove = TRUE
  ) |>
  tidyr::separate_longer_delim(
    cols = marca,
    delim = stringr::regex(',[ ]?\\"')
  ) |>
  dplyr::mutate(
    marca = stringr::str_remove_all(marca, '\\"'),
    marca = stringr::str_trim(marca),
    marca_formatada = stringr::str_remove_all(marca_formatada, '\\"|,$'),
    marca_formatada = stringr::str_trim(marca_formatada)
  ) |>
  writexl::write_xlsx("inst/tabelas_depara/depara_marca.xlsx")
