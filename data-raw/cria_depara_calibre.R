library(tidyverse)

texto <- 'renomeia_calibre <- function(coluna) {
  dplyr::case_when(
    coluna %in% c("3.6", ".3.6", "3,6", ".3,6") ~ "3,6 mm",

    coluna %in% c("4", ".4") ~ "4",

    coluna %in% c("4.2MM", "4,2") ~ "4,2 mm",

    coluna %in% c("4,5 milimetros", "4,5 mm (quatro milimetros e meio)",
                  "4,5 mm (quatro virgula cinco milimetros)*",
                  "4,5 mm (quatro virgula cinco milimetros)", "4,5 mm", "4,5mm",
                  "4.5 milimetros", "	4.5 mm (quatro ponto cinco milimetros)",
                  "	4.5 mm", ".4.5MM", "4.5MM"
    ) ~ "4,5 mm",

    coluna %in% c("5,5 milimetros", "5,5 mm (cinco milimetros e meio)",
                  "5,5 mm (cinco milimetros e meio)",
                  "5,5 mm","5,5 velodog (obsoleto)", "5,5", "5,5mm", "5.5 milimetros",
                  "5.5 mm (cinco ponto cinco milimetros)",
                  "5.5 mm (cinco ponto cinco milimetros) transformado para .22 (ponto vinte e dois)",
                  "5.5 mm (cinco ponto cinco milimetros)",
                  "5.5 mm", "5.5mm (cinco ponto cinco milimetros)", "5.5mm", "5.5MM"
    ) ~ "5,5 mm",

    coluna %in% c("5,56 mm (223 NATO)","223",".223 Rem","2.23","223X5.56",".223 REM","223-556",
                  ".22 REM","223 REMI", ".223 POL","222/5.56","223-556M",".556",".556X45",
                  "5.56","5.56X45","5.65MM","556","556MM", "556X45","5,56 (5,56x45mm OTAN)",
                  "5,56","5,56MM",".556","223/556","5,56X45", "5.56X49","223-5.56","556 I A2",
                  "AR 15","556*45","5.56MM","55,6","55.6","5,56 MM","556/223","5,56 X45",
                  "5.56 MM","5.56-223","AP.556"
    ) ~ ".223REM\5,56x45mm NATO",

    coluna %in% c("5,9MM") ~"5,9 mm",

    coluna %in% c("06 milimetros", "06 mm (seis milimetros)", "6 milimetros",
                  "6 mm (seis milimetros)", "6 mm", ".6MM"
    ) ~ "6 mm",

    coluna %in% c("6.35", "6.35MM", ".6,35", "6,35mm" ,"6,35mm","6,35","25 AUTO","635 MM",
                  ".25 ACP",".6.35","6.36","3,65",".25/635",".6,35",".6,358","6.35 (22",
                  "6.35 PT","6 35","   6.35",".25 AUTO","25 ACP","6..35","6.35 .25","63,5",
                  " 6.35","- 6.35",".25ACP","25 ALTO","25.","25MM","635 BR","635MM",".25",
                  ".25 AUTO",".6",".6.35","0.25","25","6","6,35MM","6.35","6.35 MM","635"
    ) ~"6.35 x 16 mm SR (.25 ACP)",

    coluna %in% c("6,5MM") ~"6,5 mm",

    coluna %in% c("07 milimetros", "07 mm (calibre real)") ~ "7 mm",

    coluna %in% c("7.6 MM") ~ "7,6 mm",

    coluna %in% c(".7,62MM", "7.62 MM", ".762" ,"7.62","762","7,62mm","7,62","7,62MM","7,62 MM",
                  "762MM","762 MM","7.62 MM", ". 762"
    ) ~ "7.62",

    coluna %in% c("7.62 x39","762X39","762X39MM","7,62x39mm","7.62X39","7.62X39M",".7.62X39",
                  "762-39","AK47 762","7,62.39","7,62 MM KALASHNIKOV (7,62X39)",
                  "7,62 MM RUSSIAN SHORT","7,62 X 39 MM (M43)","7,62X39"
    ) ~ "7,62x39mm",

    coluna %in% c("762X51","7.62X51","7,62/308","308","7.62 X 5",".308 (.308 Winchester)",
                  ".308 WIN","308W","7,62 MM NATO","7,62X51",".308",".308 WINCHESTER (7,62X51 MM)"
    ) ~ ".308WIN\7,62x51mm NATO",

    coluna %in% c("7.65 mm (sete ponto sessenta e cinco milimetros)",
                  "7,65mm browning (.32 auto)", "7.65 mm", "7,65mm", "7.65mm",
                  ".7.65 mm (sete ponto sessenta e cinco milimetros)", "765 MM", "7.65MM",
                  "7,65 MM","7,65MM", ".765 MM", ".7,65MM","7,65mm","7,65","765MM","765/32",
                  ".32 ACP","7 65","PP 7.65","765P","76.5","765 PT57","7,6","7,65  MM","57S 765",
                  "7,65,","7:65 MM",".765","32 AUTO","7,65 MM","7,65MM","7.65","7.65MM","765",
                  "765 MM", ".32auto",".32 auto",".32 auto",".32 ACP (7,65X17 MM)",".32 AUTO",
                  ".32 AUTO EXPO","7,65 MM","7,65 MM BROWNING (7,65X17 MM)"
    ) ~ "7.65 x 17 mm SR (.32 ACP)",

    coluna %in% c("8", ".8", "8mm", "8MM", "8 mm") ~"8",

    coluna %in% c("9mm", "9 mm (nove milimetros)", "9x19 mm (nove milimetros)",
                  "9mm (nove milimetros)", "9mm luger","9 mm luger",
                  "9 mm (nove milimetros) luger", "9x19mm (9mm luger)", " 9mm (parabellum)",
                  "9mm luger (9mm parabellum 9x19mm)", "9x19 (nove milimetros)",
                  "9mm luger (9x19mm 9mm parabellum)", "9x19mm",
                  "9x19 mm (nove milimetros) luger", "9mmx19", "9 mm (nove milimetros) parabellum",
                  "9x19 mm", ".9mm", ".9 mm (nove milimetros) luger", "9 mm (calibre real)",
                  "9 mm (nove milimetros) para", "9 mm mak. (9x18 mm)",
                  "9mm (nove milimetros) luger", "9mm luger (9x19mm)", "9mm parabellum",
                  "9mm x 19", "9mmm luger", "9x19 (nove milimetros) para",
                  "9x19mm (9mm luger, 9mm parabellum)", "9x19mm (9mm parabellum", "9mm luger)",
                  "09 mm (nove milimetros)", "09 milimetros", ".9MM", "9 mm Parabellum (9x19)",
                  ",9 MMM","9.00","9ML", "9MM PARA","9.MM","0.9MM","9MLM","9 MILIME",".9 MM.",
                  "9 MILÍME","9 ML","9,0 MM", "0.9","9 M","9MMLUGER","..9","09M","24/7 9MM",
                  "9X 19MM","350(9MM)","9 M.M", "9 MILITR","9,00 MM","9.0 MM","9MM LUGE","9MM.",
                  "9MM/380",",9 MM",".09mm",".9", ".9 MM",".9M",".9MM","0,9,","09 MM","09MM","9",
                  "9 M.M.","9 MILIME","9 MM","9 MM.", "9.MM","9M","9ML","9MM","9MM CBC","9X19",
                  "9X19 MM","9X19MM","9X19MM P","CAL 9MM", "PT809","9 MM","9 MM LUGER (9X19MM)",
                  "9 MM LUGER +P+","9 MM PARABELLUM (9X19MM)"
    ) ~ "9X19MM",

    coluna %in% c("10 milimetros", "10 mm (dez milimetros)", "10 mm", "10mm  (dez milimetros)",
                  "10mm (dez milimetros)", "10mm"
    ) ~ "10",

    coluna %in% c("12 milimetros", "12 milimetros", "12 mm (doze milimetros)", "12 mm",
                  "12mm (doze milimetros)", "12mm", ".12", "12 NÃO LETAL", "12", "12 (doze)",
                  ".12", ".12", "12 ", " 12", "12"
    ) ~ "12",

    coluna %in% c(".14", "14mm", "14", "14 ", " 14"
    ) ~ "14",

    coluna %in% c("16", "16 (dezesseis)", ".16", "16"
    ) ~ "16",

    coluna %in% c(".17", "17mm", "17 mm", "17MM"
    ) ~ "17",

    coluna %in% c("20 (vinte)", "20", ".20"
    ) ~ "20",

    coluna %in% c( ".22 (ponto vinte e dois)",
                   ".22 (ponto vinte e dois)",
                   ".22 (ponto vinte e dois) 5,5 milimetros",".22",
                   ".22 HORNET", ".22 ACP", ".22 WMR", ".22", ".22" ,"0.22","22",
                   "22 CBC","22-ARMA",
                   "22","22 MAGNU", ".22 CAL","22L",".22L",".22 POL","APAR. 22",
                   "CAL. 22","CAL 22",".22 MM","CAL.22","..22","22.",
                   "22 LA",".22.","AP 22", "22 RR",
                   ".22 HORN",".22 LARG",".22 LP",
                   ".22 MAGNUM"
    ) ~ "22",
    coluna %in%  c( ".22 SHORT", ".22 short", ".22 curto", ".22 (ponto vinte e dois) short",
                    ".22 (ponto vinte e dois) short (de fogo central)",".22 SHORT","22 SHORT",".22 (ponto vinte e dois) curto") ~".22 short",

    coluna %in% c(".22lr ou 22 curto",".22 (ponto vinte e dois) short or long rifle",
    ".22 lr/.22 short",".22 (ponto vinte e dois) s., l. e l.r."
    ) ~".22 lr ou short",

    coluna %in% c(".22 LR HYPER", ".22 LR-SV",".22 (ponto vinte e dois) l.",
    ".22L.","22 L","22L.","22 LARGO",
    "22 lr", "compativel com municao .22 short",
                  ".22 (ponto vinte e dois) largo", ".22 (ponto vinte e dois) long",".22 lr",".22 (ponto vinte e dois) l.r.",
                  ".22lr",  ".22 l. r.",
                  ".22 (ponto vinte e dois) lr", ".22 lr - modificado",  ".22 lr (modificado)",
                  ".22 l.r.",". 22 lr",  ".22 l.r. ", "22lr", ".22 LR", "22 LONG", "22lr e 22short",
                  "22 LONGO","22 LR", ".22 (ponto vinte e dois) l.r.", "22LR",".22 L.R.",
                  "22 L.R","22 L.R.",".22 LR",".22LR",".22 LONG",".22 LR",".22 LR (SUBSONIC)", ". 22LR"
    ) ~ ".22 lr",

    coluna %in% c("24 (vinte e quatro)", "24", ".24", " 24", "24 ", "24"
    ) ~ "24",

    coluna %in% c(".25 (ponto vinte e cinco) auto", ".25 acp", ".25 ACP", ".25",
                  ".25 Auto (6,35 mm Browning)"
    ) ~ "25",

    coluna %in% c("28 (vinte e oito)", "28", "28 (modificado)", "28 (vinte e oito)", ".28"
    ) ~ "28",

    coluna %in% c(".30-06 Springfield (7,62x63)"
    ) ~ ".30-06 Springfield (7,62x63)",

    coluna %in% c(".30 carbine", ".30 TREINA", ".30 WCF (30-30 Winchester)", ".30 M1",
                  ".30 CARABINA"
    ) ~ ".30",

    coluna %in% c("32 - modificado", ".32 s&wl", ".32 (ponto trinta e dois)", ".32s&wl","32",
                  "32 (trinta e dois)",".32 (ponto trinta e dois) long",
                  ".32 (ponto trinta e dois) s&wl",
                  ".320 (ponto trezentos e vinte) transformada para .32 (ponto trinta e dois)",
                  ".320 (ponto trezentos e vinte) transformado para .32 (ponto trinta e dois)",
                  ".320 (ponto trezentos e vinte)", ".320", ".32 (ponto trinta e dois) s&w long",
                  ".320 transformado para .32", ".32 s&w", ".32 s&wl (modificado)", ".32",
                  ".32 long", ".320 (obsoleto)",".32 (ponto trinta e dois) curto",
                  ".32 (ponto trinta e dois) largo",".32 (ponto trinta e dois) s.&w. long",
                  ".32 s&wl - modificado",
                  ".320 (ponto trezentos e vinte) short revolver cf long case", "32s&wl",
                  ".32 (ponto trinta e dois) long ctg", ".32 curto", ".320 (calibre obsoleto)",
                  ".320 short revolver cf long case", ".32 (ponto trinta e dois) longo",
                  ".32 (ponto trinta e dois) s&w l", ".32 (ponto trinta e dois) s&wl.",
                  ".32 (ponto trinta e dois) sw", ".32 (ponto trinta e dois)",
                  ".32 auto (equivalente ao calibre 7,65mm)",".32 largo",
                  ".32 s&w - modificado", ".32 s&wl (modificado*)",
                  ".32 sl (winchester self-loading)",
                  ".320 (ponto trezentos e vinte)  transformado para .32 (ponto trinta e dois)",
                  ".320 (ponto trezentos e vinte), transformada para .32 (ponto trinta e dois)",
                  ".320 (ponto trezentos e vinte), transformado para .32 (ponto trinta e dois)",
                  ".320 transformado para .32 (ponto trinta e dois)",
                  ".32s&wl (ponto trinta e dois)", "32 (trinta e dois)", ".32", "32 S&W",
                  ".32 S&W Long", ".32 ACP", ".32 Auto (7,65 mm Browning)", ".320 Short Revolver",
                  ".320", ".32-20 Winchester", ".32 S&W", ".32 S&W Long Canto-Vivo", " 32", "32 ",
                  ".32","0,32",".320",".32 LONG",".32 Long",". 32","32S&W","32 LARGO",".32 POL",
                  ".32LARGO","32 S&W","32 MM","CAL .320","32.8.",".32 CURT","AP. 32",".32 LARG",
                  "32 APAR.","APARE.32","CAL. 32",".32 S&WL","320C","APAR..32","CAL 32","CASTEL32",
                  "-32",".032",",32 E OU",".32 ALTO",".32 S&W,", ".32 S&WL",".320","0.32","32",
                  "32 - CBC","32 CBC","32 LONG","32 LONGO","32 OU 38","32 S&W","32 S&WL","32",
                  "320","320.","32S&W","32SW","8MM","CAL. 32"
    ) ~ "32",

    coluna %in% c(".357 magnum", ".357 (ponto tres cinco sete)", ".357 mag", ".357 magnum*",
                  ".357 magnun", ".357", ".357 Magnum",".357","0,357","CAL. 357","357 MAG.",
                  "3,57","3.57 MAG","357,.38",",357 MAG","357 MGNU","357/38",".357 MAG",".357MAG",
                  "357","357 MAG","357 MAGN","387") ~ "357",

    coluna %in% c("36", "36 (trinta e seis)", ".36", "36 (trinta e seis)", ".36") ~ "36",

    coluna %in% c(".38 spl", ".38 (ponto trinta e oito) special",".38spl",
                  ".38 (ponto trinta e oito)",".38 (ponto trinta e oito) spl", ".38",
                  ".38 (ponto trinta e oito) s&w special", ".38 curto",
                  ".38 special", ".38spl (ponto trinta e oito)", ".38spl ou compativel",
                  ".38 (ponto trinta e oito) curto", ".38 (ponto trinta e oito) s.&w. special",
                  ".38 (ponto trinta e oito) s&w spl",".38 (ponto trinta e oito) s&w",
                  ".38 (ponto trinta e oito) special ", ".38 (ponto trinta e oito) w.c.f.",
                  ".38 (ponto trinta e oito)", ".38 (ponto trinta e oito)", ".38 spl",
                  ".38 swc (.38 curto)", ".38(ponto trinta e oito)", "38", "38spl",
                  ".38 Special",".38",".38 S&W", ".38 Special Canto-Vivo", ".38 Special",".38",
                  "0,38",".38 POL",". 38 SPE","38 SPC","38 (?)","38 SPEC",".38 SP","PONTO 38",
                  "38SUPER","CAL .38","-38","38","38 CURTO","38  SPC" ,"38    6T","38 2","38 ESP",
                  ".38SPECI","38 S","38 TA","38 SPEC.","3,8",".38 ESPE",".038","38 ESP.","K38",
                  ". 38 ESP","38 SP.",".38 ESP.",".38 S&W","38  ESPE","38P0","CALÇA 38","CALIB 38",
                  ".038",".38",".38 CBC",".38 ESPE.", "38 SPEC",".38 SPL",".38",".38SPECI","0.38",
                  "38","38  /","38 -","38 / 380","38 CBC","38 ESPEC","38 SPECI","38 SPL","38.",
                  "38.speci","38","38*","38SPL","CAL. 38",".38 SPECIAL (9 X 29,5 MM)",
                  ".38 SPECIAL SHORT",".38 SPL",".38 SPL + P",".38 SPL CURTO",
                  ".38 SPL SPECIAL + P",".38 SPL+P+,",".38+P+"
    ) ~ ".38",

    coluna %in% c("36", ".36") ~ "36",

    coluna %in% c(".380 acp", ".380 (ponto trezentos e oitenta) acp",
                  ".380 (ponto trezentos e oitenta)", ".380auto", ".380acp", ".380 auto",
                  ".380 (ponto trezentos e oitenta) auto", "9 browning (.380 acp)", ".380",
                  ".380 (ponto trezentos e oitenta) transformada para .38 (ponto trinta e oito)",
                  ". 380 (ponto trezentos e oitenta) auto",
                  ".380 (ponto trezentos e oitenta) transformado para .38 (ponto trinta e oito)",
                  ".380 (ponto trezentos e oitenta) trasformado para .38 (ponto trinta e oito)",
                  ".380 (ponto trezentos e oitenta)", ".380 (ponto trezentos e oitenta)",
                  ".380 acp ou 9mm browning", ".380 transformado para .38", ".380ato", ".380uto",
                  "380 (ponto trezentos e oitenta) acp", "380auto", ".380 ACP",
                  ".380 Auto (9mm Browning Short)", ".380 Treina",".380",".380 Short Revolver",
                  ".380" ,"380.","380 S","380 PT","380/9MM","380CBC","380HC PL","389CBC","PT 380-",
                  "380 58 S",".380AUTO","PT-380",".380 AUT",".380 MD1",".380 POL","380 58SS",
                  "380/9M C","380C",",380 SUP","..380","380 ALTO","380 CPT","380 MD1","380MD1",
                  "9MM CURT","9MM KURZ","9MMSHORT","AP.380",".380 APC",".381","380 C","PT. 380",
                  ".380 ACP",".380/222",".380ACP",".390","380","380 ACP","380 AUTO","380 CBC",
                  "380- CBC","380-cbc","380.","380","380ACP","380MM","380PT938","CAL,380","P380",
                  "380 ACP (9X17 MM)",".380 AUTO (9X17 MM)",".380 AUTO (9X17)"
    ) ~ "380",

    coluna %in% c(".38-40 Winchester") ~"38-40",

    coluna %in% c(".40 s&w", ".40 (ponto quarenta)", ".40s&w", ".40 (ponto quarenta) s&w",
                  "40 (quarenta)","40", ".40", ".40 (poto quarenta) s&w", "40 (quarenta)",
                  ".40 S&W",".40",".44-40 Winchester",".40 Treina", "40", ".40","PT40","P40 24/7",
                  "PT 840","SW40",".4O","40/640",". 40 POL",". 40 S&W",".40CBC",".40 PT 2","4.0",
                  "PT .040",".40","-40","0,4","C40","P CAL 40","40 E 45","CAL40",".40GCMD2",
                  "40S&W",".40 CBC","(.)40",".40PT100","P   40","PTO 40",".40 MOD.",". 40",".4",
                  ".40",".40 S&W",".40 SW",".40MM",".40S&W","0.4","0.40","4.0","40","40 S&W",
                  "CAL.  40","P.40","P40S&W","PONTO 40","PT 24/7","PT 40","PT40"
    ) ~ "40 S&W",

    coluna %in% c(".41", ".41 Magnum"
    ) ~"41",

    coluna %in% c(".44 w",".44 (ponto quarenta e quatro)", ".44 w.c.f.", ".44w",
                  ".44 (ponto quarenta e quatro) w", ".44 (ponto quarenta e quatro) s&w",
                  ".44 (ponto quarenta e quatro) s.&w.",".44 (ponto quarenta e quatro) w.c.f.",
                  ".44 (ponto quarenta e quatro) wcf", ".44 s&w russian", ".44 special",
                  ".44 w.f.c.", ".44 wcf", ".44", ".44-40", ".44w (ponto quarenta e quatro)",
                  "44", ".44 Special", ".44-40 Winchester", ".44",".44 Magnum",".44" ,".44MAGNU" ,
                  ".44" ,"44" ,"44 CBC"
    ) ~ "44",

    coluna %in% c("4.4 W","44 W","44WCF",".44-40"
    ) ~ ".44-40 Winchester",

    coluna %in% c(".45 acp", ".45 (ponto quarenta e cinco)", ".45 (ponto quarenta e cinco) acp",
                  ".45 (ponto quarenta e cinco) auto", ".45", ".450 transformado para .45",
                  ".45-70 - tambem pode ser utilizada com municao de calibre 36 e .44 w",
                  ".45auto", "45auto", ".45", ".45 ACP", ".45 Colt", ".45 ACP Match" ,".45",
                  "0.45","45","45 AUTOM","45LONGO","0,45",".45 SPEC","45 AUT.",".45 AUTO","45 MM",
                  " .45",".450","45 LC","PONTO 45"
    ) ~ "45",

    coluna %in% c(".50", "50mm" ,"12.7", "12,7", ".50 BMG", ".50BMG",".50 BMG 7×99mm NATO"
    ) ~ ".50 BMG\12.7×99mm NATO",

    TRUE ~ "OUTROS")
}'

valores <- texto |>
  stringr::str_replace_all("\n","") |>
  stringr::str_replace_all("[:space:]+","") |>
  stringr::str_extract_all("%in%c(.+)") |>
  stringr::str_remove_all("coluna") |>
  stringr::str_split("%in%")

remake <- tibble(
  coluna = valores[[1]]
) |>
  separate(coluna, into = c("chaves", "classes"), sep = "~") |>
  mutate(
    chaves = purrr::map(chaves, function(x){
      eval(parse(text = x))
    }),
    classes = classes |>
      stringr::str_remove_all('"|,$')
  ) |>
  unnest(chaves) |>
  mutate(
    classes = ifelse(
      classes == "ziganaTRUE", "zigana", classes
    )
  ) |>
  rename(
    CALIBRE_ARMA = chaves,
    CALIBRE_ARMA_V2 = classes
  )

regex_calibres <- remake |>
  mutate(
    CALIBRE_ARMA = tolower(CALIBRE_ARMA)
  ) |>
  group_by(CALIBRE_ARMA_V2) |>
  summarise(
    regex = CALIBRE_ARMA |>
      unique() |>
      stringr::str_replace_all(stringr::fixed("."), "[.]") |>
      stringr::str_replace_all(stringr::fixed("+"), "[+]") |>
      stringr::str_replace_all(stringr::fixed("("), "[(]") |>
      stringr::str_replace_all(stringr::fixed(")"), "[)]") |>
      stringr::str_replace_all(stringr::fixed("*"), "[*]") |>
      paste0(collapse = "|") |>
      regex(ignore_case = TRUE)
  ) |>
  arrange(
    CALIBRE_ARMA_V2
  )

usethis::use_data(regex_calibres, overwrite = TRUE)
writexl::write_xlsx(regex_calibres, "data-raw/tabelas_regex/regex_calibres.xlsx")
