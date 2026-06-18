library(dplyr)
library(tibble)
library(purrr)

rac_list <- list(
  A = c("Armstrong","Briscoe","Carson","Childress","Collingsworth","Dallam","Deaf Smith","Donley","Gray","Hall","Hansford","Hartley","Hemphill","Hutchinson","Lipscomb","Moore","Ochiltree","Oldham","Parmer","Potter","Randall","Roberts","Sherman","Swisher","Wheeler"),
  B = c("Bailey","Borden","Castro","Cochran","Cottle","Crosby","Dawson","Dickens","Floyd","Gaines","Garza","Hale","Hockley","Kent","King","Lamb","Lubbock","Lynn","Motley","Scurry","Terry","Yoakum"),
  C = c("Archer","Baylor","Clay","Foard","Hardeman","Jack","Montague","Wichita","Wilbarger","Young"),
  D = c("Brown","Callahan","Coleman","Comanche","Eastland","Fisher","Haskell","Jones","Knox","Mitchell","Nolan","Shackelford","Stephens","Stonewall","Taylor","Throckmorton"),
  E = c("Collin","Cooke","Dallas","Denton","Ellis","Erath","Fannin","Grayson","Hood","Hunt","Johnson","Kaufman","Navarro","Palo Pinto","Parker","Rockwall","Somervell","Tarrant","Wise"),
  F = c("Bowie","Cass","Delta","Hopkins","Lamar","Morris","Red River","Titus"),
  G = c("Anderson","Camp","Cherokee","Franklin","Freestone","Gregg","Harrison","Henderson","Houston","Marion","Panola","Rains","Rusk","Shelby","Smith","Trinity","Upshur","Van Zandt","Wood"),
  H = c("Angelina","Nacogdoches","Polk","Sabine","San Augustine","San Jacinto","Tyler"),
  I = c("Culberson","El Paso","Hudspeth"),
  J = c("Andrews","Brewster","Crane","Ector","Glasscock","Howard","Jeff Davis","Loving","Martin","Midland","Pecos","Presidio","Reeves","Terrell","Upton","Ward","Winkler"),
  K = c("Coke","Concho","Crockett","Irion","Kimble","Mason","McCulloch","Menard","Reagan","Runnels","Schleicher","Sterling","Sutton","Tom Green"),
  L = c("Bell","Coryell","Hamilton","Lampasas","Milam","Mills"),
  M = c("Bosque","Falls","Hill","Limestone","McLennan"),
  N = c("Brazos","Burleson","Grimes","Leon","Madison","Robertson","Washington"),
  O = c("Bastrop","Blanco","Burnet","Caldwell","Fayette","Hays","Lee","Llano","San Saba","Travis","Williamson"),
  P = c("Atascosa","Bandera","Bexar","Comal","Dimmit","Edwards","Frio","Gillespie","Gonzales","Guadalupe","Karnes","Kendall","Kerr","Kinney","La Salle","Maverick","Medina","Real","Uvalde","Val Verde","Wilson","Zavala"),
  Q = c("Austin","Colorado","Fort Bend","Harris","Matagorda","Montgomery","Walker","Waller","Wharton"),
  R = c("Brazoria","Chambers","Galveston","Hardin","Jasper","Jefferson","Liberty","Newton","Orange"),
  S = c("Calhoun","DeWitt","Goliad","Jackson","Lavaca","Victoria"),
  T = c("Jim Hogg","Webb","Zapata"),
  U = c("Aransas","Bee","Brooks","Duval","Jim Wells","Kenedy","Kleberg","Live Oak","McMullen","Nueces","Refugio","San Patricio"),
  V = c("Cameron","Hidalgo","Starr","Willacy")
)

rac_df <- purrr::imap_dfr(
  rac_list,
  ~ tibble(County = .x, RAC = .y, TSA = paste0("TSA-", .y))
)

# check: should be 254
nrow(rac_df)

write.csv(rac_df, "data/tx_rac.csv", row.names = FALSE)



dshs_list <- list(
  `1`  = c("Armstrong", "Bailey", "Borden", "Briscoe", "Carson", "Castro", "Childress", "Cochran", "Collingsworth", "Crosby", "Dallam", "Dawson", "Deaf Smith", "Dickens", "Donley", "Floyd", "Gaines", "Garza", "Gray", "Hale", "Hall", "Hansford", "Hartley", "Hemphill", "Hockley", "Hutchinson", "King", "Lamb", "Lipscomb", "Lubbock", "Lynn", "Moore", "Motley", "Ochiltree", "Oldham", "Parmer", "Potter", "Randall", "Roberts", "Sherman", "Swisher", "Terry", "Wheeler", "Yoakum"),
  `2/3`  = c("Archer", "Baylor", "Brown", "Callahan", "Clay", "Coleman", "Comanche", "Cottle", "Eastland", "Fisher", "Foard", "Hardeman", "Haskell", "Jack", "Jones", "Kent", "Knox", "Mitchell", "Montague", "Nolan", "Scurry", "Shackelford", "Stephens", "Stonewall", "Taylor", "Throckmorton", "Wichita", "Wilbarger", "Young",
             "Collin", "Cooke", "Dallas", "Denton", "Ellis", "Erath", "Fannin", "Grayson", "Hood", "Hunt", "Johnson", "Kaufman", "Navarro", "Palo Pinto", "Parker", "Runnels", "Rockwall", "Somervell", "Tarrant", "Wise"),
  `4/5N`  = c("Anderson", "Bowie", "Camp", "Cass", "Cherokee", "Delta", "Franklin", "Gregg", "Harrison", "Henderson", "Hopkins", "Lamar", "Marion", "Morris", "Panola", "Rains", "Red River", "Rusk", "Smith", "Titus", "Upshur", "Van Zandt", "Wood", 
              "Angelina", "Jasper", "Newton", "Houston", "Nacogdoches", "Polk", "Sabine", "San Augustine", "San Jacinto", "Shelby", "Trinity", "Tyler"),
  `6/5S` = c("Hardin", "Jefferson", "Liberty", "Orange",
             "Austin", "Brazoria", "Chambers", "Colorado", "Fort Bend", "Galveston", "Harris", "Matagorda", "Montgomery", "Walker", "Waller", "Wharton"),
  `7`  = c("Bastrop", "Bell", "Blanco", "Bosque", "Brazos", "Burleson", "Burnet", "Caldwell", "Coryell", "Falls", "Fayette", "Freestone", "Grimes", "Hamilton", "Hays", "Hill", "Lampasas", "Lee", "Leon", "Limestone", "Llano", "Madison", "McLennan", "Milam", "Mills", "Robertson", "San Saba", "Travis", "Washington", "Williamson"),
  `8`  = c("Atascosa", "Bandera", "Bexar", "Calhoun", "Comal", "DeWitt", "Dimmit", "Edwards", "Frio", "Gillespie", "Goliad", "Gonzales", "Guadalupe", "Jackson", "Karnes", "Kendall", "Kerr", "Kinney", "La Salle", "Lavaca", "Maverick", "Medina", "Real", "Uvalde", "Val Verde", "Victoria", "Wilson", "Zavala"),
  `9/10`  = c("Andrews", "Coke", "Concho", "Crane", "Crockett", "Ector", "Glasscock", "Howard", "Irion", "Loving", "Martin", "Mason", "McCulloch", "Menard", "Midland", "Kimble", "Pecos", "Reagan", "Reeves", "Schleicher", "Sterling", "Sutton", "Terrell", "Tom Green", "Upton", "Ward", "Winkler", 
              "Brewster", "Culberson", "El Paso", "Hudspeth", "Jeff Davis", "Presidio"),
  `11` = c("Aransas", "Bee", "Brooks", "Cameron", "Duval", "Hidalgo", "Jim Hogg", "Jim Wells", "Kenedy", "Kleberg", "Live Oak", "McMullen", "Nueces", "Refugio", "San Patricio", "Starr", "Webb", "Willacy", "Zapata")
)

# check: should be 254
tx_dshs_df <- purrr::imap_dfr(
  dshs_list,
  ~ tibble::tibble(county = .x, dshs_region = .y)
)


write.csv(tx_dshs_df, "data/tx_dshs_region.csv", row.names = FALSE)