###Towns randomisation code###

## Karachi Central
set.seed(12345)
towns <- c("Gulberg",
           "Liaquatabad",
           "Nazimabad",
           "New Karachi",
           "North Nazimabad")
sample(towns, size=length(towns), replace = FALSE)

##Kambar
set.seed(27612)
towns <- c("Kamber",
           "Miro Khan",
           "Nasirabad",
           "Qubo Saed Khan",
           "Shahdadkot",
           "Sijawal",
           "Warah")
sample(towns, size=length(towns), replace = FALSE)

##Hyderabad
set.seed(17236)
towns <- c("Hyderabad City",
           "Hyderabad Rural",
           "Latifabad",
           "Qasimaabad")
sample(towns, size=length(towns), replace = FALSE)

##Jacobabad
set.seed(56723)
towns <- c("Jacobabad",
           "Garhi Khairo",
           "Thul")
sample(towns, size=length(towns), replace = FALSE)

##Sujawal
set.seed(19537)
towns <- c("Sujawal",
           "Jati",
           "Mripur Bathoro",
           "Shahbander")
sample(towns, size=length(towns), replace = FALSE)

##Karachi West
set.seed(19538)
towns <- c("Orangi",
           "S.I.T.E",
           "West-Gadap")
sample(towns, size=length(towns), replace = FALSE)