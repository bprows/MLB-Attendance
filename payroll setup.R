library(baseballr)


all_teams <- c( "ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", "HOU", "KCR",
                "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI", "PIT", "SDP", "SEA",
                "SFG", "STL", "TBR", "TEX", "TOR", "WSN")
head(team_results_bref("SDP", 2019))

payrolls <- read.table(text = '
1,Boston Red Sox,AL,East,"222,171,123",84,78,.519,3,7,15,"235,649,368",David Price (5)
2,Chicago Cubs,NL,Central,"210,358,488",84,78,.519,3,8,14,"183,463,459",Jon Lester (12)
3,New York Yankees,AL,East,"208,084,848",103,59,.636,1,2,3,"168,541,038",Giancarlo Stanton (13)
4,Los Angeles Dodgers,NL,West,"196,729,548",106,56,.654,1,1,2,"186,138,211",Clayton Kershaw (6)
5,San Francisco Giants,NL,West,"178,286,222",77,85,.475,3,10,18,"208,509,901",Johnny Cueto (22)
6,St. Louis Cardinals,NL,Central,"162,830,430",91,71,.562,1,4,10,"161,005,146",Yadier Molina (39)
7,Washington Nationals,NL,East,"161,877,935",93,69,.574,2,3,9,"181,586,759",Max Scherzer (1)
8,Houston Astros,AL,West,"160,774,666",107,55,.660,1,1,1,"159,529,730",Justin Verlander (9)
9,Los Angeles Angels,AL,West,"159,378,583",72,90,.444,4,11,22,"174,592,209",Mike Trout (3)
10,New York Mets,NL,East,"158,999,270",86,76,.531,3,6,12,"155,765,125",Yoenis Cespedes (8)
11,Seattle Mariners,AL,West,"150,906,143",69,84,.451,5,9,20,"162,480,174",Felix Hernandez (11)
12,Colorado Rockies,NL,West,"147,433,009",71,91,.438,4,12,23,"141,339,833",Nolan Arenado (14)
13,Philadelphia Phillies,NL,East,"140,711,962",81,81,.500,4,9,16,"96,848,371",Jake Arrieta (16)
14,Texas Rangers,AL,West,"127,660,652",78,84,.481,3,8,17,"144,498,078",Shin-Soo Choo (33)
15,Cincinnati Reds,NL,Central,"126,423,214",75,87,.463,4,11,19,"101,185,579",Joey Votto (17)
16,Milwaukee Brewers,NL,Central,"124,755,400",89,73,.549,2,5,11,"90,237,112",Yasmani Grandal (53)
17,Arizona Diamondbacks,NL,West,"123,932,983",85,77,.525,2,7,13,"132,495,446",Zack Greinke (4)
18,Minnesota Twins,AL,Central,"121,276,933",101,61,.623,1,3,4,"131,374,996",Nelson Cruz (80)
19,Cleveland Indians,AL,Central,"119,241,701",93,69,.574,2,6,8,"137,346,173",Carlos Santana (34)
20,Detroit Tigers,AL,Central,"116,478,400",47,114,.292,5,15,30,"129,917,192",Miguel Cabrera (7)
21,Atlanta Braves,NL,East,"113,778,369",97,65,.599,1,2,5,"120,542,785",Josh Donaldson (21)
22,Toronto Blue Jays,AL,East,"112,037,006",67,95,.414,4,12,26,"162,664,502",Justin Smoak (167)
23,Kansas City Royals,AL,Central,"100,658,316",59,103,.364,4,13,27,"129,967,987",Alex Gordon (32)
24,San Diego Padres,NL,West,"99,205,017",70,92,.432,5,13,24,"96,131,798",Eric Hosmer (26)
25,Oakland Athletics,AL,West,"92,318,333",97,65,.599,2,4,6,"68,534,631",Khris Davis (63)
26,Chicago White Sox,AL,Central,"89,354,534",72,89,.447,3,10,21,"72,177,640",Jose Abreu (65)
27,Pittsburgh Pirates,NL,Central,"76,589,154",69,93,.426,5,14,25,"87,860,654",Francisco Cervelli (111)
28,Baltimore Orioles,AL,East,"72,722,906",54,108,.333,5,14,29,"143,600,045",Chris Davis (31)
29,Miami Marlins,NL,East,"72,449,404",57,105,.352,5,15,28,"98,826,644",Wei-Yin Chen (42)
30,Tampa Bay Rays,AL,East,"60,444,931",96,66,.593,2,5,7,"78,231,049",Justin Smoak (166)',sep=",")
str(payrolls)
names(payrolls) <- c('pay_rank','Team_Name','League','Division','Team_Payroll','W','L','WPct','Rank','lgRank','mlbRank',"Last_Yr_Payroll","Top_Salary")

all_teams
teams_set_up <- c("BOS,CHC,NYY,LAD,SFG,STL,WSN,HOU,LAA,NYM,SEA,COL,PHI,TEX,CIN,MIL,ARI,MIN,CLE,DET,ATL,TOR,KCR,SDP,OAK,CHW,PIT,BAL,MIA,TBR")
teams_payroll <- unlist(strsplit(teams_set_up,","))
teams_payroll
payrolls$Team <- teams_payroll
str(payrolls)
payrolls$Team_Payroll <- as.numeric(gsub(",","",payrolls$Team_Payroll))
payrolls$Last_Yr_Payroll <- as.numeric(gsub(",","",payrolls$Last_Yr_Payroll))

write.csv(payrolls, "Payroll_2019.csv",row.names=F)



x1 <- 10.12
x2 <- 18.78
s1 <- 3.9
s2 <- 7.4
s12 <- s1**2
s22 <- s2**2
n1 <- 10
n2 <- 10

(x1-x2) / sqrt((s12/n1) + (s22/n2))

((s12/n1) + (s22/n2))**2 / ((s12/n1)/(n1-1) + (s22/n2)/(n2-1))

(x1-x2) + c(-1,1)*1.96*sqrt(s12/n1 + s22/n2)


x1 = 198
s1 = 20
n1 = 331
x2 = 214 
s2 = 24
n2 = 331

sp2 <- ((n1-1) * s1**2 + (n2-1)*s2**2) / (n1+n2-2)
sp <- sqrt(sp2)
sp

(x1-x2) / (sp * sqrt(1/n1 + 1/n2))

20.93 / (37.74 / sqrt(15))
