# create a named vector for mapping
country_map <- c(
  "1" = "belgium", "2" = "denmark", "3" = "germany", "4" = "greece",
  "5" = "spain", "6" = "france", "7" = "ireland", "8" = "italy",
  "10" = "netherlands", "11" = "united kingdom", "12" = "portugal",
  "13" = "austria", "14" = "finland", "16" = "sweden", "20" = "bulgaria",
  "21" = "czech republic", "22" = "estonia", "23" = "hungary",
  "24" = "latvia", "25" = "lithuania", "26" = "poland", "27" = "romania",
  "28" = "slovakia", "29" = "slovenia", "31" = "croatia", "34" = "turkey",
  "35" = "norway", "36" = "switzerland", "37" = "malta", "38" = "luxembourg",
  "40" = "cyprus", "45" = "iceland"
)

# Create a party map
party_map <- c(
  # Belgium
  "102" = "Socialist Party",
  "103" = "Socialist Party Differently",
  "104" = "Ecolo",
  "105" = "Green",
  "106" = "Reformist Movement",
  "107" = "Open Flemish Liberals and Democrats",
  "108" = "Humanist Democratic Centre",
  "109" = "Christian Democratic and Flemish",
  "110" = "New Flemish Alliance",
  "112" = "Flemish Interest",
  "119" = "Workers' Party of Belgium",

  # Denmark
  "201" = "Social Democracy",
  "202" = "Radical Left-Social Liberal Party",
  "203" = "Conservative People's Party",
  "206" = "Socialist People's Party",
  "211" = "Venstre, Liberal Party of Denmark",
  "213" = "Unity List/Red-Green Alliance",
  "215" = "Danish People's Party",
  "218" = "Liberal Alliance",
  "219" = "The Alternative",
  "220" = "The New Right",

  # Germany
  "301" = "Christian Democratic Union of Germany",
  "302" = "Social Democratic Party of Germany",
  "303" = "Free Democratic Party",
  "304" = "Alliance '90/The Greens",
  "306" = "The Left",
  "308" = "Christian Social Union in Bavaria",
  "310" = "Alternative for Germany",
  "311" = "Pirate Party of Germany",
  "312" = "Human Environment Animal Protection",

  # Greece
  "401" = "Panhellenic Socialist Movement",
  "402" = "New Democracy",
  "403" = "Coalition of the Radical Left",
  "404" = "Communist Party of Greece",
  "415" = "Popular Association—Golden Dawn",
  "416" = "Greek Solution",
  "417" = "European Realistic Disobedience Front [MeRa25]",
  "418" = "Movement of Democratic Socialists",

  # Spain
  "501" = "Spanish Socialist Workers' Party",
  "502" = "People's Party",
  "504" = "United Left",
  "506" = "Basque Nationalist Party",
  "511" = "Republican Left of Catalonia",
  "513" = "Galician Nationalist Bloc",
  "517" = "Canarian Coalition",
  "524" = "Basque Country Unite/Gather",
  "525" = "We Can",
  "526" = "Citizens—Party of the Citizenry",
  "527" = "Voice (Latin)",
  "528" = "More Country",
  "550" = "Catalan European Democratic Party",

  # France
  "601" = "French Communist Party",
  "602" = "Socialist Party",
  "605" = "Europe Ecology—The Greens",
  "609" = "The Republicans",
  "610" = "National Rally",
  "613" = "Democratic Movement",
  "626" = "The Republic Forward",
  "627" = "Unbowed France",
  "628" = "France Arise",

  # Ireland
  "701" = "Soldiers of Destiny",
  "702" = "Family of the Irish",
  "703" = "Labour",
  "705" = "Green Party",
  "707" = "We Ourselves",
  "709" = "Solidarity—People Before Profit",
  "710" = "Social Democrats",
  "711" = "Independents for Change",
  "712" = "Renua Ireland",

  # Italy
  "811" = "Northern League",
  "813" = "Italian Radicals",
  "815" = "Forward Italy",
  "827" = "South Tyrolean People's Party",
  "837" = "Democratic Party",
  "838" = "Italian Left",
  "844" = "Brothers of Italy",
  "845" = "Five Star Movement",

  # Netherlands
  "1001" = "Christian Democratic Appeal",
  "1002" = "Labour Party",
  "1003" = "People's Party for Freedom and Democracy",
  "1004" = "Democrats 66",
  "1005" = "GreenLeft",
  "1006" = "Reformed Political Party",
  "1014" = "Socialist Party",
  "1016" = "Christian Union",
  "1017" = "Party for Freedom",
  "1018" = "Party for the Animals",
  "1020" = "50PLUS",
  "1050" = "Think",
  "1051" = "Forum for Democracy",

  # United Kingdom
  "1101" = "Conservative Party",
  "1102" = "Labour Party",
  "1104" = "Liberal Democratic Party",
  "1105" = "Scottish National Party",
  "1106" = "Party of Wales",
  "1107" = "Green Party",
  "1108" = "United Kingdom Independence Party",
  "1110" = "Brexit Party",

  # Portugal
  "1201" = "Democratic Unitarian Coalition",
  "1202" = "CDS—People's Party",
  "1205" = "Socialist Party",
  "1206" = "Social Democratic Party",
  "1208" = "Left Bloc",
  "1210" = "Portuguese Communist Party",
  "1211" = "Ecologist Party \"The Greens\"",
  "1250" = "People—Animals—Nature",

  # Austria
  "1301" = "Social Democratic Party of Austria",
  "1302" = "Austrian People's Party",
  "1303" = "Freedom Party of Austria",
  "1304" = "The Greens—The Green Alternative",
  "1306" = "NEOS—The New Austria and Liberal Forum",

  # Finland
  "1401" = "Social Democratic Party of Finland",
  "1402" = "National Coalition Party",
  "1403" = "Centre Party",
  "1404" = "Left Alliance",
  "1405" = "The Finns Party",
  "1406" = "The Swedish People's Party of Finland",
  "1408" = "Green League",
  "1409" = "Christian Democrats",

  # Sweden
  "1601" = "Left Party",
  "1602" = "Swedish Social Democratic Party",
  "1603" = "Center Party",
  "1604" = "Liberal People's Party",
  "1605" = "Moderate Party",
  "1606" = "Christian Democrats",
  "1607" = "Environment Party—The Greens",
  "1610" = "Sweden Democrats",

  # Bulgaria
  "2003" = "Bulgarian Socialist Party",
  "2004" = "Movement for Rights and Freedoms",
  "2005" = "Bulgarian National Movement",
  "2007" = "Attack",
  "2008" = "Democrats for a Strong Bulgaria",
  "2010" = "Citizens for European Development of Bulgaria",
  "2014" = "National Front for the Salvation of Bulgaria",
  "2017" = "Will",
  "2018" = "Yes, Bulgaria!",
  "2019" = "Party of Slavi Trifonov",

  # Czech Republic
  "2101" = "Czech Social Democratic Party",
  "2102" = "Civic Democratic Party",
  "2103" = "Communist Party of Bohemia and Moravia",
  "2104" = "Christian Democratic Union-Czechoslovak People's Party",
  "2109" = "Tradition Responsibility Prosperity 09",
  "2111" = "Action of Dissatisfied Citizens",
  "2114" = "Czech Pirate Party",
  "2115" = "Freedom and Direct Democracy Tomio Okamura",
  "2116" = "Mayors and Independents",

  # Estonia
  "2201" = "Pro Patria and Res Publica Union",
  "2202" = "Estonian Center Party",
  "2203" = "Estonian Reform Party",
  "2204" = "Social Democratic Party",
  "2209" = "Conservative People's Party",
  "2210" = "Estonia 200",

  # Hungary
  "2301" = "Hungarian Socialist Party",
  "2302" = "Fidesz—Hungarian Civic Union",
  "2308" = "Jobbik—Movement for a Better Hungary",
  "2309" = "Politics Can Be Different",
  "2310" = "Together 2019",
  "2311" = "Democratic Coalition",
  "2314" = "Momentum Movement",

  # Latvia
  "2402" = "Latvian Russian Union",
  "2405" = "Union of Greens and Farmers",
  "2406" = "National Alliance",
  "2410" = "Social Democratic Party \"Harmony\"",
  "2412" = "Unity",
  "2414" = "Latvian Association of Regions",
  "2415" = "Who owns the state?",
  "2416" = "New Conservative Party",
  "2417" = "Development/For!",

  # Lithuania
  "2501" = "Social Democratic Party of Lithuania",
  "2506" = "Homeland Union",
  "2507" = "Lithuanian Peasant Union",
  "2511" = "Electoral Action of Lithuania's Poles",
  "2515" = "Order and Justice",
  "2516" = "Labour Party",
  "2518" = "Liberal Movement of the Republic of Lithuania",
  "2521" = "Lithuanian Centre Party",
  "2522" = "Lithuanian Green Party",
  "2523" = "Public election committee \"Aušra Maldeikienė's Train\"",

  # Poland
  "2601" = "Democratic Left Alliance",
  "2603" = "Civic Platform",
  "2605" = "Law and Justice Party",
  "2606" = "Polish People's Party",
  "2617" = "Kukiz '15",
  "2618" = "Modern",
  "2619" = "Confederation Liberty and Independence",
  "2620" = "Left Together",
  "2621" = "Spring",

  # Romania
  "2701" = "Social Democratic Party",
  "2705" = "National Liberal Party",
  "2706" = "Hungarian Democratic Union of Romania",
  "2711" = "People's Movement Party",
  "2712" = "Alliance of Liberals and Democrats",
  "2713" = "Save Romania Union",
  "2714" = "PRO Romania",

  # Slovakia
  "2803" = "Direction—Social Democracy",
  "2804" = "Hungarian Coalition",
  "2805" = "Christian Democratic Movement",
  "2809" = "Slovak National Party",
  "2812" = "Freedom and Solidarity",
  "2813" = "Bridge",
  "2814" = "Ordinary People and Independent",
  "2816" = "Network",
  "2817" = "People's Party—Our Slovakia",
  "2818" = "We are family—Boris Kollar",
  "2819" = "Progressive Slovakia",
  "2820" = "TOGETHER—Civic Democracy",
  "2821" = "For the People",

  # Slovenia
  "2902" = "Slovenian Democratic Party",
  "2903" = "Social Democrats",
  "2905" = "New Slovenia-Christian People's Party",
  "2906" = "Democratic Party of Pensioners of Slovenia",
  "2907" = "Slovenian National Party",
  "2911" = "Party of Miro Cerar",
  "2912" = "The Left",
  "2913" = "Party of Alenka Bratusek",
  "2915" = "List of Marjan Sarec",

  # Croatia
  "3101" = "Croatian Democratic Union",
  "3102" = "Social Democratic Party of Croatia",
  "3103" = "Croatian Peasant Party",
  "3104" = "Croatian Social Liberal Party",
  "3105" = "Croatian People's Party—Liberal Democrats",
  "3106" = "Istrian Democratic Assembly",
  "3107" = "Croatian Democratic Assembly of Slavonija and Baranja",
  "3108" = "Croatian Party of Pensioners",
  "3110" = "Independent Democratic Serb Party",
  "3115" = "Bridge of Independent Lists",
  "3116" = "Human Shield",
  "3117" = "Milan Bandic 365—The Party of Labour and Solidarity",
  "3118" = "People's Party-Reformists",
  "3119" = "Croatian Conservative Party",

  # Turkey
  "3401" = "Justice and Development Party",
  "3402" = "Republican People's Party",
  "3403" = "Nationalist Movement Party",
  "3407" = "Peoples' Democratic Party",
  "3408" = "Good Party",

  # Norway
  "3501" = "Labour Party",
  "3502" = "Progress Party",
  "3503" = "Conservative Party",
  "3504" = "Socialist Left Party",
  "3505" = "Centre Party",
  "3506" = "Christian Democratic Party",
  "3507" = "Liberal Party",
  "3508" = "Green Party",
  "3509" = "Red Party",

  # Switzerland
  "3601" = "Swiss People's Party",
  "3602" = "Social Democratic Party of Switzerland",
  "3603" = "FDP. The Liberals",
  "3604" = "Christian Democratic People's Party",
  "3605" = "Green Party",
  "3606" = "Green Liberal Party",
  "3607" = "Evangelical People's Party",
  "3612" = "Conservative Democratic Party",

  # Malta
  "3701" = "Labour Party",
  "3702" = "Nationalist Party",

  # Luxembourg
  "3801" = "Christian Social People's Party",
  "3802" = "The Greens",
  "3803" = "Democratic Party",
  "3804" = "Luxembourg Socialist Workers' Party",
  "3805" = "Alternative Democratic Reform Party",
  "3806" = "The Left",
  "3807" = "Pirate Party Luxembourg",

  # Cyprus
  "4001" = "Democratic Rally",
  "4003" = "Progressive Party of Working People",
  "4004" = "Democratic Party",
  "4005" = "Movement for Social Democracy EDEK",
  "4006" = "Ecological and Environmental Movement",
  "4007" = "Citizens' Alliance",
  "4008" = "Solidarity Movement",
  "4009" = "National Popular Front",

  # Iceland
  "4501" = "Independence Party",
  "4502" = "Left-Green Movement",
  "4503" = "Progressive Party",
  "4504" = "Centre Party",
  "4505" = "Social Democratic Alliance",
  "4506" = "Pirate Party",
  "4507" = "Reform Party",
  "4508" = "People's Party"
)
