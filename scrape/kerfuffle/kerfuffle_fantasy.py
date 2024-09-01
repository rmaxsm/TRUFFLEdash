from bs4 import BeautifulSoup
import requests
import pandas as pd
import re



cookies = {
    'region_code': 'MI',
    'fly_device': 'desktop',
    'fly_ab_uid': '8168e253-e210-4519-be4d-9a978ad5959a',
    'surround': 'a|3',
    'AMCVS_10D31225525FF5790A490D4D%40AdobeOrg': '1',
    'optimizelyEndUserId': 'oeu1724094463749r0.528063961133233',
    's_ecid': 'MCMID%7C65302577355307439494543278293908418520',
    'AMCV_10D31225525FF5790A490D4D%40AdobeOrg': '179643557%7CMCIDTS%7C19955%7CMCMID%7C65302577355307439494543278293908418520%7CMCAAMLH-1724699263%7C7%7CMCAAMB-1724699263%7C6G1ynYcLPuiQxYZrsz_pkqfLG9yMXBpb2zX5dvJdYQJzPXImdj0y%7CMCOPTOUT-1724101663s%7CNONE%7CMCAID%7CNONE%7CvVersion%7C5.5.0',
    's_cc': 'true',
    '_ga': 'GA1.2.1218016554.1724094464',
    '_gid': 'GA1.2.555823094.1724094464',
    '_gat_tealium_0': '1',
    '_gcl_au': '1.1.74595198.1724094464',
    '_ga_7T7CTRQ10D': 'GS1.2.1724094464.1.0.1724094464.0.0.0',
    's_sq': 'cbsicbssportssite%3D%2526c.%2526a.%2526activitymap.%2526page%253Dcbssports%25253A%25252Flogin%2526link%253DLog%252520In%2526region%253Dlogin_form%2526pageIDType%253D1%2526.activitymap%2526.a%2526.c%2526pid%253Dcbssports%25253A%25252Flogin%2526pidt%253D1%2526oid%253DLog%252520In%2526oidt%253D3%2526ot%253DSUBMIT',
    'pid': 'L%3A114%3ALWOW60w2n0Ldprvn9fjGKw%253D%253D%3A1',
    'anon': 'FALSE',
    'fly_tracking': '%7B%22userType%22%3A%22registered%22%2C%22userState%22%3A%22authenticated%22%2C%22userId%22%3A68154479%7D',
    'tpid': 'A%3AEF8JjULwRszW4VJwvgMmAv2FYTxLRGwL6NHewSmJfzg%253D%3A1',
    'ppid': '11a83c4bfdb54a60e65744a0d9fff25e',
    'pid_coppa': 'IVNQTE4wS3piRGo1NnEyc3NaeGZYeUEzZnc5QDI3',
    'privacyConsent': '%7B%22privacy_id%22%3A%22c9b519905fa3d42a2ea8b5f62d56a5b538110fa41ce883f4e661582a25815479%22%2C%22privacy_jwt%22%3A%22eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiIsImtpZCI6ImNic3Nwb3J0cyJ9.eyJzdWIiOiJjOWI1MTk5MDVmYTNkNDJhMmVhOGI1ZjYyZDU2YTViNTM4MTEwZmE0MWNlODgzZjRlNjYxNTgyYTI1ODE1NDc5In0.UxfXxkTpl2QogrlSp8hqK-e6ye4Y1H6THt3G9FQ4oeY%22%7D',
    'last_access': '1724094491',
    'fly_geo': '{"countryCode":"us","region":"mi"}',
    'XFP_FIRSTPAGE': '0',
    'OptanonAlertBoxClosed': '2024-08-18T15:45:20.589928901Z',
    'eupubconsent-v2': 'CQDi7JgQDi7JgAcABBENBCFgAAAAAAAAACiQAAAAAAGhQCAACoATgBUAD0AIoAUgAvABzAEqAOAAhABHYCvAK-Ae0BLQCpYHUgdUEAOQAMAA0ACEAFwAYABbADkAOgAjABOACiAFqAMIAxQBlAGwAOUAgACCAEYAI8AUgAuQBxAGNANAAlkBe4DFAGNgMgAcwA6EB5gSACAEQeANAAVACcAKgAegBFACcAFIAOYA4ICvAK-Ae0A_gCWoHUgdUOAQwAMAA0ACAAGAAWgA6ACMAFEALQAXgAwgBlADYAG4AOUAgACCAEYAJUAXIA1QBxAFNAMaAaABawC3gF7gMUAY2AyABzADoQHmAPZHQAQAiEQA4AKgBwQFeAV8A9oB_AEtQOpA6ogAOADCAGwASoAuQBqgDiAKaAY0BawC3gF7gMUAZAA5glASAAWABwAHgARAAmABcADFAIYAiQBHACjAMUAdQBF4CRAF5gMkAZYBAEkACAAuApMoAgAAaABAADAALQAdABGACiAFsALwAYQAygBsgDeAOUAgACCAEZAJUAlgBxAEIAJaAU0AwIBjQDMgGgARqAvcBf4DFAHMAOhgdSB1QDzCoAiAE4AVABFADmAOCArwCvgJaAWsAyApABACIAAA.YAAAAAAAAAAA',
    'OTAdditionalConsentString': '1~',
    'cbsiaa': '68154479',
    'OptanonConsent': 'groups=3%3A0%2C4%3A0%2C2%3A0%2C1%3A1%2C5%3A0%2CBG1259%3A0&datestamp=Mon+Aug+19+2024+15%3A08%3A14+GMT-0400+(Eastern+Daylight+Time)&version=202401.1.0&isGpcEnabled=0&browserGpcFlag=0&geolocation=US%3BMI&isIABGlobal=false&consentId=c9b519905fa3d42a2ea8b5f62d56a5b538110fa41ce883f4e661582a25815479&identifierType=Cookie+Unique+Id&hosts=&interactionCount=0&landingPath=https%3A%2F%2Fkerfuffle.football.cbssports.com%2Fteams%3Ftid%3D1724094491%26login%3Dconfirmed&GPPCookiesCount=1',
    'OTGPPConsent': 'DBABLA~BVQVAAAAAgA.QA',
    'utag_main': 'v_id:01916c09b8450018c7a51f0933ae0507d008407500978$_sn:1$_se:4$_ss:0$_st:1724096294907$ses_id:1724094462022%3Bexp-session$_pn:2%3Bexp-session$vapi_domain:cbssports.com$dc_visit:1$dc_event:1%3Bexp-session$dc_region:us-east-1%3Bexp-session',
    'usprivacy': '1YYN',
    '__gads': 'ID=48d5f3660bca6bdc:T=1724094495:RT=1724094495:S=ALNI_MYNnnwa1izpxyzbBYZLMQN8FYTTzA',
    '__gpi': 'UID=00000ecca23f4203:T=1724094495:RT=1724094495:S=ALNI_Ma1GVTREkwHUbPckhfzY8UItFJnrw',
    '__eoi': 'ID=11f7a61c2d983e65:T=1724094495:RT=1724094495:S=AA-AfjYMKWaLH7DQGLbaqS_w9RSu',
    'FCNEC': '%5B%5B%22AKsRol-jYSHbsGUoGkpFLRHZpuCLe40bPrQCyq5kR4ZsxRewDHfCdgh8v0eG4vUsmCZFYTox-Tw3mi4JEudSSclVteb_PeuvnrZEgc4Jn2CebyW14ZdiocOdO91uo0VsmSHPIQzQjg5lcSvkOlYMsqfnezUxbWGCmA%3D%3D%22%5D%5D',
    'RT': '"z=1&dm=cbssports.com&si=98d55cea-5c5e-4fc0-b7e0-29e0dbb2a170&ss=m01dcffq&sl=1&tt=3cz&rl=1&ld=3d2&ul=643"',
}

headers = {
    'accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
    'accept-language': 'en-US,en;q=0.9',
    # 'cookie': 'region_code=MI; fly_device=desktop; fly_ab_uid=8168e253-e210-4519-be4d-9a978ad5959a; surround=a|3; AMCVS_10D31225525FF5790A490D4D%40AdobeOrg=1; optimizelyEndUserId=oeu1724094463749r0.528063961133233; s_ecid=MCMID%7C65302577355307439494543278293908418520; AMCV_10D31225525FF5790A490D4D%40AdobeOrg=179643557%7CMCIDTS%7C19955%7CMCMID%7C65302577355307439494543278293908418520%7CMCAAMLH-1724699263%7C7%7CMCAAMB-1724699263%7C6G1ynYcLPuiQxYZrsz_pkqfLG9yMXBpb2zX5dvJdYQJzPXImdj0y%7CMCOPTOUT-1724101663s%7CNONE%7CMCAID%7CNONE%7CvVersion%7C5.5.0; s_cc=true; _ga=GA1.2.1218016554.1724094464; _gid=GA1.2.555823094.1724094464; _gat_tealium_0=1; _gcl_au=1.1.74595198.1724094464; _ga_7T7CTRQ10D=GS1.2.1724094464.1.0.1724094464.0.0.0; s_sq=cbsicbssportssite%3D%2526c.%2526a.%2526activitymap.%2526page%253Dcbssports%25253A%25252Flogin%2526link%253DLog%252520In%2526region%253Dlogin_form%2526pageIDType%253D1%2526.activitymap%2526.a%2526.c%2526pid%253Dcbssports%25253A%25252Flogin%2526pidt%253D1%2526oid%253DLog%252520In%2526oidt%253D3%2526ot%253DSUBMIT; pid=L%3A114%3ALWOW60w2n0Ldprvn9fjGKw%253D%253D%3A1; anon=FALSE; fly_tracking=%7B%22userType%22%3A%22registered%22%2C%22userState%22%3A%22authenticated%22%2C%22userId%22%3A68154479%7D; tpid=A%3AEF8JjULwRszW4VJwvgMmAv2FYTxLRGwL6NHewSmJfzg%253D%3A1; ppid=11a83c4bfdb54a60e65744a0d9fff25e; pid_coppa=IVNQTE4wS3piRGo1NnEyc3NaeGZYeUEzZnc5QDI3; privacyConsent=%7B%22privacy_id%22%3A%22c9b519905fa3d42a2ea8b5f62d56a5b538110fa41ce883f4e661582a25815479%22%2C%22privacy_jwt%22%3A%22eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiIsImtpZCI6ImNic3Nwb3J0cyJ9.eyJzdWIiOiJjOWI1MTk5MDVmYTNkNDJhMmVhOGI1ZjYyZDU2YTViNTM4MTEwZmE0MWNlODgzZjRlNjYxNTgyYTI1ODE1NDc5In0.UxfXxkTpl2QogrlSp8hqK-e6ye4Y1H6THt3G9FQ4oeY%22%7D; last_access=1724094491; fly_geo={"countryCode":"us","region":"mi"}; XFP_FIRSTPAGE=0; OptanonAlertBoxClosed=2024-08-18T15:45:20.589928901Z; eupubconsent-v2=CQDi7JgQDi7JgAcABBENBCFgAAAAAAAAACiQAAAAAAGhQCAACoATgBUAD0AIoAUgAvABzAEqAOAAhABHYCvAK-Ae0BLQCpYHUgdUEAOQAMAA0ACEAFwAYABbADkAOgAjABOACiAFqAMIAxQBlAGwAOUAgACCAEYAI8AUgAuQBxAGNANAAlkBe4DFAGNgMgAcwA6EB5gSACAEQeANAAVACcAKgAegBFACcAFIAOYA4ICvAK-Ae0A_gCWoHUgdUOAQwAMAA0ACAAGAAWgA6ACMAFEALQAXgAwgBlADYAG4AOUAgACCAEYAJUAXIA1QBxAFNAMaAaABawC3gF7gMUAY2AyABzADoQHmAPZHQAQAiEQA4AKgBwQFeAV8A9oB_AEtQOpA6ogAOADCAGwASoAuQBqgDiAKaAY0BawC3gF7gMUAZAA5glASAAWABwAHgARAAmABcADFAIYAiQBHACjAMUAdQBF4CRAF5gMkAZYBAEkACAAuApMoAgAAaABAADAALQAdABGACiAFsALwAYQAygBsgDeAOUAgACCAEZAJUAlgBxAEIAJaAU0AwIBjQDMgGgARqAvcBf4DFAHMAOhgdSB1QDzCoAiAE4AVABFADmAOCArwCvgJaAWsAyApABACIAAA.YAAAAAAAAAAA; OTAdditionalConsentString=1~; cbsiaa=68154479; OptanonConsent=groups=3%3A0%2C4%3A0%2C2%3A0%2C1%3A1%2C5%3A0%2CBG1259%3A0&datestamp=Mon+Aug+19+2024+15%3A08%3A14+GMT-0400+(Eastern+Daylight+Time)&version=202401.1.0&isGpcEnabled=0&browserGpcFlag=0&geolocation=US%3BMI&isIABGlobal=false&consentId=c9b519905fa3d42a2ea8b5f62d56a5b538110fa41ce883f4e661582a25815479&identifierType=Cookie+Unique+Id&hosts=&interactionCount=0&landingPath=https%3A%2F%2Fkerfuffle.football.cbssports.com%2Fteams%3Ftid%3D1724094491%26login%3Dconfirmed&GPPCookiesCount=1; OTGPPConsent=DBABLA~BVQVAAAAAgA.QA; utag_main=v_id:01916c09b8450018c7a51f0933ae0507d008407500978$_sn:1$_se:4$_ss:0$_st:1724096294907$ses_id:1724094462022%3Bexp-session$_pn:2%3Bexp-session$vapi_domain:cbssports.com$dc_visit:1$dc_event:1%3Bexp-session$dc_region:us-east-1%3Bexp-session; usprivacy=1YYN; __gads=ID=48d5f3660bca6bdc:T=1724094495:RT=1724094495:S=ALNI_MYNnnwa1izpxyzbBYZLMQN8FYTTzA; __gpi=UID=00000ecca23f4203:T=1724094495:RT=1724094495:S=ALNI_Ma1GVTREkwHUbPckhfzY8UItFJnrw; __eoi=ID=11f7a61c2d983e65:T=1724094495:RT=1724094495:S=AA-AfjYMKWaLH7DQGLbaqS_w9RSu; FCNEC=%5B%5B%22AKsRol-jYSHbsGUoGkpFLRHZpuCLe40bPrQCyq5kR4ZsxRewDHfCdgh8v0eG4vUsmCZFYTox-Tw3mi4JEudSSclVteb_PeuvnrZEgc4Jn2CebyW14ZdiocOdO91uo0VsmSHPIQzQjg5lcSvkOlYMsqfnezUxbWGCmA%3D%3D%22%5D%5D; RT="z=1&dm=cbssports.com&si=98d55cea-5c5e-4fc0-b7e0-29e0dbb2a170&ss=m01dcffq&sl=1&tt=3cz&rl=1&ld=3d2&ul=643"',
    'priority': 'u=0, i',
    'sec-ch-ua': '"Not)A;Brand";v="99", "Microsoft Edge";v="127", "Chromium";v="127"',
    'sec-ch-ua-mobile': '?0',
    'sec-ch-ua-platform': '"Windows"',
    'sec-fetch-dest': 'document',
    'sec-fetch-mode': 'navigate',
    'sec-fetch-site': 'none',
    'sec-fetch-user': '?1',
    'upgrade-insecure-requests': '1',
    'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/127.0.0.0 Safari/537.36 Edg/127.0.0.0',
}

#this is the main pandas frame that will be added to throughout
dfGlobal = pd.DataFrame()

#get year and week for url/formatting
season = input("What SEASON is it..? ")
while(len(season) != 4):
  print("get the SEASON right dumbass")
  season = input("What SEASON is it..? ")

week = input("What WEEK is it..? ")
while(len(week) >= 3):
  print("get the week right dumbass")
  week = input("What WEEK is it..? ")

#use current input week year tuple as comparison
currentWeekYear = week + season


#get information from teams document to refer to for shortcuts ext
teamsPd = pd.read_csv("data/teams.csv")

# re-assign the PD to get only KERFUFFLE
teamsPd = teamsPd[teamsPd['League'] == "KERFUFFLE"]

teamsDict = {}
for index, row in teamsPd.iterrows():
  teamsDict[row["LogsScrape"]] = row["Abbrev"]  

#returns team abbreviation from team name
def getTeamAbbreviation(team):
  try:
    waived =  re.compile("W ")
    if(waived.match(team)):
      return team
    return teamsDict[team]
  except Exception as exp:
    print("An Error Occuring while trying to get the team abbreviation for " + team)
    return "err"

#separates the column names
#returns list representing the columns for tables 
def separateColumns(row):
  allCols = []
  for i in row:
    allCols.append(i.getText())
  allCols[1] = "TRUFFLE"
  return allCols

#takes in a single row of html and returns the stats as list (for players/dst)
def separatePlayers(rows):
  itr = 0
  curRow = []
  for i in rows:
    if itr == 0:    #first value always empty string
      curRow.append("")
    elif itr == 1:    #second value convert to team abbreviation
      curRow.append(getTeamAbbreviation(i.getText()).strip())   
    else:       #after first two iterations just get exact data from source
      curRow.append(i.getText())
    itr += 1
  return curRow

#lamba functions to split player name team and position
getNFLTeam = lambda x: pd.Series([i.strip() for i in x.split("•")])
getPosition = lambda y: pd.Series([i for i in y.split(" ")][-1])
getPlayer = lambda z: pd.Series(' '.join([i for i in z.split(" ")][:-1]))

for index, row in teamsPd.iterrows():

    teamNum = row["TeamNum"]
    #where the connection is made to the truffle cbs website
    url = "https://kerfuffle.football.cbssports.com/stats/stats-main/team:{}/period-{}:f/TRUFFLEoffense/".format(teamNum,week)
    response = requests.get(url, cookies=cookies, headers=headers)
    soup = BeautifulSoup(response.content, 'html.parser')

    #finds the outermost 'tables' containing the stats(3 off,kicker,dst in order)
    tbls =  soup.find_all("table", class_="data pinHeader borderTop")
    tblOffense = tbls[0]
    tblDefense = tbls[1]

    #gets the column header information as 'label'
    combined = tblOffense.find_all("tr", class_="label")
    label = combined[1]

    #calls function to clean column headers stored as cols (used in pandas df)
    cols = separateColumns(label)

    #store all players per team as list of lists
    allPlayers = []

    #regex used to find row1/2 (\d means only numbers following exact match of row)
    allRowsOffense = tblOffense.find_all("tr", class_=re.compile("row\d"))

    #for every player - clean the row and add to list of lists
    for i in allRowsOffense:
        allPlayers.append(separatePlayers(i))

    # simlar to offense but with slightly different logic
    defRows = tblDefense.find_all("tr", {"class": re.compile("row\d")})
    for i in defRows:
        curDef = separatePlayers(i)
    #once defense stats are cleaned need to fill the rest of the list as empty
    noneList = ["" for i in range(len(allPlayers[0]) - len(curDef))]
    for j in noneList:
        curDef.append(j)
    allPlayers.append(curDef)

    #pandas df to represent team
    df = pd.DataFrame(allPlayers, columns=cols)
    df = df.drop(columns=["Action"])
    df = df.drop(columns=["Bye"])

    #apply lambda fcts to correct columns
    playerTeam = df["Player"].apply(getNFLTeam)
    position = playerTeam[0].apply(getPosition)
    player =  playerTeam[0].apply(getPlayer)
    nfl = pd.Series(playerTeam[1])


    #add/remove columns for TRUFFLE formatting
    # df = df.drop(["Bye","Rost", "Start"],axis=1)
    # FIND ME - this has been changed because 'Bye' is no longer on the website
    df = df.drop(["Rost", "Start"],axis=1)
    df["Player"] = player
    df.insert(0,"Season", season)
    df.insert(1,"Week", week)
    df.insert(3,"Pos", position[0])
    df.insert(5,"NFL", nfl)

    #moves defensive data - empties out old
    finalVal = len(df.index) - 1
    compToAvg = df.iloc[[finalVal]]["Comp"].values[0]
    df.loc[finalVal,"Avg"] = compToAvg
    df.loc[finalVal, "Comp"] = ''
    total = df.iloc[[finalVal]]["ATT"].values[0]
    df.loc[finalVal,"Total"] = total
    df.loc[finalVal, "ATT"] = ''

    #on the first iteration we need to 'create' the main pandas table - it is naive to the size before scraping
    if index == 0:
        dfGlobal = df
    else:
        dfGlobal = pd.concat([dfGlobal,df], ignore_index=True)


#dfGlobal = dfGlobal.replace('', "NA", regex = True)

print(dfGlobal)

masterFile = "data/kerfuffle/kerfuffle_fantasy.csv"

#read the existing csv as a pd df for error checking
masterDf = pd.read_csv(masterFile)
filepath = "data/kerfuffle/backup/kerfuffle_fantasy_backup.csv"
masterDf.to_csv(filepath, index=False)

#reassign the columns to be equal to that of the existing csv
dfGlobal.columns = masterDf.columns

#player name replacements
dfGlobal['Player'] = dfGlobal['Player'].str.replace(r'.', '', regex=True)
dfGlobal['Player'] = dfGlobal['Player'].str.replace(r' Jr', '', regex=True)
dfGlobal['Player'] = dfGlobal['Player'].str.replace(r' Sr', '', regex=True)
dfGlobal['Player'] = dfGlobal['Player'].str.replace(r' III', '', regex=True)
dfGlobal['Player'] = dfGlobal['Player'].str.replace(r' II', '', regex=True)
dfGlobal['Player'] = dfGlobal['Player'].str.replace(r'Will Fuller V', 'Will Fuller', regex=True)

#create weekyear column to conditionally remove existing data from week being scraped
masterDf["WeekYear"] = masterDf["Week"].astype(str) + masterDf["Season"].astype(str)
#remove
masterDf = masterDf[masterDf["WeekYear"] != currentWeekYear]
#drop the weekyear column post check
masterDf = masterDf.drop(['WeekYear'], axis=1)

#concat scraped df and the masterDf
newmaster = pd.concat([masterDf, dfGlobal], ignore_index=True)

# stores as csv
filepath = "data/kerfuffle/backup/kerfuffle_fantasy_scraperesult.csv"
newmaster.to_csv(masterFile, index=False)
dfGlobal.to_csv(filepath, index=False)

#ending print outs
# print(dfGlobal)
print("\nstored file in location {}".format(filepath))
print("\n\nkerfuffle fantasy script complete.")
