#this is a python script.
#currently POC concept. 

import os
import requests
import datetime
import pandas as pd
import numpy as np
import re
from bs4 import BeautifulSoup
from bs4 import NavigableString


#below are 'keys' of some sort from copy and pasting - unclear how they fit in left for reference

#_sn:90$_ss:0$_st:1651846165170$vapi_domain:cbssports.com$dc_visit:34$_se:8$ses_id:1651842081415%3Bexp-session$_pn:7%3Bexp-session$dc_event:12%3Bexp-session$dc_region:us-east-1%3Bexp-session = os.getenv('_sn:90$_ss:0$_st:1651846165170$vapi_domain:cbssports.com$dc_visit:34$_se:8$ses_id:1651842081415%3Bexp-session$_pn:7%3Bexp-session$dc_event:12%3Bexp-session$dc_region:us-east-1%3Bexp-session')
#_sn:90$_ss:0$_st:1651846165170$vapi_domain:cbssports.com$dc_visit:34$_se:8$ses_id:1651842081415%3Bexp-session$_pn:7%3Bexp-session$dc_event:12%3Bexp-session$dc_region:us-east-1%3Bexp-session; = os.getenv('_sn:90$_ss:0$_st:1651846165170$vapi_domain:cbssports.com$dc_visit:34$_se:8$ses_id:1651842081415%3Bexp-session$_pn:7%3Bexp-session$dc_event:12%3Bexp-session$dc_region:us-east-1%3Bexp-session;')

#cookies from website (https://curlconverter.com/#python)
cookies = {
    'ppid': 'bf794872c9de88772307c75ceb0df52d',
    'anon': 'FALSE',
    '_admrla': '2.',
    '_ga': 'GA1.2.1672772832.1599155327',
    's_ecid': 'MCMID%7C82547878818585342582963872258268284067',
    'AAMC_cbsi_0': 'REGION%7C7',
    '_pubcid': '970a1c39-1681-4966-a885-2a4f3a08fff1',
    'fly_ab_uid': '74c6bc7e-15a4-4de0-bccc-244a5382d2a3',
    '_fbp': 'fb.1.1627519029382.448829092',
    '_cc_id': '89fa3237cd8f8ccd38e26ffcd348b802',
    '__gads': 'ID=808c4e442a5e0371:T=1635787656:S=ALNI_MbNJVKIiw9QIOojjH5Mj7n4ZKQh8g',
    'cbsiaa': '47117081',
    '_gcl_au': '1.1.1353310833.1645018619',
    'aam_uuid': '82757680018916183142987646788512285147',
    'fantasy_dirty': '1651599323',
    'pid': 'L%3A14%3AMTPixsYXU4UIxrf4Zs4peQ%253D%253D%3A1',
    'aamgam': 'segid%3D17873358%2C17873364%2C18265430%2C16185172%2C16007068%2C15463662%2C14334689%2C13100185%2C12735092%2C12734882%2C12681061%2C12680596%2C1608821%2C1608815%2C13100219',
    'fly_geo': '{"countryCode":"us","region":"mi"}',
    'region_code': 'MI',
    'surround': 'e|2',
    '_BB.bs': 'e|2',
    'AMCVS_10D31225525FF5790A490D4D%40AdobeOrg': '1',
    's_cc': 'true',
    'fly_device': 'desktop',
    'AMCV_10D31225525FF5790A490D4D%40AdobeOrg': '-1712354808%7CMCIDTS%7C19119%7CMCMID%7C82547878818585342582963872258268284067%7CMCAAMLH-1652447427%7C7%7CMCAAMB-1652447427%7CRKhpRz8krg2tLO6pguXWp5olkAcUniQYPHaMWWgdJ3xzPWQmdj0y%7CMCOPTOUT-1651849827s%7CNONE%7CMCAID%7CNONE%7CvVersion%7C4.3.0%7CMCCIDH%7C-1686719201',
    'fly-session': '193pgn5l07agkhmsdkd7rigagn',
    'fly_tracking': '%7B%22userId%22%3A47117081%2C%22userState%22%3A%22authenticated%22%2C%22userType%22%3A%22registered%22%7D',
    'sports_user': '712b3cd9674f1deac7bc8080f7f7095b64380ae19534be747e38daf7b774a724',
    '_gid': 'GA1.2.462645314.1651842628',
    '_awl': '2.1651842628.0.5-e433cd9c77306797c88205bc9e8d856f-6763652d75732d63656e7472616c31-7',
    '__gpi': 'UID=000004a9e8eeacff:T=1650580974:RT=1651842628:S=ALNI_MboWZz7b1G4Fa76DZbZFMvjIyuAtg',
    'panoramaId_expiry': '1652447431426',
    'panoramaId': '6d82685df667cb37034f31ea6aec4945a702704aa7585124b2969b8a40768c74',
    'RT': '"z=1&dm=cbssports.com&si=b9df2fa3-220c-4ad8-b659-7a3652cfd527&ss=l2ugg8aa&sl=1&tt=2mc&bcn=%2F%2Fb8a9a27e.akstat.io%2F&ul=2mc&ld=3dj&hd=3mx"',
    'XFP_FIRSTPAGE': '0',
    'last_access': '1651844364',
    '_BB.d': '0|||6',
    'OptanonAlertBoxClosed': '2022-05-06T13:39:24.887Z',
    'OptanonConsent': 'isIABGlobal=false&datestamp=Fri+May+06+2022+09%3A39%3A25+GMT-0400+(Eastern+Daylight+Time)&version=6.30.0&hosts=&consentId=fae818b1-7561-493d-a0ea-b201fc041f2c&interactionCount=1&landingPath=NotLandingPage&groups=1%3A1%2C2%3A1%2C3%3A1%2C4%3A1%2C5%3A1&AwaitingReconsent=false&geolocation=US%3BMI&isGpcEnabled=0',
    's_sq': '%5B%5BB%5D%5D',
    #'utag_main': f"v_id:01745515cdbe00460086bbf910c803072004f06a00fb8{_sn:90$_ss:0$_st:1651846165170$vapi_domain:cbssports.com$dc_visit:34$_se:8$ses_id:1651842081415%3Bexp-session$_pn:7%3Bexp-session$dc_event:12%3Bexp-session$dc_region:us-east-1%3Bexp-session}",
    'fantasy_cookie': '%3A8000%3A1651844215%3Atheradicalultimatefflexperience.football.cbssports.com',
}

#headers from website (https://curlconverter.com/#python)
headers = {
    'authority': 'theradicalultimatefflexperience.football.cbssports.com',
    'accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
    'accept-language': 'en-US,en;q=0.9',
    'cache-control': 'max-age=0',
    # Requests sorts cookies= alphabetically
    # 'cookie': f"ppid=bf794872c9de88772307c75ceb0df52d; anon=FALSE; _admrla=2.; _ga=GA1.2.1672772832.1599155327; s_ecid=MCMID%7C82547878818585342582963872258268284067; AAMC_cbsi_0=REGION%7C7; _pubcid=970a1c39-1681-4966-a885-2a4f3a08fff1; fly_ab_uid=74c6bc7e-15a4-4de0-bccc-244a5382d2a3; _fbp=fb.1.1627519029382.448829092; _cc_id=89fa3237cd8f8ccd38e26ffcd348b802; __gads=ID=808c4e442a5e0371:T=1635787656:S=ALNI_MbNJVKIiw9QIOojjH5Mj7n4ZKQh8g; cbsiaa=47117081; _gcl_au=1.1.1353310833.1645018619; aam_uuid=82757680018916183142987646788512285147; fantasy_dirty=1651599323; pid=L%3A14%3AMTPixsYXU4UIxrf4Zs4peQ%253D%253D%3A1; aamgam=segid%3D17873358%2C17873364%2C18265430%2C16185172%2C16007068%2C15463662%2C14334689%2C13100185%2C12735092%2C12734882%2C12681061%2C12680596%2C1608821%2C1608815%2C13100219; fly_geo={\"countryCode\":\"us\",\"region\":\"mi\"}; region_code=MI; surround=e|2; _BB.bs=e|2; AMCVS_10D31225525FF5790A490D4D%40AdobeOrg=1; s_cc=true; fly_device=desktop; AMCV_10D31225525FF5790A490D4D%40AdobeOrg=-1712354808%7CMCIDTS%7C19119%7CMCMID%7C82547878818585342582963872258268284067%7CMCAAMLH-1652447427%7C7%7CMCAAMB-1652447427%7CRKhpRz8krg2tLO6pguXWp5olkAcUniQYPHaMWWgdJ3xzPWQmdj0y%7CMCOPTOUT-1651849827s%7CNONE%7CMCAID%7CNONE%7CvVersion%7C4.3.0%7CMCCIDH%7C-1686719201; fly-session=193pgn5l07agkhmsdkd7rigagn; fly_tracking=%7B%22userId%22%3A47117081%2C%22userState%22%3A%22authenticated%22%2C%22userType%22%3A%22registered%22%7D; sports_user=712b3cd9674f1deac7bc8080f7f7095b64380ae19534be747e38daf7b774a724; _gid=GA1.2.462645314.1651842628; _awl=2.1651842628.0.5-e433cd9c77306797c88205bc9e8d856f-6763652d75732d63656e7472616c31-7; __gpi=UID=000004a9e8eeacff:T=1650580974:RT=1651842628:S=ALNI_MboWZz7b1G4Fa76DZbZFMvjIyuAtg; panoramaId_expiry=1652447431426; panoramaId=6d82685df667cb37034f31ea6aec4945a702704aa7585124b2969b8a40768c74; RT=\"z=1&dm=cbssports.com&si=b9df2fa3-220c-4ad8-b659-7a3652cfd527&ss=l2ugg8aa&sl=1&tt=2mc&bcn=%2F%2Fb8a9a27e.akstat.io%2F&ul=2mc&ld=3dj&hd=3mx\"; XFP_FIRSTPAGE=0; last_access=1651844364; _BB.d=0|||6; OptanonAlertBoxClosed=2022-05-06T13:39:24.887Z; OptanonConsent=isIABGlobal=false&datestamp=Fri+May+06+2022+09%3A39%3A25+GMT-0400+(Eastern+Daylight+Time)&version=6.30.0&hosts=&consentId=fae818b1-7561-493d-a0ea-b201fc041f2c&interactionCount=1&landingPath=NotLandingPage&groups=1%3A1%2C2%3A1%2C3%3A1%2C4%3A1%2C5%3A1&AwaitingReconsent=false&geolocation=US%3BMI&isGpcEnabled=0; s_sq=%5B%5BB%5D%5D; utag_main=v_id:01745515cdbe00460086bbf910c803072004f06a00fb8{_sn:90$_ss:0$_st:1651846165170$vapi_domain:cbssports.com$dc_visit:34$_se:8$ses_id:1651842081415%3Bexp-session$_pn:7%3Bexp-session$dc_event:12%3Bexp-session$dc_region:us-east-1%3Bexp-session;} fantasy_cookie=%3A8000%3A1651844215%3Atheradicalultimatefflexperience.football.cbssports.com",
    'referer': 'https://theradicalultimatefflexperience.football.cbssports.com/teams',
    'sec-ch-ua': '" Not A;Brand";v="99", "Chromium";v="100", "Google Chrome";v="100"',
    'sec-ch-ua-mobile': '?0',
    'sec-ch-ua-platform': '"Windows"',
    'sec-fetch-dest': 'document',
    'sec-fetch-mode': 'navigate',
    'sec-fetch-site': 'same-origin',
    'sec-fetch-user': '?1',
    'upgrade-insecure-requests': '1',
    'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.127 Safari/537.36',
}

begin_time = datetime.datetime.now()

#get information from teams document to refer to for shortcuts ext
teamsPd = pd.read_csv("data/teams.csv")

teamsDict = {}
for index, row in teamsPd.iterrows():
  teamsDict[row["FullName"]] = row["Abbrev"]  

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
  
#takes in a single row of html and returns the stats as list (for players/dst)
def separatePlayers(rows):
  curRow = []
  for i in rows:
    curRow.append(i.getText())
  return curRow

#takes in a single row of html and returns the stats as list deadcap players only
def separatePlayersDeadCap(rows):
  itr = 0
  curRow = []
  for i in rows:
    if itr == 0:
      curRow.append("DC")
    else:
      curRow.append(i.getText())
    itr += 1
  return curRow

#separates the column names
#returns list representing the columns for tables 
def separateColumns(row):
  allCols = []
  for i in row:
    allCols.append(i.getText())
  allCols[1] = "Player"
  return allCols

getNFLTeam = lambda x: pd.Series([i.strip() for i in x.split("•")])
getPosition = lambda y: pd.Series([i for i in y.split(" ")][-1])
getPlayer = lambda z: pd.Series(' '.join([i for i in z.split(" ")][:-1]))
  
#where the connection is made to the truffle cbs website
url = "https://theradicalultimatefflexperience.football.cbssports.com/teams/all"
response = requests.get(url, cookies=cookies, headers=headers)
soup = BeautifulSoup(response.content, 'html.parser')

# complete =  soup.find("div", {"id": "sortableStats"})
tbls =  soup.findAll("table", {"class":"data data3 pinHeader borderTop"})
dfGlobal = pd.DataFrame()
counter = 0

for tbl in tbls:
  # print(tbl.prettify())
  tableRows = tbl.findAll('tr')
  
  teamName = ""
  cols = []
  players = []
  
  for row in tableRows:
    if row.has_attr('class') and row['class'][0] == "title":
      # print("THIS IS THE TEAM NAME")
      nameSplit = row.getText().split(" ")
      nameSplit.pop()
      teamName = ' '.join(nameSplit)
    elif row.has_attr('class') and row['class'][0] == "label" and len(row['class']) == 1:
      # print("THIS IS THE COLUMN HEADERS")
      cols = separateColumns(row)
    elif row.has_attr('class') and row['class'][0] == "playerRow" and len(row['class']) == 2:
      # print("THESE ARE ALL THE STARTING PLAYERS")
      players.append(separatePlayers(row))
    elif row.has_attr('class') and row['class'][0] == "playerRow" and len(row['class']) == 3:
      # print("THESE ARE ALL THE BENCHED PLAYERS")
      playerName = row.find('a').getText()
      if (playerName.split(" ")[-1] == "Cap"):
        players.append(separatePlayersDeadCap(row))
      else:
        players.append(separatePlayers(row))

  dfTeam = pd.DataFrame(players, columns = cols)
  dfTeam["TRUFFLE"] = getTeamAbbreviation(teamName)
  
  #apply lambda fcts to correct columns
  playerTeam = dfTeam["Player"].apply(getNFLTeam)
  position = playerTeam[0].apply(getPosition)
  player =  playerTeam[0].apply(getPlayer)
  nfl = pd.Series(playerTeam[1])
  
  #add/remove columns for TRUFFLE formatting
  dfTeam["Player"] = player
  #dfTeam.insert(0,"Season", season)
  #dfTeam.insert(1,"Week", week)
  dfTeam.insert(1,"PosTRUFFLE", position[0])
  dfTeam.insert(3,"NFL", nfl)
  
  dfTeam['Player'] = dfTeam['Player'].str.replace(r'.', '', regex=True)
  dfTeam['Player'] = dfTeam['Player'].str.replace(r' Jr', '', regex=True)
  dfTeam['Player'] = dfTeam['Player'].str.replace(r' Sr', '', regex=True)
  dfTeam['Player'] = dfTeam['Player'].str.replace(r' III', '', regex=True)
  dfTeam['Player'] = dfTeam['Player'].str.replace(r' II', '', regex=True)
  dfTeam['Player'] = dfTeam['Player'].str.replace(r'Will Fuller V', 'Will Fuller', regex=True)

  if counter == 0:
    dfGlobal = dfTeam
  else:
    dfGlobal = pd.concat([dfGlobal, dfTeam], ignore_index = True)
  counter += 1
  
  
for index, row in dfGlobal.loc[~(dfGlobal['Pos'] == dfGlobal['PosTRUFFLE'])].iterrows():
  if row['Pos'] == "DC":
    # print("DEAD CAP HARD CODE ASSIGN")
    dfGlobal.at[index, 'Pos'] = "DC"
  else:
    # print("NEED TO ASSIGN THE Pos = PosTRUFFLE")
    dfGlobal.at[index, 'Pos'] = row['PosTRUFFLE']
    
#shifting/deleting/sorting columns for formatting
truffle = dfGlobal.pop("TRUFFLE")
dfGlobal.insert(2,"TRUFFLE", truffle)
dfGlobal = dfGlobal.drop(["PosTRUFFLE"], axis=1)
# dfGlobal = dfGlobal.sort_values(by=['TRUFFLE'])

#create roster backup
#read the existing csv as a pd df for error checking
masterDf = pd.read_csv("data/rosters.csv")
#create backup copy
filepath = "data/backup/rosters_backup.csv"
masterDf.to_csv(filepath, index=False)

#save final csv
dfGlobal.to_csv("data/rosters.csv", index = False)

print(dfGlobal)
print("\n\nscript complete. execution time:")
print(datetime.datetime.now() - begin_time)
