import requests
import pandas as pd
import numpy as np
import re
from bs4 import BeautifulSoup

#separates the column names
#returns list representing the columns for tables 
def separateColumns(row):
  allCols = []
  for i in row:
    if len(i.getText()) > 0:
      if "." in i.getText():
        allCols.append(i.getText().split(".")[1].strip())
      else:
        allCols.append(i.getText())
  return allCols

#takes in a single row of html and returns the stats as list
def separatePlayers(row):
  itr = 0
  curRow = []
  for i in row:
    if itr == 0:
      nameTeam = i.getText().split(",")
      team = nameTeam[-1].strip()
      name = nameTeam[0]
      cleanName = name.split(' ', 1)[-1]
      curRow.append(cleanName)
    else:
      if len(i.getText()) > 0:
        curRow.append(i.getText())
    itr += 1
  return curRow

def runXTD():
  cookies = {
    'edition': 'espn-en-us',
    'edition-view': 'espn-en-us',
    'country': 'us',
    'region': 'ccpa',
    '_dcf': '1',
    'SWID': '{D9BC25C4-FB05-4521-BC25-C4FB0505218C}',
    's_ensCDS': '0',
    's_ensRegion': 'ccpa',
    'AMCV_EE0201AC512D2BE80A490D4C%40AdobeOrg': '-330454231%7CMCIDTS%7C19257%7CMCMID%7C67149640962320798353113185715890725900%7CMCAID%7CNONE%7CMCOPTOUT-1663787934s%7CNONE%7CMCAAMLH-1664385534%7C7%7CMCAAMB-1664385534%7Cj8Odv6LonN4r3an7LhD3WZrU1bUpAkFkkiY1ncBR96t2PTI%7CvVersion%7C3.1.2',
    'cookieMonster': '1',
    '_omnicwtest': 'works',
    'connectionspeed': 'full',
    's_ecid': 'MCMID%7C67149640962320798353113185715890725900',
    'AMCVS_EE0201AC512D2BE80A490D4C%40AdobeOrg': '1',
    'client_type': 'html5',
    'client_version': '4.6.0',
    'OptanonConsent': 'isIABGlobal=false&datestamp=Wed+Sep+21+2022+13%3A19%3A13+GMT-0400+(Eastern+Daylight+Time)&version=6.31.0&hosts=&consentId=bfd2c56c-d8ef-41f1-afe8-e45f71eb69fb&interactionCount=1&landingPath=NotLandingPage&groups=C0001%3A1%2CC0003%3A1%2CSPD_BG%3A1%2CC0004%3A1%2CC0002%3A1&AwaitingReconsent=false',
    's_c24': '1663780753025',
    's_omni_lid': '%5B%5BB%5D%5D',
    's_cc': 'true',
    'check': 'true',
    'tveAuth': '',
    'tveMVPDAuth': '',
    'mbox': 'session#06d6544b111149839b1c02a33637a965#1663782612|PC#06d6544b111149839b1c02a33637a965.34_0#1727025486',
    'userab': '48',
    'IR_gbd': 'espn.com',
    'IR_9070': '1663780754201%7C0%7C1663780754201%7C%7C',
    '_cb': 'CbBxrtCVbGg3CcjI1Z',
    '_chartbeat2': '.1663780687039.1663780754075.1.BPXl09FY61jDboSOZDYdaUuDFjC2G.2',
    '_fbp': 'fb.1.1663780687332.352122500',
    '__gads': 'ID=cae9cc58870c46e1-22d4c1e344d7003f:T=1663780686:S=ALNI_MY9KjERLN2j1aBKoLrkJ5mollsAOg',
    '__gpi': 'UID=0000086976b6144f:T=1663780686:RT=1663780686:S=ALNI_MZU4TmDN1HXUNhNE7MeAm4HV8gRnQ',
    'espnCreative138271411242': 'true',
    '_gcl_au': '1.1.734141533.1663780688',
    's_sq': '%5B%5BB%5D%5D',
    '_uetsid': '5d6be74039d111eda155b128521e6b42',
    '_uetvid': '5d6bd81039d111edb3dd55ac83dfa8a1',
    '_scid': 'b7d4d681-4fd4-49b5-bc34-ae532d11ea81',
    'ESPN-ONESITE.WEB-PROD.api': 'LEs5ZI5AnzkflxHoJZF1U8TFo9oJ110FkngzQcupGGJmAh1o0T1abvuq+5biolIz30ftrEskbYWcYnSkl9DD4JUhl073',
    'device_47f98427': '56128db4-9bf2-423d-aabb-8d576b7789fd',
    'espn_s2': 'AECEiSjWE0EjjriOII1j3BZSRYHqXXgNbcbMff0xi7pcySw6GSesEqnussezLRIj1wYtfWXWGbv74F1aoXgdewzFB2BdJRBtUA6Q8Une3E6DDVd7%2BC4j8Z10lR0PkodvUoOlS1lYCiJbUcas8YLnodbZ12dtPrlP%2BkofnfBdvdtaNTHADwQ6qfvgsM%2FoTV15rc5zCea%2BQZygg%2Bx9W3m9U7mx1H0jlT3QgNxOUSH6kmKAGDqyU6KLeqhdzXF4p7ZzYas6DVwkjYHi%2FxCduFhJ4GlI',
    'ESPN-ONESITE.WEB-PROD.token': '5=eyJhY2Nlc3NfdG9rZW4iOiI3NDg3YTIxZDNjZTQ0ZmEwYmI3YTlkNzUwNjcxYzM5NSIsInJlZnJlc2hfdG9rZW4iOiI0YmYwNjcyNzE5ZDI0ODA1ODE4ZDQxYTYxNGNjZGFmNSIsInN3aWQiOiJ7RDlCQzI1QzQtRkIwNS00NTIxLUJDMjUtQzRGQjA1MDUyMThDfSIsInR0bCI6ODY0MDAsInJlZnJlc2hfdHRsIjoxNTU1MjAwMCwiaGlnaF90cnVzdF9leHBpcmVzX2luIjoxNzk5LCJpbml0aWFsX2dyYW50X2luX2NoYWluX3RpbWUiOjE2NjM3ODA3MjEyNDcsImlhdCI6MTY2Mzc4MDcyMTAwMCwiZXhwIjoxNjYzODY3MTIxMDAwLCJyZWZyZXNoX2V4cCI6MTY3OTMzMjcyMTAwMCwiaGlnaF90cnVzdF9leHAiOjE2NjM3ODI1MjAwMDAsInNzbyI6bnVsbCwiYXV0aGVudGljYXRvciI6ImRpc25leWlkIiwibG9naW5WYWx1ZSI6bnVsbCwiY2xpY2tiYWNrVHlwZSI6bnVsbCwic2Vzc2lvblRyYW5zZmVyS2V5IjoiU1VCdG5RT0REWlBCcTE1VkFRVmt5VGdwOVg5bC80QWxlSzNkQlNlakcwTEx6ZnBlLzlCUVg4SmRFd0YrbkhUK2lYYVkrbWhBaXVnYitqRXI1WFBnWHNjK1lGNFZxbEJJY1RCVlBhUHNVU1ZRa1Z2VkE3OD0iLCJjcmVhdGVkIjoiMjAyMi0wOS0yMVQxNzoxODo0MS45MzlaIiwibGFzdENoZWNrZWQiOiIyMDIyLTA5LTIxVDE3OjE4OjQxLjkzOVoiLCJleHBpcmVzIjoiMjAyMi0wOS0yMlQxNzoxODo0MS4wMDBaIiwicmVmcmVzaF9leHBpcmVzIjoiMjAyMy0wMy0yMFQxNzoxODo0MS4wMDBaIn0=|eyJraWQiOiJxUEhmditOL0tONE1zYnVwSE1PWWxBc0pLcWVaS1U2Mi9DZjNpSm1uOEJ6dzlwSW5xbTVzUnc9PSIsImFsZyI6IlJTMjU2In0.eyJpc3MiOiJodHRwczovL2F1dGhvcml6YXRpb24uZ28uY29tIiwic3ViIjoie0Q5QkMyNUM0LUZCMDUtNDUyMS1CQzI1LUM0RkIwNTA1MjE4Q30iLCJhdWQiOiJFU1BOLU9ORVNJVEUuV0VCLVBST0QiLCJleHAiOjE2NjM4NjcxMjEsImlhdCI6MTY2Mzc4MDcyMSwianRpIjoiMEdnSFA3aHdfM2V6TEFtODZQd2hkQSIsIm5iZiI6MTY2Mzc4MDY2MSwiYV90eXAiOiJPTkVJRF9UUlVTVEVEIiwiYV9jYXQiOiJHVUVTVCIsImF0ciI6ImRpc25leWlkIiwic2NvcGVzIjpbImR0c3MtZW50aXRsZW1lbnQtdXNlci1hZG1pbiIsIkFVVEhaX0dVRVNUX1NFQ1VSRURfU0VTU0lPTiJdLCJjX3RpZCI6IjEzMjQiLCJpZ2ljIjoxNjYzNzgwNzIxMjQ3LCJodGF2IjoyLCJodGQiOjE4MDAsInJ0dGwiOjE1NTUyMDAwLCJlbWFpbCI6Ijc3cm1zNzdAZ21haWwuY29tIn0.LGfPQUa9nxFbcFO30dLDb5tyClk3dhyFHeFd75843ADKJTlU73yGMzrqAX5T6CsyvAi8YhbKeBsmeefZcp7N3dCMEnxKsLCF3bVyQALNe_RmFc32wbt9uh0y4XJpTQCYrd9lXpipOyZwP6kQl-MliF2AFJgaYaOLrkz-SN89_hFgCQVeuBT6QB0XJG8aYhv93FUxi7km4Ye2upYI3Wcqf09quqNmoksYiDoSDpyEcKncGMONic-CL6Dpfd71ZGVLqqXKrGP-OYI5xjSIh0D6UMS1hv_iJouQJOSlW2q4GJ2h3NUwoSVTDN0m1zTdciiov5jJPwvAgaVtxmuMhIUtDg',
    'ESPN-ONESITE.WEB-PROD-ac': 'XUS',
    'ESPN-ONESITE.WEB-PROD.idn': '0035acb9c8',
    's_nr': '1663780723128-New',
    'IR_PI': '2b48ff62-95cc-3c86-bdf1-7fbe1763f618%7C1663867090145',
    'usprivacy': '1YNY',
    'userZip': '48083',
    'country': 'us',
    'hashedIp': '3fb0fd14eef20a0d1e447c1ba0948c972312556ffbe6bd2e7bb0101ed290e098',
    'OneTrustWPCCPAGoogleOptOut': 'false',
    'SWID_NT': '0',
    'dtcAuth': 'ESPN_PLUS',
    'optimizelyEndUserId': 'oeu1663780732745ds.98852fdd1e1a9581',
    'ab.storage.userId.96ad02b7-2edc-4238-8442-bc35ba85853c': '%7B%22g%22%3A%22%7BD9BC25C4-FB05-4521-BC25-C4FB0505218C%7D%22%2C%22c%22%3A1663780734770%2C%22l%22%3A1663780734770%7D',
    'ab.storage.sessionId.96ad02b7-2edc-4238-8442-bc35ba85853c': '%7B%22g%22%3A%2263bdac1c-8009-c7bc-1171-340a94c548f2%22%2C%22e%22%3A1663782554271%2C%22c%22%3A1663780734771%2C%22l%22%3A1663780754271%7D',
    'ab.storage.deviceId.96ad02b7-2edc-4238-8442-bc35ba85853c': '%7B%22g%22%3A%2287c59eca-6c97-6e2a-4a5a-17f3f9be38cd%22%2C%22c%22%3A1663780734772%2C%22l%22%3A1663780734772%7D',
    'espnAuth': '{swid:{D9BC25C4-FB05-4521-BC25-C4FB0505218C}}',
  }
  
  headers = {
      'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:100.0) Gecko/20100101 Firefox/100.0',
      'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8',
      'Accept-Language': 'en-US,en;q=0.5',
      # 'Accept-Encoding': 'gzip, deflate, br',
      'Connection': 'keep-alive',
      # Requests sorts cookies= alphabetically
      # 'Cookie': 'edition=espn-en-us; edition-view=espn-en-us; country=us; region=ccpa; _dcf=1; SWID={D9BC25C4-FB05-4521-BC25-C4FB0505218C}; s_ensCDS=0; s_ensRegion=ccpa; AMCV_EE0201AC512D2BE80A490D4C%40AdobeOrg=-330454231%7CMCIDTS%7C19257%7CMCMID%7C67149640962320798353113185715890725900%7CMCAID%7CNONE%7CMCOPTOUT-1663787934s%7CNONE%7CMCAAMLH-1664385534%7C7%7CMCAAMB-1664385534%7Cj8Odv6LonN4r3an7LhD3WZrU1bUpAkFkkiY1ncBR96t2PTI%7CvVersion%7C3.1.2; cookieMonster=1; _omnicwtest=works; connectionspeed=full; s_ecid=MCMID%7C67149640962320798353113185715890725900; AMCVS_EE0201AC512D2BE80A490D4C%40AdobeOrg=1; client_type=html5; client_version=4.6.0; OptanonConsent=isIABGlobal=false&datestamp=Wed+Sep+21+2022+13%3A19%3A13+GMT-0400+(Eastern+Daylight+Time)&version=6.31.0&hosts=&consentId=bfd2c56c-d8ef-41f1-afe8-e45f71eb69fb&interactionCount=1&landingPath=NotLandingPage&groups=C0001%3A1%2CC0003%3A1%2CSPD_BG%3A1%2CC0004%3A1%2CC0002%3A1&AwaitingReconsent=false; s_c24=1663780753025; s_omni_lid=%5B%5BB%5D%5D; s_cc=true; check=true; tveAuth=; tveMVPDAuth=; mbox=session#06d6544b111149839b1c02a33637a965#1663782612|PC#06d6544b111149839b1c02a33637a965.34_0#1727025486; userab=48; IR_gbd=espn.com; IR_9070=1663780754201%7C0%7C1663780754201%7C%7C; _cb=CbBxrtCVbGg3CcjI1Z; _chartbeat2=.1663780687039.1663780754075.1.BPXl09FY61jDboSOZDYdaUuDFjC2G.2; _fbp=fb.1.1663780687332.352122500; __gads=ID=cae9cc58870c46e1-22d4c1e344d7003f:T=1663780686:S=ALNI_MY9KjERLN2j1aBKoLrkJ5mollsAOg; __gpi=UID=0000086976b6144f:T=1663780686:RT=1663780686:S=ALNI_MZU4TmDN1HXUNhNE7MeAm4HV8gRnQ; espnCreative138271411242=true; _gcl_au=1.1.734141533.1663780688; s_sq=%5B%5BB%5D%5D; _uetsid=5d6be74039d111eda155b128521e6b42; _uetvid=5d6bd81039d111edb3dd55ac83dfa8a1; _scid=b7d4d681-4fd4-49b5-bc34-ae532d11ea81; ESPN-ONESITE.WEB-PROD.api=LEs5ZI5AnzkflxHoJZF1U8TFo9oJ110FkngzQcupGGJmAh1o0T1abvuq+5biolIz30ftrEskbYWcYnSkl9DD4JUhl073; device_47f98427=56128db4-9bf2-423d-aabb-8d576b7789fd; espn_s2=AECEiSjWE0EjjriOII1j3BZSRYHqXXgNbcbMff0xi7pcySw6GSesEqnussezLRIj1wYtfWXWGbv74F1aoXgdewzFB2BdJRBtUA6Q8Une3E6DDVd7%2BC4j8Z10lR0PkodvUoOlS1lYCiJbUcas8YLnodbZ12dtPrlP%2BkofnfBdvdtaNTHADwQ6qfvgsM%2FoTV15rc5zCea%2BQZygg%2Bx9W3m9U7mx1H0jlT3QgNxOUSH6kmKAGDqyU6KLeqhdzXF4p7ZzYas6DVwkjYHi%2FxCduFhJ4GlI; ESPN-ONESITE.WEB-PROD.token=5=eyJhY2Nlc3NfdG9rZW4iOiI3NDg3YTIxZDNjZTQ0ZmEwYmI3YTlkNzUwNjcxYzM5NSIsInJlZnJlc2hfdG9rZW4iOiI0YmYwNjcyNzE5ZDI0ODA1ODE4ZDQxYTYxNGNjZGFmNSIsInN3aWQiOiJ7RDlCQzI1QzQtRkIwNS00NTIxLUJDMjUtQzRGQjA1MDUyMThDfSIsInR0bCI6ODY0MDAsInJlZnJlc2hfdHRsIjoxNTU1MjAwMCwiaGlnaF90cnVzdF9leHBpcmVzX2luIjoxNzk5LCJpbml0aWFsX2dyYW50X2luX2NoYWluX3RpbWUiOjE2NjM3ODA3MjEyNDcsImlhdCI6MTY2Mzc4MDcyMTAwMCwiZXhwIjoxNjYzODY3MTIxMDAwLCJyZWZyZXNoX2V4cCI6MTY3OTMzMjcyMTAwMCwiaGlnaF90cnVzdF9leHAiOjE2NjM3ODI1MjAwMDAsInNzbyI6bnVsbCwiYXV0aGVudGljYXRvciI6ImRpc25leWlkIiwibG9naW5WYWx1ZSI6bnVsbCwiY2xpY2tiYWNrVHlwZSI6bnVsbCwic2Vzc2lvblRyYW5zZmVyS2V5IjoiU1VCdG5RT0REWlBCcTE1VkFRVmt5VGdwOVg5bC80QWxlSzNkQlNlakcwTEx6ZnBlLzlCUVg4SmRFd0YrbkhUK2lYYVkrbWhBaXVnYitqRXI1WFBnWHNjK1lGNFZxbEJJY1RCVlBhUHNVU1ZRa1Z2VkE3OD0iLCJjcmVhdGVkIjoiMjAyMi0wOS0yMVQxNzoxODo0MS45MzlaIiwibGFzdENoZWNrZWQiOiIyMDIyLTA5LTIxVDE3OjE4OjQxLjkzOVoiLCJleHBpcmVzIjoiMjAyMi0wOS0yMlQxNzoxODo0MS4wMDBaIiwicmVmcmVzaF9leHBpcmVzIjoiMjAyMy0wMy0yMFQxNzoxODo0MS4wMDBaIn0=|eyJraWQiOiJxUEhmditOL0tONE1zYnVwSE1PWWxBc0pLcWVaS1U2Mi9DZjNpSm1uOEJ6dzlwSW5xbTVzUnc9PSIsImFsZyI6IlJTMjU2In0.eyJpc3MiOiJodHRwczovL2F1dGhvcml6YXRpb24uZ28uY29tIiwic3ViIjoie0Q5QkMyNUM0LUZCMDUtNDUyMS1CQzI1LUM0RkIwNTA1MjE4Q30iLCJhdWQiOiJFU1BOLU9ORVNJVEUuV0VCLVBST0QiLCJleHAiOjE2NjM4NjcxMjEsImlhdCI6MTY2Mzc4MDcyMSwianRpIjoiMEdnSFA3aHdfM2V6TEFtODZQd2hkQSIsIm5iZiI6MTY2Mzc4MDY2MSwiYV90eXAiOiJPTkVJRF9UUlVTVEVEIiwiYV9jYXQiOiJHVUVTVCIsImF0ciI6ImRpc25leWlkIiwic2NvcGVzIjpbImR0c3MtZW50aXRsZW1lbnQtdXNlci1hZG1pbiIsIkFVVEhaX0dVRVNUX1NFQ1VSRURfU0VTU0lPTiJdLCJjX3RpZCI6IjEzMjQiLCJpZ2ljIjoxNjYzNzgwNzIxMjQ3LCJodGF2IjoyLCJodGQiOjE4MDAsInJ0dGwiOjE1NTUyMDAwLCJlbWFpbCI6Ijc3cm1zNzdAZ21haWwuY29tIn0.LGfPQUa9nxFbcFO30dLDb5tyClk3dhyFHeFd75843ADKJTlU73yGMzrqAX5T6CsyvAi8YhbKeBsmeefZcp7N3dCMEnxKsLCF3bVyQALNe_RmFc32wbt9uh0y4XJpTQCYrd9lXpipOyZwP6kQl-MliF2AFJgaYaOLrkz-SN89_hFgCQVeuBT6QB0XJG8aYhv93FUxi7km4Ye2upYI3Wcqf09quqNmoksYiDoSDpyEcKncGMONic-CL6Dpfd71ZGVLqqXKrGP-OYI5xjSIh0D6UMS1hv_iJouQJOSlW2q4GJ2h3NUwoSVTDN0m1zTdciiov5jJPwvAgaVtxmuMhIUtDg; ESPN-ONESITE.WEB-PROD-ac=XUS; ESPN-ONESITE.WEB-PROD.idn=0035acb9c8; s_nr=1663780723128-New; IR_PI=2b48ff62-95cc-3c86-bdf1-7fbe1763f618%7C1663867090145; usprivacy=1YNY; userZip=48083; country=us; hashedIp=3fb0fd14eef20a0d1e447c1ba0948c972312556ffbe6bd2e7bb0101ed290e098; OneTrustWPCCPAGoogleOptOut=false; SWID_NT=0; dtcAuth=ESPN_PLUS; optimizelyEndUserId=oeu1663780732745ds.98852fdd1e1a9581; ab.storage.userId.96ad02b7-2edc-4238-8442-bc35ba85853c=%7B%22g%22%3A%22%7BD9BC25C4-FB05-4521-BC25-C4FB0505218C%7D%22%2C%22c%22%3A1663780734770%2C%22l%22%3A1663780734770%7D; ab.storage.sessionId.96ad02b7-2edc-4238-8442-bc35ba85853c=%7B%22g%22%3A%2263bdac1c-8009-c7bc-1171-340a94c548f2%22%2C%22e%22%3A1663782554271%2C%22c%22%3A1663780734771%2C%22l%22%3A1663780754271%7D; ab.storage.deviceId.96ad02b7-2edc-4238-8442-bc35ba85853c=%7B%22g%22%3A%2287c59eca-6c97-6e2a-4a5a-17f3f9be38cd%22%2C%22c%22%3A1663780734772%2C%22l%22%3A1663780734772%7D; espnAuth={swid:{D9BC25C4-FB05-4521-BC25-C4FB0505218C}}',
      'Upgrade-Insecure-Requests': '1',
      'Sec-Fetch-Dest': 'document',
      'Sec-Fetch-Mode': 'navigate',
      'Sec-Fetch-Site': 'none',
      'Sec-Fetch-User': '?1',
      'If-None-Match': 'W/512483eb531a053b813e1053b334fca28b487287',
  }
  
  response = requests.get('https://www.espn.com/fantasy/insider/football/insider/story/_/id/34585389/fantasy-football-rankings-nfl-expected-touchdowns-opportunity-2022', cookies=cookies, headers=headers)
  soup = BeautifulSoup(response.content, 'html.parser')
  
  complete =  soup.find("section", {"id": "article-feed"})
  combined = complete.find_all("aside", class_="inline inline-table")
  tbls = combined[1].find("table")
  headers = tbls.find_all("th")
  #using scraped data create columns/players then combine into pandas dataframe :)
  
  colHeaders = separateColumns(headers)
  allPlayers = []
  
  allRows = tbls.find_all("tr", class_="last")
  
  for i in allRows:
    allPlayers.append(separatePlayers(i))
    
    
  #pandas df to represent team
  df = pd.DataFrame(allPlayers, columns=colHeaders)

  df.to_pickle("dre/ESPN/xtd.pkl")
  
  
def main():
  runXTD()

if __name__ == "__main__":
  main()

