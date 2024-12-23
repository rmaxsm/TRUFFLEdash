import json

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

# Save dictionary to a JSON file
with open('dre/kerfuffle_cookies.json', 'w') as file:
    json.dump(cookies, file)
with open('dre/kerfuffle_headers.json', 'w') as file:
    json.dump(headers, file)