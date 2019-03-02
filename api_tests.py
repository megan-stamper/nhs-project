########### Python 2.7 #############
import httplib, urllib, xmltodict

headers = {
    # Request headers
    'subscription-key': 'e2bacd9a7fe54f719fd26abf47cd580c',
    }

organisation = 'dentists'
organisationID = 45826
page = 'overview'

conn = httplib.HTTPSConnection('api.nhs.uk')
conn.request("GET", "/data/%s/%s/%s" % (organisation, organisationID, page), "{body}", headers)
response = conn.getresponse()
data = response.read()
print(data)
conn.close()

data_parsed = xmltodict.parse(data)
practice_name = data_parsed['feed']['entry']['content']['s:overview']['s:name']['#text']
print(practice_name)

