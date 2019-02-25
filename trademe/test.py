# Import libraries
import urllib2
import json
import datetime
import re
import time
from bs4 import BeautifulSoup

dt = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

# specify the url
root = 'https://www.trademe.co.nz'

all_listings =[]

links = [
    'https://www.trademe.co.nz/property/residential-property-for-sale/auction-1963562081.htm?rsqid=76daa49dceb7489687\ede62c8a1e79ac',
    'https://www.trademe.co.nz/property/residential-property-for-sale/auction-1942084072.htm?rsqid=6232a754952b4568b253870d8d101e23',
    'https://www.trademe.co.nz/property/residential-property-for-sale/auction-1960723244.htm?rsqid=ec5f695abc5f4927b8cbed56055d355e',
    'https://www.trademe.co.nz/property/residential-property-for-sale/auction-1958149572.htm?rsqid=759654c68ac2495ca2bbe43c5529e188'
]
   
for link in links:

    url = link

    # query the website and return the html to the variable 'page'
    page = urllib2.urlopen(url)
                        
    # parse the html using beautiful soup and store in soup
    soup = BeautifulSoup(page, 'html.parser')
        
    attributes = soup.find('table', attrs={'id','ListingAttributes'})
    rows_list = attributes.find_all('tr')
        
    listing = {}
        
    description = soup.find('div', attrs={'class': 'ListingDescription'})

    listing['Description'] = description.text.encode('utf-8').strip()
    listing['Link'] = link
        
    for row in rows_list:
        
        key = row.find('th')
        value = key.next_sibling.next_sibling.text.encode('utf-8').strip()
        key = key.text.encode('utf-8').strip()
        listing[key] = value
                                                                                                

    agent_container = soup.find('div', attrs={'id': 'ClassifiedActions_AgentDetails'})

    ## Check if private sale
    if str(agent_container) == 'None':
        print 'private sale'
    else:

        ## Get agency details
    
        agency_details = agent_container.find('div', attrs={'class': 'agency'})

        agency_name = agency_details.find('div', attrs={'data-purpose': 'agency-name'}).text.encode('utf-8').strip()
        agency_reaa = agency_details.find('span', attrs={'data-purpose': 'agency-reaa'}).text.encode('utf-8').strip()
        agency_phone = agency_details.find('div', attrs={'id': 'ClassifiedActions_AgencyPhone'}).text.encode('utf-8').strip()

        listing['agency_name'] = agency_name
        listing['agency_reaa'] = agency_reaa
        listing['agency_phone'] = agency_phone

        listing['agents'] = []



        ## Annoying agents without profiles
        #########################################################################################

        agent_details = agent_container.find_all('div', attrs={'class': 'AgentContainer'})

        for agent in agent_details:

            profile = {}

            img_link = agent.find('img').get('src').strip()
            name = agent.find('div', attrs={'class': 'AgentName'}).text.encode('utf-8').strip()

            profile['name'] = name
            profile['img_link'] = img_link
            profile['phone'] = []
    
            phone_list = agent.find_all('li', attrs={'class': 'agent-phone-number'})
            for phone in phone_list:
                phone = phone.text.strip()
                profile['phone'].append(phone)

            listing['agents'].append(profile)



        ## Normal agents with profiles
        #########################################################################################

        agent_details = agent_container.find_all('div', attrs={'class': 'AgentDirectoryDetails'})

        for agent in agent_details:

            profile = {}
    
            img_link = agent.find('img').get('src').strip()
            name = agent.find('span').text.encode('utf-8').strip()
            profile_link = agent.find('a', attrs={'class': 'btn-agent-profile'}).get('href').strip()
            contact_time = agent.find('li', attrs={'data-purpose': 'contact-time'}).get('data-value').encode('utf-8').strip()

            profile['name'] = name
            profile['img_link'] = img_link
            profile['profile_link'] = profile_link
            profile['contact_time'] = contact_time
            profile['phone'] = []
    
            phone_list = agent.find_all('li', attrs={'class': 'agent-phone-number'})
            for phone in phone_list:
                phone = phone.get('data-value').encode('utf-8').strip()
                profile['phone'].append(phone)

            listing['agents'].append(profile)

        #########################################################################################

    
        print json.dumps(listing, indent = 4, sort_keys = True)

# print json.dumps(all_listing, indent = 4, sort_keys = True)

# with open('tradeMeDetail.json', 'w') as outfile:
#         json.dump(all_listings, outfile)
