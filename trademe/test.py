# Import libraries
import urllib2
import json

from bs4 import BeautifulSoup

# specify the url

# One agent
# url = 'https://www.trademe.co.nz/property/residential-property-for-sale/auction-1942084072.htm?rsqid=6232a754952b4568b253870d8d101e23'

# # Two agents
url = 'https://www.trademe.co.nz/property/residential-property-for-sale/auction-1958149572.htm?rsqid=759654c68ac2495ca2bbe43c5529e188'

# query the website and return the html to the variable 'page'
page = urllib2.urlopen(url)

# parse the html using beautiful soup and store in soup
soup = BeautifulSoup(page, 'html.parser')

# agent_details = soup.find('section', attrs={'id': 'ClassifiedActions_PropertyAgentProfiles'})

agent_container = soup.find('div', attrs={'id': 'ClassifiedActions_AgentDetails'})
agency_details = agent_container.find('div', attrs={'class': 'agency'})

print agency_details.prettify()

agent_details = agent_container.find_all('div', attrs={'class': 'AgentDirectoryDetails'})
for agent in agent_details:
    print agent.prettify()
    
    

# photo_link
# phone_number
