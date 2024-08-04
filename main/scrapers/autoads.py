
import requests
from bs4 import BeautifulSoup

# Define the base URL and the number of pages you want to scrape
base_url = 'https://www.autoadsja.com/search.asp?SearchSB=5&page='
num_pages = 200 # Replace with the actual number of pages



class car:
  def __init__(self):
    self.year = None
    self.make = None
    self.model = None
    self.img_url = None
    self.price = None

cars = []

# Loop through the paginated pages
for page in range(1, num_pages + 1):

   
    # Construct the full URL for the current page
    url = f'{base_url}{page}'
    print(f'Fetching URL: {url}')
    
    # Send a GET request to the page
    response = requests.get(url)
    
    # Check if the request was successful
    if response.status_code == 200:
        # Parse the HTML content
        soup = BeautifulSoup(response.text, 'html.parser')

        # Find all car listings
        car_listings = soup.find_all('div', class_='thumbnail')

        
        # Extract make and model
        for listing in car_listings:          
           # Get thumbnail image
            new_car = car()
            img_tag_src = listing.find('img')['src']
            if img_tag_src:
               img_url = img_tag_src.strip()
               new_car.img_url = img_url

            #Get make model and year
            description = listing.find('div', class_='description')
            if description:
                span_tag = description.find('span', class_='visible-xs')
                if span_tag:
                    car_price = ''.join(filter(str.isdigit,span_tag.text.strip()))
                    new_car.price = car_price
                h2_tag = description.find('h2')
                if h2_tag:
                    car_info  = h2_tag.text.strip().split()
                    new_car.year  = car_info[0]
                    new_car.make  = car_info[1]
                    new_car.model = car_info[2]
            
            cars.append(new_car)
               
    else:
        print(f'Failed to retrieve page {page}')

# Print extracted make and model
for car in cars:
    print(car.year, car.make, car.model, car.img_url, car.price)

