# Define the base URL and the number of pages you want to scrape
base_url = 'https://www.autoadsja.com/search.asp?SearchSB=5&page='
num_pages = 5  # Replace with the actual number of pages
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
            img_tag_src = listing.find('img')['src']
            if img_tag_src:
               img_url = img_tag_src.strip()
               cars.append(img_url)

            #Get make model and year
            description = listing.find('div', class_='description')
            if description:
                h2_tag = description.find('h2')
                if h2_tag:
                    car_info = h2_tag.text.strip()
                    cars.append(car_info)
               
    else:
        print(f'Failed to retrieve page {page}')

# Print extracted make and model
for car in cars:
    print(car)

