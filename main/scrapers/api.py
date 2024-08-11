from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from uuid import uuid4
from decimal import Decimal
from typing import List, Optional
from Orm.orm import CarDatabase,CarListings
from sqlalchemy.ext.asyncio import AsyncSession, create_async_engine
from sqlalchemy.orm import sessionmaker
import os
import aiomysql

app = FastAPI()
db = CarDatabase()

class CarListingRequest(BaseModel):
    WEBSITE: str
    IMAGE_URL: Optional[str]
    YEAR: int
    MAKE: str
    MODEL: str
    PRICE: Decimal

@app.post("/cars/")
def add_car(car: CarListingRequest):
    new_car = CarListings(
        UUID=str(uuid4()),
        WEBSITE=car.WEBSITE,
        IMAGE_URL=car.IMAGE_URL,
        YEAR=car.YEAR,
        MAKE=car.MAKE,
        MODEL=car.MODEL,
        PRICE=car.PRICE
    )
    db.add_car_listing(new_car)
    return {"message": "Car listing added successfully", "UUID": new_car.UUID}

@app.get("/cars/max-price/")
def get_car_by_max_price():
    car = db.get_car_by_max_price()
    if not car:
        raise HTTPException(status_code=404, detail="No car listings found")
    return car

@app.get("/cars/min-price/")
def get_car_by_min_price():
    car = db.get_car_by_min_price()
    if not car:
        raise HTTPException(status_code=404, detail="No car listings found")
    return car

@app.get("/cars/most-common/")
def get_most_common_car():
    car = db.get_most_common_car()
    if not car:
        raise HTTPException(status_code=404, detail="No car listings found")
    return car

@app.get("/cars/top-ten-common/")
def get_top_ten_most_common_cars():
    cars = db.get_top_ten_most_common_cars()
    if not cars:
        raise HTTPException(status_code=404, detail="No car listings found")
    return cars

@app.get("/cars/top-ten-cheapest/")
def get_top_ten_cheapest_cars():
    cars = db.get_top_ten_cheapest_cars()
    if not cars:
        raise HTTPException(status_code=404, detail="No car listings found")
    return cars

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)