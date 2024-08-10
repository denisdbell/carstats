from sqlalchemy import create_engine, Column, String, Integer, DECIMAL, PrimaryKeyConstraint, func
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
import os

Base = declarative_base()

class CarListings(Base):
    __tablename__ = 'CarListings'
    
    UUID = Column(String(36), nullable=False, primary_key=True)
    WEBSITE = Column(String(255), nullable=False, primary_key=True)
    IMAGE_URL = Column(String(255))
    YEAR = Column(Integer, nullable=False, primary_key=True)
    MAKE = Column(String(100), nullable=False, primary_key=True)
    MODEL = Column(String(100), nullable=False, primary_key=True)
    PRICE = Column(DECIMAL(10, 2))

    __table_args__ = (
        PrimaryKeyConstraint('UUID', 'WEBSITE', 'MAKE', 'MODEL', 'YEAR'),
    )

class CarDatabase:
    def __init__(self):
        database_url = f"mysql+pymysql://{os.environ['DBUSER']}:{os.environ['DBPASSWORD']}@{os.environ['DBHOST']}/{os.environ['DATABASE']}"
        self.engine = create_engine(database_url)
        Base.metadata.create_all(self.engine)
        self.Session = sessionmaker(bind=self.engine)
    
    def add_car_listing(self, car_listing):
        session = self.Session()
        session.add(car_listing)
        session.commit()
        session.close()

    def get_car_by_max_price(self):
        session = self.Session()
        result = session.query(CarListings).order_by(CarListings.PRICE.desc()).first()
        session.close()
        return result

    def get_car_by_min_price(self):
        session = self.Session()
        result = session.query(CarListings).order_by(CarListings.PRICE.asc()).first()
        session.close()
        return result

    def get_most_common_car(self):
        session = self.Session()
        result = session.query(
            CarListings.MAKE, CarListings.MODEL, func.count('*').label('count')
        ).group_by(CarListings.MAKE, CarListings.MODEL).order_by(func.count('*').desc()).first()
        session.close()
        return result

    def get_top_ten_most_common_cars(self):
        session = self.Session()
        result = session.query(
            CarListings.MAKE, CarListings.MODEL, func.count('*').label('count')
        ).group_by(CarListings.MAKE, CarListings.MODEL).order_by(func.count('*').desc()).limit(10).all()
        session.close()
        return result

    def get_top_ten_cheapest_cars(self):
        session = self.Session()
        result = session.query(CarListings).order_by(CarListings.PRICE.asc()).limit(10).all()
        session.close()
        return result