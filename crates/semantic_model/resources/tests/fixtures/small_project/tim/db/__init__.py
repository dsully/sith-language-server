from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker

from tim.core import settings

# PROJECT_ROOT_DIR = Path(__file__).parents[2]
# SQLALCHEMY_DATABASE_URL = f"sqlite:///{PROJECT_ROOT_DIR}/tim.db"
engine = create_engine(settings.SQLALCHEMY_DATABASE_URL)

SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

Base = declarative_base()
