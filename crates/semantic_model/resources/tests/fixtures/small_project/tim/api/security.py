import secrets

from jose import jwt
from passlib.context import CryptContext
from sqlalchemy.orm import Session

from tim.db import crud

pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")

ALGORITHM = "HS256"
SECRET_KEY = secrets.token_urlsafe(32)


def create_access_token(data: dict):
    return jwt.encode(data, SECRET_KEY, algorithm=ALGORITHM)


def verify_password(plain_password: str, hashed_password: str) -> bool:
    return pwd_context.verify(plain_password, hashed_password)


def get_password_hash(password: str) -> str:
    return pwd_context.hash(password)


def authenticate_user(db: Session, *, email: str, password: str):
    user = crud.get_user_by_email(db, email)
    if not user:
        return False

    if not verify_password(password, user.hashed_password):
        return False
    return user
