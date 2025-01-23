from pydantic import BaseModel

from .item import Item
from .utils import convert_to_optional


class UserBase(BaseModel):
    name: str
    email: str
    is_admin: bool = False


class UserCreate(UserBase):
    password: str


class UserUpdate(UserCreate):
    __annotations__ = convert_to_optional(UserBase, UserCreate)


class User(UserBase):
    id: int
    items: list[Item] = []

    class Config:
        orm_mode = True
