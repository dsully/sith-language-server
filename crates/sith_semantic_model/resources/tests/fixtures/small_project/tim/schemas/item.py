from decimal import Decimal
from typing import Optional

from pydantic import BaseModel

from .utils import convert_to_optional


class ItemBase(BaseModel):
    title: str
    bar_code: str
    description: Optional[str] = None
    price: Decimal
    image_path: Optional[str] = None
    quantity: int = 0


class ItemCreate(ItemBase):
    pass


class ItemUpdate(ItemBase):
    __annotations__ = convert_to_optional(ItemBase)


class Item(ItemBase):
    id: int
    owner_id: int

    class Config:
        orm_mode = True
