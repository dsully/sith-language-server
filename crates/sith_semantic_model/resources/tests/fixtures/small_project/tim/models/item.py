from sqlalchemy import Column, ForeignKey, Integer, String, DECIMAL
from sqlalchemy.orm import relationship

from tim.db import Base


class Item(Base):
    __tablename__ = "items"

    id = Column(Integer, primary_key=True, index=True)
    title = Column(String(100), index=True)
    description = Column(String(240), index=True, default=None)
    bar_code = Column(String(13), index=True)
    price = Column(DECIMAL(10, 2), index=True)
    # TODO: Instead of storing the path, store the actual image bytes with LargeBinary
    image_path = Column(String, default=None)
    quantity = Column(Integer, default=0)
    owner_id = Column(Integer, ForeignKey("users.id"))

    owner = relationship("User", back_populates="items")

    def __repr__(self) -> str:
        return (
            f"Item(id={self.id}, title={self.title}, description={self.description}, price={self.price},"
            f" image_path={self.image_path}, owner={self.owner_id})"
        )
