from sqlalchemy import Column, Integer, String, Boolean
from sqlalchemy.orm import relationship

from tim.db import Base


class User(Base):
    __tablename__ = "users"

    id = Column(Integer, primary_key=True, index=True)
    name = Column(String(100), index=True, nullable=False)
    email = Column(String(100), unique=True, index=True, nullable=False)
    hashed_password = Column(String(60), nullable=False)
    is_admin = Column(Boolean, default=False)

    items = relationship("Item", back_populates="owner")

    def __repr__(self) -> str:
        return (
            f"User(id={self.id}, name={self.name}, email={self.email}, is_admin={self.is_admin},"
            f" hashed_password={self.hashed_password}, items={self.items})"
        )
