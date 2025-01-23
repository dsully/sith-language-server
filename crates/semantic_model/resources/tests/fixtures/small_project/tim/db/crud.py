from typing import Optional

from sqlalchemy.orm import Session

from tim import models, schemas
from tim.api import security


def get_user(db: Session, user_id: int) -> models.User:
    return db.query(models.User).filter(models.User.id == user_id).first()


def get_user_by_email(db: Session, email: str) -> models.User:
    return db.query(models.User).filter(models.User.email == email).first()


def get_user_by_name(db: Session, name: str) -> models.User:
    return db.query(models.User).filter(models.User.name == name).first()


def get_users(db: Session, skip: int = 0, limit: int = 100) -> list[models.User]:
    return db.query(models.User).offset(skip).limit(limit).all()


def create_user(db: Session, user: schemas.UserCreate) -> models.User:
    hashed_password = security.get_password_hash(user.password)
    db_user = models.User(name=user.name, email=user.email, is_admin=user.is_admin, hashed_password=hashed_password)

    db.add(db_user)
    db.commit()
    db.refresh(db_user)

    return db_user


def update_user(db: Session, user_db: models.User, updated_user: schemas.UserUpdate) -> models.User:
    updated_user_dict = updated_user.dict(exclude_unset=True)

    if not updated_user_dict:
        return user_db

    for field, value in updated_user_dict.items():
        if field == "password":
            field = "hashed_password"
            value = security.get_password_hash(updated_user.password)
        setattr(user_db, field, value)

    db.add(user_db)
    db.commit()
    db.refresh(user_db)

    return user_db


def delete_user(db: Session, *, id: int) -> Optional[models.User]:
    user = db.query(models.User).get(id)
    if not user:
        return None

    db.delete(user)
    db.commit()

    return user


def get_items(db: Session, skip: int = 0, limit: int = 100) -> list[models.Item]:
    return db.query(models.Item).offset(skip).limit(limit).all()


def create_user_item(db: Session, item: schemas.ItemCreate, user_id: int) -> models.Item:
    db_item = models.Item(**item.dict(), owner_id=user_id)

    db.add(db_item)
    db.commit()
    db.refresh(db_item)

    return db_item


def get_item_by_title(db: Session, title: str) -> models.Item:
    return db.query(models.Item).filter(models.Item.title == title).first()


def get_item(db: Session, id: int) -> models.Item:
    return db.query(models.Item).filter(models.Item.id == id).first()


def update_item(db: Session, item_db: models.Item, updated_item: schemas.ItemUpdate) -> models.Item:
    updated_item_dict = updated_item.dict(exclude_unset=True)

    if not updated_item_dict:
        return item_db

    for field, value in updated_item_dict.items():
        setattr(item_db, field, value)

    db.add(item_db)
    db.commit()
    db.refresh(item_db)

    return item_db


def delete_item(db: Session, *, id: int) -> Optional[models.Item]:
    item = db.query(models.Item).get(id)
    if not item:
        return None

    db.delete(item)
    db.commit()

    return item
