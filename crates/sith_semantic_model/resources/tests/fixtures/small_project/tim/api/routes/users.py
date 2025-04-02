from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.orm import Session

from tim import schemas
from tim.api.dependencies import (get_current_admin_user, get_current_user,
                                  get_db)
from tim.db import crud

route = APIRouter(prefix="/users", tags=["users"])


@route.post("/register", response_model=schemas.User, status_code=status.HTTP_201_CREATED)
async def create_user(
    user: schemas.UserCreate, db: Session = Depends(get_db), _: schemas.User = Depends(get_current_admin_user)
):
    db_user = crud.get_user_by_email(db, email=user.email)
    if db_user:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Email already registered!")

    return crud.create_user(db=db, user=user)


@route.put("/update/me", response_model=schemas.User)
async def update_user_me(
    user: schemas.UserUpdate, db: Session = Depends(get_db), current_user: schemas.User = Depends(get_current_user)
):
    user_db = crud.get_user(db, current_user.id)  # TODO: Avoid doing another query to DB
    return crud.update_user(db, user_db, user)


@route.put("/update/{user_id}", response_model=schemas.User)
async def update_user(
    user_id: int,
    user: schemas.UserUpdate,
    db: Session = Depends(get_db),
    _: schemas.User = Depends(get_current_admin_user),
):
    db_user = crud.get_user(db, user_id)
    if db_user is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="User not found")
    return crud.update_user(db, db_user, user)


@route.delete("/delete/{user_id}", response_model=schemas.User)
async def delete_user(user_id: int, db: Session = Depends(get_db), _: schemas.User = Depends(get_current_admin_user)):
    deleted_user = crud.delete_user(db, id=user_id)
    if deleted_user is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="User not found")
    return deleted_user


@route.get("/", response_model=list[schemas.User])
async def read_users(
    skip: int = 0, limit: int = 100, db: Session = Depends(get_db), _: schemas.User = Depends(get_current_admin_user)
):
    users = crud.get_users(db, skip=skip, limit=limit)
    return users


@route.get("/me", response_model=schemas.User)
async def read_user_me(current_user: schemas.User = Depends(get_current_user)):
    return current_user


@route.get("/{user_id}", response_model=schemas.User)
def read_user(user_id: int, db: Session = Depends(get_db), _: schemas.User = Depends(get_current_admin_user)):
    db_user = crud.get_user(db, user_id=user_id)
    if db_user is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="User not found")

    return db_user


@route.post("/{user_id}/items/", response_model=schemas.Item, status_code=status.HTTP_201_CREATED)
def create_item_for_user(
    user_id: int,
    item: schemas.ItemCreate,
    db: Session = Depends(get_db),
    _: schemas.User = Depends(get_current_user),
):
    _ = get_current_user()
    return crud.create_user_item(db, item, user_id)
