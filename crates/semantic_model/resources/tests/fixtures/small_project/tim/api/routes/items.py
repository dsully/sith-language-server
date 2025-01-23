from fastapi import APIRouter, Depends, HTTPException, Query, status
from sqlalchemy.orm import Session

from tim import schemas
from tim.api.dependencies import get_current_user, get_db
from tim.db import crud

route = APIRouter(prefix="/items", tags=["items"], dependencies=[Depends(get_current_user)])


@route.get("/", response_model=list[schemas.Item])
def read_items(
        skip: int = Query(0, ge=0),
        limit: int = Query(100, ge=0),
        db: Session = Depends(get_db),
        _: schemas.User = Depends(get_current_user)
):
    items = crud.get_items(db, skip, limit)
    return items


@route.get("/{title}", response_model=schemas.Item)
async def read_item(title: str, db: Session = Depends(get_db), _: schemas.User = Depends(get_current_user)):
    db_item = crud.get_item_by_title(db, title)
    if db_item is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Item not found")
    return db_item


@route.put("/update/{item_id}", response_model=schemas.Item)
async def update_item(
    item_id: int,
    item: schemas.ItemUpdate,
    db: Session = Depends(get_db),
    _: schemas.User = Depends(get_current_user)
):
    db_item = crud.get_item(db, item_id)
    if db_item is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Item not found")
    return crud.update_item(db, db_item, item)


@route.delete("/delete/{item_id}", response_model=schemas.Item)
async def delete_item(item_id: int, db: Session = Depends(get_db), _: schemas.User = Depends(get_current_user)):
    deleted_item = crud.delete_item(db, id=item_id)
    if deleted_item is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Item not found")
    return deleted_item


@route.get("/withdraw/{item_id}", response_model=schemas.Item)
async def withdraw_item(
    item_id: int,
    quantity: int = Query(..., ge=0),
    db: Session = Depends(get_db),
    _: schemas.User = Depends(get_current_user)
):
    db_item = crud.get_item(db, item_id)
    if db_item is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Item not found")

    if db_item.quantity < quantity:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Available item quantity, {db_item.quantity}, is less than the requested quantity, {quantity}",
        )

    return crud.update_item(db, db_item, schemas.ItemUpdate(quantity=db_item.quantity - quantity))
