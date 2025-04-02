from fastapi import FastAPI

from .api.routes import items, login, users

app = FastAPI(title="T.I.M - Triangle Inventory Management")

app.include_router(users.route.add_api_route)
app.include_router(items.route)
app.include_router(login.route)

class Test:
    def __init__(self):
        self.foobar = 1