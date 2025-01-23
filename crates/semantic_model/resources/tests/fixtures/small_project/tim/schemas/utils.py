from typing import Optional, Type
from pydantic import BaseModel

def convert_to_optional(*schemas: Type[BaseModel]) -> dict[str, object]:
    """
    Turn the fields from schemas into Optional fields
    """
    annotations = {}
    for schema in schemas:
        annotations.update({k: Optional[v] for k, v in schema.__annotations__.items()})

    return annotations
