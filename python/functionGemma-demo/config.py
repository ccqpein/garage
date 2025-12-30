import json

from transformers.utils import get_json_schema


class Config:
    def __init__(self):
        self.theme = "light"
        self.font_size = 14


def update_config(config: Config):
    """
    Updates the configuration of the system.

    Args:
        config: A Config object

    Returns:
        True if the configuration was successfully updated, False otherwise.
    """


update_config_schema = {
    "type": "function",
    "function": {
        "name": "update_config",
        "description": "Updates the configuration of the system.",
        "parameters": {
            "type": "object",
            "properties": {
                "config": {
                    "type": "object",
                    "description": "A Config object",
                    "properties": {
                        "theme": {"type": "string"},
                        "font_size": {"type": "number"},
                    },
                },
            },
            "required": ["config"],
        },
    },
}

print(f"--- [Automatic] ---")
print(json.dumps(get_json_schema(update_config), indent=2))

print(f"\n--- [Manual Schemas] ---")
print(json.dumps(update_config_schema, indent=2))
