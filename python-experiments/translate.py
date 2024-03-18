import json

from dotenv import load_dotenv
from openai import OpenAI


# Load environment variables. Create a file named `.env` in the same directory as this file
# and add the following line to it:
#
# OPENAI_API_KEY="your-api-key"
load_dotenv()

def translate_lines():
    """Translate the lines in the input file and write the translations to the output file."""

    prompt = """Translate the following consecutive lines of dialog from Spanish (es) to English (en):

```json
[
    {
        "original": "El Templo del Aire es uno de los lugares más",
        "translation": null
    },
    {
        "original": "hermosos del mundo.",
        "translation": null
    },
    {
        "original": "Aang, sé que estás ansioso, pero han pasado cien años",
        "translation": null
    },
    {
        "original": "desde que te fuiste.",
        "translation": null
    }
]
```

Please call the function `report_translation` with your output.
"""

    tools = [{
        "type": "function",
        "function": {
            "name": "report_translation",
            "description": "Report the translations of the lines of dialog.",
            "parameters": {
                "type": "object",
                "properties": {
                    "lines": {
                        "type": "array",
                        "items": {
                            "type": "object",
                            "properties": {
                                "original": {"type": "string"},
                                "translation": {"type": "string"}
                            },
                            "required": ["original", "translation"]
                        }
                    }
                },
                "required": ["lines"]
            }
        }
    }]
    print(json.dumps(tools, indent=4))

    # Generate the translations using GPT-3.5.
    client = OpenAI()
    response = client.chat.completions.create(
        model="gpt-3.5-turbo",
        messages=[
            {"role": "system", "content": "You are a subtitle translator helping language learners."},
            {"role": "user", "content": prompt},
        ],
        tools = tools,
        tool_choice = {"type": "function", "function": {"name": "report_translation"}},
    )

    # Print the response.
    tool_calls = response.choices[0].message.tool_calls
    for tool_call in tool_calls:
        args = json.loads(tool_call.function.arguments)
        print(f"{json.dumps(args, indent=4)}")


# Command line entry point.
if __name__ == "__main__":
    translate_lines()
