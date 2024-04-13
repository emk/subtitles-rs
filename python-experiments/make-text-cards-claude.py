#!/usr/bin/env python
#
# Usage:
#   python make-text-cards.py <deck> <source-name> <input-text-file> <output-csv-file>

import abc
import csv
import json
import os
from random import randint
from typing import Any, Dict, List, Optional

from anthropic import Anthropic
from dotenv import load_dotenv
from markdown import markdown
from openai import OpenAI


# Load environment variables. Create a file named `.env` in the same directory as this file
# and add the following line to it:
#
# OPENAI_API_KEY="your-api-key"
load_dotenv()

class Model(abc.ABC):
    """Interface to a model, allowing us to support multiple AI
    providers."""

    @staticmethod
    def cheapest_reasonable() -> "Model":
        """Select a reasonably cheap model that still produces decent results."""
        if os.getenv("ANTHROPIC_API_KEY"):
            return AnthropicFamily("claude-3-haiku-20240307")
        elif os.getenv("OPENAI_API_KEY"):
            return OpenAIFamily("gpt-3.5-turbo")
        else:
            raise ValueError("Could not find ANTHROPIC_API_KEY or OPENAI_API_KEY in the environment")

    @abc.abstractmethod
    def create_tool_message(
        self, *,
        system_message: str,
        messages: List[Dict[str, str]],
        tools: List[Dict[str, Any]],
        tool_choice: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Ask the model to generate the next message in a conversation, which
        should be a tool call."""

    @abc.abstractmethod
    def function_call_messages(
        self, *,
        function_name: str,
        arguments: Dict[str, Any]
    ) -> List[Dict[str, Any]]:
        """Ask the model to generate a message that calls a function with the
        given arguments."""


class OpenAIFamily(Model):
    """OpenAI's GPT models."""

    client: OpenAI
    model_name: str

    def __init__(self, model_name: str) -> None:
        self.client = OpenAI()
        self.model_name = model_name

    def create_tool_message(
        self, *,
        system_message: str,
        messages: List[Dict[str, str]],
        tools: List[Dict[str, Any]],
        tool_choice: Dict[str, Any]
    ) -> Dict[str, Any]:
        response = self.client.chat.completions.create(
            model = self.model_name,
            messages = [system_message] + messages,
            tools = tools,
            tool_choice = tool_choice,
        )
        tool_calls = response.choices[0].message.tool_calls
        if len(tool_calls) != 1:
            raise ValueError(f"Expected 1 tool call, got {len(tool_calls)}")
        return json.loads(tool_calls[0].function.arguments)

    def function_call_messages(
        self, *,
        function_name: str,
        arguments: Dict[str, Any]
    ) -> List[Dict[str, Any]]:
        return [{
            "role": "function",
            "name": function_name,
            "content": json.dumps(arguments),
        }]

class AnthropicFamily(Model):
    """Anthropic's models."""

    client: Anthropic
    model_name: str

    def __init__(self, model_name: str) -> None:
        self.client = Anthropic()
        self.model_name = model_name

    def create_tool_message(
        self, *,
        system_message: str,
        messages: List[Dict[str, str]],
        tools: List[Dict[str, Any]],
        tool_choice: Dict[str, Any]
    ) -> Dict[str, Any]:
        message = self.client.beta.tools.messages.create(
            max_tokens = 1024,
            model = self.model_name,
            system = system_message,
            messages = messages,
            tools = [{
                "name": t["function"]["name"],
                "description": t["function"]["description"],
                "input_schema": t["function"]["parameters"]
            } for t in tools ],
            #tool_choice = tool_choice,
        )
        #print(f"Response: {message.model_dump_json(indent=2)}")
        if message.stop_reason != "tool_use":
            raise ValueError(f"Expected a tool use response, got {message.model_dump_json(indent=2)}")
        tool = next(c for c in message.content if c.type == "tool_use")
        return tool.input

    def function_call_messages(
        self, *,
        function_name: str,
        arguments: Dict[str, Any]
    ) -> List[Dict[str, Any]]:
        # Unfortunately, this requires a lot more machinery under Anthropic
        # than OpenAI.
        id = f"toolu_{randint(1000, 9999)}"
        return [{
            "role": "assistant",
            "content": [{
                "id": id,
                "type": "tool_use",
                "name": function_name,
                "input": arguments,
            }],
        }, {
            "role": "user",
            "content": [{
                "type": "tool_result",
                "tool_use_id": id
            }]
        }, {
            "role": "assistant",
            "content": "Continue"
        }]

def generate_cards(input_texts: List[str], *, source: Optional[str] = None) -> List[Dict[str, str]]:
    """Read in a file of text snippets and convert them into cards.

    Output fields should be:

    - Front: The original text
    - Back: The translation
    - Notes: Extra notes or context generated by the model for text marked with
      "[[...]]".
    - Source: The source of the text snippet.
    """

    # We need to build up a sample dialog between the "user" and the
    # "assistant", before asking our actual question. This "teaches" the model
    # how to respond, essentially by putting words into its mouth.
    system_message = """
You are a translator helping prepare Anki cards for a language learner. You will
be given short text in Spanish, which will put onto the front of cards. Your job
is to translate the short text to English. Following the translation, you should
briefly break down any phrases surrounded by [[ ]] and explain how they work.

There should be exactly one entry in "explanations" for each pharse marked with
[[ ]]. Please do not include explanations the learner didn't ask for, because it
distracts them! If no phrase is marked, leave "explanations" blank.

If I ask you translate a phrase again, please respond the same way you did the
first time.
"""
    prompt_1 = "Translate: Tenía un alma de tigre.\n\nExplain: Omit."
    response_1 = {
        "translation": "He had a tiger's soul."
    }
    prompt_2 = """Translate: Ni [[siquiera]] hay una gramola.\n\nExplain:\n\n- **siquiera:**"""
    response_2 = {
        "translation": "There isn't even a jukebox.",
        "explanations": "- **siquiera:** The word \"siquiera\" in Spanish is used to add emphasis, typically in negative contexts, similar to the English word \"even.\" In this sentence, \"Ni siquiera\" translates directly to \"not even,\" emphasizing that there isn’t a jukebox at all.",
    }

    # Declare the function that the model should call.
    tools = [{
        "type": "function",
        "function": {
            "name": "add_data_to_card",
            "description": "Add the translation (and optionally explanations) to the current card.",
            "parameters": {
                "type": "object",
                "properties": {
                    "translation": {
                        "type": "string",
                        "description": "The translated text that should be placed on the back of card."
                    },
                    "explanations": {
                        "type": "string",
                        "description": "If and only if any phrases are marked with [[ ]], this should paramater should be passed, containing a Markdown-formatted list explaining each phrase marked with [[ ]]. It should not contain explanations for any phrases not marked with [[ ]]. If a marked phrase can be explained by a simple definition in English, just give that. If it's more complicated, use a longer explanation."
                    },
                },
                "required": ["translation"]
            }
        }
    }]

    # Generate the translations using GPT-3.5.
    model = Model.cheapest_reasonable()

    result = []
    for input_text in input_texts:

        # Extract sections marked with [[ ]].
        expressions_to_explain = []
        to_parse = input_text
        while "[[" in to_parse:
            start = to_parse.index("[[")
            end = to_parse.index("]]")
            expressions_to_explain.append(to_parse[start+2:end])
            to_parse = to_parse[end+2:]

        # Build our prompt template.
        prompt = f"""Translate: {input_text}"""
        if expressions_to_explain:
            explanations_template = "\n".join([f"- **{e}:**" for e in expressions_to_explain])
            prompt += f"\n\nExplain:\n\n{explanations_template}"""
        else:
            prompt += "\n\nExplain: Omit."

        print(f"Input: {input_text}")
        args = model.create_tool_message(
            system_message=system_message,
            messages = [
                {"role": "user", "content": prompt_1},
                *model.function_call_messages(function_name="add_data_to_card", arguments=response_1),
                {"role": "user", "content": prompt_2},
                *model.function_call_messages(function_name="add_data_to_card", arguments=response_2),
                {"role": "user", "content": prompt_3},
                *model.function_call_messages(function_name="add_data_to_card", arguments=response_3),
                {"role": "user", "content": prompt},
            ],
            tools = tools,
            tool_choice = {"type": "function", "function": {"name": "add_data_to_card"}},
        )
        print(f"{json.dumps(args, indent=4)}")

        # Convert [[ and ]] to ** and **.
        front = input_text.replace("[[", "**").replace("]]", "**")

        # Convert the explanations to Markdown.
        if args.get("explanations"):
            explanations = markdown(args["explanations"])
        else:
            explanations = None

        result.append({
            "Front": markdown(front),
            "Back": markdown(args["translation"]),
            "Notes": explanations,
            "Source": source,
        })

    return result

def texts_to_csv(in_texts_path: str, out_csv_path: str, *, deck: str, source: Optional[str] = None) -> None:
    """Read in a file of text snippets separated by "\\n--\\n" and write the
    generated cards to a CSV file."""

    with open(in_texts_path, "r") as f:
        input_texts = f.read().strip().split("\n--\n")

    cards = generate_cards(input_texts, source=source)

    # Write CSV correctly using a library. Note that Anki imports work much
    # better if we provide a header.
    with open(out_csv_path, "w", newline="") as f:
        f.write(f"""#separator:Semicolon
#html:true
#notetype:Text Snippet
#deck:{deck}
#columns:""")
        writer = csv.DictWriter(f, fieldnames=["Front", "Back", "Notes", "Source"], delimiter=";")
        writer.writeheader()
        writer.writerows(cards)

# Command line entry point.
if __name__ == "__main__":
    import sys

    if len(sys.argv) != 5:
        print(f"Usage: {sys.argv[0]} <deck> <source-name> <input-text-file> <output-csv-file>")
        sys.exit(1)

    deck = sys.argv[1]
    source = sys.argv[2]
    in_texts_path = sys.argv[3]
    out_csv_path = sys.argv[4]

    texts_to_csv(in_texts_path, out_csv_path, deck=deck, source=source)
