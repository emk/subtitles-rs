#!/usr/bin/env python
#
# Usage:
#   python make-text-cards-with-context.py <deck> <source-name> <input-clippings> <input-bilingual-jsonl> <output-csv-file>

import csv
from dataclasses import asdict, dataclass
import json
import re
from typing import Dict, List, Optional
from unicodedata import normalize

from dotenv import load_dotenv
from markdown import markdown
from openai import OpenAI


# Load environment variables. Create a file named `.env` in the same directory as this file
# and add the following line to it:
#
# OPENAI_API_KEY="your-api-key"
load_dotenv()

def strip_brackets(s: str) -> str:
    """Remove all brackets from a string."""
    return s.replace("[[", "").replace("]]", "")

@dataclass(kw_only=True)
class Alignment:
    """A bilingual sentence alignment. Technically either side may contain
    multiple sentences.

    Foreign expressions to be explained may be marked with [[...]]."""
    foreign: str
    native: str

    @staticmethod
    def from_jsonl(path: str) -> List["Alignment"]:
        """Load alignments from a file in JSONL format, where each
        line looks like `{ "f": "foreign text", "n": "native text" }`."""
        alignments = []
        with open(path, "r", encoding="utf-8") as f:
            for line in f.readlines():
                record = json.loads(line)
                alignments.append(Alignment(
                    foreign=record["f"],
                    native=record["n"],
                ))
        return alignments

@dataclass(kw_only=True)
class Card:
    """An Anki card with optional context.

    Text will be interpreted as Markdown. The "Foreign" text may include [[ ]]
    marks around phrases that should be explained."""
    ForeignCurr: str
    NativeCurr: str
    ForeignPrev: Optional[str]
    NativePrev: Optional[str]
    ForeignNext: Optional[str]
    NativeNext: Optional[str]
    Source: Optional[str]
    Hint: Optional[str]
    Notes: Optional[str]

    def from_alignments(prev: Optional[Alignment], curr: Alignment, next: Optional[Alignment], *, source: Optional[str] = None) -> "Card":
        """Create a card from the current alignment and optional context."""
        return Card(
            ForeignCurr=curr.foreign,
            NativeCurr=curr.native,
            ForeignPrev=prev.foreign if prev else None,
            NativePrev=prev.native if prev else None,
            ForeignNext=next.foreign if next else None,
            NativeNext=next.native if next else None,
            Source=source,
            Hint=None,
            Notes=None,
        )

    def to_anki_dict(self) -> Dict[str, str]:
        """Convert the card to a dictionary suitable for writing to an Anki CSV."""
        d = {}
        for field, value in asdict(self).items():
            if value is not None:
                d[field] = markdown(value.replace("[[", "**").replace("]]", "**"))
        return d

    def expressions_to_explain(self) -> List[str]:
        """Return a list of expressions in the foreign text that should be explained."""
        return re.findall(r"\[\[(.*?)\]\]", self.ForeignCurr)

    def generate_explanations_for_note(self, client: OpenAI):
        """Generate explanations for the expressions to be explained."""
        to_explain = self.expressions_to_explain()
        if not to_explain:
            return

        # Only keep [[...]] expressions in self.ForeignCurr.
        context = []
        if self.ForeignPrev:
            context.append(strip_brackets(self.ForeignPrev))
        context.append(self.ForeignCurr)
        if self.ForeignNext:
            context.append(strip_brackets(self.ForeignNext))

        # Build a Markdown template for the explanations, to be filled in by the
        # LLM.
        explanation_template = []
        for expression in to_explain:
            explanation_template.append(f"- **{expression}:**")

        # Prompts.
        system_message = """\
You are a skilled language tutor helping an experienced language learner prepare
an Anki card. Your goal is to explain the meaning of the expressions marked with
[[ ]], as a Markdown list. Prefer simple translations where they exist, but give
longer explanations where necessary. Consider whether a marked expression might be
part of a larger idiom, and if so, explain the whole idiom in this context."""

        prompt_1 = "Los polis nunca lo hubiesen reconocido, pero [[a veces]] parecían casi reacios a perseguirlo.\n\nExplain:\n\n- **a veces:**"
        response_1 = {
            "thinking": "**a veces** means \"sometimes\" here, so explain it with a direct translation.",
            "explanations": "- **a veces:** Sometimes.",
        }
        prompt_2 = """Ni [[siquiera]] hay una gramola.\n\nExplain:\n\n- **siquiera:**"""
        response_2 = {
            "thinking": "**ni siquiera** means \"not even\" here, but **siquiera** can also mean \"even\", \"if only\" or \"at least\". This might be confusing, so let's clarify.""",
            "explanations": """- **(ni) siquiera:** Not even. Also:
    - _Siquiera pudieras llamar para avisar_ "**If only** you could call to let know."
    - _¿Puedes intentar siquiera hacer algo hoy?_ "Can you **at least** try to do something today?"
    - _Ni siquiera lo intentes._ "**Don't even** try it.\""""
        }
        prompt_3 = f"""\
{" ".join(context)}

Explain:

{" ".join(explanation_template)}"""
        print(f"Prompt: {prompt_3}", file=sys.stderr)

        # Declare the function that the model should call.
        tools = [{
            "type": "function",
            "function": {
                "name": "add_explanations_to_card",
                "description": "Add the explanation to the current card.",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "thinking": {
                            "type": "string",
                            "description": "Explain your thoughts about how to prepare this card briefly."
                        },
                        "explanations": {
                            "type": "string",
                            "description": "If and only if any phrases are marked with [[ ]], this should paramater should be passed, containing a Markdown-formatted list explaining each phrase marked with [[ ]]. It should not contain explanations for any phrases not marked with [[ ]]. If a marked phrase can be explained by a simple definition in English, just give that. If it's more complicated, use a longer explanation."
                        },
                    },
                    "required": ["explanations"]
                }
            }
        }]

        # Generate the explanations using GPT-3.5.
        response = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": system_message},
                {"role": "user", "content": prompt_1},
                {"role": "function", "name": "add_explanations_to_card", "content": json.dumps(response_1)},
                {"role": "user", "content": prompt_2},
                {"role": "function", "name": "add_explanations_to_card", "content": json.dumps(response_2)},
                {"role": "user", "content": prompt_3},
            ],
            tools=tools,
            tool_choice={"type": "function", "function": {"name": "add_explanations_to_card"}},
        )

        # Extract the tool call from the response.
        tool_calls = response.choices[0].message.tool_calls
        assert len(tool_calls) == 1
        args = json.loads(tool_calls[0].function.arguments)
        print(f"{json.dumps(args, indent=4)}", file=sys.stderr)

        # Add the explanations to the card.
        self.Notes = args["explanations"]

def highlights_to_cards(highlights: List[str], alignments: List[Alignment], *,
source: Optional[str] = None) -> List[Card]:
    """Our input is:

    - A list of foreign-language highlights, typically a single sentence.
    - A list of bilingual alignments, where each alignment is a pair of sentences.
    """

    def to_key(s: str) -> str:
        """Normalize a string for comparison."""
        return normalize("NFC", re.sub(r"\s+", "", s).replace("—", ""))

    foreign_to_alignments: Dict[str, (Optional[Alignment], Alignment, Optional[Alignment])] = {}
    for i, alignment in enumerate(alignments):
        if alignment.foreign not in foreign_to_alignments:
            prev = alignments[i - 1] if i > 0 else None
            curr = alignment
            next = alignments[i + 1] if i < len(alignments) - 1 else None
            foreign_to_alignments[to_key(alignment.foreign)] = (prev, curr, next)

    cards = []
    for highlight in highlights:
        highlight_key = to_key(strip_brackets(highlight))
        if highlight_key in foreign_to_alignments:
            prev, curr, next = foreign_to_alignments[highlight_key]
            curr_with_brackets = Alignment(foreign=highlight, native=curr.native)
            cards.append(Card.from_alignments(prev, curr_with_brackets, next, source=source))
        else:
            print(f"WARNING: Couldn't find: {repr(highlight)}", file=sys.stderr)

    return cards


def highlights_and_alignments_to_csv(highlights_path: str, alignments_path: str, out_csv_path: str, *, deck: str, source: Optional[str] = None) -> None:
    """Read in a file of highlights and a file of bilingual alignments and write
    the generated cards to a CSV file."""

    # Get our highlights.
    with open(highlights_path, "r", encoding="utf-8-sig") as f:
        highlights = f.read().strip().split("\n--\n")
        if not highlights[-1]:
            highlights.pop()
        if highlights and highlights[-1].endswith("\n--"):
            highlights[-1] = highlights[-1][:-3]

    # Get our alignments and generate cards.
    alignments = Alignment.from_jsonl(alignments_path)
    cards = highlights_to_cards(highlights, alignments, source=source)

    # Generate explanations for the cards.
    client = OpenAI()
    for card in cards:
        card.generate_explanations_for_note(client)

    # Write CSV correctly using a library. Note that Anki imports work much
    # better if we provide a header.
    with open(out_csv_path, "w", newline="") as f:
        f.write(f"""#separator:Semicolon
#html:true
#notetype:Aligned Text
#deck:{deck}
#columns:""")
        writer = csv.DictWriter(f, fieldnames=["ForeignCurr", "NativeCurr", "ForeignPrev", "NativePrev", "ForeignNext", "NativeNext", "Source", "Hint", "Notes"], delimiter=";")
        writer.writeheader()
        writer.writerows(card.to_anki_dict() for card in cards)

# Command line entry point.
if __name__ == "__main__":
    import sys

    if len(sys.argv) != 6:
        print(f"Usage: {sys.argv[0]} <deck> <source-name> <input-highlights-file> <input-alignments-file> <output-csv-file>")
        sys.exit(1)

    deck = sys.argv[1]
    source = sys.argv[2]
    highlights_path = sys.argv[3]
    alignments_path = sys.argv[4]
    out_csv_path = sys.argv[5]

    highlights_and_alignments_to_csv(highlights_path, alignments_path, out_csv_path, deck=deck, source=source)
