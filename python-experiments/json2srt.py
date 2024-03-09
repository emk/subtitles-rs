# Convert Whisper JSON output to song lyrics.
#
# Usage: python json2srt.py whisper_output.json lyrics.srt
#
# The input `whisper_output.json` should have been generated using `whisper.py`
# or another tool that includes Whisper's `segments` output.

import json
import re
from typing import Iterable, List, TypedDict

# Data types.
Word = TypedDict("Word", {"word": str, "start": float, "end": float})
Segment = TypedDict("Segment", {"text": str, "start": float, "end": float, "no_speech_prob": float})
Transcript = TypedDict("Transcript", {"language": str, "duration": float, "text": str, "words": List[Word], "segments": List[Segment]})

def json_to_srt(json_path: str, srt_path: str):
    """Reformat Whisper JSON output as an SRT subtitle file."""

    # Load the JSON file.
    with open(json_path, "r") as f:
        json_data: Transcript = json.load(f)

    # Print segments as SRT entries.
    lyrics = []
    next_index = 1
    word_infos = json_data["words"]
    for s in json_data["segments"]:
        # Skip things which might not be speech.
        #if s["no_speech_prob"] > 0.95:
        #    continue

        # Clean up the text.
        text = clean_text(s["text"])
        if not text:
            continue

        # Unfortunately, the segment timestamps are not very accurate, so we
        # need to use the word timestamps to get more accurate timings. We do
        # this by splitting the segment text into words and trying to match
        # them up with word_infos.

        # Replace all punctuation with spaces.
        segment_word_text = word_characters_only(text)
        segment_words = segment_word_text.split()
        word_infos = valid_word_infos(word_infos)
        matching_word_infos = []
        for sw in segment_words:
            tries = 10
            while tries > 0:
                candidate = next(word_infos, None)
                if candidate is None:
                    raise ValueError(f"Ran out of word_infos for segment '{text}'")
                if candidate["word"].lower() == sw.lower():
                    break
                print(f"Expecting '{sw}', SKIPPING '{candidate['word']}'")
                tries -= 1
            if tries == 0:
                raise ValueError(f"Expected '{sw}', got '{candidate['word']}'")
            print(f"Matched '{sw}' to '{candidate['word']}'")
            matching_word_infos.append(candidate)
        if matching_word_infos:
            start_time = matching_word_infos[0]["start"]
            end_time = matching_word_infos[-1]["end"]
        else:
            print("No words in segment", file=sys.stderr)
            start_time = s["start"]
            end_time = s["end"]

        start_time_text = format_time(start_time)
        end_time_text = format_time(end_time)
        subtitle = f"{next_index}\n{start_time_text} --> {end_time_text}\n{text}\n\n"
        lyrics.append(subtitle)
        next_index += 1

    # Print the lyrics.
    with open(srt_path, "w") as f:
        f.write("".join(lyrics))

def clean_text(text: str) -> str:
    """Clean up text from Whisper."""
    text = text.replace("\U0001F3B5", " ").replace("\U0001F3B6", " ")
    text = " ".join(text.split())
    return text.strip()

def word_characters_only(text: str) -> str:
    """Remove non-word characters from text."""
    return re.sub(r"[^\w\s]", " ", text)

# Format a time in seconds as an SRT timestamp in "00:00:01,000" format.
def format_time(seconds: float) -> str:
    minutes, seconds = divmod(seconds, 60)
    hours, minutes = divmod(minutes, 60)
    return f"{int(hours):02d}:{int(minutes):02d}:{seconds:06.3f}".replace(".", ",")

# An iterator that returns the next valid-looking "word info" from a list of
# word infos. We filter out words that don't look like text.
def valid_word_infos(word_infos: List[Word]) -> Iterable[Word]:
    for word_info in word_infos:
        word = word_info["word"]
        word = word_characters_only(word)
        word = clean_text(word)
        if word:
            word_info = word_info.copy()
            word_info["word"] = word
            yield word_info

# Command line entry point.
if __name__ == "__main__":
    import sys
    if len(sys.argv) != 3:
        print("Usage: python json2srt.py whisper_output.json lyrics.srt")
        sys.exit(1)
    json_path = sys.argv[1]
    srt_path = sys.argv[2]
    json_to_srt(json_path, srt_path)



