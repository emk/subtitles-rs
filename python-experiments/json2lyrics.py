# Convert Whisper JSON output to song lyrics.
#
# Usage: python json2lyrics.py whisper_output.json

import json

def json_to_lyrics(json_path: str):
    """Reformat Whisper JSON output as song lyrics using some basic heuristics.
    This will likely require further manual editing."""

    # Load the JSON file. This is formatted as:
    #
    # [{ "word": "En", "start": 18.3799991607666, "end": 15.220000267028809 }, ..]
    with open(json_path, "r") as f:
        word_infos = json.load(f)

    # Basic algorithm:
    #
    # 1. If a word starts with a capital letter, insert a line break.
    # 2. Otherwise, insert a space.
    lyrics = []
    for word_info in word_infos:
        word = word_info["word"]
        # If we're not the first word, insert spacing.
        if lyrics:
            if word[0].isupper():
                lyrics.append("\n")
            else:
                lyrics.append(" ")
        lyrics.append(word)

    # Print the lyrics.
    print("".join(lyrics))

# Command line entry point.
if __name__ == "__main__":
    import sys
    if len(sys.argv) != 2:
        print("Usage: python json2lyrics.py whisper_output.json")
        sys.exit(1)
    json_path = sys.argv[1]
    json_to_lyrics(json_path)



