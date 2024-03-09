import json

from dotenv import load_dotenv
from openai import OpenAI


# Load environment variables. Create a file named `.env` in the same directory as this file
# and add the following line to it:
#
# OPENAI_API_KEY="your-api-key"
load_dotenv()

def text_to_speech(audio_file_path: str, prompt_path: str, output_path: str):
    """Convert text to speech using OpenAI's whisper model."""
    client = OpenAI()

    prompt = open(prompt_path, "r").read()
    audio_file = open(audio_file_path, "rb")
    transcript = client.audio.transcriptions.create(
        file=audio_file,
        model="whisper-1",
        prompt=prompt,
        response_format="verbose_json",
        # "segment" gives punctuation, but "word" actually seems to give
        # more accurate timestamps. Not perfect ones, unfortunately.
        timestamp_granularities=["segment", "word"]
    )

    # Write the transcript to a JSON file.
    transcript_data = {
        "language": transcript.language,
        "duration": transcript.duration,
        "text": transcript.text,
        "words": transcript.words,
        "segments": transcript.segments,
    }
    with open(output_path, "w") as f:
        f.write(json.dumps(transcript_data, indent=4))

# Command line entry point.
#
# Usage: python whisper.py audio_file_path prompt_path output_path
if __name__ == "__main__":
    import sys
    if len(sys.argv) != 4:
        print("Usage: python whisper.py audio_file_path prompt_path output_path")
        sys.exit(1)
    audio_file_path = sys.argv[1]
    prompt_path = sys.argv[2]
    output_path = sys.argv[3]
    text_to_speech(audio_file_path, prompt_path, output_path)
