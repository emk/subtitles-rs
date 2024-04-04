from dotenv import load_dotenv
from openai import OpenAI

load_dotenv()

def text_to_speech(input_text, output_file):
    client = OpenAI()

    response = client.audio.speech.create(
        model="tts-1",
        voice="shimmer",
        input=input_text
    )

    response.stream_to_file(output_file)

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 3:
        print("Usage: python tts.py <input_text> <output_mp3_file>")
        sys.exit(1)
    text_to_speech(sys.argv[1], sys.argv[2])
