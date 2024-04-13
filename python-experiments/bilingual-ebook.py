# Script to convert a JSON file with bilingual text into an interlinear HTML
# "ebook".
#
# Input file format is [JSON Lines](https://jsonlines.org/), where each line is
# a JSON array with two strings: the foreign language text and the native
# language text.
#
# The input was generated using a custom
# [Bertalign](https://github.com/bfsujason/bertalign) notebook.

import json
import re
import sys

CHAPTER_REGEX = r"Chapter "

def escape_html(text):
    html_escape_table = {
        "&": "&amp;",
        '"': "&quot;",
        "'": "&apos;",
        ">": "&gt;",
        "<": "&lt;",
    }
    return "".join(html_escape_table.get(c, c) for c in text)

def process_ebook(input_file, output_file):
    with open(input_file, 'r', encoding='utf-8') as infile, open(output_file, 'w', encoding='utf-8') as outfile:
        outfile.write('<!DOCTYPE html>\n<html>\n<head>\n<meta charset="UTF-8">\n')

        for i, line in enumerate(infile):
            data = json.loads(line)
            spanish_text, english_text = data
            spanish_text = escape_html(spanish_text)
            english_text = escape_html(english_text)

            if i == 0:
                outfile.write(f'<title>{spanish_text} / {english_text}</title>\n</head>\n<body>\n')
                outfile.write(f'<h1>{spanish_text} / <i>{english_text}</i></h1>\n\n')
            elif re.match(CHAPTER_REGEX, english_text):
                outfile.write(f'<h2>{spanish_text} / <i>{english_text}</i></h2>\n\n')
            else:
                outfile.write(f'<p>{spanish_text}<br>\n<i>{english_text}</i></p>\n\n')

        outfile.write('</body>\n</html>')

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: python bilingual-ebook.py input.jsonl out.html")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]
    process_ebook(input_file, output_file)
