#!/usr/bin/env python3
#
# Usage: python3 parse-kindle-clippings.py <path-to-clippings-file> <book-title> <out-file>
#
# This script parses the clippings file from a Kindle device and prints out the
# highlights in a more readable format.
#
# Clippings have the following format:
#     Book (Series) (Spanish Edition) (Author)
#     - Votre surlignement sur la page 9 | emplacement 52-53 | Ajouté le samedi 30 mars 2024 23:26:32
#
#     Text text text
#     and more quoted text.
#     ==========
#
# The second line is too dependent on the language of the device, so we'll
# ignore it.
#
# The out file is stored with in the format:
#
#     Quote 1.
#     --
#     Quote 2.
#     --
#
# Note that some text in the output file may be surrounded by [[ and ]]. This
# is added later by hand an should be ignored if when we're deciding whether
# a highlight is already in the output file.

import os
import re
import sys
from typing import List

class Highlight:
    position: int
    title: str
    author: str
    text: str

    def __init__(self, *, position: int, title: str, author: str, text: str):
        self.position = position
        self.title = title
        self.author = author
        self.text = text

def parse_clippings_file(path: str) -> List[Highlight]:
    with open(path, encoding='utf-8-sig') as f:
        lines = f.readlines()

    highlights: List[Highlight] = []

    # Consume one line at a time.
    i = 0
    position = 0
    while i < len(lines):
        # The author is the _last_ parenthesized expression.
        title_author = lines[i].strip()
        title, author = title_author.rsplit('(', 1)
        title = title.strip()
        author = author[:-1].strip()
        print(f'Title: {repr(title)}')
        i += 3

        # The text is everything until the next line of equals signs. But strip
        # leading and trailing whitespace, and convert whitespace to a single
        # space.
        text_lines = []
        while i < len(lines) and lines[i].strip() != '==========':
            text_lines.append(lines[i].strip())
            i += 1
        i += 1
        text = re.sub(r'\s+', ' ', ' '.join(text_lines)).strip()

        highlights.append(Highlight(position=position, title=title, author=author, text=text))
        position += 1

    # Now we need to deal with highlights that are subsets of other highlights.
    # We'll do this by sorting by length, descending, and then iterating through
    # the highlights and removing any that are substrings of ones we've already
    # seen.
    highlights.sort(key=lambda h: len(h.text), reverse=True)
    seen: set[str] = set()
    deduped_highlights: List[Highlight] = []
    for h in highlights:
        # Check against all the highlights we've already seen.
        if any(h.text in s for s in seen):
            continue
        seen.add(h.text)
        deduped_highlights.append(h)

    # Now sort by position.
    deduped_highlights.sort(key=lambda h: h.position)
    return deduped_highlights

def write_highlights(highlights: List[Highlight], out_file: str):
    # First, we need to keep track of known highlights.
    known_highlights: set[str] = set()
    try:
        with open(out_file) as f:
            known_highlights_iter = (
                kh.replace('[[', '').replace(']]', '').strip()
                for kh in f.read().split('\n--\n')
            )
            known_highlights = set(kh for kh in known_highlights_iter if kh)
    except FileNotFoundError:
        pass

    # Then append the new highlights.
    with open(out_file, 'a') as f:
        for h in highlights:
            if h.text in known_highlights:
                continue
            f.write(f'{h.text}\n--\n')

if __name__ == '__main__':
    if len(sys.argv) != 4:
        print('Usage: python3 parse-kindle-clippings.py <path-to-clippings-file> <book-title> <out-file>')
        sys.exit(1)

    clippings_file = sys.argv[1]
    book_title = sys.argv[2]
    out_file = sys.argv[3]

    highlights = parse_clippings_file(clippings_file)
    highlights = [h for h in highlights if h.title == book_title]
    print(f'Found {len(highlights)} highlights for {repr(book_title)}.')
    write_highlights(highlights, out_file)
