# Python experiments & scratch code

Python is the standard language for machine learning and AI these days. This directory contains various experiments which I may translate into Rust if they prove useful.

This code is intended for people who are comfortable using Python and who want to try various experiments. Using this is going to be harder than using the `substudy` command-line tool.

## Setting up a development environment

You will need to install [asdf](https://asdf-vm.com/guide/getting-started.html) and run:

```sh
asdf plugin add python
asdf install
pip install pipenv
pipenv install
```

Once you have installed everything, you can run a shell with:

```sh
pipenv shell
```

## Setting up an OpenAI key

You will need to set up a paid OpenAI account and get an API key. Once you have the key, create a file called `.env` in this directory and add the following line:

```sh
OPENAI_API_KEY="your-key-here"
```

## Using the code

Each file should have a least a couple of comments showing what it does and how to call it.
