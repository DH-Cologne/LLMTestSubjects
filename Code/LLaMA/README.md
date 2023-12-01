## node-llama-cpp client for running automated experiments against LLaMA-based models

### Requirements

- [NodeJS](https://nodejs.org/en)

### Setup

Install dependencies:

`npm install`

Download and build llama.cpp:

`npx --no node-llama-cpp download`

For CUDA support:

`npx --no node-llama-cpp download --cuda`

For intel-based Macs:

`npx --no node-llama-cpp download --no-metal`

### How to use

Run the script using ts-node.

`npx ts-node --esm index.ts --model "[full path to model GGUF]" --temperature [number between 0 and 2] --experiment-file "[full path to experiment tab seperated value file]" --system "[full path to file containing the system prompts for the experiment]"`

The script has the following required parameters:
- --model \
Full path to a GGUF model
- --temperature \
Number between 0 and 2
- --experiment-file \
Full path to a file containing tab seperated prompts \
Each line should contain a series of prompts seperated by tabs \
The first value in each line is not used as a prompt (they are considered experiment identifiers)
  

And the following optional parameters:
- --system \
A system prompt that will be fed to the model before the first experiment prompt
- --no-output \
Prevents outputting the answers into a csv. Useful for testing system prompts
- --max-tokens \
A number to limit the amount of tokens generated per response. \
The default is set to a low value (64) to prevent "infinite" generation.

All answers will be saved in a folder structure called "answers" after the experiment file has been fully processed.

### Models used so far

- [EM German Leo Mistral](https://huggingface.co/TheBloke/em_german_leo_mistral-GGUF)
