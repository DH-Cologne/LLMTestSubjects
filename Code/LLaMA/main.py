import argparse
import csv
import os
import re
from pathlib import Path
from typing import List, Tuple

import torch
from transformers import AutoModelForCausalLM, AutoTokenizer, GenerationConfig, BitsAndBytesConfig
import bitsandbytes as bnb

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", type=str, required=True, help="Model name or path")
    parser.add_argument("--cache-dir", type=str, required=True, help="Cache directory")
    parser.add_argument("--temperature", type=float, required=True, help="Generation temperature")
    parser.add_argument("--system", type=str, required=True, help="System prompt file")
    parser.add_argument("--experiment-file", type=str, required=True, help="Experiment file")
    parser.add_argument("--skip-output", action="store_true", help="Skip writing output")
    parser.add_argument("--max-tokens", type=int, default=64, help="Maximum tokens to generate")
    parser.add_argument("--out-dir", type=str, help="Output directory")
    return parser.parse_args()

def setup_output_path(args) -> Path:
    model_name = os.path.basename(args.model)
    base_dir = Path(args.out_dir) if args.out_dir else Path("answers") / model_name / f"temperature-{args.temperature}"
    base_dir.mkdir(parents=True, exist_ok=True)

    experiment_name = Path(args.experiment_file).stem
    return base_dir / f"{experiment_name}_answers.csv"

def load_experiment_file(path: str) -> List[List[str]]:
    with open(path, 'r', encoding='utf-8') as f:
        return [line.strip().split('\t') for line in f if line.strip()]

def load_system_prompt(path: str) -> Tuple[str, List[dict]]:
    with open(path, 'r', encoding='utf-8') as f:
        content = f.read()

    parts = re.split(r'(SYSTEM|USER|ASSISTANT):\s+', content, flags=re.IGNORECASE | re.MULTILINE)
    parts = [p.strip() for p in parts if p.strip()]

    system_prompt = next(p for i, p in enumerate(parts) if i % 2 == 1)
    history = []

    for i in range(2, len(parts), 2):
        role = parts[i].lower()
        content = parts[i + 1]
        if role == "user":
            history.append({"role": "user", "content": content})
        else:
            history.append({"role": "assistant", "content": content})

    return system_prompt, history

class ExperimentRunner:
    def __init__(self, args):
        self.args = args

        # Configure quantization
        quantization_config = BitsAndBytesConfig(
            load_in_4bit=True,
            bnb_4bit_compute_dtype=torch.float16,
            bnb_4bit_use_double_quant=True,
            bnb_4bit_quant_type="nf4",
            bnb_4bit_quant_storage=torch.uint8
        )

        # Load model and tokenizer
        self.model = AutoModelForCausalLM.from_pretrained(
            args.model,
            cache_dir=args.cache_dir,
            torch_dtype=torch.bfloat16,
            low_cpu_mem_usage=True,
            trust_remote_code=True,
            local_files_only=True,
            device_map="auto",
            quantization_config=quantization_config,
        ).eval()

        # Model size after quantization
        human_readable_size = self.model.get_memory_footprint() / 1024 / 1024
        print(f"Model size: {human_readable_size :.2f} MB")

        self.tokenizer = AutoTokenizer.from_pretrained(
            args.model,
            cache_dir=args.cache_dir,
            trust_remote_code=True,
            use_fast=False,
            local_files_only=True
        )

        self.generation_config = GenerationConfig(
            max_new_tokens=args.max_tokens,
            do_sample=True if args.temperature > 0 else False,
            temperature=args.temperature,
            pad_token_id=self.tokenizer.pad_token_id,
            eos_token_id=self.tokenizer.eos_token_id,
        )

    def format_prompt(self, prompt: str, history=None) -> str:
        if history is None:
            history = []

        # Format might need to be adjusted based on your specific model
        formatted_prompt = ""
        for h in history:
            if h["role"] == "user":
                formatted_prompt += f"User: {h['content']}\n"
            else:
                formatted_prompt += f"Assistant: {h['content']}\n"

        formatted_prompt += f"User: {prompt}\nAssistant:"
        return formatted_prompt

    def run_conversation(self, prompt: str, history=None):
        if history is None:
            history = []

        formatted_prompt = self.format_prompt(prompt, history)
        inputs = self.tokenizer(formatted_prompt, return_tensors="pt", truncation=True)
        inputs = {k: v.to(self.model.device) for k, v in inputs.items()}

        # Generate response
        outputs = self.model.generate(
            **inputs,
            generation_config=self.generation_config
        )

        response = self.tokenizer.decode(outputs[0], skip_special_tokens=True)

        # Extract just the assistant's response (might need adjustment based on model)
        response = response.split("Assistant:")[-1].strip()

        # Update history
        history = history + [
            {"role": "user", "content": prompt},
            {"role": "assistant", "content": response}
        ]

        return response, history

    def run_experiment(self, experiment_data: List[str], system_prompt: str, conversation_history: List[dict]):
        results = [experiment_data[0]]  # Start with experiment ID
        prompts = experiment_data[1:]

        # Add system prompt to history if needed
        history = [{"role": "system", "content": system_prompt}] + conversation_history

        for prompt in prompts:
            print(f">>> {prompt}")
            response, history = self.run_conversation(prompt, history)
            print(f"<<< {response}\n")

            results.extend([prompt, response])

        return results

def main():
    args = parse_args()
    output_path = setup_output_path(args)

    # Load experiment data
    experiment_data = load_experiment_file(args.experiment_file)
    system_prompt, conversation_history = load_system_prompt(args.system)

    # Initialize results storage
    all_results = []

    # Check for existing results
    if output_path.exists() and not args.skip_output:
        with open(output_path, 'r', newline='') as f:
            reader = csv.reader(f, delimiter='\t', quotechar="'")
            existing_results = list(reader)
            if len(existing_results) == len(experiment_data):
                print(f"Answer file already exists: {output_path}")
                return

            if existing_results:
                all_results = existing_results
                experiment_data = experiment_data[len(existing_results):]
                print(f"Continuing from existing state. {len(existing_results)} of {len(experiment_data)} answers found")

    runner = ExperimentRunner(args)

    # Run experiments
    for i, data in enumerate(experiment_data):
        print(f"Experiment {i + 1} of {len(experiment_data)}")
        results = runner.run_experiment(data, system_prompt, conversation_history)
        all_results.append(results)

        # Save intermediate results
        if not args.skip_output:
            with open(output_path, 'w', newline='') as f:
                writer = csv.writer(f, delimiter='\t', quotechar="'")
                writer.writerows(all_results)

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("Interrupted by user")
        exit(1)
