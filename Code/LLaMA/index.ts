import { dirname, join } from "path";
import { fileURLToPath } from "url";
import minimist from "minimist";
import { basename, extname } from "path/posix";
import { access, mkdir, readFile, writeFile } from "fs/promises";
import { stringify } from "csv-stringify/sync";
import { getLlama, ChatHistoryItem, LlamaContext, LlamaChatSession } from "node-llama-cpp";

type Arguments = {
  model: string;
  temperature: number;
  system: string;
  "experiment-file": string;
  "no-output": string | undefined;
  "max-tokens": number | undefined;
  "out-dir": string | undefined;
};

const args = minimist(process.argv.slice(2)) as unknown as Arguments;
if (!args.model) {
  throw new Error("No model provided");
}
if (args.temperature === undefined || Number.isNaN(args.temperature)) {
  throw new Error("No temperature provided");
}
if (!args["experiment-file"]) {
  throw new Error("No experiment file provided");
}
if (!args.system) {
  throw new Error("No system prompt provided");
}

const baseSystemPrompt = await readFile(args.system, "utf-8");
const noOutput = args["no-output"] !== undefined;
const maxTokens = args["max-tokens"] ? args["max-tokens"] : 64;

const experiment = (await readFile(args["experiment-file"], "utf-8"))
  .split("\n")
  .filter((_) => _)
  .map((line) => line.split("\t"));

const { outfile } = await (async () => {
  const __dirname = dirname(fileURLToPath(import.meta.url));
  const modelname = basename(args.model).replaceAll(".gguf", "");
  const answersDir = args["out-dir"]
    ? args["out-dir"]
    : join(__dirname, "answers", modelname, `temperature-${args.temperature}`);
  await mkdir(answersDir, { recursive: true });

  const filename = basename(args["experiment-file"]);
  const ext = extname(filename);
  const finalname = filename.replace(ext, `_answers.csv`);

  const outfile = join(answersDir, finalname);

  return { outfile };
})();

const fullHistory: string[][] = [];

if (
  !noOutput &&
  (await access(outfile).then(
    () => true,
    () => false,
  ))
) {
  const answers = (await readFile(outfile, "utf-8"))
    .split("\n")
    .filter((_) => _)
    .map((line) => line.split("\t"));
  if (answers.length === experiment.length) {
    console.log(`Answer file already exists: ${outfile}`);
    process.exit(0);
  } else {
    console.log(
      `Continuing from existing state. ${answers.length} of ${experiment.length} answers found in ${outfile}`,
    );
    experiment.splice(0, answers.length);
    fullHistory.push(...answers);
  }
}

// Model setup
/*const safeGpuLayers = {
  "8x7b": 10,
  "7b": 80,
  "8b": 40,
  "13b": 32,
  "14b": 34,
  "Phi-3-medium": 34,
  "34b": 24,
  "70b": 10,
};

const gpuLayers = (() => {
  for (const [params, layers] of Object.entries(safeGpuLayers)) {
    if (basename(args.model).toLocaleLowerCase().includes(params.toLocaleLowerCase())) {
      return layers;
    }
  }
  return undefined;
})();

if (!gpuLayers) {
  throw new Error(
    "Could not determine model params from model name, or no gpu layer information available",
  );
}*/

const seed = Math.round(Math.random() * 1000000);

function* chunks<T>(arr: T[], n: number): Generator<T[], void> {
  for (let i = 0; i < arr.length; i += n) {
    yield arr.slice(i, i + n);
  }
}
const [[_, systemPrompt], ...history] = [
  ...chunks(
    baseSystemPrompt
      .split(/(SYSTEM|USER|ASSISTANT):\s+/gim)
      .map((v) => v.trim())
      .filter((_) => _),
    2,
  ),
] as ['user' | 'assistant', string][];
const typeMap = {  'user': 'user',  'assistant': 'model',} as const;
const conversationHistory: ChatHistoryItem[] = history.map(([from, text]) => {
  const type = typeMap[from];
  return type === 'user' ? { type, text } : { type, response: [text] };
});

const llama = await getLlama("lastBuild");

const model = await llama.loadModel({
  modelPath: args.model!,
  // gpuLayers,
});

console.log(`Using model:\t${basename(args.model)}`);
console.log(`Using temperature:\t${args.temperature}`);
console.log(`Seed:\t${seed}`);

const getAnswser = async ({
  prompt,
  session,
}: {
  prompt: string;
  session: LlamaChatSession;
}) => {
  const chunks: string[] = [];
  console.log(`>>> ${prompt}`);
  
  await session.prompt(prompt, {
    maxTokens,
    customStopTriggers: ["<|begin_of_text|>", "\n"],
    onToken(chunk) {
      const decoded = model.detokenize(chunk);
      process.stdout.write(decoded);
      chunks.push(decoded);
      if (decoded.includes("\n")) {
      }
    },
    temperature: args.temperature,
  });
  console.log();
  return chunks.join("");
};

for (let i = 0; i < experiment.length; i++) {
  console.time("experiment");

  const data = experiment[i];
  const context = await model.createContext({
    seed,
  });

  const session = new LlamaChatSession({
    contextSequence: context.getSequence(),
    systemPrompt,
  });

  session.setChatHistory(conversationHistory);

  console.log(
    `Experiment ${i + 1} of ${experiment.length} in file ${basename(args["experiment-file"])}`,
  );

  const prompts = data.slice(1);
  const history: string[] = [];
  while (prompts.length > 0) {
    const prompt = prompts.splice(0, 1)[0];
    const answer = await getAnswser({ prompt, session });
    const cleanAnswer = answer.trim().split("\n")[0].trim();
    history.push(prompt, cleanAnswer);
  }
  fullHistory.push([data[0], ...history]);
  console.log([data[0], ...history]);
  console.timeEnd("experiment");

  await context.dispose();

  if (!noOutput) {
    await writeFile(
      outfile,
      stringify(fullHistory, {
        delimiter: "\t",
        quote: "'",
      }),
    );
  }
}

console.log(fullHistory.map((_) => _.join("\t")).join("\n"));

if (!noOutput) {
  await writeFile(
    outfile,
    stringify(fullHistory, {
      delimiter: "\t",
      quote: "'",
    }),
  );
}
