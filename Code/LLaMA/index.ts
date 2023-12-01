import {LlamaChatSession, LlamaContext, LlamaModel, Token} from "node-llama-cpp";
import {dirname, join} from 'path';
import {fileURLToPath} from "url";
import minimist from "minimist";
import {basename, extname} from "path/posix";
import {mkdir, readFile, writeFile} from "fs/promises";
import {stringify} from 'csv-stringify/sync';

type Arguments = {
    model: string;
    temperature: number;
    system: string | undefined;
    'experiment-file': string;
    'no-output': string | undefined;
    'max-tokens': number | undefined;
}

const args = minimist(process.argv.slice(2)) as unknown as Arguments;
if (!args.model) {
    throw new Error("No model provided");
}
if (!args.temperature) {
    throw new Error("No temperature provided");
}
if (!args['experiment-file']) {
    throw new Error("No experiment file provided");
}

const systemPrompt = args.system ? await readFile(args.system, 'utf-8') : undefined;
const noOutput = args['no-output'] !== undefined;
const maxTokens = args['max-tokens'] ? args['max-tokens'] : 64;

const experiment = (await readFile(args['experiment-file'], 'utf-8')).split('\n').filter(_ => _).map(line => line.split('\t'));

const safeGpuLayers = {
    '7b': 80,
    '13b': 40,
    '34b': 24,
    '70b': 14,
}

const modelParams = Array.from(basename(args.model).match(/\d{1,2}b/gi) || [])[0] as keyof typeof safeGpuLayers | undefined;

if (!modelParams || !safeGpuLayers[modelParams]) {
    throw new Error("Could not determine model params from model name, or no gpu layer information available");
}

const seed = Math.round(Math.random() * 1000000);

const model = new LlamaModel({
    modelPath: args.model!,
    gpuLayers: safeGpuLayers[modelParams!],
    seed: seed,
    temperature: args.temperature,
});


console.log(`Using model:\t${basename(args.model)}`);
console.log(`Using temperature:\t${args.temperature}`);
console.log(`Seed:\t${seed}`);

const getAnswser = async ({prompt, session, context}: {
    prompt: string;
    session: LlamaChatSession;
    context: LlamaContext
}) => {
    const chunks: string[] = [];
    // console.log(`>>> ${prompt}`);
    await session.prompt(prompt, {
        maxTokens,
        onToken(chunk: Token[]) {
            const decoded = context.decode(chunk);
            // process.stdout.write(decoded);
            chunks.push(decoded);
        },
        temperature: args.temperature,
    })
    // process.stdout.write('\n');
    return chunks.join('');
};


const fullHistory: string[][] = [];

console.time('experiment');

for (const data of experiment) {
    const context = new LlamaContext({
        model,
        seed: seed,
    });

    const session = new LlamaChatSession({
        context,
        systemPrompt,
    });
    await session.init();

    const prompts = data.slice(1);
    const history: string[] = [];
    while (prompts.length > 0) {
        const prompt = prompts.splice(0, 1)[0];
        const answer = await getAnswser({prompt, session, context});
        const cleanAnswer = answer.split('\n')[0].trim();
        history.push(prompt, cleanAnswer);
    }
    fullHistory.push([data[0], ...history]);
    console.log([data[0], ...history]);
}


console.log(fullHistory.map(_ => _.join('\t')).join('\n'));

console.timeEnd('experiment');



if (!noOutput) {
    const csv = stringify(fullHistory,
        {
            delimiter: '\t',
            quote: '\'',
        });

    const __dirname = dirname(fileURLToPath(import.meta.url));
    const modelname = basename(args.model).replaceAll('.gguf', '');
    const answersDir = join(__dirname, 'answers', modelname, `temperature-${args.temperature}`);
    await mkdir(answersDir, {recursive: true});

    const filename = basename(args["experiment-file"]);
    const ext = extname(filename);
    const finalname = filename.replace(ext, `_answers.csv`);

    await writeFile(join(answersDir, finalname), csv);
}
