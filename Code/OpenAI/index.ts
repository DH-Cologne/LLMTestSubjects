import OpenAI from 'openai';
import minimist from "minimist";
import {readFile, mkdir, writeFile} from "fs/promises";
import {stringify} from "csv";
import {fileURLToPath} from "url";
import {basename, dirname, join, extname} from 'path';

type Arguments = {
    /*model: string;*/
    temperature: number;
    system: string;
    'experiment-file': string;
    'max-tokens': number | undefined;
    'out-dir': string | undefined;
}

const args = minimist(process.argv.slice(2)) as unknown as Arguments;
/*if (!args.model) {
    throw new Error("No model provided");
}*/
if (args.temperature === undefined || Number.isNaN(args.temperature)) {
    throw new Error("No temperature provided");
}
if (!args['experiment-file']) {
    throw new Error("No experiment file provided");
}
if (!process.env.OPENAI_API_KEY) {
    throw new Error("No OpenAI API key provided");
}
if (!args.system) {
    throw new Error("No system prompt provided");
}

const systemPrompt = await readFile(args.system, 'utf-8');
const maxTokens = args['max-tokens'] ? args['max-tokens'] : 64;
const model = 'gpt-4-1106-preview';

const experiment = (await readFile(args['experiment-file'], 'utf-8')).split('\n').filter(_ => _).map(line => line.split('\t'));

type HistoryElement = OpenAI.Chat.Completions.ChatCompletionMessageParam;
type History = HistoryElement[];

const roleRegex = new RegExp(/^(System|User|Assistant):\s+/gmi);

function* chunks<T>(arr: T[], n: number): Generator<T[], void> {
    for (let i = 0; i < arr.length; i += n) {
        yield arr.slice(i, i + n);
    }
}

const splitSystemPrompt = systemPrompt.split(roleRegex).map(v => v.trim()).filter(_ => _);
const systemPromptAsHistory: History = [...chunks(splitSystemPrompt, 2)].map(([role, content]) => ({
    role,
    content
} as HistoryElement));

const openai = new OpenAI();

const getAnswer = async (history: History) => {
    const {choices} = await openai.chat.completions.create({model, messages: history, n: 1, temperature: args.temperature});
    return choices[0]!.message.content ?? '';
}

const fullHistory: string[][] = []

console.log('Starting history', systemPromptAsHistory);

for (const data of experiment) {
    const prompts = data.slice(1);
    const history: History = [
        ...systemPromptAsHistory,
    ];
    while (prompts.length > 0) {
        const prompt = prompts.splice(0, 1)[0];
        history.push({
            role: 'user',
            content: prompt,
        });
        const answer = await getAnswer(history);
        const cleanAnswer = answer.trim().split('\n')[0].trim();
        history.push({
            role: 'assistant',
            content: cleanAnswer,
        });
    }
    const cleanedHistory = history.slice(systemPromptAsHistory.length).map(({content}) => content as string);
    console.log([data[0], ...cleanedHistory]);
    fullHistory.push([data[0], ...cleanedHistory]);
}

if (args['out-dir']) {
    const answersDir = args["out-dir"];
    await mkdir(answersDir, {recursive: true});

    const filename = basename(args["experiment-file"]);
    const ext = extname(filename);
    const finalname = filename.replace(ext, `_answers.csv`);

    const outfile = join(answersDir, finalname);
    const csv = stringify(fullHistory,
        {
            delimiter: '\t',
            quote: '\'',
        });
    await writeFile(outfile, csv);
}