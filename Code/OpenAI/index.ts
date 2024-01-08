import OpenAI from 'openai';
import minimist from "minimist";
import {readFile, mkdir, writeFile, access} from "fs/promises";
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

const { outfile } = await (async () => {
    const answersDir = args["out-dir"];
    if (!answersDir) {
        return { outfile: undefined };
    }
    await mkdir(answersDir, {recursive: true});

    const filename = basename(args["experiment-file"]);
    const ext = extname(filename);
    const finalname = filename.replace(ext, `_answers.csv`);

    const outfile = join(answersDir, finalname);

    return { outfile };
})();

const experiment = (await readFile(args['experiment-file'], 'utf-8')).split('\n').filter(_ => _).map(line => line.split('\t'));

const fullHistory: string[][] = []
if (outfile && await access(outfile).then(() => true, () => false)) {
    const answers = (await readFile(outfile, 'utf-8')).split('\n').filter(_ => _).map(line => line.split('\t'));
    if (answers.length === experiment.length) {
        console.log(`Answer file already exists: ${outfile}`);
        process.exit(0);
    } else {
        console.log(`Continuing from existing state. ${answers.length} of ${experiment.length} answers found in ${outfile}`);
        experiment.splice(0, answers.length);
        fullHistory.push(...answers);
    }
}

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

console.log('Starting history', systemPromptAsHistory);

for (let i = 0; i < experiment.length; i++) {
    console.time('experiment');
    const data = experiment[i];

    console.log(`Experiment ${i + 1} of ${experiment.length} in file ${basename(args['experiment-file'])}`);

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

    console.timeEnd('experiment');

    if (outfile) {
        await writeFile(outfile, stringify(fullHistory,
            {
                delimiter: '\t',
                quote: '\'',
            }));
    }
}

if (outfile) {
    await writeFile(outfile, stringify(fullHistory,
        {
            delimiter: '\t',
            quote: '\'',
        }));
}
