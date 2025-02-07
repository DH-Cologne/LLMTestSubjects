#! /usr/bin/fish

set MODEL_DIR "/home/kniebes/HuggingFace/out"
set LLAMA_CLI "/home/kniebes/llama.cpp/build/bin/llama-perplexity"
set TAB (echo -e "\t")

# Function for running the llama-perplexity with a too large context size
# This way we can find out how many tokens the prompt tokenizes to
function get_prompt_tokens
    set -l model_path $argv[1]
    set -l prompt $argv[2]

    set -l temp_file (mktemp)
    $LLAMA_CLI --gpu-layers 30 --ctx-size 1024 --model $model_path --prompt $prompt > $temp_file 2>&1
    set -l tokens (cat $temp_file | awk '/tokenizes to only/ {print $10}')
    rm $temp_file

    echo $tokens
end

# Function for running the llama-perplexity and extracting the perplexity
function get_perplexity
    set -l model_path $argv[1]
    set -l prompt $argv[2]
    set -l prompt_tokens $argv[3]
    set -l ctx_size (math floor (math $prompt_tokens / 2))

    set -l temp_file (mktemp)
    $LLAMA_CLI --gpu-layers 30 --ctx-size $ctx_size --model $model_path --prompt $prompt > $temp_file 2>&1
    set -l ppl (cat $temp_file | awk '/Final estimate/ {print $5}')
    rm $temp_file

    echo $ppl
end

for model in \
    # "mixtral-8x7b-instruct-v0.1.Q6_K.gguf|MIX_8x7b" \
    "gemma-2-9b-it-Q6_K.gguf|GEMMA2_9b" \
    "ibm-granite--granite-3.0-3b-a800m-instruct.Q6_K.gguf|GRAN_MoE_3b" \
    "ibm-granite--granite-3.0-8b-instruct.Q6_K.gguf|GRAN_8b" \
    "seedboxai--Llama-3-KafkaLM-8B-v0.1.Q6_K.gguf|KFK3_8b" \
    "Meta-Llama-3.1-8B-Instruct-Q6_K.gguf|ML3.1_8b" \
    "microsoft--Phi-3-medium-4k-instruct.Q6_K.gguf|PHI3_14b" \
    "VAGOsolutions--SauerkrautLM-v2-14b-DPO.Q6_K.gguf|SKv2_14b" \
    "VAGOsolutions--SauerkrautLM-gemma-2-9b-it.Q6_K.gguf|SKGEMMA2_9b" \
    ;
    set BASEDIR "$PWD/../../Pronomina/LLMAnswers"
    set PERPLEXITY "$BASEDIR/Perplexity/"

    set MODEL_FILENAME (string split "|" $model | head -1)
    set MODEL_ABRV (string split "|" $model | tail -1)

    set MODEL_PATH "$MODEL_DIR/$MODEL_FILENAME"
    set ABRV_PATH (string join '' $MODEL_ABRV)

    set PERPLEXITY (string join '' $PERPLEXITY $ABRV_PATH _ppx)

    echo $MODEL_PATH $ABRV_PATH
    echo $PERPLEXITY

    mkdir -p $PERPLEXITY

   	for file in (find ~/LLMTestSubjects/Pronomina/Data/average_ratings*.tsv)
        set EXP_FILE_NAME (basename $file | string replace "average_ratings_" "" | string replace ".tsv" "")
        set ANSWER_FILE (string join '' $PERPLEXITY '/' $EXP_FILE_NAME '_answers.tsv')
        if test -e $ANSWER_FILE
            set LINE_COUNT_ANSWER (cat $ANSWER_FILE | wc -l)
            set LINE_COUNT_FILE (math (cat $file | wc -l) - 1)
            if [ $LINE_COUNT_ANSWER -eq $LINE_COUNT_FILE ]
                echo "Answer File already exists: $ANSWER_FILE"
                continue
            end
        end

        echo "Processing $file"
        set first_line true
  		while read -l line
            if $first_line # Skip first line
                set first_line false
                continue
            end

            set EXP_PROMPT (string split $TAB $line | head -1)
            set PROMPT_TOKENS (get_prompt_tokens $MODEL_PATH $EXP_PROMPT)

            set ppl (get_perplexity $MODEL_PATH $EXP_PROMPT $PROMPT_TOKENS)
            echo -e "Model: $MODEL_PATH\nPrompt: $EXP_PROMPT\nTokens: $PROMPT_TOKENS\tPerplexity: $ppl"
            echo -e "$EXP_PROMPT\t$ppl" >> $ANSWER_FILE
        end < $file
    end
end # model iterator
