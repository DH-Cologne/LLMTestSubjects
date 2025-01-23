#! /usr/bin/fish

set MODEL_DIR "/home/kniebes/HuggingFace/out"
set LLAMA_CLI "/home/kniebes/llama.cpp/build/bin/llama-perplexity"
set TAB (echo -e "\t")

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

   	for file in (find ~/LLMTestSubjects/Pronomina/ExperimentParticipantsLists/B*)
        set EXP_FILE_NAME (basename $file | string replace ".csv" "")
        set ANSWER_FILE (string join '' $PERPLEXITY '/' $EXP_FILE_NAME '_answers.csv')
        if test -e $ANSWER_FILE
            set LINE_COUNT_ANSWER (cat $ANSWER_FILE | wc -l)
            set LINE_COUNT_FILE (cat $file | wc -l)
            if [ $LINE_COUNT_ANSWER -eq $LINE_COUNT_FILE ]
                echo "Answer File already exists: $ANSWER_FILE"
                continue
            end
        end

        echo "Processing $file"
  		while read -l line
            set EXP_ID (string split $TAB $line | head -1)
            set EXP_PROMPT (string split $TAB $line | tail -1 | string split "\n" | head -1)

            set temp_file (mktemp)
            $LLAMA_CLI --gpu-layers 30 --ctx-size 4 --model $MODEL_PATH --prompt $EXP_PROMPT > $temp_file 2>&1
            set ppl (cat $temp_file | awk '/Final estimate/ {print $5}')
            rm $temp_file
            echo -e "$EXP_ID\t$EXP_PROMPT\t$ppl" >> $ANSWER_FILE
        end < $file
        break # only one file is needed for perplexity, since lines are the same over files just shuffled
    end
end # model iterator
