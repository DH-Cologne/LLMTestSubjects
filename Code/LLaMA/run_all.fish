#! /usr/bin/fish

set MODEL_DIR "/home/kniebes/HuggingFace/out"

for temp in 0 0.2 0.5
    for model in \
        ## 2nd round
        #"DiscoResearch--Llama3-DiscoLeo-Instruct-8B-v0.1.Q6_K.gguf|DL3_8b" \
        #"VAGOsolutions--Llama-3-SauerkrautLM-8b-Instruct.fixed.Q6_K.gguf|SK3_8b" \
        #"meta-llama--Meta-Llama-3-8B-Instruct.Q6_K.gguf|ML3_8b" \
        #"seedboxai--Llama-3-KafkaLM-8B-v0.1.Q6_K.gguf|KFK3_8b" \
        #"microsoft--Phi-3-medium-4k-instruct.Q6_K.gguf|PHI3_14b" \
        #"VAGOsolutions--Llama-3-SauerkrautLM-70b-Instruct.fixed.Q6_K.gguf|SK3_70b" \
        #"meta-llama--Meta-Llama-3-70B-Instruct.Q6_K.gguf|ML3_70b" \
        ## 3rd round
        "VAGOsolutions--SauerkrautLM-v2-14b-DPO.Q6_K.gguf|SKv2_14b" \
        "VAGOsolutions--SauerkrautLM-gemma-2-9b-it.Q6_K.gguf|SKGEMMA2_9b" \
        "ibm-granite--granite-3.0-3b-a800m-instruct.Q6_K.gguf|GRAN_MoE_3b" \
        "ibm-granite--granite-3.0-8b-instruct.Q6_K.gguf|GRAN_8b" \
        "meta-llama--Llama-3.2-3B-Instruct.Q6_K.gguf|ML3.2_3b" \
        "Ministral-8B-Instruct-2410-Q6_K.gguf|Ministral_8b" \
        "openchat-3.6-8b-20240522.Q6_K.gguf|OC3.6_8b" \
        "Meta-Llama-3.1-8B-Instruct-Q6_K.gguf|ML3.1_8b" \
        "gemma-2-9b-it-Q6_K.gguf|GEMMA2_9b" \
        "Mistral-Nemo-Instruct-2407-Q6_K.gguf|MISTRALNEMO_12b" \
        "qwen2.5-14b-instruct-q6_k.gguf|QWEN2.5_14b" \
        ;
        set TEMPSTRING (string replace "." "" $temp)
        set TEMPSTRING (string pad -w 2 -c '0' -r $TEMPSTRING)

        set BASEDIR "$PWD/../../Pronomina/LLMAnswers"
        set RATINGS "$BASEDIR/Ratings/"
        set COMPLETIONS "$BASEDIR/Completions/"

        set MODEL_FILENAME (string split "|" $model | head -1)
        set MODEL_ABRV (string split "|" $model | tail -1)

        set MODEL_PATH "$MODEL_DIR/$MODEL_FILENAME"
        set ABRV_PATH (string join '' $MODEL_ABRV _t $TEMPSTRING)

        set RATINGS (string join '' $RATINGS $ABRV_PATH _rtgs)
        set COMPLETIONS (string join '' $COMPLETIONS $ABRV_PATH _comp)

        echo $MODEL_PATH $ABRV_PATH
        echo $RATINGS
        echo $COMPLETIONS

    	for file in (find ~/LLMTestSubjects/Pronomina/ExperimentParticipantsLists/B*)
            set EXP_FILE_NAME (basename $file | string replace ".csv" "")
            set ANSWER_FILE (string join '' $RATINGS '/' $EXP_FILE_NAME '_answers.csv')
            if test -e $ANSWER_FILE
                set LINE_COUNT_ANSWER (cat $ANSWER_FILE | wc -l)
                set LINE_COUNT_FILE (cat $file | wc -l)
                if [ $LINE_COUNT_ANSWER -eq $LINE_COUNT_FILE ]
                    echo "Answer File already exists: $ANSWER_FILE"
                    continue
                end
            end
    		bun run index.ts \
        		--model $MODEL_PATH \
            	--temperature $temp \
                --experiment-file $file \
                --system "$PWD/../../Pronomina/IntroductoryPrompts/llama3_b_prompt_1shot.txt" \
                --out-dir $RATINGS
                #--skip-output
        end

    	for file in (find ~/LLMTestSubjects/Pronomina/ExperimentParticipantsLists/A*)
            set EXP_FILE_NAME (basename $file | string replace ".csv" "")
            set ANSWER_FILE (string join '' $COMPLETIONS '/' $EXP_FILE_NAME '_answers.csv')
            if test -e $ANSWER_FILE
                set LINE_COUNT_ANSWER (cat $ANSWER_FILE | wc -l)
                set LINE_COUNT_FILE (cat $file | wc -l)
                if [ $LINE_COUNT_ANSWER -eq $LINE_COUNT_FILE ]
                    echo "Answer File already exists: $ANSWER_FILE"
                    continue
                end
            end
    		bun run index.ts \
    			--model $MODEL_PATH \
    			--temperature $temp \
    			--experiment-file $file \
    			--system "$PWD/../../Pronomina/IntroductoryPrompts/openai_gpt_a_prompt.txt" \
    			--out-dir $COMPLETIONS
                #--skip-output
    	end
    end # model iterator
end # temperature iterator
