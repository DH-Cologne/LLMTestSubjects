#! /usr/bin/fish

set MODEL_DIR "/home/kniebes/HuggingFace/out"

for temp in 0 0.2 0.5
    for model in \
        "DiscoResearch--Llama3-DiscoLeo-Instruct-8B-v0.1.Q6_K.gguf|DL3_8b" \
        "VAGOsolutions--Llama-3-SauerkrautLM-8b-Instruct.fixed.Q6_K.gguf|SK3_8b" \
        "meta-llama--Meta-Llama-3-8B-Instruct.Q6_K.gguf|ML3_8b" \
        "seedboxai--Llama-3-KafkaLM-8B-v0.1.Q6_K.gguf|KFK3_8b" \
        "microsoft--Phi-3-medium-4k-instruct.Q6_K.gguf|PHI3_14b" \
        "VAGOsolutions--Llama-3-SauerkrautLM-70b-Instruct.fixed.Q6_K.gguf|SK3_70b" \
        "meta-llama--Meta-Llama-3-70B-Instruct.Q6_K.gguf|ML3_70b" \
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
    		bunx tsx index.ts \
        		--model $MODEL_PATH \
            	--temperature $temp \
                --experiment-file $file \
                --system "$PWD/../../Pronomina/IntroductoryPrompts/llama3_b_prompt_1shot.txt" \
                --out-dir $RATINGS
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
    		bunx tsx index.ts \
    			--model $MODEL_PATH \
    			--temperature $temp \
    			--experiment-file $file \
    			--system "$PWD/../../Pronomina/IntroductoryPrompts/openai_gpt_a_prompt.txt" \
    			--out-dir $COMPLETIONS
    	end
    end # model iterator
end # temperature iterator
