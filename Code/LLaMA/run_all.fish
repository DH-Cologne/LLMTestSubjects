#! /usr/bin/fish

for temp in 0 0.2 0.5
	set TEMPSTRING (string replace "." "" $temp)
	set TEMPSTRING (string pad -w 2 -c '0' -r $TEMPSTRING)

    set BASEDIR "$PWD/../../Pronomina/LLMAnswers"
    set RATINGS "$BASEDIR/Ratings/"
    set COMPLETIONS "$BASEDIR/Completions/"

    set MODEL "$PWD/models/em_german_70b_v01.Q4_K_M.gguf"
    set MODEL_ABRV "EMG_70b_t$TEMPSTRING"

	set RATINGS (string join '' $RATINGS $MODEL_ABRV _rtgs)
    set COMPLETIONS (string join '' $COMPLETIONS $MODEL_ABRV _comp)

    echo $MODEL
    echo $MODEL_ABRV
    echo $RATINGS
    echo $COMPLETIONS

	for file in (find ~/LLMTestSubjects/Pronomina/ExperimentParticipantsLists/B*)
		bunx tsx index.ts \
			--model $MODEL \
			--temperature $temp \
			--experiment-file $file \
			--max-tokens 1 \
			--system "$PWD/../../Pronomina/IntroductoryPrompts/em_german_leo_mistral_experiment_b_prompt.txt" \
			--out-dir $RATINGS
	end

	for file in (find ~/LLMTestSubjects/Pronomina/ExperimentParticipantsLists/A*)
		bunx tsx index.ts \
			--model $MODEL \
			--temperature $temp \
			--experiment-file $file \
			--system "$PWD/../../Pronomina/IntroductoryPrompts/em_german_leo_mistral_experiment_a_prompt.txt" \
			--out-dir $COMPLETIONS
	end
end
