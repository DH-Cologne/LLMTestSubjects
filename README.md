# LLMTestSubjects
Series of experiments to evaluate whether LLMs can be used to replace participants in linguistic experiments.

## Pronomina
Based on the two papers ([Patterson and Schumacher 2021](https://www.cambridge.org/core/journals/applied-psycholinguistics/article/interpretation-preferences-in-contexts-with-three-antecedents-examining-the-role-of-prominence-in-german-pronouns/E8F581347980C5A0A3D3D938B8F8F30A) and [Patterson et al. 2022](https://www.frontiersin.org/articles/10.3389/fpsyg.2021.672927/full)), we analyse whether LLMs 
* could make similar judgements about the acceptability of pronoun references as human subjects.
* prefer the same R-expressions for pronouns as human subjects.
* can be used reliably in anaphora resolution.

# Answers directory structure

The LLMAnswers directory splits the answers based on experiment, model & temperature.

The notation for LLaMA-based models is as follows:

`ModelAbbrevation_Parameters_Temperature_Experiment`

The OpenAI GPT notation is similar, but without the Parameters.

The following models were used:

| Abbreviation | Parameters | Link to model                                                                      |
|--------------|------------|------------------------------------------------------------------------------------|
| EGLM         | 7B         | [EM German Leo Mistral](https://huggingface.co/jphme/em_german_leo_mistral)        |
| EMG          | 70B        | [EM German 70b v01](https://huggingface.co/jphme/em_german_70b_v01)                |
| SKLM         | 7B         | [SauerkrautLM Her0](https://huggingface.co/VAGOsolutions/SauerkrautLM-7b-HerO)     |
| GPT4         | NA         | [OpenAI GPT4 Turbo](https://platform.openai.com/docs/models/gpt-4-and-gpt-4-turbo) |
