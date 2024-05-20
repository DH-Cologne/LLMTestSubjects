# LLMTestSubjects
Series of experiments to evaluate whether LLMs can be used to replace participants in linguistic experiments.

## Pronomina
Based on the two papers ([Patterson and Schumacher 2021](https://www.cambridge.org/core/journals/applied-psycholinguistics/article/interpretation-preferences-in-contexts-with-three-antecedents-examining-the-role-of-prominence-in-german-pronouns/E8F581347980C5A0A3D3D938B8F8F30A) and [Patterson et al. 2022](https://www.frontiersin.org/articles/10.3389/fpsyg.2021.672927/full) - further ExpA and ExpB), we analyse whether LLMs 
* could make similar judgements about the acceptability of pronoun references as human subjects.
* prefer the same R-expressions for pronouns as human subjects.
* can be used reliably in anaphora resolution.

### Step 1: Collect data from experiments, generate lists for participants/llms

The data of the experiments from the two papers is available online, references can be found on the linked pages. We received the combined data from the authors in a file that we pre-processed with the script 01_CreateParticipantsList.R

The code generates two data frames (ExpAData, ExpBData) that we use to generate 
our prompts for the large language models (folder ExperimentParticipantsLists) 
and exports them to RDS files.
These files allow us (and you) to feed the LLMs and use our code 
(02/03 A/B) to analyse our/your results without having to rely on the original data.  

### Step 2: Use LLMs as simulated participants

We used the generated participants lists to generate the prompts for the various LLMs (that we used as simulated participants). 

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

### Step 3: Read out the answers given by the LLMs and aggregate them together with the experiment data.

Since ExpA (rating) examined ditransitive verbs (ExpA1) and benefactive verbs (ExpA2), we've generated two a new data frames for ExpA 
(ExpA1DataAnswers and ExpA2DataAnswers). 
Data from ExpB and completion-answers from LLMs were collected within a one data frame (ExpBDataAnswers). 
See 02A_CollectCompletionAnswers.R and 02B_CollectRatingAnswers.R

### Step 4: Analyse behaviour of LLMs compared to behaviour of participants of the original experiments
See 02A_AnalyseCompletionAnswers.R and 03B_AnalyseRatingAnswers.R
