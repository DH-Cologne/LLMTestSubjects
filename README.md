# LLMTestSubjects
Series of experiments to evaluate whether LLMs can be used to replace participants in linguistic experiments.

## Pronomina
Based on the two papers ([Patterson and Schumacher 2021](https://www.cambridge.org/core/journals/applied-psycholinguistics/article/interpretation-preferences-in-contexts-with-three-antecedents-examining-the-role-of-prominence-in-german-pronouns/E8F581347980C5A0A3D3D938B8F8F30A) and [Patterson et al. 2022](https://www.frontiersin.org/articles/10.3389/fpsyg.2021.672927/full) - further ExpB and ExpA), we analyse whether LLMs 
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

| Round | Abbreviation | Parameters | Link to model                                                                                                      |
|-------|--------------|------------|--------------------------------------------------------------------------------------------------------------------|
| 1     | EGLM         | 7B         | [EM German Leo Mistral](https://huggingface.co/jphme/em_german_leo_mistral)                                        |
| 1     | EMG          | 70B        | [EM German 70b v01](https://huggingface.co/jphme/em_german_70b_v01)                                                |
| 1     | SKLM         | 7B         | [SauerkrautLM Her0](https://huggingface.co/VAGOsolutions/SauerkrautLM-7b-HerO)                                     |
| 1     | GPT4         | NA         | [OpenAI GPT4 Turbo](https://platform.openai.com/docs/models/gpt-4-and-gpt-4-turbo)                                 |
|       |              |            |                                                                                                                    |
| 2     | ML3          | 8B         | [Meta-Llama-3-8B-Instruct](https://huggingface.co/meta-llama/Meta-Llama-3-8B-Instruct)                             |
| 2     | ML3          | 70B        | [Meta-Llama-3-70B-Instruct](https://huggingface.co/meta-llama/Meta-Llama-3-70B-Instruct)                           |
| 2     | DL3          | 8B         | [Llama3-DiscoLeo-Instruct 8B (version 0.1)](https://huggingface.co/DiscoResearch/Llama3-DiscoLeo-Instruct-8B-v0.1) |
| 2     | SK3          | 8B         | [Llama-3-SauerkrautLM-8b-Instruct](https://huggingface.co/VAGOsolutions/Llama-3-SauerkrautLM-8b-Instruct)          |
| 2     | SK3          | 70B        | [Llama-3-SauerkrautLM-70b-Instruct](https://huggingface.co/VAGOsolutions/Llama-3-SauerkrautLM-70b-Instruct)        |
| 2     | KFK3         | 8B         | [Llama-3-KafkaLM-8B-v0.1](https://huggingface.co/seedboxai/Llama-3-KafkaLM-8B-v0.1)                                |
| 2     | PHI3         | 8B         | [Phi-3-medium-4k-instruct](https://huggingface.co/microsoft/Phi-3-medium-4k-instruct)                              |

#### Differences between Round 1 and Round 2

Since Round 1 was made prior to the release of Meta Llama 3, some differences exist between the two rounds:

- Round 1 experiments were run using different Quanizations (Q4 and Q5), while Round 2 experiments were run using Q6.
- Round 2 models had some issues adhering to the Experiment B question, thus we had to use a 1-shot prompt.

### Step 3: Read out the answers given by the LLMs and aggregate them together with the experiment data.

Since ExpA (completion) examined ditransitive verbs (ExpA1) and benefactive verbs (ExpA2), we've generated two a new data frames for ExpA 
(ExpA1DataAnswers and ExpA2DataAnswers). 
Data from ExpB and rating-answers from LLMs were collected within a one data frame (ExpBDataAnswers). 
See 02A_CollectCompletionAnswers.R and 02B_CollectRatingAnswers.R

### Step 4: Analyse behaviour of LLMs compared to behaviour of participants of the original experiments
See 02A_AnalyseCompletionAnswers.R and 03B_AnalyseRatingAnswers.R
