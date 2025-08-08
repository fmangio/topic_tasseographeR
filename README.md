# topic tasseographeR

topic tassepgrapheR is an R-based protocol aimed at making topic modelling journey less of hustle by leveraging upon top-down, bottom-up and LLM-based automated text analysis tasks.
It comprises three tools:
-topic *content* tasseographeR and topic *function* tasseographeR, which are aimed at computing respectively topic content and function scores useful for interpreting and/or validating topic modelling's outputs
-topic labelleR, which performs a fully automated topic labelling via direct prompting to Ollama LLMs

## How to Use

1. Open the project in RStudio.
2. Run the main script: `analysis.R` (or your script name).
3. Required packages:
   ```R
   install.packages(c("dplyr", "ggplot2"))
   
data/: Contains raw or cleaned data

scripts/: R scripts used for analysis

output/: Plots or results