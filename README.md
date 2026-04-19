# Probabilistic Benchmark Forecasts in Macroeconomics

This repository contains all code used for the creation of plots, predictions, and evaluations for the Bachelor's Thesis:

**"Comparing Benchmark Forecasting Methods for Uncertainty Quantification in Macroeconomics"**

---

## Repository Structure

### 1. Scripts

The `scripts` folder contains all implementation code for the forecasting methods and supporting functionality.

#### Method-specific code

Each forecasting method has its own subfolder:


Within each method folder, there are two main R files:

- `..._prediction.R`  
  Contains function calls and generates predictions. Outputs are saved in the `results` folder.

- `..._functions.R`  
  Contains the core implementation of the prediction methods.

---

#### Utility functions

General-purpose functions are located in:

- `data_transformation_functions.R`  
  Functions used to transform raw data from:
  
- `utility_functions.R`  
Contains general helper functions, including scoring functions.  
These functions are tailored to the structure of the prediction and input dataframes.

---

#### Time series models

Time series forecasting code is located in:

These scripts handle the fitting of time series models and generate **point forecasts**, which are saved in:

---

#### Graphics and plots

All code used to generate plots and illustrations is located in:

The plotting scripts use the aggregated evaluation dataset:

---

### 2. Data

- **Raw data**  
- **Processed data (point forecasts)**  
- **Evaluation results**  
This folder contains:
- A summary Excel file  
- A CSV file with the evaluation dataframe (2001–2012)  
- The most recent evaluation datasets used in the thesis  
- A Python script to merge individual evaluation dataframes into a single CSV file  

---

### 3. Results

All outputs are stored in:


- Separate subfolders exist for each forecasting method  
- Each folder contains:
  - Predictions  
  - Evaluation results  
- Results are provided for:
  - Quarterly forecasts  
  - Annual forecasts  
- Across all forecast sources (referred to as *datasets* in the code)

---

## How to Run the Code

- Method scripts require manual adjustment of function call parameters and file paths.  
- Users need to modify strings to call specific configurations.

**Example:**
- For Gaussian predictions:
  - Adjust parameters such as:
    - `fitted_mean = TRUE / FALSE`
    - `unbiased_sd = TRUE / FALSE`
  - Update folder paths and file names according to the selected configuration  

- Output files are automatically saved with timestamps.

---

### Computational Considerations

- Many prediction methods use multithreading.  
- Users should consider:
  - Number of available CPU cores/threads  
  - Available system memory  

- Some methods (especially **Bayesian Quantile Regression**) have high memory requirements.  
- Running all function calls simultaneously may lead to crashes or interruptions.  

**Recommendation:**
- Execute only a few function calls at a time  
- Monitor memory usage during execution  

---

## AI Disclaimer

Generative AI was used in the development of this codebase and is explicitly disclosed.

- **Model used:** GPT-5  
- **Usage levels:**

### Low
- Minor assistance such as debugging  
- Self-written code was provided and suggestions were applied  

### Medium
- Partial code generation (e.g., individual lines or function calls)  
- Example:
  - Writing `dplyr` operations such as grouping and calculating interval widths  
- Code was always reviewed and adapted  
- Does **not** include fully generated scripts  

### High
- Fully generated code segments  
- Examples:
  - Plotting with `ggplot` (e.g., WIS visualization with facet wrapping)  
  - Simulation of simple processes (e.g., AR(1))  

---

## Notes

- All evaluation and plotting rely on the aggregated evaluation dataset.  
- Functions are designed specifically for the structure of the input and prediction data used in this project.
