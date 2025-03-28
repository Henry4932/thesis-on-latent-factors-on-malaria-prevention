# 🦟 Malaria Frailty Models  

This repository contains code and data for analyzing malaria prevention behaviors using **frailty models**. The study uses **inverse Gaussian frailty models** to account for unobserved heterogeneity in malaria prevention behaviors.

---

## 📁 Folder Structure  
The repository is organized as follows:

- **`code/`** - Contains R scripts for analysis  
- **`data/`** - Includes datasets used in the study (excluding sensitive data)  

---

## 🔧 Requirements  
To run the scripts, you need the following software and libraries:

### **Software**:
- R (latest version)  
- RStudio (for R users)  

### **R Libraries**:
- `survival` - for survival analysis  
- `frailty` - for implementing frailty models  
- `tidyverse` - for data manipulation
-  `haven` - for importing `.sav` (SPSS) datasets  
- `dplyr` - for data manipulation  
- `survival` - for survival analysis  
- `survminer` - for Kaplan-Meier plots  
- `ggsci` - for scientific color palettes  
- `ggplot2` - for advanced data visualization  
- `writexl` - for exporting data to Excel  
- `readxl` - for reading Excel files  
- `naniar` - for handling missing data  
- `mice` - for multiple imputation of missing values  
- `flexsurv` - for flexible parametric survival modeling  



---
### **Installation**  
If these libraries are not installed, run the following command in R:

```r
install.packages(c("haven", "dplyr", "survival", "survminer", "ggsci", "ggplot2", 
                   "writexl", "readxl", "naniar", "mice", "flexsurv"))

## 🚀 Usage  
### **1️⃣ Clone the Repository**  
To use this repository on your local machine, clone it using the following command:

```sh
git clone https://github.com/YOUR_USERNAME/thesis-on-latent-factors-on-malaria-prevention.git
