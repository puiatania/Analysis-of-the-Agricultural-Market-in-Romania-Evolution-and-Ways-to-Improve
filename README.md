# Analysis-of-the-Agricultural-Market-in-Romania-Evolution-and-Ways-to-Improve
This repository includes the code, data (anonymized), and documentation related to my thesis: 'Analysis of the Agricultural Market in Romania: Evolution and Improvement Strategies'. The project involves statistical modeling using R (VECM analysis), market trends evaluation, and survey-based research.

# 📊 Agricultural Market Analysis in Romania

## 📌 Overview
This repository contains research, statistical analysis, and supporting materials related to my thesis: **"Analysis of the Agricultural Market in Romania: Evolution and Improvement Strategies"**. The project includes:

- Data-driven insights into agricultural trends in Romania.
- Statistical modeling using **Vector Error Correction Models (VECM)**.
- Survey-based research on consumer preferences for organic products.
- Anonymized datasets and R scripts for reproducible analysis.
- Various econometric and statistical techniques applied to understand price indices, market trends, and consumer behavior.

## 📁 Repository Structure
```
📂 thesis-repo
│── 📂 code/          # R scripts for data analysis
│   ├── VECM.R
│   ├── chestionar.R
│
│── 📂 data/          # Anonymized datasets
│   ├── cleaned_chestionar.csv
│   ├── synthetic_data.xlsx
│
│── 📂 docs/          # Documentation and key findings
│   ├── thesis_summary.md
│   ├── survey_results.txt
│   ├── key_findings.pdf
│
│── 📂 outputs/       # Generated reports and visualizations
│   ├── VECM.html
│   ├── chestionar.html
│
│── README.md         # Project description and instructions
```

## 🔬 Methodology and Analyses Used
The research incorporates multiple statistical and econometric methods, including:

### **1️⃣ Time Series Analysis**
- **Vector Error Correction Model (VECM)**: Used to analyze the long-term relationship between Romanian and EU agricultural price indices.
- **Stationarity Tests**:
  - Augmented Dickey-Fuller (ADF)
  - Phillips-Perron (PP)
  - KPSS Test
- **Cointegration Analysis** to assess long-term equilibrium relationships between variables.
- **Impulse Response Functions (IRF)** to observe the dynamic impact of shocks on agricultural price indices.

### **2️⃣ Exploratory Data Analysis (EDA)**
- **Descriptive statistics** to summarize key trends in price indices and investment patterns.
- **Correlation matrices and heatmaps** to identify relationships between different economic variables.
- **Data visualization** using ggplot2 and corrplot for better interpretability.

### **3️⃣ Survey Data Analysis**
- **Chi-Square Tests** to determine associations between demographic factors and consumer preferences.
- **Correspondence Analysis (CA)** to identify relationships between categorical variables in the survey data.
- **Principal Component Analysis (PCA)** for dimensionality reduction in multi-variable datasets.

## 📌 Key Findings
- The **VECM model** indicates a long-term relationship between agricultural prices in Romania and the EU market.
- Survey results show a **growing consumer preference** for organic products, but price remains a major barrier.
- Statistical tests confirm that certain economic factors significantly impact **investment trends in agriculture**.
- Cointegration analysis suggests that Romanian agricultural markets are influenced by EU-wide economic trends.
- Chi-square tests reveal that income levels and education significantly impact the choice of organic products.


