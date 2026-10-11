o_ngoai <- "F:/R_project/library"

if (dir.exists(o_ngoai)) {
    .libPaths(c(o_ngoai, .libPaths()))
    message("[OK] Da ket noi thu vien tu o cung di dong.")
} else {
    message("[CANH BAO] Khong tim thay o cung ngoai. Package se duoc tim o o C.")
}

# Bước 2: Check package 'haven' if not
if (!requireNamespace("haven", quietly = TRUE)) {
    message("Dang tien hanh tai va cai dat package 'haven' vao o cung ngoai...")
    install.packages("haven")
}

# Bước 3: KÍCH HOẠT THƯ VIỆN
library(haven)
library(tidyverse) # Đã bao gồm dplyr, ggplot2, stringr, purrr
library(data.table)# Đọc dữ liệu siêu nhanh
library(lubridate) # Xử lý ngày tháng
library(tidytext)  # Khai thác văn bản
install.packages(tidytext)
install.packages("tidytext")
du_lieu <- read_csv("F:/Dataset/archive/clin_trials.csv")
glimpse(du_lieu)
## Clean data
trials_clean <- du_lieu %>%
    mutate(
        #Start Date` 
        start_year = str_extract(`Start Date`, "^\\d{4}") %>% as.numeric(),
             
            #  `Organization Class`
            sponsor_type = case_when(
                `Organization Class` == "INDUSTRY" ~ "Industry",
                `Organization Class` == "OTHER"    ~ "Academic / Hospital",
                `Organization Class` %in% c("NIH", "FED") ~ "Government",
                TRUE ~ "Other / Unknown"
                 ),
        
              # Phase clean `Phases`
            phase_clean = case_when(
                str_detect(Phases, "PHASE1") ~ "Phase 1",
                str_detect(Phases, "PHASE2") ~ "Phase 2",
                str_detect(Phases, "PHASE3") ~ "Phase 3",
                str_detect(Phases, "PHASE4") ~ "Phase 4",
                is.na(Phases) | Phases == "Unknown" ~ "Not Specified",
                TRUE ~ "Other / Early Phase"
                      )
            ) %>%
# Research from 2000 until now
     filter(!is.na(start_year) & start_year >= 2000 & start_year <= 2026)

     # 3. Filter from  `Conditions`
    top_conditions <- trials_clean %>%
    filter(!is.na(Conditions)) %>%
    separate_rows(Conditions, sep = ",\\s*") %>%
    mutate(Conditions = str_trim(Conditions)) %>%
    count(Conditions, sort = TRUE) %>%
    head(15)
View(trials_clean)
library(tidyverse)
library(gtsummary)
# 4. Filter from `Medical Subject Headings`
top_mesh <- trials_clean %>%
    filter(!is.na(`Medical Subject Headings`)) %>%
    separate_rows(`Medical Subject Headings`, sep = ",\\s*") %>%
    mutate(mesh_term = str_trim(`Medical Subject Headings`)) %>%
    filter(mesh_term != "") %>%
    count(mesh_term, sort = TRUE) %>%
    head(15)
View(trials_clean)
View(top_conditions)
View(top_mesh)
# 1. Phase trends (lọc từ 2000 đến 2025)
phase_trends <- trials_clean %>%
    filter(phase_clean %in% c("Phase 1", "Phase 2", "Phase 3", "Phase 4")) %>%
    filter(start_year >= 2000 & start_year <= 2025) %>%
    count(start_year, phase_clean)

# 2. Graph
p1 <- ggplot(phase_trends, aes(x = start_year, y = n, color = phase_clean)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    scale_color_brewer(palette = "Set1") +
    
    
    scale_x_continuous(
        breaks = seq(2000, 2025, by = 5), # Having: 2000, 2005, 2010, 2015, 2020, 2025
        limits = c(2000, 2025)             # From 2000 to 2005
    ) +
    
    labs(
        title = "Clinical Trial Trends by Phase (2000–2025)",
        subtitle = "Annual number of initiated trials categorized by phase",
        x = "Start Year",
        y = "Number of Trials",
        color = "Phase"
    ) +
    theme_minimal(base_size = 14) + 
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 16)
    )

# Print
print(p1)
library(tidyverse)

# Filter and count MeSH Headings
top_mesh <- trials_clean %>%
    filter(!is.na(`Medical Subject Headings`) & `Medical Subject Headings` != "") %>%
    separate_rows(`Medical Subject Headings`, sep = ",\\s*|;\\s*") %>%
    mutate(mesh_term = str_trim(`Medical Subject Headings`)) %>%
    filter(mesh_term != "") %>%
    count(mesh_term, sort = TRUE) %>%
    head(15)

# Graph
ggplot(top_mesh, aes(x = reorder(mesh_term, n), y = n)) +
    geom_col(fill = "#2b5c8f", alpha = 0.85) +
    coord_flip() +
    labs(
        title = "Top 15 Medical Subject Headings (MeSH)",
        x = "MeSH Term",
        y = "Number of Trials"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14))
#  Top 5 Conditions
top5_conditions_list <- trials_clean %>%
    filter(!is.na(Conditions) & Conditions != "") %>%
    separate_rows(Conditions, sep = ",\\s*") %>%
    mutate(condition_clean = str_trim(Conditions)) %>%
    filter(condition_clean != "") %>%
    count(condition_clean, sort = TRUE) %>%
    slice_max(n, n = 5) %>%
    pull(condition_clean)

# Condition_trends_by year
condition_trends <- trials_clean %>%
    filter(!is.na(Conditions)) %>%
    separate_rows(Conditions, sep = ",\\s*") %>%
    mutate(condition_clean = str_trim(Conditions)) %>%
    filter(condition_clean %in% top5_conditions_list) %>%
    count(start_year, condition_clean)

# Trend graph
ggplot(condition_trends, aes(x = start_year, y = n, color = condition_clean)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.8) +
    scale_x_continuous(breaks = seq(2000, 2025, by = 5), limits = c(2000, 2025)) +
    scale_color_brewer(palette = "Set1") +
    labs(
        title = "Trends of Top 5 Medical Conditions (2000–2025)",
        x = "Start Year",
        y = "Number of Trials",
        color = "Condition"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14)
    )
library(tidyverse)

# STEP 1: filter trials have tag "Healthy"
healthy_trials <- trials_clean %>%
    filter(
        str_detect(Conditions, "(?i)healthy") | 
            str_detect(`Medical Subject Headings`, "(?i)healthy")
    )

# -------------------------------------------------------------
# Top 15 conditions (MeSH Headings) have "Healthy"
# -------------------------------------------------------------
healthy_mesh_diseases <- healthy_trials %>%
    filter(!is.na(`Medical Subject Headings`)) %>%
    separate_rows(`Medical Subject Headings`, sep = ",\\s*|;\\s*") %>%
    mutate(mesh_term = str_trim(`Medical Subject Headings`)) %>%
    # Delete "Healthy", keep "mesh term"
    filter(!str_detect(mesh_term, "(?i)healthy") & mesh_term != "") %>%
    count(mesh_term, sort = TRUE) %>%
    head(15)

# Print
print(healthy_mesh_diseases)

# -------------------------------------------------------------
# Filter_Therapeutic Areas
# -------------------------------------------------------------
healthy_by_area <- healthy_trials %>%
    mutate(
        # text_combined
        text_combined = paste(Conditions, `Medical Subject Headings`, `Brief Title`, sep = " "),
        
        # therapeutic_area
        therapeutic_area = case_when(
            str_detect(text_combined, "(?i)cancer|neoplasm|tumor|oncology|carcinoma|lymphoma|leukemia") ~ "Oncology (Ung thư)",
            str_detect(text_combined, "(?i)hypertension|cardiovascular|heart|vascular|coronary|arrhythmia") ~ "Cardiovascular (Tim mạch)",
            str_detect(text_combined, "(?i)diabetes|obesity|metabolic|lipid|glucose|insulin") ~ "Metabolism / Endocrinology (Chuyển hóa)",
            str_detect(text_combined, "(?i)alzheimer|parkinson|neurology|depression|schizophrenia|pain|central nervous") ~ "Neurology / Psychiatry (Thần kinh - Tâm thần)",
            str_detect(text_combined, "(?i)covid|influenza|hiv|hepatitis|infection|vaccine|bacterial|viral") ~ "Infectious Diseases / Vaccines (Truyền nhiễm / Vaccine)",
            str_detect(text_combined, "(?i)asthma|copd|pulmonary|respiratory") ~ "Respiratory (Hô hấp)",
            TRUE ~ "Other / General Phase 1"
        )
    ) %>%
    count(therapeutic_area, sort = TRUE)

# Print
print(healthy_by_area)
# Print Healthy Trials_Therapeutic areas
ggplot(healthy_by_area, aes(x = reorder(therapeutic_area, n), y = n)) +
    geom_col(fill = "#008080", alpha = 0.85) +
    geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 3.8) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
        title = "Distribution of Healthy Volunteer Trials by Therapeutic Area",
        subtitle = "Analysis of Phase 1 / PK / Safety trials involving healthy participants",
        x = "Therapeutic Area",
        y = "Number of Trials"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.major.y = element_blank()
    )
library(tidyverse)

# 1. attrition_data
attrition_data <- trials_clean %>%
    filter(`Overall Status` %in% c("COMPLETED", "TERMINATED", "WITHDRAWN", "SUSPENDED")) %>%
    mutate(
        # B 1 = stop, 0 = completed
        is_stopped = if_else(`Overall Status` %in% c("TERMINATED", "WITHDRAWN", "SUSPENDED"), 1, 0),
        status_group = if_else(is_stopped == 1, "Stopped / Terminated", "Completed")
    )

# Percent for stop trial
attrition_data %>%
    count(status_group) %>%
    mutate(pct = round(n / sum(n) * 100, 2))
# Phase_attrition_phase
phase_attrition <- attrition_data %>%
    filter(phase_clean %in% c("Phase 1", "Phase 2", "Phase 3", "Phase 4")) %>%
    group_by(phase_clean) %>%
    summarise(
        total_trials = n(),
        stopped_trials = sum(is_stopped),
        failure_rate = round(mean(is_stopped) * 100, 2)
    )

# Graph_termination rate by phase
ggplot(phase_attrition, aes(x = phase_clean, y = failure_rate, fill = phase_clean)) +
    geom_col(width = 0.6, alpha = 0.85) +
    geom_text(aes(label = paste0(failure_rate, "%")), vjust = -0.5, fontface = "bold") +
    scale_fill_brewer(palette = "Reds") +
    scale_y_continuous(limits = c(0, max(phase_attrition$failure_rate) * 1.2)) +
    labs(
        title = "Trial Termination Rate by Phase",
        subtitle = "Identifying the Fail Phase in clinical development",
        x = "Phase",
        y = "Termination Rate (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))
# Phase_organization class
sponsor_phase_attrition <- attrition_data %>%
    filter(phase_clean %in% c("Phase 1", "Phase 2", "Phase 3", "Phase 4")) %>%
    filter(sponsor_type %in% c("Industry", "Academic / Hospital")) %>%
    group_by(phase_clean, sponsor_type) %>%
    summarise(
        total = n(),
        failure_rate = round(mean(is_stopped) * 100, 2),
        .groups = "drop"
    )

# Graph 
ggplot(sponsor_phase_attrition, aes(x = phase_clean, y = failure_rate, fill = sponsor_type)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(
        aes(label = paste0(failure_rate, "%")), 
        position = position_dodge(width = 0.7), 
        vjust = -0.5, size = 3.5
    ) +
    scale_fill_manual(values = c("Industry" = "#2b5c8f", "Academic / Hospital" = "#d95f02")) +
    scale_y_continuous(limits = c(0, max(sponsor_phase_attrition$failure_rate) * 1.2)) +
    labs(
        title = "Termination Rate: Industry vs. Academic Across Phases",
        x = "Phase",
        y = "Termination Rate (%)",
        fill = "Sponsor Type"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
# Top 10 conditions_termination rate
condition_attrition <- attrition_data %>%
    filter(!is.na(Conditions) & Conditions != "") %>%
    separate_rows(Conditions, sep = ",\\s*") %>%
    mutate(condition_clean = str_trim(Conditions)) %>%
    filter(condition_clean != "" & condition_clean != "Healthy") %>%
    group_by(condition_clean) %>%
    summarise(
        total_trials = n(),
        stopped_trials = sum(is_stopped),
        failure_rate = round(mean(is_stopped) * 100, 2)
    ) %>%
    filter(total_trials >= 500) %>% 
    slice_max(total_trials, n = 10)

# Graph
ggplot(condition_attrition, aes(x = reorder(condition_clean, failure_rate), y = failure_rate)) +
    geom_col(fill = "#b2182b", alpha = 0.85) +
    geom_text(aes(label = paste0(failure_rate, "%")), hjust = -0.1) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(condition_attrition$failure_rate) * 1.25)) +
    labs(
        title = "Termination Rate Among Top 10 Conditions",
        x = "Condition",
        y = "Termination Rate (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
# 1. Chi-square: Sponsor Type_termination rate
chisq_test <- chisq.test(table(attrition_data$sponsor_type, attrition_data$is_stopped))
print(chisq_test)

# 2. Logistic Regression 
model <- glm(
    is_stopped ~ phase_clean + sponsor_type, 
    data = attrition_data %>% filter(phase_clean %in% c("Phase 1", "Phase 2", "Phase 3", "Phase 4")), 
    family = binomial
)

#Print Odds Ratio
summary(model)
exp(coef(model)) # Odds Ratio > 1 nghĩa là tăng rủi ro bị dừng
library(tidyverse)
library(lubridate)

library(tidyverse)

intervention_classified <- trials_clean %>%
    filter(!is.na(Interventions) | !is.na(`Intervention Description`)) %>%
    mutate(
        # combined text
        combined_text = paste(Interventions, `Intervention Description`, sep = " "),
        
        # Drug class
        drug_class = case_when(
            # 1. Gen & cell
            str_detect(combined_text, "(?i)car-t|gene therapy|mrna|crispr|vector|adeno|stem cell|chimeric") ~ "Gene / Cell Therapy",
            
            # 2. Antibody
            str_detect(combined_text, "(?i)mab|monoclonal|antibody|biologic|fusion protein|recombinant|biosimilar") ~ "Biologics / Monoclonal Ab",
            
            # 3. Vaccine
            str_detect(combined_text, "(?i)vaccine|immunization|prophylactic vaccine") ~ "Vaccine",
            
            # 4. Remaining
            TRUE ~ "Others/Device/Behaviour/Small Molecule / Chemical Drug"
        )
    )

# Percent each class
intervention_classified %>%
    count(drug_class, sort = TRUE) %>%
    mutate(percentage = round(n / sum(n) * 100, 2))
library(tidyverse)

# 1. Percent class_year
intervention_trends <- intervention_classified %>%
    filter(!is.na(start_year) & start_year >= 2000 & start_year <= 2025) %>%
    count(start_year, drug_class)

# 2. Graph (100% Stacked Area Chart)
p_modality_share <- ggplot(intervention_trends, aes(x = start_year, y = n, fill = drug_class)) +
    # position = "fill" 
    geom_area(position = "fill", alpha = 0.85) +
    
    # transfer to (0%, 25%, 50%, 75%, 100%)
    scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(2000, 2025, by = 5), limits = c(2000, 2025), expand = c(0, 0)) +
    
    # Color
    scale_fill_manual(values = c(
        "Gene / Cell Therapy"            = "#e41a1c", # Đỏ 
        "Biologics / Monoclonal Ab"      = "#377eb8", # Xanh dương
        "Vaccine"                        = "#ff7f00", # Cam
        "Small Molecule / Chemical Drug" = "#4daf4a"  # Xanh lá
    )) +
    
    labs(
        title = "Proportion of Clinical Intervention Modalities Over Time (2000–2025)",
        x = "Start Year",
        y = "Proportion (%)",
        fill = "Modality Class"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank()
    )

# Print graph
print(p_modality_share)

# 3.Print out
ggsave(
    filename = "intervention_modality_proportion.png",
    plot = p_modality_share,
    width = 10,
    height = 6,
    dpi = 300
)
library(knitr)
library(gt)       # Dùng tạo bảng báo cáo chuẩn xuất bản


# 1. intervention%_year
modality_summary <- intervention_classified %>%
    filter(!is.na(start_year) & start_year >= 2000 & start_year <= 2025) %>%
    mutate(
        period = case_when(
            start_year <= 2005 ~ "2000–2005",
            start_year <= 2010 ~ "2006–2010",
            start_year <= 2015 ~ "2011–2015",
            start_year <= 2020 ~ "2016–2020",
            TRUE               ~ "2021–2025"
        )
    ) %>%
    count(period, drug_class) %>%
    group_by(period) %>%
    mutate(
        total_in_period = sum(n),
        pct = round(n / total_in_period * 100, 1)
    ) %>%
    ungroup()


table_pct_wide <- modality_summary %>%
    mutate(display_val = paste0(pct, "% (", scales::comma(n), ")")) %>%
    select(period, drug_class, display_val) %>%
    pivot_wider(
        names_from = drug_class, 
        values_from = display_val, 
        values_fill = "0% (0)"
    )

# -------------------------------------------------------------
# Show table in R
# -------------------------------------------------------------
gt_table <- table_pct_wide %>%
    gt() %>%
    tab_header(
        title = md("**Shift in Clinical Intervention Modalities (2000–2025)**"),
        subtitle = "Proportion (%) and Trial Count by 5-Year Cohorts"
    ) %>%
    cols_label(
        period = "Time Period",
        `Others/Device/Behaviour/Small Molecule / Chemical Drug` = "Others",
        `Biologics / Monoclonal Ab` = "Biologics / Monoclonal Ab",
        `Gene / Cell Therapy` = "Gene / Cell Therapy",
        `Vaccine` = "Vaccine"
    ) %>%
    tab_options(
        heading.title.font.size = 14,
        column_labels.font.weight = "bold"
    )

# Print table gt
print(gt_table)
colnames(table_pct_wide)
install.packages("writexl")
library(writexl)
write_xlsx(table_pct_wide, "intervention_modality_summary.xlsx")
library(tidyverse)

# SFilter again drug_class  (exclude Device, Behavioral...)
# 
drugs_only <- trials_clean %>%
    filter(!is.na(Interventions)) %>%

    filter(!str_detect(Interventions, "(?i)^device:|^behavioral:|^procedure:"))

#  Regex pharmacology
intervention_classified_v2 <- drugs_only %>%
    mutate(
        combined_text = paste(Interventions, `Intervention Description`, sep = " "),
        
        drug_class = case_when(
            # 1. Gene therapy
            str_detect(combined_text, "(?i)\\b\\w+cel\\b|car-t|gene therapy|mrna|crispr|adeno|lentivirus|aav\\d?|vector") ~ "Gene / Cell Therapy",
            
            # 2.  -mab, -cept, monoclonal, recombinant...)
            str_detect(combined_text, "(?i)\\b\\w+mab\\b|\\b\\w+cept\\b|monoclonal|antibody|biologic|fusion protein|recombinant|biosimilar") ~ "Biologics / Monoclonal Ab",
            
            # 3. Vaccine
            str_detect(combined_text, "(?i)vaccine|immunization") ~ "Vaccine",
            
            # 4.  (Small Molecules / Chemical Drugs)
            TRUE ~ "Small Molecule / Chemical Drug"
        )
    )
library(tidyverse)
library(gt)
library(writexl)

# ==============================================================================
# Filter confounding factor
# ==============================================================================

# 1. Keep pharmaceutical trials
drugs_only <- trials_clean %>%
    filter(!is.na(Interventions) & Interventions != "") %>%
    # Reject device_behaviour_procedure_diagnostic
    filter(!str_detect(Interventions, "(?i)^device:|^behavioral:|^procedure:|^diagnostic:"))

# 2. intervention_classified_v2
intervention_classified_v2 <- drugs_only %>%
    mutate(
        # text_combines
        text_combined = paste(Interventions, `Intervention Description`, sep = " "),
        
        drug_class = case_when(
            # Group 1: -cel, CAR-T, CRISPR, Vector...)
            str_detect(text_combined, "(?i)\\b\\w+cel\\b|car-t|gene therapy|mrna|crispr|adeno|lentivirus|aav\\d?|vector") ~ "Gene / Cell Therapy",
            
            # Group 2:  -mab, -cept, Monoclonal, Recombinant...)
            str_detect(text_combined, "(?i)\\b\\w+mab\\b|\\b\\w+cept\\b|monoclonal|antibody|biologic|fusion protein|recombinant|biosimilar") ~ "Biologics / Monoclonal Ab",
            
            # Group 3: Vaccine
            str_detect(text_combined, "(?i)vaccine|immunization") ~ "Vaccine",
            
            # Group 4: Small molecule)
            TRUE ~ "Small Molecule / Chemical Drug"
        )
    )

# ==============================================================================
# (5-YEAR COHORTS)
# ==============================================================================

# 1. %intervention_classified_v2
modality_summary_v2 <- intervention_classified_v2 %>%
    filter(!is.na(start_year) & start_year >= 2000 & start_year <= 2025) %>%
    mutate(
        period = case_when(
            start_year <= 2005 ~ "2000–2005",
            start_year <= 2010 ~ "2006–2010",
            start_year <= 2015 ~ "2011–2015",
            start_year <= 2020 ~ "2016–2020",
            TRUE               ~ "2021–2025"
        )
    ) %>%
    count(period, drug_class) %>%
    group_by(period) %>%
    mutate(
        total_in_period = sum(n),
        pct = round(n / total_in_period * 100, 1),
        display_val = paste0(pct, "% (", scales::comma(n), ")")
    ) %>%
    ungroup()

# 2. Pivot Wider
table_modality_wide <- modality_summary_v2 %>%
    select(period, drug_class, display_val) %>%
    pivot_wider(
        names_from = drug_class,
        values_from = display_val,
        values_fill = "0% (0)"
    )

# ==============================================================================
# (GT TABLE & EXCEL)
# ==============================================================================

# 1. Table modality `gt`
gt_modality_table <- table_modality_wide %>%
    gt() %>%
    tab_header(
        title = md("**Shift in Clinical Drug Intervention Modalities (2000–2025)**"),
        subtitle = "Proportion (%) and Trial Count of Drug/Biological Interventions by 5-Year Cohorts"
    ) %>%
    cols_label(
        period = "Time Period" 
    ) %>%
    tab_options(
        heading.title.font.size = 14,
        column_labels.font.weight = "bold",
        table.font.size = 12
    )

# Print
print(gt_modality_table)

# 2. file Excel 
write_xlsx(table_modality_wide, "clinical_modality_trends_refined.xlsx")
library(gtsummary)
install.packages("parameters")
library(parameters)
# 1. Logistic regression
tbl_reg_output <- model %>%
    tbl_regression(
        exponentiate = TRUE, 
        label = list(
            phase_clean ~ "Clinical Phase",
            sponsor_type ~ "Sponsor Category"
        )
    ) %>%
    bold_p() %>%        
    add_global_p()       
# Print 
tbl_reg_output

library(gtsummary)
library(flextable)

# 1.  gtsummary to flextable
flex_tbl <- tbl_reg_output %>%
    as_flex_table() %>%
    autofit()

# 2. file Word 
save_as_docx(flex_tbl, path = "logistic_regression_full.docx")

library(tidyverse)
library(gtsummary)
library(lubridate)

# ==============================================================================
# Pre-analyst
# ==============================================================================

model_data <- trials_clean %>%
    # 1. Filter trials have clear status
    filter(`Overall Status` %in% c("COMPLETED", "TERMINATED", "WITHDRAWN", "SUSPENDED")) %>%
    mutate(
        #  (Outcome): 1 = terminated, 0 = completed
        is_stopped = if_else(`Overall Status` %in% c("TERMINATED", "WITHDRAWN", "SUSPENDED"), 1, 0),
        
        # 2. (start_year) from'Start Date'
        # 
        start_year = as.numeric(substr(`Start Date`, 1, 4)),
        
        # 3. Primary Purpose
        primary_purpose = case_when(
            str_detect(`Primary Purpose`, "(?i)treatment") ~ "Treatment",
            str_detect(`Primary Purpose`, "(?i)prevention") ~ "Prevention",
            str_detect(`Primary Purpose`, "(?i)diagnostic") ~ "Diagnostic",
            str_detect(`Primary Purpose`, "(?i)supportive|support") ~ "Supportive Care",
            TRUE ~ "Other / Unspecified"
        ),
        primary_purpose = factor(primary_purpose, levels = c("Treatment", "Prevention", "Diagnostic", "Supportive Care", "Other / Unspecified")),
        
        # 4. condition_group
        condition_group = case_when(
            str_detect(Conditions, "(?i)cancer|neoplasm|carcinoma|tumor|oncology|leukemia|lymphoma") ~ "Oncology",
            str_detect(Conditions, "(?i)cardiovascular|heart|hypertension|infarction|stroke|vascular") ~ "Cardiovascular",
            str_detect(Conditions, "(?i)alzheimer|parkinson|depression|schizophrenia|neurolog|brain|mental") ~ "CNS / Neurology",
            str_detect(Conditions, "(?i)infection|viral|bacterial|covid|influenza|hiv|hepatitis") ~ "Infectious Diseases",
            str_detect(Conditions, "(?i)diabete|metabolic|obesity|thyroid") ~ "Endocrine / Metabolic",
            TRUE ~ "Other Conditions"
        ),
        condition_group = factor(condition_group, levels = c("Other Conditions", "Oncology", "Cardiovascular", "CNS / Neurology", "Infectious Diseases", "Endocrine / Metabolic")),
        
        #  (Reference level)_Phase_Sponsor
        phase_clean = factor(phase_clean, levels = c("Phase 1", "Phase 2", "Phase 3", "Phase 4")),
        sponsor_type = factor(sponsor_type, levels = c("Academic / Hospital", "Industry", "Government", "Other"))
    ) %>%
    
    drop_na(is_stopped, phase_clean, sponsor_type, start_year, primary_purpose, condition_group)


# ==============================================================================
# Logistic regression
# ==============================================================================

full_model <- glm(
    is_stopped ~ phase_clean + sponsor_type + start_year + primary_purpose + condition_group,
    data = model_data,
    family = binomial(link = "logit")
)


# ==============================================================================
#  ADJUSTED ODDS RATIO (aOR)
# ==============================================================================

tbl_full_model <- full_model %>%
    tbl_regression(
        exponentiate = TRUE, # Q
        label = list(
            phase_clean ~ "Clinical Phase",
            sponsor_type ~ "Sponsor Category",
            start_year ~ "Start Year (Continuous)",
            primary_purpose ~ "Primary Study Purpose",
            condition_group ~ "Disease Indication Group"
        )
    ) %>%
    bold_p(t = 0.05) %>%   
    add_global_p()        


tbl_full_model
library(flextable)
word_table <- tbl_full_model %>%
    as_flex_table() %>%
    autofit() 

save_as_docx(word_table, path = "Full_Logistic_Model_Results.docx")
