# 1) chercher les données
#
# 4 fichiers: 
#     "2.CLSA METABOLOMICS_v2_July2024"
#     "25CA004_UdeM_AJTessier_Baseline"
#     "25CA004_UdeM_AJTessier_FUP1"
#     "25CA004_UdeM_AJTessier_FUP2"

list.files("/project/def-ajtess/clsa_data/25CA004_UdeM_AJTessier_Baseline")
baseline<- read.csv("/project/def-ajtess/clsa_data/25CA004_UdeM_AJTessier_Baseline/25CA004_UdeM_AJTessier_Baseline_CoPv7-1_Qx_PA_BS.csv")
View(baseline)
