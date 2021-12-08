source("setup.R")

ROOT_ <- "/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout"
accelerometry_phase1 <- file.path(ROOT_,"data/test/Actigraphy/Phase1")
accelerometry_phase2 <- file.path(ROOT_,"data/test/Actigraphy/Phase2")
gps_phase1 <- file.path(ROOT_,"data/test/GPS/Phase2")
gps_phase2 <- file.path(ROOT_,"data/test/GPS/Phase2")
path_result <- process_many_subject(auto_search = TRUE, acc_folder_path = accelerometry_phase2, gps_folder_path = gps_phase2)
