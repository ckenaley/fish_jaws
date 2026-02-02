library(StereoMorph)

image_directory <- "data/all_images"
landmark_directory <- "data/jaw_landmarks"

#run once
# dir.create("data/all_images")
# f <- list.files("data/images",full.names = T,recursive = T)
# file.copy(f,paste0("data/all_images/",basename(f)))

if (!dir.exists(landmark_directory)) { dir.create(landmark_directory) }


# individual fixed landmarks not associated with a curve:
landmarks_ref <- c("7_meck_foss")


# curves are defined in a 3-column matrix: curve name, curve start, curve end
curves_ref <- matrix(c("jaw_dorsal", "1_ant_tip", "2_cor_process", # make a curve called 'head' which connects the premax to the start of the dorsal fin
                       "jaw_cor_joint", "2_cor_process", "4_joint",
                       "jaw_joint_art", "4_joint", "3_art_process",
                       "jaw_art_ret","3_art_process","2_ret_process",
                       "jaw_vent","2_ret_process","1_ant_tip"
),
ncol = 3, byrow = TRUE)


digitizeImages(image.file = image_directory,
               shapes.file = landmark_directory, 
               landmarks.ref = landmarks_ref,
               curves.ref = curves_ref, landmark.color.blur = "cyan", 
               landmark.color.focus = "yellow", 
               control.point.color.blur = "orchid", control.point.color.focus = "hotpink", 
               landmark.radius = 5)

