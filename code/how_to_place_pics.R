rm (list = ls ())

# ---- Set parameters ----

SAMPLE_DURATION <- .15
TEST_DURATION <- 2

RETENTION_DURATION <- .9
ITI <- 2

N_TRIALS_PER_BLOCK <- 60
# They will be have the same viewport size as the main trials
N_TRIALS_PRACTISE <- 8

#BACKGROUND.COL <- "#808080"
BACKGROUND.COL <- "#FFFFFF"
RECT_SIDE_LENGTH <- .03
#RECT_SIDE_LENGTH <- .02
#SMALL_VIEWPORT_SCALE_VP <- .25
#SMALL_VIEWPORT_SCALE_VP <- .125
#SMALL_VIEWPORT_SCALE_VP <- 1/c(2, 4, 8)
SMALL_VIEWPORT_SCALE_VP <- 1/c(2, 4, 6)
# Relative magnification of objects in small viewport
SMALL_VIEWPORT_SCALE_OBJECT <- 1 / SMALL_VIEWPORT_SCALE_VP

PIC_DIR <- 'pics'
OUTPUT_DIR <- "scenes.pics.750x750_1_2_4_6"

# ---- Load libraries --- 

source ("/Users/endress/R.ansgar/ansgarlib/R/tt.R")
library (grid)

# Set current directory
rstudioapi::getSourceEditorContext()$path %>% 
    dirname %>% 
    setwd

# ---- Define helper functions ---- 

rep.data.frame <- function (data = ., ...) as.data.frame(lapply(data, rep, ...))

make.basic.trials <- function (factor.list = ., n.trial.total = -1){
    
    basic.trials <- expand.grid(factor.list) %>%
        as.data.frame
    
    if (n.trial.total > nrow(basic.trials) ){
        basic.trials <- basic.trials %>% 
            rep.data.frame (ceil (n.trial.total / nrow (basic.trials))) 
    }
    
    basic.trials
}

list2gList <- function (...) do.call (grid::gList, ...)

is.distance.ok <- function (coords, width = .1, min.margin = width/10) {
    
    min.dist <- (2 * sqrt (2) * width/2) + min.margin
    
    none (dist (coords) < min.dist)
}

generate.coord.set <- function (set_size = 4, min_coord = 0, max_coord = 1, width = .1, scaling = 1, box_margin = width / 2){
    
    accept.coords <- FALSE
    while (!accept.coords){
        coords <- cbind (x = runif (set_size, 
                                    min_coord + box_margin, 
                                    max_coord - box_margin),
                         y = runif (set_size, 
                                    min_coord + box_margin, 
                                    max_coord - box_margin))
        
        accept.coords <- is.distance.ok (coords, width)
    }
    
    coords <- as.data.frame (coords)
    return (coords)        
    
}

add.colors.to.coords <- function (coords = ., color_list = colors_balaban){
    
    .set_size = nrow (coords)
    
    .current_colors <- sample (color_list,
                               .set_size + 1) %>%
        unlist
    
    coords_with_cols <- cbind (coords,
                               # Colors for sample
                               col.sample = .current_colors[1:.set_size],
                               # Colors for changed test
                               # We always change the first color as the 
                               # coordinates are random anyhow
                               col.changed = c(.current_colors[.set_size + 1],
                                               .current_colors[2:.set_size])) %>%
        remove_rownames
    
    return (coords_with_cols)    
}

add.info.to.basic.trials <- function(basic.trials = ., width = .1, color_list = colors_balaban) {
    # Add coordinates and color info
    trials <- lapply (sample (nrow(basic.trials)),
                      function (i) list (coords = generate.coord.set(basic.trials[i,]$set.size,
                                                                     width = width ) %>% 
                                             add.colors.to.coords (color_list),
                                         set.size = basic.trials[i,]$set.size,
                                         change = basic.trials[i,]$change))
    return (trials)
}

coords.to.rects <- function (coords = ., width = .05, height = .05, 
                             col.col = "col.sample", 
                             background.rect = NULL,
                             pic.data = NULL, ...){
    
    # Code for generating a single rectangle
    # r1 <- rectGrob (.9, .9, .05, .05, gp = gpar(fill="green"))
    
    if (is.null (pic.data)){
        
        # The stimuli are simple colored squares
        
        rects <- apply (coords, 
                        1, 
                        function (X) {
                            rectGrob(X["x"], X["y"], 
                                     width, height, 
                                     gp = gpar(fill=X[col.col]))
                        })
    } else {
        
        # The stimuli are pictures
        
        rects <- apply (coords, 
                        1, 
                        function (X) {
                            rasterGrob(pic.data[[X[col.col]]], 
                                       x = X["x"], y = X["y"],
                                       width = width, height = width,
                                       interpolate=TRUE)
                        })
    }
    
    rects <- rects %>% 
        list2gList
    
    if (!is.null (background.rect))
        rects <- gList (background.rect, rects)
    
    return (rects)
}

get.trial.info <- function (trial = ., ...){
    
    trial_info <- cbind (trial$coords %>% 
                             rownames_to_column("n") %>%
                             data.table::setDT(.) %>% 
                             data.table::dcast(. ~ n, value.var= names(.)[-1]) %>%
                             data.table::setDF(.) %>%
                             dplyr::select (-1),
                         set.size = nrow (trial$coords),
                         change = trial$change)
    
    return (trial_info)
}

grid.prepare.page %<a-% 
{
    grid.newpage()
    grid.draw(background.rect)
    pushViewport(vp_list[[vpi]])
    
}

get.out.file.names <- function (prefix, counter, vp_name, dir){
    
    out_file_names <- list (
        sample = sprintf (
            "%s/%s%04d_%s_sample.jpg",
            dir,
            prefix,
            counter,
            vp_name),
        changed = sprintf (
            "%s/%s%04d_%s_changed.jpg",
            dir,
            prefix,
            counter,
            vp_name))
    
    return (out_file_names)
}

save.scene <-function (filename, scene, background.rect, vp, height = 400, width = 400, save.as.file = TRUE){
    
    if (save.as.file){
        jpeg (filename,
              height = height,
              width = width)
    }
    
    grid.newpage()
    grid.draw(background.rect)
    pushViewport(vp)
    grid.draw(scene)
    popViewport()
    if (save.as.file){
        dev.off ()
    }
}

process.trials <- function (trials, trial_list, type, current_vp, current_vp_name, first_vp_name, background_rect, current_object_scaling, pic.data = NULL){
    
    for (trial_counter in 1:length (trials)){
        
        current.trial <- trials[[trial_counter]]
        
        current.rects.sample <- current.trial$coords %>%
            coords.to.rects (width = RECT_SIDE_LENGTH * current_object_scaling,
                             height = RECT_SIDE_LENGTH * current_object_scaling,
                             col.col = "col.sample",
                             pic.data = pic.data)
        
        current.rects.changed <- current.trial$coords %>%
            coords.to.rects (width = RECT_SIDE_LENGTH * current_object_scaling,
                             height = RECT_SIDE_LENGTH * current_object_scaling,
                             col.col = "col.changed",
                             pic.data = pic.data)
        
        
        if (current.trial$change == 1){
            
            current.rects.test <- current.rects.changed
            
        } else {
            
            current.rects.test <- current.rects.sample
        }   
        
        scene.sample <- gTree (children = current.rects.sample)
        scene.test <- gTree (children = current.rects.test)
        
        
        out_file_name <- get.out.file.names (type, trial_counter, current_vp_name, OUTPUT_DIR)
        
        save.scene (out_file_name$sample,
                    scene.sample,
                    background.rect,
                    current_vp,
                    height = 750, 
                    width = 750)
        
        save.scene (out_file_name$changed,
                    gTree (children = current.rects.changed),
                    background.rect,
                    current_vp,
                    height = 750, 
                    width = 750)
        
        current_trial_info <- get.trial.info(current.trial)
        
        trial_list <- plyr::rbind.fill (
            trial_list,
            cbind (subjectGroup = 1,
                   first.viewport.size = first_vp_name,
                   change.group = 1,
                   type = "test",
                   myPhase  = type,
                   viewport.size = current_vp_name,
                   random = 1,
                   current_trial_info,
                   stim1 = out_file_name$sample,
                   stim2 = out_file_name[[1+current_trial_info$change]],
                   #key = 2 - current_trial_info$change))
                   key = c("n", "c")[1 + current_trial_info$change]))
        
        # Add trial info for second subject group 
        # where change and no change trials are switched
        current_trial_info$change <- 1 - current_trial_info$change
        trial_list <- plyr::rbind.fill (
            trial_list,
            cbind (subjectGroup = 2,
                   first.viewport.size = first_vp_name,
                   change.group = 2,
                   type = "test",
                   myPhase = type,
                   viewport.size = current_vp_name,
                   random = 1,
                   current_trial_info,
                   stim1 = out_file_name$sample,
                   stim2 = out_file_name[[1+current_trial_info$change]],
                   #key = 2 - current_trial_info$change))
                   key = c("n", "c")[1 + current_trial_info$change]))
        
    }
    
    return (trial_list)
}

post.process.cols %<a-% 
{
    . %>%
        dplyr::select(-c(paste ("col.changed", 2:8, sep="_"))) %>%
        mutate_at(c("stim1", "stim2"), function (X) sub ("^.*\\/", "", X)) %>%
        mutate_at(c("stim1", "stim2"), function (X) sub ("\\.jpg$", "", X)) %>%
        add_column(stim3 = "any_square_change", 
                   keyboard = "c n",
                   .after = "stim2") %>% 
        add_column(stimFormat = ".jpg") %>% 
#        add_column(button1 = "Change") %>%
#        add_column(button2 = "No change") %>% 
        add_column(responseWindow = paste (1000*(SAMPLE_DURATION+RETENTION_DURATION), "_", sep="")) %>% 
        add_column(presTime = paste(1000*SAMPLE_DURATION, 1000*TEST_DURATION, "", sep=";")) %>% 
        add_column(ISI = paste(1000*RETENTION_DURATION, 0, sep=";")) %>%
        add_column(ITI = 1000*ITI) 
}

# ---- Colors from Balaban et al., 2019, Cognition and Objects from Rossion, B., & Pourtois, G. (2004). Revisiting Snodgrass and Vanderwart's object set: The role of surface detail in basic-level object recognition. Perception, 33, 217-236. ----
colors_balaban <- list (red = c(254,0,0),
                        magenta = c(255,0,254),
                        blue = c(0,0,254),
                        cyan = c(0,255,255),
                        green = c(0,255,1),
                        yellow = c(255,255,0),
                        orange = c(255,128,65),
                        brown = c(128,64,0),
                        black = c(0,0,0)) %>% 
    lapply (., 
            function (X) rgb (X[1], X[2], X[3],
                              maxColorValue = 255))

# From Rossion, B., & Pourtois, G. (2004). Revisiting Snodgrass and Vanderwart's object set: The role of surface detail in basic-level object recognition. Perception, 33, 217-236.
# Downloaded from https://wiki.cnbc.cmu.edu/Objects

pic.files <- list.files (PIC_DIR, ".jpg$", full.name=T)
pic.data <- lapply (pic.files,
                    jpeg::readJPEG)
names (pic.data) <- pic.files %>% 
    basename %>% 
    gsub (".jpg$", "", .)


# ---- Define viewports ----

background.rect <- rectGrob (.5, .5, 1, 1, 
                             gp = gpar(col = BACKGROUND.COL,
                                       fill=BACKGROUND.COL))

vp_x1 <- viewport(x = 0, y = 0, 
                     width = 1, height = 1,
                     just = c("left", "bottom"))

vp_x2 <- viewport(x = 0.5, y = 0.5, 
                  width = SMALL_VIEWPORT_SCALE_VP[1], 
                  height = SMALL_VIEWPORT_SCALE_VP[1],
                  just = c("center"))
#                      just = c("left", "bottom"))

vp_x4 <- viewport(x = 0.5, y = 0.5, 
                  width = SMALL_VIEWPORT_SCALE_VP[2], 
                  height = SMALL_VIEWPORT_SCALE_VP[2],
                  just = c("center"))
#                      just = c("left", "bottom"))

vp_x6 <- viewport(x = 0.5, y = 0.5, 
                     width = SMALL_VIEWPORT_SCALE_VP[3], 
                     height = SMALL_VIEWPORT_SCALE_VP[3],
                     just = c("center"))
#                      just = c("left", "bottom"))

vp_list <- list (x6 = vp_x6,
                 x4 = vp_x4,
                 x2 = vp_x2,
                 x1 = vp_x1)

# ---- Define trial information ----

basic.trials.practise <- make.basic.trials(list(set.size = c(4, 8), 
                                                change = c(0, 1)), 
                                           N_TRIALS_PRACTISE)

basic.trials <- make.basic.trials(list(set.size = c(4, 8), 
                                       change = c(0, 1)), 
                                  N_TRIALS_PER_BLOCK)


trials.practise <- add.info.to.basic.trials(basic.trials.practise,
                                            # We need to scale the width to rescale the 
                                            # distances for the small viewport
                                            width = RECT_SIDE_LENGTH * max(SMALL_VIEWPORT_SCALE_OBJECT),
                                            # The colors are just strings
                                            color_list = names (pic.data))
                                            

trials <- add.info.to.basic.trials(basic.trials,
                                   # We need to scale the width to rescale the 
                                   # distances for the small viewport
                                   width = RECT_SIDE_LENGTH * max(SMALL_VIEWPORT_SCALE_OBJECT),
                                   # The colors are just strings
                                   color_list = names (pic.data))
                                   

# ---- Set up data frame for data collection 
trial_list <- data.frame (
    # Subject groups differ in which trials are change and no-change trials (1 vs. and 3 vs. 4)
    # And whether they start with the large or the small display
    subjectGroup = numeric(), 
    first.viewport.size = character(), 
    change.group = numeric(), 
    # (practise|test) 
    type = character (),
    myPhase = character(),
    viewport.size = character(),
    set.size = numeric (),
    change = character (),
    # This is constant (1)
    random = numeric(), 
    
    x_1 = numeric(),
    y_1 = numeric(),
    col.sample_1 = character (),
    col.changed_1 = character (),
    
    x_2 = numeric(),
    y_2 = numeric(),
    col.sample_2 = character (),
    col.changed_2 = character (),
    
    x_3 = numeric(),
    y_3 = numeric(),
    col.sample_3 = character (),
    col.changed_3 = character (),
    
    x_4 = numeric(),
    y_4 = numeric(),
    col.sample_4 = character (),
    col.changed_4 = character (),
    
    x_5 = numeric(),
    y_5 = numeric(),
    col.sample_5 = character (),
    col.changed_5 = character (),
    
    x_6 = numeric(),
    y_6 = numeric(),
    col.sample_6 = character (),
    col.changed_6 = character (),
    
    x_7 = numeric(),
    y_7 = numeric(),
    col.sample_7 = character (),
    col.changed_7 = character (),
    
    x_8 = numeric(),
    y_8 = numeric(),
    col.sample_8 = character (),
    col.changed_8 = character (),
    
#    trialText = character (),
    stim1 = character (),
    stim2 = character (),
    key = character(),
    feedback = character(),
    feedbackTime = numeric())
# The following constant colums will be added at the end 
# * stimFormat
# * button1
# * button2
# * responseWindow
# * presTime 
# * ISI
# * ITI

# This is the output of the script, not the list of trials defined above
practise_list <- trial_list

# ---- Generate trials ---- 

for (vpi in 1:length(vp_list)){
    
        
        current_object_scaling <- 1 / 
            convertUnit (vp_list[[vpi]]$width, "npc", valueOnly = TRUE)
    
    practise_list <- process.trials (trials.practise, 
                                     practise_list,
                                     "practise", 
                                     vp_list[[vpi]], names (vp_list)[vpi], names(vp_list)[1], 
                                     background_rect, current_object_scaling, pic.data)
    
    trial_list <- process.trials (trials, 
                    trial_list,
                    "test", 
                    vp_list[[vpi]], names (vp_list)[vpi], names(vp_list)[1], 
                    background_rect, current_object_scaling, pic.data)
}

# --- Add constant columns and save information for testable ----

practise_list <- practise_list %>%
    mutate (feedback = "correct:This is correct!; incorrect: This is incorrect!") %>%
    mutate (feedbackTime = 1000) %>%
    post.process.cols %>% 
    mutate(first.viewport.size = viewport.size) %>% 
    filter (first.viewport.size %in% c("x1", "x6")) %>% 
    mutate (subjectGroup = ifelse (first.viewport.size=="x6",
                                   subjectGroup,
                                   subjectGroup + 2)) %>%
    arrange (subjectGroup)
    
trial_list <- trial_list %>% 
    post.process.cols %>% 
    arrange (subjectGroup)

 trial_list_groups_34 <- trial_list %>%
     mutate (subjectGroup = subjectGroup + 2) %>% 
     mutate (first.viewport.size = names (vp_list)[4]) %>%
     arrange (subjectGroup, desc(viewport.size))

 trial_list <- rbind (trial_list,
                      trial_list_groups_34,
                     practise_list) %>%
    arrange (subjectGroup, desc(myPhase))

trial_list <- trial_list %>% 
    mutate (random = number.factor.combos(.,
    subjectGroup, first.viewport.size, change.group, type, viewport.size))
        
    
save.data.frame(trial_list, 
                row.names = FALSE,
                na = "",
                .sep = ",")


# Stuff for saveGIF
# animation::saveGIF({
#     ani.options(nmax = 100)
# INSERT CODE HERE
# }, interval = 0.05, 
# movie.name = '/Users/endress/Desktop/test.gif', 
# ani.width = 600, 
# ani.height = 600)
