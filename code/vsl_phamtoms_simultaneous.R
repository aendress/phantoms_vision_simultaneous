rm (list=ls())

options (digits = 3)
knitr::opts_chunk$set(
    # Run the chunk
    eval = TRUE,
    # Don't include source code
    echo = FALSE, 
    # Print warnings to console rather than the output file
    warning = FALSE,  
    # Stop on errors
    error = FALSE,
    # Print message to console rather than the output file
    message = FALSE,
    # Include chunk output into output
    include = TRUE,
    # Don't reformat R code
    tidy = FALSE,
    # Center images
    fig.align = 'center',
    # Default image width
    out.width = '80%')

# other knits options are here:
# https://yihui.name/knitr/options/


if (Sys.info()[["user"]] %in% c("ansgar", "endress")){
    source ("/Users/endress/R.ansgar/ansgarlib/R/tt.R")
    source ("/Users/endress/R.ansgar/ansgarlib/R/null.R")
    #source ("helper_functions.R")
} else {
    # Note that these will probably not be the latest versions
    source("http://endress.org/progs/tt.R")
    source("http://endress.org/progs/null.R")
}


library (tidyverse)
library (purrr)


# For directly generating figures
library(grid)
library(gridExtra)

# Set seed to Cesar's birthday
set.seed (1207100)


scene.to.testable.trial.fam <- function (scene = ., extra.cols = NULL){
    
    scene %>% 
        mutate (id = row_number(),
                .before = 1) %>% 
        mutate (stimPos = str_c(x, y, sep = " "),
                .keep = "unused") %>% 
        pivot_wider(
            id_cols = config.id,
            names_from = id,
            values_from = -c(id, config.id),
            names_sep = "") %>% 
        rowwise() %>% 
        mutate (stimPos = str_c (c_across(starts_with("stimPos")), 
                                 sep =";", collapse = ";"),
                .keep = "unused") -> scene.testable
    
    if (!is.null(extra.cols)){
        
        if (!is.data.frame(extra.cols)){
            stop ("Argument extra.cols should be a data frame")
        }
        
        bind_cols(
            extra.cols,
            scene.testable
        ) -> scene.testable
        
    }
    
    scene.testable
}

scene.to.testable.trial.test <- function (scene = ., extra.cols = NULL){
    
    # Scene is a data frame with columns 
    # test.type 
    # item.type (legal vs. foil) 
    # stim
    # config.id
    # orientation
    # x
    # y
    # key
    
    
    scene %>% 
        # Number the shapes within a trial; if key == 2, the rows of the input need to be reversed
        # These numbers will later be used for the stim1... stimN columns
        mutate (id = case_when(
            key == 1 ~ row_number(),
            key == 2 ~ c (
                ((nrow(.)/2) + 1):nrow(.),
                1:(nrow(.)/2)),
            TRUE ~ NA_integer_),
            .before = 1) %>% 
        mutate (stimPos = str_c(x, y, sep = " "),
                .keep = "unused") %>% 
        pivot_wider(
            id_cols = c(config.id, orientation, test.type, key),
            names_from = id,
            values_from = -c(id, config.id, orientation, test.type, item.type, key),
            names_sep = "",
            names_sort = TRUE) %>% 
        rowwise() %>% 
        mutate (stimPos = str_c (c_across(starts_with("stimPos")), 
                                 sep =";", collapse = ";"),
                .keep = "unused") %>% 
        mutate (item1 = str_c (c_across(paste0 ("stim", 1:3)),
                               sep =":", collapse = ":"),
                item2 = str_c (c_across(paste0 ("stim", 4:6)),
                               sep =":", collapse = ":")) %>% 
        mutate (legal = case_when (
            key == 1 ~ item1,
            key == 2 ~ item2,
            TRUE ~ NA_character_),
            foil = case_when (
                key == 2 ~ item1,
                key == 1 ~ item2,
                TRUE ~ NA_character_)) %>% 
        # This removes key as well
        #.keep = "unused") -> scene.testable
        dplyr::select (-matches ("^items\\d$")) -> scene.testable
    
    if (!is.null(extra.cols)){
        
        if (!is.data.frame(extra.cols)){
            stop ("Argument extra.cols should be a data frame")
        }
        
        bind_cols(
            extra.cols,
            scene.testable
        ) -> scene.testable
        
    }
    
    scene.testable    
}    



DEBUG.FLAG <- FALSE


# Edge size of the pictures; hand code for convenience
PIC.LENGTH <- 74

# Testable coordinates:
# * The origin is (0, 0)
# * Positive x coordinates point right
# * Positive y coordinates point down
#
# Coordinates used here:
# * x steps go right
# * y steps go up (i.e., negative in testable coordinates)
# * We place the origin in the bottom left corner of our configurations 
COORD.STEP <- c (x = PIC.LENGTH, y = -PIC.LENGTH)
COORD.ORIGIN <- c (x = 0, y = 0) - 1.5 * COORD.STEP

PIC.DIR <- "../stimuli/stim.border_6px/"
GENERATE.PIC.FILES <- FALSE

EXP.DIR <- "../experiments/"

ISI <- 1000
ITI <- 1000
PRESTIME <- list (familiarization = "2000",
                  test.recognition = "2000*6")
#SUBJECTGROUP <- 1
N.SUBJECTGROUPS <- 10

# Generation of multiple languages
# * Loop through SUBJECTGROUP
#   * Initialize the following dfs before the loop
#       - dat.scenes.fam.testable
#       - dat.scenes.test.testable
#   * Combine these dfs into dat.testable.complete after the loop
#   * For each language, random (as a testable column) is determined as follows:
#       - Familiarization: random = 2 * SUBJECTGROUP - 1
#       - Test: random = 2 * SUBJECTGROUP




pic.files <- list.files (PIC.DIR, ".bmp$", full.name=T)

if (GENERATE.PIC.FILES){
    # Read files if we use them later for generating pictures
    # This is not needed if the pictures are combined using testable
    
    pic.data <- pic.files %>% 
        map (jpeg::readJPEG) %>% 
        map (rasterGrob)
    
    
    names (pic.data) <- pic.files %>% 
        basename %>% 
        gsub (".jpg$", "", .)
    
    # If we were to generate pictures, we would use code like this one:
    # ppp <- arrangeGrob (pic.data[[2]], pic.data[[3]], 
    #                     layout_matrix = cbind (c(1, NA, NA, NA), 
    #                                            rep(NA,4), 
    #                                            rep(NA,4), 
    #                                            c(NA, NA, NA, 2)) )
    # ggsave('filename.png', ppp, width = 100, height = 100, units = "px")
} 

pic.files %>% 
    basename %>% 
    gsub (".bmp$", "", .) -> pic.files




EXPERIMENT.TITLE <- "Learning shape combinations"

TRIALTEXT <- list (
    familiarization = 'Please pay attention to the object combinations.',
    test.recognition = 'Which object combination is more like the ones you have seen before? '
)

INSTRUCTIONS <- list (
    familiarization = list (
        list (content = "\"<p style='text-align:left;'>Hello, and thank you for taking part in this experiment.<br>In this experiment, we would like to study how people remember combinations of objects.<br><br>Please press 'g' to continue&hellip;</p>\"",
              keyboard = 'g'),
        list (content = "\"<p style='text-align:left;'>We will now show you some 'scenes' with combinations of objects. Please pay attention to these scenes.<br>Pleae press 'b' to continue&hellip;</p>\"",
              keyboard = 'b')
    ),
    test.recognition = list (
        list (content = "\"<p style='text-align:left;'>We hope you enjoyed the scenes. We will now show you pairs of new scenes with fewer objects. In each pair, one scene was embedded in the scenes you have viewed previously, while the other was not. Please tell us which scenes look more familiar.<br>Pleae press 't' to continue&hellip;</p>\"",
              keyboard = 't'),
        list (content = "\"<p style='text-align:left;'>If you think that the first scene was embedded in one of the earlier scenes, please press 1; if you think the second was embedded in one of the earlier scenes, please press 2.<br>Pleae press 't' to continue&hellip;</p>\"",
              keyboard = 't')
    )
)




# We initialize the data frames for when we put a loop for generating multiple languages

dat.scenes.fam.testable <- data.frame ()
dat.scenes.test.testable <- data.frame ()


# We will generate multiple random assignments of the shapes to create multiple languages
# To do so, we will loop through SUBJECTGROUP


for (SUBJECTGROUP in 1:N.SUBJECTGROUPS){
    
    
    # We first randomly distribute all available shapes into phantom-words and extra syllables
    
    l.all.shapes <- sample (pic.files, 18, replace = FALSE)
    
    l.phantom.words <- 
        # Set 1
        list (
            list (
                l.all.shapes[1:3],
                l.all.shapes[4:6]
            ),
            
            # Set 2    
            list (
                l.all.shapes[10:12],
                l.all.shapes[13:15]
            )
        )
    
    l.extra.shapes <- list (
        # Set 1
        list (
            l.all.shapes[7:9],
            l.all.shapes[7:9]
        ),
        
        # Set 2
        list (
            l.all.shapes[16:18],
            l.all.shapes[16:18]
        )
    )
    
    
    # We then generate words based on these phantom-words
    
    # words are list with two entries: 
    # - actual: vector of shapes in the actual word
    # - phantom: phantom word from which the word is derived
    
    generate.words <- function (phantom, extra.shapes){
        
        
        list (
            list (
                actual = c (phantom[1], phantom[2], extra.shapes[3]),
                phantom = phantom
            ),
            list (
                actual = c (extra.shapes[1], phantom[2], phantom[3]),
                phantom = phantom
            ),
            list (
                actual = c (phantom[1], extra.shapes[2], phantom[3]),
                phantom = phantom
            )
        )
        
    }
    
    
    map2 (
        # Loop over sets
        l.phantom.words,
        l.extra.shapes,
        
        # Loop over elements in sets, that is, individual phantom-words + extra syllables
        ~ map2 (.x, .y, 
                ~ generate.words(.x, .y)) %>% 
            unlist (recursive = FALSE) 
    ) -> l.words
    
    
    
    # We next generate configurations of coordinates. 
    # The configurations are lists of 6 coordinates for the 6 shapes in a scene
    
    # Definition of l.configurations, that is the list of configurations
    # This is a list of four lists (for the 4 configurations), containing
    # * fam: data frame, contaning configuration ID (1..4) and the coordinates of the 6 shapes
    # * test: list, containing two dataframes (for legal and foil items), containing config ID, orientation (horizontal/vertical), status (legal/foil), shapes selected for the item (among 6), and coordinates
    
    
    # We first define the familiarization configurations
    list (
        list (
            fam = data.frame (
                config.id = 1,
                x = COORD.ORIGIN["x"] + COORD.STEP["x"] * c(0, 0, 0, 0, 1, 2),
                y = COORD.ORIGIN["y"] + COORD.STEP["y"] * c(0, 1, 2, 3, 3, 3)
                
            ),
            test = list (
                # Vertical arrangement
                legal = data.frame (
                    config.id = 1,
                    orientation = "vertical",
                    status = "legal",
                    selected.shapes = 1:3,
                    # We will define those coordinates later
                    x = NA,
                    y = NA
                ),
                
                foil = data.frame (
                    config.id = 1,
                    orientation = "vertical",
                    status = "foil",
                    selected.shapes = 2:4,
                    # We will define those coordinates later
                    x = NA,
                    y = NA
                )
                
            )
        ),
        
        list (
            fam = data.frame (
                config.id = 2,
                x = COORD.ORIGIN["x"] + COORD.STEP["x"] * c(0, 0, 0, 1, 2, 3),
                y = COORD.ORIGIN["y"] + COORD.STEP["y"] * c(1, 2, 3, 3, 3, 3)
                
            ),
            test = list (
                # Horizontal arrangement
                legal = data.frame (
                    config.id = 2,
                    orientation = "horizontal",
                    status = "legal",
                    selected.shapes = 4:6,
                    # We will define those coordinates later
                    x = NA, 
                    y = NA 
                ),
                
                foil = data.frame (
                    config.id = 2,
                    orientation = "horizontal",
                    status = "foil",
                    selected.shapes = 3:5,
                    # We will define those coordinates later
                    x = NA,
                    y = NA 
                )
                
            )
        ),
        
        list (
            fam = data.frame (
                config.id = 3,
                x = COORD.ORIGIN["x"] + COORD.STEP["x"] * c(0, 1, 2, 3, 3, 3),
                y = COORD.ORIGIN["y"] + COORD.STEP["y"] * c(3, 3, 3, 3, 2, 1)
                
            ),
            
            test = list (
                legal = data.frame (
                    config.id = 3,
                    orientation = "horizontal",
                    status = "legal",
                    selected.shapes = 1:3,
                    # We will define those coordinates later
                    x = NA,
                    y = NA
                ),
                
                foil = data.frame (
                    config.id = 3,
                    orientation = "horizontal",
                    status = "foil",
                    selected.shapes = 2:4,
                    # We will define those coordinates later
                    x = NA,
                    y = NA
                )
                
            )
        ),
        
        list (
            fam = data.frame (
                config.id = 4,
                x = COORD.ORIGIN["x"] + COORD.STEP["x"] * c(1, 2, 3, 3, 3, 3),
                y = COORD.ORIGIN["y"] + COORD.STEP["y"] * c(3, 3, 3, 2, 1, 0)
                
            ),
            test = list (
                legal = data.frame (
                    config.id = 4,
                    orientation = "vertical",
                    status = "legal",
                    selected.shapes = 4:6,
                    # We will define those coordinates later
                    x = NA,
                    y = NA
                ),
                
                foil = data.frame (
                    config.id = 4,
                    orientation = "vertical",
                    status = "foil",
                    selected.shapes = 3:5,
                    # We will define those coordinates later
                    x = NA,
                    y = NA
                )
                
            )
        )
        
    ) -> l.configurations
    
    # Now update the test coordinates
    for (current.config in 1:length (l.configurations)){
        l.configurations[[current.config]]$test$legal %>% 
            mutate (x = l.configurations[[current.config]]$fam$x[.$selected.shapes],
                    y = l.configurations[[current.config]]$fam$y[.$selected.shapes]) %>% 
            mutate (x = x - mean(x),
                    y = y - mean (y)) -> l.configurations[[current.config]]$test$legal
        
        
        l.configurations[[current.config]]$test$foil %>% 
            mutate (x = l.configurations[[current.config]]$fam$x[.$selected.shapes],
                    y = l.configurations[[current.config]]$fam$y[.$selected.shapes]) %>% 
            mutate (x = x - mean(x),
                    y = y - mean (y)) -> l.configurations[[current.config]]$test$foil
    }
    
    
    # We generate a list of data frames with 3 columns 
    # - shape
    # - x
    # - y
    
    bind_rows(
        # Order 1
        expand.grid(word1 = l.words[[1]],
                    word2 = l.words[[2]],
                    configuration =  get.from.list(l.configurations, "fam")),
        # Order 2
        expand.grid(word1 = l.words[[2]],
                    word2 = l.words[[1]],
                    configuration =  get.from.list(l.configurations, "fam"))
    ) %>% 
        pmap (~ data.frame (
            stim = c(..1$actual, ..2$actual),
            config.id = ..3$config.id,
            x = ..3$x,
            y = ..3$y
        )) -> l.scenes.fam
    
    
    
    # We generate a list of data frames with 3 columns 
    # - item.type (legal vs. foil)
    # - shape 
    # - configuration ID
    # - x
    # - y
    # - key
    
    # These inputs are reused in the PhW vs. PW test
    l.test.inputs.w.pw <- list (
        word1 = c(sample (l.words[[1]]), sample (l.words[[2]])),
        word2 = c(sample (l.words[[2]]), sample (l.words[[1]])),
        # Use test right away
        configuration = c (sample (get.from.list(l.configurations, "test")), 
                           sample (get.from.list(l.configurations, "test")), 
                           sample (get.from.list(l.configurations, "test"))),
        key = c(sample.rep (1:2, 3), sample.rep (1:2, 3))) 
    
    l.test.inputs.w.pw %>% 
        pmap (~ bind_rows (
            data.frame (
                item.type = "legal",
                stim = c(..1$actual, ..2$actual)[..3$legal$selected.shapes],
                config.id = ..3$legal$config.id,
                orientation = ..3$legal$orientation,
                x = ..3$legal$x,
                y = ..3$legal$y,
                key = ..4),
            
            data.frame (
                item.type = "foil",
                stim = c(..1$actual, ..2$actual)[..3$foil$selected.shapes],
                config.id = ..3$foil$config.id,
                orientation = ..3$foil$orientation,
                x = ..3$foil$x,
                y = ..3$foil$y,
                key = ..4)
        )) -> l.scenes.test.w.pw
    
    l.scenes.test.w.pw %>% 
        map (~ .x %>% 
                 mutate (test.type = "w.pw", 
                         .before =1)) -> l.scenes.test.w.pw
    
    
    
    # Provide coordinates for test: We just select the last two coordinates as the 
    # other two are identical, but will check the we really have both horizontal and vertical
    # coordinates. Sorry. 
    (l.configurations %>%  
            get.from.list("test") %>% 
            get.from.list("legal"))[3:4] -> l.configurations.test.w.phw
    
    if ((l.configurations.test.w.phw %>% 
         get.from.list("orientation") %>% 
         unlist %>% 
         nlevels2) != 2){
        stop ("We need both horizontal and vertical coordinates")    
    }
    
    # We generate a list of data frames with 3 columns 
    # - item.type (legal vs. foil)
    # - shape 
    # - orientation
    # - x
    # - y
    # - key
    
    list (word = c(sample (l.words[[1]]), sample (l.words[[2]])),
          # This essentially repeats the test coordinates so we have an equal
          # number of horizontal/vertical orientation for each word set
          coords = map (l.words, 
                        ~ sample.rep (l.configurations.test.w.phw, 
                                      length (.x)/2)) %>% 
              unlist (recursive = FALSE),
          key = map (l.words, 
                     ~ sample.rep (1:2, 
                                   length (.x)/2)) %>% 
              unlist (recursive = FALSE)) %>% 
        pmap (~ bind_rows (
            data.frame (
                item.type = "legal",
                stim = ..1$actual,
                config.id = NA,
                orientation = ..2$orientation,
                x = ..2$x,
                y = ..2$y,
                key = ..3),
            
            data.frame (
                item.type = "foil",
                stim = ..1$phantom,
                config.id = NA,
                orientation = ..2$orientation,
                x = ..2$x,
                y = ..2$y,
                key = ..3),
        )) -> l.scenes.test.w.phw
    
    
    l.scenes.test.w.phw %>%
        map (~ .x %>%
                 mutate (test.type = "w.phw",
                         .before =1)) -> l.scenes.test.w.phw
    
    
    
    
    
    # We generate a list of data frames with 3 columns 
    # - item.type (legal vs. foil)
    # - shape 
    # - configuration ID
    # - x
    # - y
    # - key
    
    l.test.inputs.w.pw %>% 
        pmap (~ bind_rows (
            data.frame (
                item.type = "legal",
                stim = c(..1$phantom, ..2$phantom)[..3$legal$selected.shapes],
                config.id = ..3$legal$config.id,
                orientation = ..3$legal$orientation,
                x = ..3$legal$x,
                y = ..3$legal$y,
                key = ..4),
            
            data.frame (
                item.type = "foil",
                stim = c(..1$phantom, ..2$phantom)[..3$foil$selected.shapes],
                config.id = ..3$foil$config.id,
                orientation = ..3$foil$orientation,
                x = ..3$foil$x,
                y = ..3$foil$y,
                key = ..4)
        )) -> l.scenes.test.phw.pw
    
    l.scenes.test.phw.pw %>% 
        map (~ .x %>% 
                 mutate (test.type = "phw.pw", 
                         .before =1)) -> l.scenes.test.phw.pw
    
    
    
    
    bind_rows(
        dat.scenes.fam.testable,
        l.scenes.fam %>% 
            map (scene.to.testable.trial.fam,
                 data.frame (
                     my.phase = "familiarization",
                     type = "learn",
                     subjectGroup = SUBJECTGROUP,
                     # Will be added later
                     random = NA,
                     ISI = NA,
                     ITI = ITI,
                     presTime = PRESTIME$familiarization,
                     stimFormat = ".bmp",
                     trialText = TRIALTEXT$familiarization, 
                     trialTextOptions = "", #afterStim",
                     button1 = "", #'1<sup>st</sup> scene',
                     button2 = "", #'2<sup>nd</sup> scene',
                     responseWindow = "")) %>% 
            bind_rows()
    ) -> dat.scenes.fam.testable
    
    
    
    bind_rows (
        dat.scenes.test.testable,
        bind_rows (
            l.scenes.test.w.pw %>% 
                map (scene.to.testable.trial.test),
            l.scenes.test.w.phw %>% 
                map (scene.to.testable.trial.test),
            l.scenes.test.phw.pw %>% 
                map (scene.to.testable.trial.test)
        ) %>% 
            mutate (
                my.phase = "test.recognition",
                type = "test",
                subjectGroup = SUBJECTGROUP,
                # Will be added later
                random = NA,
                # Remove the last ; to avoid stimPos being ignored
                #ISI = str_c (c(rep(";", 2), ISI, rep(";", 2)), collapse = ""),
                ISI = str_c (c(rep(";", 2), ISI), collapse = ""),
                ITI = ITI,
                presTime = PRESTIME$test.recognition,
                stimFormat = ".bmp",
                trialText = TRIALTEXT$test.recognition, 
                trialTextOptions = "afterStim",
                button1 = '1<sup>st</sup> scene',
                button2 = '2<sup>nd</sup> scene',
                responseWindow = "afterStim",
                .before = 1) 
    ) -> dat.scenes.test.testable
    
    
    # We will generate multiple random assignments of the shapes to create multiple languages
    # To do so, we will loop through SUBJECTGROUP
    
    
    
} #for (SUBJECTGROUP in 1:N.SUBJECTGROUPS){


bind_rows(
    dat.scenes.fam.testable %>% 
        mutate (color.type = "white.on.black",
                .after = "subjectGroup"),
    
    dat.scenes.fam.testable %>% 
        mutate (across (matches ("stim\\d$"), ~ str_replace_all(.x, "new", "new_inv"))) %>% 
        mutate (subjectGroup = N.SUBJECTGROUPS + subjectGroup) %>%
        mutate (color.type = "black.on.white",
                .after = "subjectGroup")
) -> dat.scenes.fam.testable 

bind_rows(
    dat.scenes.test.testable %>% 
        mutate (color.type = "white.on.black",
                .after = "subjectGroup"),
    
    dat.scenes.test.testable %>% 
        mutate (across (matches ("stim\\d$"), ~ str_replace_all(.x, "new", "new_inv"))) %>% 
        mutate (subjectGroup = N.SUBJECTGROUPS + subjectGroup) %>%
        mutate (color.type = "black.on.white",
                .after = "subjectGroup")
) -> dat.scenes.test.testable 



dat.scenes.fam.testable %>% 
    mutate (random = subjectGroup) -> dat.scenes.fam.testable

dat.scenes.test.testable %>%
    mutate (random = subjectGroup + (2*N.SUBJECTGROUPS)) -> dat.scenes.test.testable

bind_rows(
    # Instructions
    
    INSTRUCTIONS$familiarization %>% 
        map_df (
            ~ tribble(
                ~my.phase, ~type, ~ subjectGroup, ~random,  ~ITI, ~presTime, ~title, ~content, ~keyboard,
                "instructions.general", "instructions", 0, NA, 500, "", EXPERIMENT.TITLE, .$content, .$keyboard)
        ),
    
    tribble(
        ~my.phase, ~type, ~ subjectGroup, ~random,  ~ITI, ~presTime, ~title, ~content, ~keyboard,
        "instructions.general", "instructions", 0, NA, 500, "2000", EXPERIMENT.TITLE, "We will start in 2 seconds &hellip;", "",
    ),
    
    # Familiarization 
    dat.scenes.fam.testable,
    
    # Instructions
    INSTRUCTIONS$test.recognition %>% 
        map_df (
            ~ tribble(
                ~my.phase, ~type, ~ subjectGroup, ~random,  ~ITI, ~presTime, ~title, ~content, ~keyboard,
                "instructions.general", "instructions", 0, NA, 500, "", EXPERIMENT.TITLE, .$content, .$keyboard)
        ),
    
    tribble(
        ~my.phase, ~type, ~ subjectGroup, ~random,  ~ITI, ~presTime, ~title, ~content, ~keyboard,
        "instructions.general", "instructions", 0, NA, 500, "2000", EXPERIMENT.TITLE, "We will start in 2 seconds &hellip;", "",
    ),
    
    # Test
    dat.scenes.test.testable
) -> dat.testable.complete




bind_cols(
    dat.testable.complete,
    
    # Words
    map (l.words, 
         ~ get.from.list (.x, "actual") %>% 
             map (paste0, collapse = ":") %>% 
             str_c (collapse = ";")) %>% 
        as.data.frame() %>% 
        setNames(paste0 ("words.set", 1:2)),
    
    # Phantoms 
    map (l.words, 
         ~ get.from.list (.x, "phantom") %>% 
             map (paste0, collapse = ":") %>% 
             unique %>% 
             str_c (collapse = ";")) %>% 
        as.data.frame() %>% 
        setNames(paste0 ("phantoms.set", 1:2))
) -> dat.testable.complete



# Make various changes to make debugging easier 
if (DEBUG.FLAG){
    dat.testable.complete %>% 
        # Don't randomize stuff
        mutate (random = NA) %>% 
        # Make things faster
        mutate (presTime = case_when(
            presTime == "2000" ~ "",
            presTime == "2000*6" ~ "500*6",
            TRUE ~ presTime)) %>% 
        # Proceed to next familiarization  item with button press
        mutate (button1 = ifelse (type == "learn",
                                  "NEXT",
                                  button1)) %>% 
        mutate (type = ifelse (type == "learn",
                               "test",
                               type)) %>% 
        # Show informative trial text
        mutate (trialText = ifelse (is.na (test.type),
                                    trialText,
                                    str_c(test.type, 
                                          "; key: ", key, 
                                          "; config: ", ifelse (is.na(config.id),
                                                                -99,
                                                                config.id))),
                trialTextOptions = ifelse (is.na (test.type),
                                           trialTextOptions,
                                           "allStim")) ->
        dat.testable.complete 
    
    # Write separate output files for the different trial types
    for (tt in dat.testable.complete$test.type %>% levels2){
        write.csv (dat.testable.complete %>% 
                       filter (test.type == tt),
                   file = paste0 (EXP.DIR, "phantoms.simultaneous.testable.", N.SUBJECTGROUPS, "groups.", tt,".csv"),
                   #append = FALSE,
                   quote = FALSE,
                   #sep = ",",
                   na = "",
                   row.names = FALSE)    
        
    }    
    
}


write.csv (dat.testable.complete,
           file = paste0(EXP.DIR, "phantoms.simultaneous.testable.", N.SUBJECTGROUPS, "groups.csv"),
           #append = FALSE,
           quote = FALSE,
           #sep = ",",
           na = "",
           row.names = FALSE)
