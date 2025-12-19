theme_set(
  theme_minimal(base_size = 12)+
    theme(panel.grid.minor = element_blank())
)


img_body_new <- png::readPNG(here("img", "background.png"))



draw_my_pain <-function(id, facet = TRUE, .alpha = 0.8, verbose = TRUE, flip = FALSE, boxes = FALSE, id_label = TRUE){
  
  if(is_tibble(id)){
    print("Taking id from the id column in the provided tibble")
    id_tmp <- id %>% pull(id)
  }else{
    print("Taking id from vector or value provided")
    id_tmp <- {{id}}
    print(id_tmp)
  }
  
  if(verbose){
    .data <- id %>% 
      select(id, starts_with("Area"))
  }else{
      .data <- id %>% select(id)
    }
  
  point_size <- case_when(length(unique(id_tmp)) > 100 ~ 1/ length(unique(id_tmp)),
                          length(unique(id_tmp)) > 10 ~ 0.2,
                          length(unique(id_tmp)) > 5 ~ 0.5,
                          length(unique(id_tmp)) > 2 ~ 1,
                          .default = 1.5)
  
  # if(length(unique(id_tmp)) > 100){
  #   point_size <- c(".")
  # }
  # 
  
  # Should coords be flipped?
  if(flip){
    tmp_pd_coord <- pd_coord_flipped %>%
      filter(id %in% id_tmp) 
  }else{
    tmp_pd_coord <- pd_coord_flipped %>%
      filter(id %in% id_tmp)
    message("COORDS ARE STILL FLIPPED FIX THIS LATER!")
  }
  
  
  # START PLOT
  base_plot <- tmp_pd_coord %>%
    full_join(.data %>% filter(id %in% id_tmp),
              by = "id") %>% 
    
  ggplot(aes(x = X, y = Y))+
  annotation_raster(as.raster(img_body_new), -Inf, Inf, -Inf, Inf) +
  geom_point(alpha = .alpha, color = "red", size = point_size)+
  coord_fixed(xlim = c(0,450), ylim = c(0,500), expand = FALSE)+
  theme_void(base_size = 12)
  
  if(facet){
    out_plot <- base_plot +
    facet_wrap(~id)+
    theme(strip.text = element_blank())
    
    if(id_label){
      out_plot <- out_plot+
           geom_label(aes(label = paste("ID: ", id)),
               x = 225, y = 480, size = 2)
        }
    
    
  }else{
    if(length(unique(id_tmp)) > 1){
      
      out_plot <- base_plot
      
      if(id_label){
        out_plot <- out_plot+
          annotate(geom = "label", 
               label = paste("Nr of id's plotted:", length(unique(id_tmp))),
               x = 225, y = 480, size = 2)
      }
      
      
    }else{
        out_plot <- base_plot
        
        if(id_label){
          out_plot <- out_plot +
            annotate(geom = "label", 
               label = paste("id:", id_tmp),
               x = 225, y = 480, size = 2)
        }
        
    }
  }
  
  # Add boxes?
  if(boxes){
    out_plot <- out_plot %>%
      draw_my_pain_add_boxes()
    }
    

  
  # Verbose?
  if(verbose){
    # Plot can only be verbose if id is a tibble
    if(is_tibble(id) & sum(names(id) %in% c("Area_total", "Area_inside", "Area_outside", "Area_N_regions")) == 4){
      out_plot <- out_plot+
        geom_label(aes(label = paste("Tot:",   round(Area_total),
                                   "\nIns:", round(Area_inside),
                                   "\nOut:", round(Area_outside),
                                   "\nN R:", round(Area_N_regions))),
                 x = 225, y = 100, size = 2)
    }else{
      out_plot <- out_plot
    }
  }
  
  out_plot
}




draw_my_pain_add_boxes <- function(.plot){
  
  # TEST if ggplot
  if(!is.ggplot(.plot)){
    print("This is not a ggplot object")
    print("Maybe you need to run draw_my_pain() first?")
    return()
  }
  
  .plot + 
  ## Right SIDE ANTERIOR!
    
  # Wrist + hand  
  annotate(geom = "rect",
           xmin = 5,
           xmax = 60,
           ymin = 230,
           ymax = 300,
           alpha = 0.1,
           color = "purple")+       
    
  # Lower arm   
  annotate(geom = "rect",
           xmin = 25,
           xmax = 70,
           ymin = 300,
           ymax = 335,
           alpha = 0.1,
           color = "purple")+    

  # Upper arm   
  annotate(geom = "rect",
           xmin = 35,
           xmax = 70,
           ymin = 335,
           ymax = 390,
           alpha = 0.1,
           color = "purple")+        
    
    
  # Shoulder   
  annotate(geom = "rect",
           xmin = 40,
           xmax = 92,
           ymin = 390,
           ymax = 430,
           alpha = 0.1,
           color = "purple")+     
    
    
  # Thorax   
  annotate(geom = "rect",
           xmin = 70,
           xmax = 92,
           ymin = 350,
           ymax = 390,
           alpha = 0.1,
           color = "purple")+    
    
    
  # Lower abdomen  
  annotate(geom = "rect",
           xmin = 70,
           xmax = 92,
           ymin = 300,
           ymax = 350,
           alpha = 0.1,
           color = "purple")+    
    
  # Ingui 
  annotate(geom = "rect",
           xmin = 60,
           xmax = 92,
           ymin = 260,
           ymax = 300,
           alpha = 0.1,
           color = "purple")+
    
    
  # Upper leg  
  annotate(geom = "rect",
           xmin = 60,
           xmax = 105,
           ymin = 180,
           ymax = 260,
           alpha = 0.1,
           color = "purple")+  
    
  # Lower leg  
  annotate(geom = "rect",
           xmin = 60,
           xmax = 105,
           ymin = 0,
           ymax = 180,
           alpha = 0.1,
           color = "purple")+  
    
  ## Left SIDE ANTERIOR!

# Wrist + hand  
  annotate(geom = "rect",
           xmin = 150,
           xmax = 205,
           ymin = 230,
           ymax = 300,
           alpha = 0.1,
           color = "purple")+       
    
  # Lower arm   
  annotate(geom = "rect",
           xmin = 140,
           xmax = 185,
           ymin = 300,
           ymax = 335,
           alpha = 0.1,
           color = "purple")+    

  # Upper arm   
  annotate(geom = "rect",
           xmin = 140,
           xmax = 175,
           ymin = 335,
           ymax = 390,
           alpha = 0.1,
           color = "purple")+        
    
    
  # Shoulder   
  annotate(geom = "rect",
           xmin = 118,
           xmax = 170,
           ymin = 390,
           ymax = 430,
           alpha = 0.1,
           color = "purple")+        
    
    
  # Thorax   
  annotate(geom = "rect",
           xmin = 118,
           xmax = 140,
           ymin = 350,
           ymax = 390,
           alpha = 0.1,
           color = "purple")+            

        
  # Lower abdomen  
  annotate(geom = "rect",
           xmin = 118,
           xmax = 140,
           ymin = 300,
           ymax = 350,
           alpha = 0.1,
           color = "purple")+    
    
  # Ingui 
  annotate(geom = "rect",
           xmin = 118,
           xmax = 150,
           ymin = 260,
           ymax = 300,
           alpha = 0.1,
           color = "purple")+
    
    
  # Upper leg  
  annotate(geom = "rect",
           xmin = 105,
           xmax = 150,
           ymin = 180,
           ymax = 260,
           alpha = 0.1,
           color = "purple")+  
    
  # Lower leg  
  annotate(geom = "rect",
           xmin = 105,
           xmax = 150,
           ymin = 0,
           ymax = 180,
           alpha = 0.1,
           color = "purple")+    
    
    
    
    
  ## LEFT SIDE POSTERIOR!

  # Wrist + Hand
  annotate(geom = "rect",
           xmin = 225,
           xmax = 276,
           ymin = 220,
           ymax = 285,
           alpha = 0.1,
           color = "steelblue")+   
    

  # Lower arm
  annotate(geom = "rect",
           xmin = 235,
           xmax = 286,
           ymin = 285,
           ymax = 310,
           alpha = 0.1,
           color = "steelblue")+      
        
    
    
  # Elbow
  annotate(geom = "rect",
           xmin = 250,
           xmax = 290,
           ymin = 310,
           ymax = 350,
           alpha = 0.1,
           color = "steelblue")+   
    
      
  # Shoulder/arm
  annotate(geom = "rect",
           xmin = 260,
           xmax = 285,
           ymin = 350,
           ymax = 418,
           alpha = 0.1,
           color = "steelblue")+     

  # Shoulder/Thor
  annotate(geom = "rect",
           xmin = 285,
           xmax = 311,
           ymin = 375,
           ymax = 430,
           alpha = 0.1,
           color = "steelblue")+         

    
  # Upper Back
  annotate(geom = "rect",
           xmin = 285,
           xmax = 311,
           ymin = 350,
           ymax = 375,
           alpha = 0.1,
           color = "steelblue")+    
    
    
  # Low Back
  annotate(geom = "rect",
           xmin = 291,
           xmax = 311,
           ymin = 310,
           ymax = 350,
           alpha = 0.1,
           color = "steelblue")+
  
  
  # Upper bottock
  annotate(geom = "rect",
           xmin = 286,
           xmax = 311,
           ymin = 285,
           ymax = 310,
           alpha = 0.1,
           color = "steelblue")+
  
  
  
  # Lower bottock
  annotate(geom = "rect",
           xmin = 276,
           xmax = 323,
           ymin = 250,
           ymax = 285,
           alpha = 0.1,
           color = "steelblue")+
  
  
  # Upper Leg
  annotate(geom = "rect",
           xmin = 276,
           xmax = 323,
           ymin = 180,
           ymax = 250,
           alpha = 0.1,
           color = "steelblue")+
  
  # Lower 
    annotate(geom = "rect",
           xmin = 276,
           xmax = 323,
           ymin = 0,
           ymax = 180,
           alpha = 0.1,
           color = "steelblue") +

  
  
  
  
  
  
  ## RIGHT SIDE BACK!
    
  # Wrist + Hand
  annotate(geom = "rect",
           xmin = 370, 
           xmax = 421,
           ymin = 220,
           ymax = 285,
           alpha = 0.1,
           color = "steelblue")+   
    

  # Lower arm
  annotate(geom = "rect",
           xmin = 360, 
           xmax = 411,
           ymin = 285,
           ymax = 310,
           alpha = 0.1,
           color = "steelblue")+      
        
    
    
  # Elbow
  annotate(geom = "rect",
           xmin = 355,
           xmax = 395,
           ymin = 310,
           ymax = 350,
           alpha = 0.1,
           color = "steelblue")+


  # Shoulder/arm
  annotate(geom = "rect",
           xmin = 361,
           xmax = 387,
           ymin = 350,
           ymax = 418,
           alpha = 0.1,
           color = "steelblue")+

  # Shoulder/Thor
  annotate(geom = "rect",
           xmin = 335,
           xmax = 361,
           ymin = 375,
           ymax = 430,
           alpha = 0.1,
           color = "steelblue")+


  # Upper Back
  annotate(geom = "rect",
           xmin = 335,
           xmax = 361,
           ymin = 350,
           ymax = 375,
           alpha = 0.1,
           color = "steelblue")+

  

  # Low Back
  annotate(geom = "rect",
           xmin = 335,
           xmax = 355,
           ymin = 310,
           ymax = 350,
           alpha = 0.1,
           color = "steelblue")+
  
  
  # Upper bottock
  annotate(geom = "rect",
           xmin = 335,
           xmax = 360,
           ymin = 285,
           ymax = 310,
           alpha = 0.1,
           color = "steelblue")+
  
  
  
  # Lower bottock
  annotate(geom = "rect",
           xmin = 323,
           xmax = 370,
           ymin = 250,
           ymax = 285,
           alpha = 0.1,
           color = "steelblue")+
  
  
  # Upper Leg
  annotate(geom = "rect",
           xmin = 323,
           xmax = 370,
           ymin = 180,
           ymax = 250,
           alpha = 0.1,
           color = "steelblue")+
  
  # Lower 
    annotate(geom = "rect",
           xmin = 323,
           xmax = 370,
           ymin = 0,
           ymax = 180,
           alpha = 0.1,
           color = "steelblue")
}


### FUNCTION TO FLIP COORD
flip_my_coords <- function(.data, .add_names = FALSE){
  names <- names(.data)
  
  data_out <- .data %>% 
    group_by(id) %>% 
    
    # Assign points to regions (dummy variables)
    
    mutate(
    # ANTERIOR
    ## Right SIDE
    ARW =  if_else(X>= 5     & X<= 60 &  Y>= 230    & Y<= 300, TRUE, FALSE), 
    ARLArm =  if_else(X>= 25 & X<= 70 &  Y>= 300    & Y<= 335, TRUE, FALSE), 
    ARUArm =  if_else(X>= 35 & X<= 70 &  Y>= 335    & Y<= 390, TRUE, FALSE), 
    ARS =  if_else(X>= 40    & X<= 92 &  Y>= 390    & Y<= 430, TRUE, FALSE),  
    ART =  if_else(X>= 70    & X<= 92 &  Y>= 350    & Y<= 390, TRUE, FALSE),  
    ARLA = if_else(X>= 70    & X<= 92   & Y>= 300   & Y<= 350, TRUE, FALSE),
    ARI = if_else(X>= 60     & X<= 92   & Y>= 260   & Y<= 300, TRUE, FALSE),
    ARUL = if_else(X>= 60    & X<= 105  & Y>= 180   & Y<= 260, TRUE, FALSE),
    ARLL = if_else(X>= 60    & X<= 105  & Y>= 0     & Y<= 180, TRUE, FALSE),     
    
    ## Left SIDE
    ALW =  if_else(X>= 150    & X<= 205 & Y>= 230  & Y<= 300, TRUE, FALSE), 
    ALLArm =  if_else(X>= 140 & X<= 185 & Y>= 300  & Y<= 335, TRUE, FALSE), 
    ALUArm =  if_else(X>= 140 & X<= 175 & Y>= 335  & Y<= 390, TRUE, FALSE), 
    ALS =  if_else(X>= 118    & X<= 170 & Y>= 390  & Y<= 430, TRUE, FALSE),
    ALT =  if_else(X>= 118    & X<= 140 & Y>= 350  & Y<= 390, TRUE, FALSE),
    ALLA = if_else(X>= 118    & X<= 140 & Y>= 300  & Y<= 350, TRUE, FALSE),
    ALI = if_else(X>= 118     & X<= 150 & Y>= 260  & Y<= 300, TRUE, FALSE),
    ALUL = if_else(X>= 105    & X<= 150 & Y>= 180  & Y<= 260, TRUE, FALSE),
    ALLL = if_else(X>= 105    & X<= 150 & Y>= 0    & Y<= 180, TRUE, FALSE),    
        
    
    # POSTERIOR
    ## LEFT SIDE
    PLWH = if_else(X>= 225    & X<= 276 & Y>= 220 & Y<= 285, TRUE, FALSE),
    PLLArm = if_else(X>= 235  & X<= 286 & Y>= 285 & Y<= 310, TRUE, FALSE),
    PLE = if_else(X>= 250     & X<= 290 & Y>= 310 & Y<= 350, TRUE, FALSE),
    PLSA = if_else(X>= 260    & X<= 285 & Y>= 350 & Y<= 418, TRUE, FALSE),
    PLST = if_else(X>= 285    & X<= 311 & Y>= 375 & Y<= 430, TRUE, FALSE),
    PLUBa = if_else(X>= 285   & X<= 311 & Y>= 350 & Y<= 375, TRUE, FALSE),
    PLLBa = if_else(X>= 291   & X<= 311 & Y>= 310 & Y<= 350, TRUE, FALSE),
    PLUB = if_else(X>= 286    & X<= 311 & Y>= 285 & Y<= 310, TRUE, FALSE),
    PLLB = if_else(X>= 276    & X<= 323 & Y>= 250 & Y<= 285, TRUE, FALSE),
    PLUL = if_else(X>= 276    & X<= 323 & Y>= 180 & Y<= 250, TRUE, FALSE),
    PLLL = if_else(X>= 276    & X<= 323 & Y>= 0   & Y<= 180, TRUE, FALSE),     
    ## RIGHT SIDE
    PRWH = if_else(X>= 370    & X<= 421 & Y>= 220 & Y<= 285, TRUE, FALSE),
    PRLArm = if_else(X>= 360  & X<= 411 & Y>= 285 & Y<= 310, TRUE, FALSE),
    PRE = if_else(X>= 355     & X<= 395 & Y>= 310 & Y<= 350, TRUE, FALSE),
    PRSA = if_else(X>= 361    & X<= 387 & Y>= 350 & Y<= 418, TRUE, FALSE),
    PRST = if_else(X>= 335    & X<= 361 & Y>= 375 & Y<= 430, TRUE, FALSE),
    PRUBa = if_else(X>= 335   & X<= 361 & Y>= 350 & Y<= 375, TRUE, FALSE),      
    PRLBa = if_else(X>= 335   & X<= 355 & Y>= 310 & Y<= 350, TRUE, FALSE),
    PRUB = if_else(X>= 335    & X<= 360 & Y>= 285 & Y<= 310, TRUE, FALSE),
    PRLB = if_else(X>= 323    & X<= 370 & Y>= 250 & Y<= 285, TRUE, FALSE),
    PRUL = if_else(X>= 323    & X<= 370 & Y>= 180 & Y<= 250, TRUE, FALSE),   
    PRLL = if_else(X>= 323    & X<= 370 & Y>= 0   & Y<= 180, TRUE, FALSE),      
  ) %>% 
  
  
  
  ## FLIP UNILATERAL COORDINATES
  
  # Create dummies for each region. Should it be flpped?
  mutate(
    # Anterior
    flip_ARW    = any(ARW   ) & !any(ALW   ), 
    flip_ARLArm = any(ARLArm) & !any(ALLArm),
    flip_ARUArm = any(ARUArm) & !any(ALUArm),
    flip_ARS    = any(ARS   ) & !any(ALS   ), 
    flip_ART    = any(ART   ) & !any(ALT   ), 
    
    flip_ARLA  = any(ARLA)  & !any(ALLA),
    flip_ARI   = any(ARI)   & !any(ALI),
    flip_ARUL  = any(ARUL)  & !any(ALUL),
    flip_ARLL  = any(ARLL)  & !any(ALLL),
    
    # Posterior
    flip_PRWH   = any(PRWH  ) & !any(PLWH  ),
    flip_PRLArm = any(PRLArm) & !any(PLLArm),
    flip_PRE    = any(PRE   ) & !any(PLE   ), 
    flip_PRSA   = any(PRSA  ) & !any(PLSA  ),
    flip_PRST   = any(PRST  ) & !any(PLST  ),
    flip_PRUBa  = any(PRUBa)  & !any(PLUBa), 
      
    flip_PRLBa = any(PRLBa) & !any(PLLBa),
    flip_PRUB  = any(PRUB)  & !any(PLUB),
    flip_PRLB  = any(PRLB)  & !any(PLLB),
    flip_PRUL  = any(PRUL)  & !any(PLUL),
    flip_PRLL  = any(PRLL)  & !any(PLLL),
   ) %>% 
  
  # Flip if it should be flipped
  mutate(
    # Anterior
    X = case_when(
      flip_ARW    & ARW    ~ 105+(105-X),
      flip_ARLArm & ARLArm ~ 105+(105-X),
      flip_ARUArm & ARUArm ~ 105+(105-X),
      flip_ARS    & ARS    ~ 105+(105-X),
      flip_ART    & ART    ~ 105+(105-X),
      flip_ARLA & ARLA ~ 105+(105-X), 
      flip_ARI  & ARI  ~ 105+(105-X), 
      flip_ARUL & ARUL ~ 105+(105-X),
      flip_ARLL & ARLL ~ 105+(105-X),
      .default = X
    ),
    
    # Posterior
    X = case_when(
      flip_PRWH   & PRWH   ~ 324-(X-323),
      flip_PRLArm & PRLArm ~ 324-(X-323),
      flip_PRE    & PRE    ~ 324-(X-323),
      flip_PRSA   & PRSA   ~ 324-(X-323),
      flip_PRST   & PRST   ~ 324-(X-323),
      flip_PRUBa  & PRUBa  ~ 324-(X-323),
      
      flip_PRLBa & PRLBa ~ 324-(X-323),
      flip_PRUB  & PRUB  ~ 324-(X-323),
      flip_PRLB  & PRLB  ~ 324-(X-323),
      flip_PRUL  & PRUL  ~ 324-(X-323),
      flip_PRLL  & PRLL  ~ 324-(X-323),
      .default = X
      )) %>% 
    ungroup() 
  
  if(!.add_names){
    data_out <- data_out %>% 
      select(all_of(names))
  }
  
  data_out
}





