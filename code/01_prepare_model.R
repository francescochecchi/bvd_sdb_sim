#...............................................................................
### ++++ EBOLA SAFE AND DIGNIFIED BURIALS IN DRC: SCENARIO EXPLORATION +++++ ###
#...............................................................................

#...............................................................................
## ----- SCRIPT TO PREPARE STOCHASTIC MODEL AND DEFINE MODEL SCENARIOS ------ ##
#...............................................................................

                              # Written by Francesco Checchi, LSHTM (May 2026)
                              # francesco.checchi@lshtm.ac.uk 


#...............................................................................                           
### Specifying parameters / setting up scenarios
#...............................................................................

  #...................................       
  ## Read in simulation parameters

    # Read parameter file and create parameter vector
    pars_df <- read_xlsx(here(dir_path, "in", "evd_sdb_sim_parameters.xlsx"))
    pars <- pars_df$value
    names(pars) <- pars_df$parameter
    
    # Effect of safe and dignified burial (SDB) on reproduction number, based on
      # two methods (Hirano-Imbens [hi], Propensity Weights [pw])
      # in Checchi et al. (2025)
    effect_hi <- read.csv(here(dir_path,
      "in", "out_dose_resp_rn_p_success_hi.csv"))
    effect_pw <- read.csv(here(dir_path,
      "in", "out_dose_resp_rn_p_success_pw.csv"))
    effect_sdb <- (effect_hi + effect_pw) / 2 # simple average of two methods
    x <- c("mean", "low", "high")
    colnames(effect_sdb) <- c("prop_success", x)
    for (i in 2:nrow(effect_sdb)) {effect_sdb[i, x] <- 
      effect_sdb[i, x] - effect_sdb[1, x]}
    effect_sdb[1, x] <- c(0, 0, 0)
    colnames(effect_sdb) <- c("eff", "mean", "low", "high")
    effect_sdb <- abs(effect_sdb)
    
    # Plot effect in English
    df <- effect_sdb
    df[, c("mean", "low", "high")] <- -df[, c("mean", "low", "high")]
    plot <- ggplot(df, aes(x = eff, y = mean)) +
      geom_line(colour = palette_gen[5], linewidth = 2) +
      geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.5, 
        colour = palette_gen[5], fill = palette_gen[5]) +
      theme_bw() +
      scale_x_continuous("SDB effectiveness", labels = percent, 
        expand = expansion(add = c(0, 0.02))) +
      scale_y_continuous("reduction in the reproduction number")
    ggsave(here(dir_path, "out", "sdb_effect_en.png"), dpi = "print",
      units = "cm", height = 10, width = 15 * hw)  

    # Plot effect in French
    df <- effect_sdb
    df[, c("mean", "low", "high")] <- -df[, c("mean", "low", "high")]
    plot <- ggplot(df, aes(x = eff, y = mean)) +
      geom_line(colour = palette_gen[5], linewidth = 2) +
      geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.5, 
        colour = palette_gen[5], fill = palette_gen[5]) +
      theme_bw() +
      scale_x_continuous("complétude de l'EDS (%)", labels = percent, 
        expand = expansion(add = c(0, 0.02))) +
      scale_y_continuous("réduction du nombre de reproduction")
    ggsave(here(dir_path, "out", "sdb_effect_fr.png"), dpi = "print",
      units = "cm", height = 10, width = 15 * hw)  
        
  #...................................       
  ## Set up scenarios
    
    # Safe and dignified burial (SDB) intervention scenarios    
    scenarios <- expand.grid(
      n_seeds = seq(1, 19, 2),
      R_I = seq(0.50, 1.50, 0.10),
      R_D = seq(0.80, 1.60, 0.10),
      cov = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0), 
      eff = unique(effect_sdb$eff)
    )
    scenarios <- merge(scenarios, effect_sdb, by = "eff", all.x = T)    
    scenarios$id <- 1:nrow(scenarios)
    scenarios$Rn <- paste0("R = ", round(scenarios$R_I + pars["cfr"] * 
      scenarios$R_D, digits = 1), " (R_D = ", scenarios$R_D, ")")
    
    
#...............................................................................                           
### Setting up stochastic model
#...............................................................................

  #...................................       
  ## Set up stochastic model
    
    # SEIRD time step
    seird_step <- Csnippet("
      double dN_SE = rbinom(S, 1 - exp(-(R_I/T_I * I/N)) * 
        exp(-(R_D/T_B * U/N)) * exp(-((R_D-delta_eff/cfr)/T_B * C/N)));
      double dN_EI = rbinom(E, 1 - exp(-(1/T_E)));
      double dN_IF = rbinom(I, 1 - exp(-(1/T_I)));
      double dN_FD = rbinom(dN_IF, cfr);
      double dN_DC = rbinom(dN_FD, cov);
      double dN_DU = dN_FD - dN_DC;
      double dN_FR = dN_IF - dN_FD;
      double dN_CR = rbinom(C, 1 - exp(-(1/T_B)));
      double dN_UR = rbinom(U, 1 - exp(-(1/T_B)));
      
      S -= dN_SE;
      E += dN_SE - dN_EI;
      I += dN_EI - dN_IF;
      F = 0;
      C += dN_DC - dN_CR;
      U += dN_DU - dN_UR;
      R += dN_FR + dN_CR + dN_UR;
      cases += dN_EI;
    ")

    # Specify initial conditions    
    seird_init <- Csnippet("
      S = N - n_seeds;
      E = round(n_seeds * T_E / (T_E + T_I));
      I = n_seeds - E;
      F = 0;
      C = 0;
      U = 0;
      R = 0;
      cases = n_seeds;
    ")

    # Specify timeline
    timeline <- data.frame(day = 1:pars["n_days"])
    
    # Fold everything into pomp object
    evd_seird <- pomp(
      data = timeline, 
      times = "day", 
      rprocess = euler(seird_step, delta.t = 1), 
      rinit = seird_init,
      t0 = 1,
      paramnames = c("N", "R_I", "R_D", "T_E", "T_I", "T_B", "cfr", "cov", 
        "delta_eff", "n_seeds"),
      statenames = c("S", "E", "I", "F", "C", "U", "R", "cases")
      # accumvars = "cases" # use this to track daily incidence
    )


#.........................................................................................
### ENDS
#.........................................................................................


