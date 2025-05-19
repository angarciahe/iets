library(heemod)
library(tidyverse)

# +++++++++++++++++++++++++++
# Fijar directorio
# +++++++++++++++++++++++++++

getCurrentFileLocation <-  function()
{ #https://stackoverflow.com/questions/47044068/get-the-path-of-current-script
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

setwd(getCurrentFileLocation())

# +++++++++++++++++++++++++++
# +++++++++++++++++++++++++++

# ++++++++++++++++++++++++++++++++
# SETUP
# ++++++++++++++++++++++++++++++++

# Parámetros generales
param <- define_parameters(
  # Mortalidad
  p_muerte_crisis      = 0.15,   # Sin tratamiento
  p_muerte_crisis_t1   = 0.03,   # Tratamiento 1
  p_muerte_crisis_t2   = 0.15,   # Tratamiento 2
  
  # Calidad de vida en crisis infecciosa
  ql_crisis            = 0.65,   # Sin tratamiento (35% reducción)
  ql_crisis_t1         = 0.88,   # Tratamiento 1 (12% reducción)
  ql_crisis_t2         = 0.50,   # Tratamiento 2 (50% reducción)
  
  # Calidad de vida pos crisis, con secuelas
  ql_secuela           = 0.90,   # Sin tratamiento (10% reducción)
  ql_secuela_t1        = 0.88,   # Tratamiento 1 (12% reducción)
  ql_secuela_t2        = 0.96,   # Tratamiento 2 (4% reducción)
  
  # Tiempos en cada estado
  t_asintomatico       = 10 / 7,    # semanas
  t_crisis             = 2,         # semanas
  t_secuela            = (10*52) - (10/7) - 2, # 10 años menos los otros periodos
  
  # longitud de ciclo y ciclos totales
  cycle_length         = 1,         # semana como ciclo
  total_cycles         = 10*52     # 10 años
)

# Estados: Susceptible (S), Asintomático (A), Infectado en crisis (Ic),
# Pos crisis con secuelas (L), Fallecido (D)

state_names <- c("S", "A", "Ic", "L", "D")

# escala de colores usada
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")




# ++++++++++++++++++++++++++++++++
# MODELO
# ++++++++++++++++++++++++++++++++

# FUNCIÓN DE MATRIZ DE TRANSICIÓN


matriz_trans <- function(p_muerte_crisis, t_asintomatico, t_crisis, t_secuela) {
  define_transition(
    state_names = state_names,
    0, 1, 0, 0, 0,  # S
    0, 1 - (1/t_asintomatico), (1/t_asintomatico), 0, 0,  # A
    0, 0, 1 - (1/t_crisis) - p_muerte_crisis, (1/t_crisis), p_muerte_crisis,  # Ic
    0, 0, 0, 1 - (1/t_secuela), (1/t_secuela),  # L
    0, 0, 0, 0, 1   # D
  )
}

#### Por legibilidad: lo anterior se podría escribir también como

# matriz_trans <- function(p_muerte_crisis, t_asintomatico, t_crisis, t_secuela) {
#   define_transition( # C indica complemento a 1 por fila
#     state_names = state_names,
#     0, 1, 0, 0, 0,
#     0, C, (1/t_asintomatico), 0, 0,
#     0, 0, C, (1/t_crisis), p_muerte_crisis,
#     0, 0, 0, C, (1/t_secuela),
#     0, 0, 0, 0, 1
#   )
# }

# Transiciones para cada escenario
mat_trans_no_tx <- matriz_trans(
  p_muerte_crisis = p_muerte_crisis,
  t_asintomatico = t_asintomatico,
  t_crisis = t_crisis,
  t_secuela = t_secuela
)
mat_trans_t1 <- matriz_trans(
  p_muerte_crisis = p_muerte_crisis_t1,
  t_asintomatico = t_asintomatico,
  t_crisis = t_crisis,
  t_secuela = t_secuela
)
mat_trans_t2 <- matriz_trans(
  p_muerte_crisis = p_muerte_crisis_t2,
  t_asintomatico = t_asintomatico,
  t_crisis = t_crisis,
  t_secuela = t_secuela
)

# Estados para cada tratamiento
# --- SIN TRATAMIENTO
state_S  <- define_state(calidad_de_vida = 1)
state_A  <- define_state(calidad_de_vida = 1)
state_Ic <- define_state(calidad_de_vida = ql_crisis)
state_L  <- define_state(calidad_de_vida = ql_secuela)
state_D  <- define_state(calidad_de_vida = 0)

# --- TRATAMIENTO 1 # S,A,D se mantienen iguales
state_Ic_t1 <- define_state(calidad_de_vida = ql_crisis_t1)
state_L_t1  <- define_state(calidad_de_vida = ql_secuela_t1)

# --- TRATAMIENTO 2 # S,A,D se mantienen iguales
state_Ic_t2 <- define_state(calidad_de_vida = ql_crisis_t2)
state_L_t2  <- define_state(calidad_de_vida = ql_secuela_t2)

# Estrategias
strat_no_tx <- define_strategy(
  transition = mat_trans_no_tx,
  S = state_S,
  A = state_A,
  Ic = state_Ic,
  L = state_L,
  D = state_D
)
strat_t1 <- define_strategy(
  transition = mat_trans_t1,
  S = state_S,
  A = state_A,
  Ic = state_Ic_t1,
  L = state_L_t1,
  D = state_D
)
strat_t2 <- define_strategy(
  transition = mat_trans_t2,
  S = state_S,
  A = state_A,
  Ic = state_Ic_t2,
  L = state_L_t2,
  D = state_D
)

# ++++++++++++++++++++++++++++++++
# SIMULACIÓN
# ++++++++++++++++++++++++++++++++
res <- run_model(
  parameters = param,
  
  no_tx = strat_no_tx,
  tratamiento_1 = strat_t1,
  tratamiento_2 = strat_t2,
  
  cycles = eval(parse(text=quo_name(param$total_cycles))), #from quosure to value
  cost = calidad_de_vida, 
  effect = calidad_de_vida
)

# ++++++++++++++++++++++++++++++++
# RESULTADOS Y GRÁFICAS
# ++++++++++++++++++++++++++++++++


plot(res, type = "values") + 
  xlab("Ciclo (semanas)") +
  ylab("") +
  ggtitle("Calidad de vida") + theme(legend.position="none") +
  theme(plot.title = element_text(hjust=0.5))


# Para graficar los resultados de las estrategias juntas,
# se extraen los valores del modelo
values_df <- get_values(res)

values_df

plot_res <- ggplot(values_df, aes(x = model_time, y = value, color = .strategy_names)) +
  geom_line(size = 1) +
  labs(title = "Calidad de vida vs tiempo según tipos de tratamiento",
       x = "Ciclo (semanas)", y = "Calidad de vida", color = "Estrategia")  +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom") +
  scale_color_manual(name="Estrategia",
                     values = c("no_tx"=okabe_ito[1],
                                "tratamiento_1"=okabe_ito[2],
                                "tratamiento_2"=okabe_ito[3]
                     ),
                     labels = c("no_tx"="Sin tratamiento",
                                "tratamiento_1"="Tratamiento 1",
                                "tratamiento_2"="Tratamiento 2"
                     )
  )
plot_res
ggsave("images/plot_res_unificado.svg",
       plot_res, width = 7,
       height = 4, units = "in", dpi = 300)



plot_res_zoom <- plot_res +
  scale_x_continuous(limits=c(1,20))
plot_res_zoom
ggsave("images/plot_res_zoom_unificado.svg",
       plot_res_zoom, width = 7, 
       height = 4, units = "in", dpi = 300)


# ---- Diferencia de calidad de vida entre tratamientos y base

# se usa un formato ancho de values_df
values_df_wide <- reshape(subset(values_df, select = -value_names), 
                          idvar="model_time", 
                          timevar=".strategy_names",
                          direction="wide")

# Se agregan las columnas de diferencia
values_df_wide <- values_df_wide %>% mutate(diff_t1_no_tx = value.tratamiento_1 - value.no_tx,
                          diff_t2_no_tx = value.tratamiento_2 - value.no_tx)

# de vuelta a formato largo
values_df_diff <- values_df_wide %>% select(model_time,diff_t1_no_tx, diff_t2_no_tx) %>% 
  pivot_longer(!model_time, names_to="estrategia_vs_base", values_to="diff")

plot_diff_res <- ggplot(values_df_diff, aes(x = model_time, y = diff, color=estrategia_vs_base) ) +
  geom_line(size = 1) +
  labs(title = "Diferencia en calidad de vida vs tiempo según tipos de tratamiento",
       x = "Ciclo (semanas)", y = "Diferencia calidad de vida", color = "Estrategia")  +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5),legend.position = "bottom") +
  scale_color_manual(name="Diferencia",
                     values = c("diff_t1_no_tx"=okabe_ito[2],
                                "diff_t2_no_tx"=okabe_ito[3]
                     ),
                     labels = c("diff_t1_no_tx"="Tratamiento 1 vs base",
                                "diff_t2_no_tx"="Tratamiento 2 vs base"
                     )
  ) + 
  geom_hline(yintercept = 0, linetype="dashed", color="black")
plot_diff_res
ggsave("images/plot_diff_res_unificado.svg",
       plot_diff_res, width = 7, #width = 14,
       height = 4, units = "in", dpi = 300)


# +++++++++++++++++++++++++++++
# ANÁLISIS DE SENSIBILIDAD DSA (determinístico)
# +++++++++++++++++++++++++++++

# se definen los límites


def_dsa <- define_dsa(
  p_muerte_crisis,    0.05, 0.25, # original 0.15
  p_muerte_crisis_t1, 0.01, 0.15, # original 0.03
  p_muerte_crisis_t2, 0.05, 0.25, # original 0.15
  ql_crisis,          0.20, 0.80, # original 0.65
  ql_crisis_t1,       0.20, 0.80, # original 0.88
  ql_crisis_t2,       0.20, 0.80, # original 0.5
  ql_secuela,         0.20, 0.99, # original 0.9
  ql_secuela_t1,      0.20, 0.99, # original 0.88
  ql_secuela_t2,      0.20, 0.99, # original 0.96
  t_asintomatico,     1,  20/7, # original 10 días/7 (en semanas)
  t_crisis,           1.5,    4,    # original 2
  t_secuela,          5*52, 20*52,#original (10 * 52) - (10/7) - 2
  total_cycles,       5*52, 20*52 # original 10 * 52
)

res_dsa <- run_dsa(res, dsa = def_dsa)

plot_res_dsa <- plot(res_dsa, type = "simple") + 
  xlab("Calidad de vida") +
  theme_minimal()
plot_res_dsa

ggsave("images/plot_res_dsa_simple.svg")