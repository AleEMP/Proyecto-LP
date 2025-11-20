
-record(card, {
    type,       
    color,     
    name,       
    quantity   
}).

-record(organ_slot, {
    state = 0, 
    cards = [] 
}).

% Colors
-define(RED,     red).
-define(GREEN,   green).
-define(BLUE,    blue).
-define(YELLOW,  yellow).
-define(WILD,    wild).  
-define(NONE,    none).  

% Types
-define(T_ORGAN,     organ).
-define(T_VIRUS,     virus).
-define(T_MEDICINE,  medicine).
-define(T_TREATMENT, treatment).

% Organ Names
-define(N_HEART,   heart).
-define(N_STOMACH, stomach).
-define(N_BRAIN,   brain).
-define(N_BONE,    bone).
-define(N_WILD,    any_organ).

% Virus Names
-define(N_HEART_VIRUS,   heart_virus).
-define(N_STOMACH_VIRUS, stomach_virus).
-define(N_BRAIN_VIRUS,   brain_virus).
-define(N_BONE_VIRUS,    bone_virus).
-define(N_WILD_VIRUS,    any_organ_virus).

% Medicine Names
-define(N_HEART_MEDICINE,   heart_medicine).
-define(N_STOMACH_MEDICINE, stomach_medicine).
-define(N_BRAIN_MEDICINE,   brain_medicine).
-define(N_BONE_MEDICINE,    bone_medicine).
-define(N_WILD_MEDICINE,    any_organ_medicine).

% Specific Treatment Names
-define(N_CONTAGION,        contagion).
-define(N_ORGAN_THIEF,      organ_thief).
-define(N_TRANSPLANT,       transplant).           
-define(N_LATEX_GLOVE,      latex_glove).     
-define(N_MEDICAL_MISTAKE,  medical_mistake). 

-define(STARTING_HAND_SIZE, 3).
-define(MIN_PLAYERS, 2).
-define(MAX_PLAYERS, 6).
-define(ORGAN_COLORS, [?RED, ?GREEN, ?BLUE, ?YELLOW, ?WILD]).