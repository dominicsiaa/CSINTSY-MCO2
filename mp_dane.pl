% declaring the diseases and symptoms 
diseases([dengue, pneumonia, acs, tuberculosis, uti, hypertension, scabies, asthma]).

disease(Disease) :-
    diseases(Diseases),
    member(Disease, Diseases).

symptoms_dengue([cough, fever, m_j_pain, nausea_vomit, runny_nose, dengue_rash]).
symptoms_pneumonia([cough, fever, m_j_pain, nausea_vomit, runny_nose, crackels]).
symptoms_acs([heaviness, chest_pain, doom, pain_l_s, acs_sweat]).
symptoms_tuberculosis([cough, cough_blood, night_sweats, fever, chest_pain, fatigue, long_time]).
symptoms_uti([pain_urine, f_u_urine, inc_voiding, blood_urine, pain_uti, fever_chills]).
symptoms_hypertension([long_bp]).
symptoms_scabies([rash_scabies, itches, burrows, sores, crusts]).
symptoms_asthma([hard_breathing, shortness_breath, wheezing]).

symptom_of(dengue, Symptom) :-
    symptoms_dengue(Symptoms),
    member(Symptom,Symptoms).
symptom_of(pneumonia, Symptom) :-
    symptoms_pneumonia(Symptoms),
    member(Symptom,Symptoms).
symptom_of(acs, Symptom) :-
    symptoms_acs(Symptoms),
    member(Symptom,Symptoms).
symptom_of(tuberculosis, Symptom) :-
    symptoms_tuberculosis(Symptoms),
    member(Symptom,Symptoms).
symptom_of(uti, Symptom) :-
    symptoms_uti(Symptoms),
    member(Symptom,Symptoms).
symptom_of(hypertension, Symptom) :-
    symptoms_hypertension(Symptoms),
    member(Symptom,Symptoms).
symptom_of(scabies, Symptom) :-
    symptoms_scabies(Symptoms),
    member(Symptom,Symptoms).
symptom_of(asthma, Symptom) :-
    symptoms_asthma(Symptoms),
    member(Symptom,Symptoms).

symptom(Symptom) :-
    setof(Symp, Disease^(disease(Disease), symptom_of(Disease, Symp)), Symptoms),
    member(Symptom, Symptoms).
% 
% end of declaring diseases and symptoms 
% 

% patient symptoms added to this list
:- dynamic patient_symptoms/1.
patient_symptoms([]).

% hasnt really been used, but says that if a symptom is in patient symptoms then the patient has a symptom
have_symptom(Symptom) :-
    patient_symptoms(Symptoms),
    symptom(Symptom),
    member(Symptom,Symptoms).

% based on patient symptoms, declares which diseases are still possible ()
possible(Disease) :-
    disease(Disease),
    setof(Symptom, symptom_of(Disease, Symptom), Symptoms),
    patient_symptoms(PatientSymptoms),
    subset(PatientSymptoms,Symptoms).

% just puts the possible diseases into a list
possible_diseases(Diseases) :-
    setof(Disease, possible(Disease), Diseases).

% based on possible diseases, returns the common symptoms
common_symptoms(Symptoms) :-
    possible_diseases(Diseases),
    findall(DiseaseSymptoms, (member(Disease, Diseases), findall(Symptom, symptom_of(Disease, Symptom), DiseaseSymptoms)), AllSymptoms),
    intersect_lists(AllSymptoms,Symptoms).

% based on possible diseases, returns the possible symptoms 
all_possible_symptoms(Symptoms) :-
    possible_diseases(Diseases),
    findall(DiseaseSymptoms, (member(Disease, Diseases), findall(Symptom, symptom_of(Disease, Symptom), DiseaseSymptoms)), AllSymptoms),
    flatten(AllSymptoms, Symptoms1),
    list_to_set(Symptoms1, Symptoms),
    write(Symptoms).

% intersection of lists
intersect_lists([], []).
intersect_lists([L|Ls], Intersection) :-
    foldl(intersection, Ls, L, Intersection).

% not really used
diagnose(Disease) :-
    possible(Disease),
    \+ ((possible(OtherDisease), OtherDisease \= Disease)), !.


% adds a symptom to the list patient_symptoms
add_patient_symptom(Symptom) :-
    patient_symptoms(Symptoms),
    append(Symptoms, [Symptom], NewSymptoms),
    retract(patient_symptoms(Symptoms)),
    assertz(patient_symptoms(NewSymptoms)).

% condition for ending the interview, may add more conditions 
interview_over() :-
    disease(Disease),
    aggregate_all(count, possible(Disease), Count),
    Count == 1, !.

% asking the user for a particular Symptom
ask(Symptom) :-
    format('do you have ~w? (y/n)~n', [Symptom]),
    read(Answer),
    Answer == y -> add_patient_symptom(Symptom).

% the whole loop for the whole thing 
interview() :-
    interview_over(),
    possible_symptoms(PossibleSymptoms),
    common_symptoms(CommonSymptoms),
    subtract(PossibleSymptoms, CommonSymptoms, SymptomsOI),
    nth0(0, SymptomsOI, SymptomOI),
    ask(SymptomOI),
    interview().